#' Download, label, deflate and create survey design object for PNAD COVID19 microdata
#' @description Core function of package. With this function only, the user can download a PNAD COVID19 microdata from a month and get a sample design object ready to use with \code{survey} package functions.
#' @import survey readr dplyr magrittr RCurl utils timeDate readxl tibble
#' @param year The year of the data to be downloaded. Must be a number between 2020 and current year. Vector not accepted.
#' @param month The month of the year of the data to be downloaded. Must be number from 1 to 12. If the year is defined as 2020, must be a number from 5 to 12. Vector not accepted.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @param labels Logical value. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary.
#' @param deflator Logical value. If \code{TRUE}, deflator variables will be available for use in the microdata.
#' @param design Logical value. If \code{TRUE}, will return an object of class \code{survey.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @param savedir Directory to save the downloaded data. Default is to use a temporary directory.
#' @return An object of class \code{survey.design} with the data from PNAD COVID19 and its sample design, or a tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{read_covid} for reading PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_labeller} for labelling categorical variables from PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 example files.
#' @examples
#' \donttest{
#' covid.svy <- get_covid(year=2020, month=5, vars=c("C001","C002","C003"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' survey::svymean(x=~C002, design=covid.svy, na.rm=TRUE)}
#' @export

get_covid <- function(year, month, vars = NULL,
                       labels = TRUE, deflator = TRUE, design = TRUE, savedir = tempdir())
{
  if (year < 2020) {
    stop("Year must be greater or equal to 2012.")
  }
  if (year > timeDate::getRmetricsOptions("currentYear")) {
    stop("Year cannot be greater than current year.")
  }
  if (month < 1 | month > 12) {
    stop("Month number must be an integer from 1 to 12.")
  }
  if (year == 2020 & month < 5) {
    stop("If the year is defined as 2020, month must be an integer from 5 to 12.")
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    warning(paste0("The directory provided does not exist, so the directory was set to '", tempdir()), "'.")
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir)-1)
  }
  ftpdir <- ("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/")
  ftpdata <- paste0(ftpdir, "Dados/")
  ftpdoc <- paste0(ftpdir, "Documentacao/")
  datayear <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE)), "\n"))
  if (month < 10) {
    dataname <- datayear[which(startsWith(datayear, paste0("PNAD_COVID_0", month, year)))]
  }
  else {
    dataname <- datayear[which(startsWith(datayear, paste0("PNAD_COVID_", month, year)))]
  }
  if (length(dataname) == 0) {
    stop("Data unavailable for selected year.")
  }
  utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
  utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
  if (month < 10) {
    microdataname <- dir(savedir, pattern=paste0("^PNAD_COVID_0", month, year, ".*\\.csv$"), ignore.case=FALSE)
  }
  else {
    microdataname <- dir(savedir, pattern=paste0("^PNAD_COVID_", month, year, ".*\\.csv$"), ignore.case=FALSE)
  }
  microdatafile <- paste0(savedir, "/", microdataname)
  microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),])[length(microdatafile)]
  data_covid <- COVIDIBGE::read_covid(microdata=microdatafile, vars=vars)
  docfiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdoc, dirlistonly=TRUE)), "\n"))
  if (labels == TRUE) {
    if (exists("covid_labeller", where="package:COVIDIBGE", mode="function")) {
      if (month < 10) {
        dicname <- docfiles[which(startsWith(docfiles, paste0("Dicionario_PNAD_COVID_0", month, year)))]
      }
      else {
        dicname <- docfiles[which(startsWith(docfiles, paste0("Dicionario_PNAD_COVID_", month, year)))]
      }
      utils::download.file(url=paste0(ftpdoc, dicname), destfile=paste0(savedir, "/", dicname), mode="wb")
      dicfile <- paste0(savedir, "/", dicname)
      dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),])[length(dicfile)]
      data_covid <- COVIDIBGE::covid_labeller(data_covid=data_covid, dictionary.file=dicfile)
    }
    else {
      warning("Labeller function is unavailable in package COVIDIBGE.")
    }
  }
  if (deflator == TRUE) {
    if (exists("covid_deflator", where="package:COVIDIBGE", mode="function")) {
      defzip <- docfiles[which(startsWith(docfiles, "Deflatores"))]
      utils::download.file(url=paste0(ftpdoc, defzip), destfile=paste0(savedir, "/Deflatores.zip"), mode="wb")
      utils::unzip(zipfile=paste0(savedir, "/Deflatores.zip"), exdir=savedir)
      defname <- dir(savedir, pattern=paste0("^Deflator_PNAD_COVID.*\\.xls$"), ignore.case=FALSE)
      deffile <- paste0(savedir, "/", defname)
      deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),])[length(deffile)]
      data_covid <- COVIDIBGE::covid_deflator(data_covid=data_covid, deflator.file=deffile)
    }
    else {
      warning("Deflator function is unavailable in package COVIDIBGE.")
    }
  }
  if (design == TRUE) {
    if (exists("covid_design", where="package:COVIDIBGE", mode="function")) {
      data_covid <- COVIDIBGE::covid_design(data_covid=data_covid)
    }
    else {
      warning("Sample design function is unavailable in package COVIDIBGE.")
    }
  }
  return(data_covid)
}
