#' Download, label, deflate and create survey design object for PNAD COVID19 microdata
#' @description Core function of package. With this function only, the user can download a PNAD COVID19 microdata from a month and get a sample design object ready to use with \code{survey} package functions.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param year The year of the data to be downloaded. Must be a number equal to 2020. Vector not accepted.
#' @param month The month of the year of the data to be downloaded. Must be number from 5 to 11. Vector not accepted.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @param labels Logical value. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary.
#' @param deflator Logical value. If \code{TRUE}, deflator variables will be available for use in the microdata.
#' @param design Logical value. If \code{TRUE}, will return an object of class \code{survey.design} or \code{svyrep.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @param reload Logical value. If \code{TRUE}, will re-download the files even if they already exist in the save directory. If \code{FALSE}, will be checked if the files already exist in the save directory and the download will not be performed repeatedly, be careful with coinciding names of microdata files.
#' @param curlopts A named list object identifying the curl options for the handle when using functions from \code{RCurl} package.
#' @param savedir Directory to save the downloaded data. Default is to use a temporary directory.
#' @return An object of class \code{survey.design} or \code{svyrep.design} with the data from PNAD COVID19 and its sample design, or a tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{read_covid} for reading PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_labeller} for labeling categorical variables from PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 toy example files.
#' @examples
#' \donttest{
#' covid.svy <- get_covid(year=2020, month=5, vars=c("C001","C002"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE,
#'                        reload=TRUE, curlopts=list(), savedir=tempdir())
#' # Calculating proportion of people temporarily away from work
#' if (!is.null(covid.svy)) survey::svymean(x=~C002, design=covid.svy, na.rm=TRUE)}
#' @export

get_covid <- function(year, month, vars = NULL,
                       labels = TRUE, deflator = TRUE, design = TRUE, reload = TRUE, curlopts = list(), savedir = tempdir())
{
  if (year != 2020) {
    message("Year must be equal to 2020.\n")
    return(NULL)
  }
  if (month < 5 | month > 11) {
    message("Month number must be an integer from 5 to 11.\n")
    return(NULL)
  }
  if (!(labels %in% c(TRUE, FALSE))) {
    labels <- TRUE
    message("Invalid value provided for labels argument, so default value TRUE was set to this argument.\n")
  }
  if (!(deflator %in% c(TRUE, FALSE))) {
    deflator <- TRUE
    message("Invalid value provided for deflator argument, so default value TRUE was set to this argument.\n")
  }
  if (!(design %in% c(TRUE, FALSE))) {
    design <- TRUE
    message("Invalid value provided for design argument, so default value TRUE was set to this argument.\n")
  }
  if (!(reload %in% c(TRUE, FALSE))) {
    reload <- TRUE
    message("Invalid value provided for reload argument, so default value TRUE was set to this argument.\n")
  }
  if (!is.list(curlopts)) {
    curlopts <- list()
    message("Invalid value provided for curlopts argument, as the value of this argument needs to be a list, so the value provided will be ignored.\n")
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    message(paste0("The directory provided does not exist, so the directory was set to '", savedir), "'.\n")
  }
  if (savedir != tempdir()) {
    printpath <- TRUE
  }
  else {
    printpath <- FALSE
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir)-1)
  }
  ftpdir <- ("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/")
  if (!projmgr::check_internet()) {
    message("The internet connection is unavailable.\n")
    return(NULL)
  }
  if (httr::http_error(httr::GET(ftpdir, httr::timeout(60)))) {
    message("The microdata server is unavailable.\n")
    return(NULL)
  }
  restime <- getOption("timeout")
  on.exit(options(timeout=restime))
  options(timeout=max(600, restime))
  ftpdata <- paste0(ftpdir, "Dados/")
  datayear <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE, .opts=curlopts)), "\n")), "<a href=[[:punct:]]")), ".zip"))
  if (month < 10) {
    dataname <- datayear[which(startsWith(datayear, paste0("PNAD_COVID_0", month, year)))]
  }
  else {
    dataname <- datayear[which(startsWith(datayear, paste0("PNAD_COVID_", month, year)))]
  }
  if (length(dataname) == 0) {
    message("Data unavailable for selected month and year.\n")
    return(NULL)
  }
  else if (length(dataname) > 1) {
    message("There is more than one file available for the requested microdata, please contact the package maintainer.\n")
    return(NULL)
  }
  else {
    dataname <- paste0(dataname, ".zip")
  }
  if (reload == FALSE & file.exists(paste0(savedir, "/", dataname))) {
    message("The reload argument was defined as FALSE and the file of microdata was already downloaded, so the download process will not execute again.\n")
  }
  else {
    utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
    if (suppressWarnings(class(try(utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir), silent=TRUE)) == "try-error")) {
      message("The directory defined to save the downloaded data is denied permission to overwrite the existing files, please clear or change this directory.\n")
      return(NULL)
    }
    if (reload == FALSE) {
      message("The definition of FALSE for the reload argument will be ignored, since the file of microdata was not downloaded yet.\n")
    }
  }
  utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
  if (month < 10) {
    microdataname <- dir(savedir, pattern=paste0("^PNAD_COVID_0", month, year, ".*\\.csv$"), ignore.case=FALSE)
  }
  else {
    microdataname <- dir(savedir, pattern=paste0("^PNAD_COVID_", month, year, ".*\\.csv$"), ignore.case=FALSE)
  }
  microdatafile <- paste0(savedir, "/", microdataname)
  microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$mtime),])[length(microdatafile)]
  data_covid <- COVIDIBGE::read_covid(microdata=microdatafile, vars=vars)
  ftpdoc <- paste0(ftpdir, "Documentacao/")
  if (labels == TRUE) {
    if (exists("covid_labeller", where="package:COVIDIBGE", mode="function")) {
      dicfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdoc, dirlistonly=TRUE, .opts=curlopts)), "\n")), "<a href=[[:punct:]]")), ".xls"))
      if (month < 10) {
        dicname <- paste0(dicfiles[which(startsWith(dicfiles, paste0("Dicionario_PNAD_COVID_0", month, year)))], ".xls")
      }
      else {
        dicname <- paste0(dicfiles[which(startsWith(dicfiles, paste0("Dicionario_PNAD_COVID_", month, year)))], ".xls")
      }
      if (reload == FALSE & file.exists(paste0(savedir, "/", dicname))) {
        message("The reload argument was defined as FALSE and the file of dictionary was already downloaded, so the download process will not execute again.\n")
      }
      else {
        utils::download.file(url=paste0(ftpdoc, dicname), destfile=paste0(savedir, "/", dicname), mode="wb")
        if (reload == FALSE) {
          message("The definition of FALSE for the reload argument will be ignored, since the file of dictionary was not downloaded yet.\n")
        }
      }
      dicfile <- paste0(savedir, "/", dicname)
      dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$mtime),])[length(dicfile)]
      data_covid <- COVIDIBGE::covid_labeller(data_covid=data_covid, dictionary.file=dicfile)
    }
    else {
      message("Labeller function is unavailable in package COVIDIBGE.\n")
    }
  }
  if (deflator == TRUE) {
    if (exists("covid_deflator", where="package:COVIDIBGE", mode="function")) {
      arcfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdoc, dirlistonly=TRUE, .opts=curlopts)), "\n")), "<a href=[[:punct:]]")), ".zip"))
      defzip <- paste0(arcfiles[which(startsWith(arcfiles, "Deflatores"))], ".zip")
      if (reload == FALSE & file.exists(paste0(savedir, "/Deflatores.zip"))) {
        message("The reload argument was defined as FALSE and the file of deflator was already downloaded, so the download process will not execute again.\n")
      }
      else {
        utils::download.file(url=paste0(ftpdoc, defzip), destfile=paste0(savedir, "/Deflatores.zip"), mode="wb")
        if (reload == FALSE) {
          message("The definition of FALSE for the reload argument will be ignored, since the file of deflator was not downloaded yet.\n")
        }
      }
      utils::unzip(zipfile=paste0(savedir, "/Deflatores.zip"), exdir=savedir)
      defname <- dir(savedir, pattern=paste0("^Deflator_PNAD_COVID.*\\.xls$"), ignore.case=FALSE)
      deffile <- paste0(savedir, "/", defname)
      deffile <- rownames(file.info(deffile)[order(file.info(deffile)$mtime),])[length(deffile)]
      data_covid <- COVIDIBGE::covid_deflator(data_covid=data_covid, deflator.file=deffile)
    }
    else {
      message("Deflator function is unavailable in package COVIDIBGE.\n")
    }
  }
  if (design == TRUE) {
    if (exists("covid_design", where="package:COVIDIBGE", mode="function")) {
      data_covid <- COVIDIBGE::covid_design(data_covid=data_covid)
    }
    else {
      message("Sample design function is unavailable in package COVIDIBGE.\n")
    }
  }
  if (printpath == TRUE) {
    message("Paths of files downloaded in this function at the save directory provided are:")
    message(paste0(list.files(path=savedir, pattern="COVID", full.names=TRUE), collapse="\n"), "\n")
  }
  return(data_covid)
}
