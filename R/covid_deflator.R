#' Add deflator variables to PNAD COVID19 microdata
#' @description This function adds deflator variables to PNAD COVID19 microdata. For deflation of income variables, the documentation provided through the following address must be used: \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Documentacao/COVIDIBGE_Deflator.pdf}.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_covid A tibble of PNAD COVID19 microdata read with \code{read_covid} function.
#' @param deflator.file The deflator file for selected survey available on official website: (select the deflator zip file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Documentacao/}.
#' @return A tibble with the data provided from PNAD COVID19 survey and the deflator variables added for use.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{get_covid} for downloading, labeling, deflating and creating survey design object for PNAD COVID19 microdata.\cr \link[COVIDIBGE]{read_covid} for reading PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_labeller} for labeling categorical variables from PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 toy example files.
#' @examples
#' # Using data read from disk
#' data_path <- covid_example(path="exampledata.csv")
#' dictionary.path <- covid_example(path="dictionaryexample.xls")
#' deflator.path <- covid_example(path="deflatorexample.xls")
#' covid.df <- read_covid(microdata=data_path, vars=c("C001","C002"))
#' covid.df <- covid_labeller(data_covid=covid.df, dictionary.file=dictionary.path)
#' covid.df <- covid_deflator(data_covid=covid.df, deflator.file=deflator.path)
#' \donttest{
#' # Downloading data
#' covid.df2 <- get_covid(year=2020, month=5, vars=c("C001","C002"),
#'                        labels=TRUE, deflator=FALSE, design=FALSE, savedir=tempdir())
#' deflator.path2 <- covid_example(path="deflatorexample.xls")
#' covid.df2 <- covid_deflator(data_covid=covid.df2, deflator.file=deflator.path2)}
#' @export

covid_deflator <- function(data_covid, deflator.file) {
  if (sum(class(data_covid) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("Ano", "V1013", "UF") %in% names(data_covid)))) {
      data_covid <- data_covid[, !names(data_covid) %in% c("Habitual", "Efetivo", "CO3"), drop=FALSE]
      deflator <- suppressMessages(readxl::read_excel(deflator.file))
      colnames(deflator)[c(1:3)] <- c("Ano", "V1013", "UF")
      if (class(data_covid$Ano) == "integer") {
        deflator$Ano <- as.integer(deflator$Ano)
      }
      else {
        deflator$Ano <- as.character(as.integer(deflator$Ano))
      }
      if (class(data_covid$V1013) == "integer") {
        deflator$V1013 <- as.integer(deflator$V1013)
      }
      else {
        deflator$V1013 <- as.character(as.integer(deflator$V1013))
      }
      if (class(data_covid$UF) == "integer") {
        deflator$UF <- as.integer(deflator$UF)
      }
      else {
        deflator$UF <- as.factor(deflator$UF)
        if (identical(intersect(levels(deflator$UF), levels(as.factor(data_covid$UF))), character(0)) & length(levels(deflator$UF)) == length(levels(as.factor(data_covid$UF)))) {
          levels(deflator$UF) <- levels(as.factor(data_covid$UF))
        }
      }
      data_covid <- merge(x=data_covid, y=deflator, by.x=c("Ano", "V1013", "UF"), by.y=c("Ano", "V1013", "UF"), all.x=TRUE, all.y=FALSE)
      if (!(FALSE %in% (c("ID_DOMICILIO") %in% names(data_covid)))) {
        data_covid <- data_covid[order(data_covid$Estrato, data_covid$ID_DOMICILIO, data_covid$A001),]
      }
      else {
        data_covid <- data_covid[order(data_covid$Estrato, data_covid$UPA, data_covid$V1008, data_covid$A001),]
      }
      data_covid <- tibble::as_tibble(data_covid)
    }
    else {
      message("Merge variables required for adding deflator variables are missing.")
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so adding deflator variables is not possible.")
  }
  return(data_covid)
}
