#' Read PNAD COVID19 microdata
#' @description This function reads PNAD COVID19 microdata.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param microdata A comma-separated values file containing microdata from PNAD COVID19 survey, available on official website: (select a microdata file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Dados/}.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @return A tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{get_covid} for downloading, labelling, deflating and creating survey design object for PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_labeller} for labelling categorical variables from PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 example files.
#' @examples
#' data_path <- covid_example(path="exampledata.csv")
#' covid.df <- read_covid(microdata=data_path, vars="C002")
#' @export

read_covid <- function(microdata, vars = NULL) {
  data_covid <- read.csv(microdata, header=TRUE, sep=",", dec=".")
  if (!is.null(vars)) {
    if (any(!(vars %in% colnames(data_covid)))) {
      missvar <- vars[!(vars %in% colnames(data_covid))]
      message(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    keeps <- intersect(names(data_covid), c("Ano", "UF", "UPA", "Estrato", "V1008", "V1012", "V1013", "V1030", "V1031", "posest", "A001", vars))
    data_covid <- data_covid[,names(data_covid) %in% keeps]
  }
  data_covid <- as_tibble(data_covid)
  return(data_covid)
}
