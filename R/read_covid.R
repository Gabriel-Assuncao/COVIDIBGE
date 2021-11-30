#' Read PNAD COVID19 microdata
#' @description This function reads PNAD COVID19 microdata.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param microdata A comma-separated values file containing microdata from PNAD COVID19 survey, available on official website: (select a microdata file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Dados/}.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @return A tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{get_covid} for downloading, labeling, deflating and creating survey design object for PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_labeller} for labeling categorical variables from PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 toy example files.
#' @examples
#' data_path <- covid_example(path="exampledata.csv")
#' covid.df <- read_covid(microdata=data_path, vars=c("C001","C002"))
#' @export

read_covid <- function(microdata, vars = NULL) {
  data_covid <- suppressWarnings(utils::read.csv(microdata, header=TRUE, sep=",", dec="."))
  if (!is.null(vars)) {
    if (any(!(vars %in% colnames(data_covid)))) {
      missvar <- vars[!(vars %in% colnames(data_covid))]
      message(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    keeps <- intersect(names(data_covid), c("Ano", "UF", "UPA", "ID_DOMICILIO", "Estrato", "V1008", "V1012", "V1013", "V1030", "V1031", "V1032", "posest", "A001", vars))
    data_covid <- data_covid[,names(data_covid) %in% keeps]
  }
  label <- c("Ano", "UF", "CAPITAL", "RM_RIDE", "UPA", "ID_DOMICILIO", "Estrato", "V1008", "V1012", "V1013", "V1016", "V1022", "V1023", "posest",
             "A001A", "A003", "A004", "A005", "A006", "A006A", "A006B", "A006C", "A007", "A007A", "A007B", "A007C", "A008", "A009",
             "B0011", "B0012", "B0013", "B0014", "B0015", "B0016",
             "B0017", "B0018", "B0019", "B00110", "B00111", "B00112", "B00113",
             "B002", "B0031", "B0032", "B0033", "B0034", "B0035", "B0036", "B0037",
             "B0041", "B0042", "B0043", "B0044", "B0045", "B0046", "B005", "B006", "B007",
             "B008", "B009", "B009A", "B009B", "B009C", "B009D", "B009E", "B009F",
             "B0101", "B0102", "B0103", "B0104", "B0105", "B0106", "B011",
             "C001", "C002", "C003", "C004", "C005", "C006", "C007",
             "C007A", "C007B", "C007C", "C007D", "C007E", "C007F", "C009A", "C010", "C0101", "C0102", "C0103",
             "C0104", "C011A", "C011A1", "C011A2", "C012", "C013", "C014", "C015", "C016", "C017A",
             "D0011", "D0021", "D0031", "D0041",
             "D0051", "D0061", "D0071", "E001", "E0021", "E0022", "E0023", "E0024",
             "F001", "F002A1", "F002A2", "F002A3", "F002A4", "F002A5", "F0061")
  label <- intersect(names(data_covid), label)
  data_covid[label] <- lapply(data_covid[label], as.character)
  data_covid <- tibble::as_tibble(data_covid)
  data_covid <- dplyr::mutate(data_covid, ID_DOMICILIO=paste0(data_covid$UPA, data_covid$V1008))
  return(data_covid)
}
