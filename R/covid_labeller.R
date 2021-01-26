#' Label categorical variables from PNAD COVID19 microdata
#' @description This function labels categorical variables from PNAD COVID19 microdata.
#' @import survey readr dplyr magrittr RCurl utils timeDate readxl tibble
#' @param data_covid A tibble of PNAD COVID19 microdata read with \code{read_covid} function.
#' @param dictionary.file The dictionary file for selected survey available on official website: (select a dictionary xls file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Documentacao/}.
#' @return A tibble with the data provided from PNAD COVID19 survey and its categorical variables as factors with related labels.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{get_covid} for downloading, labelling, deflating and creating survey design object for PNAD COVID19 microdata.\cr \link[COVIDIBGE]{read_covid} for reading PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 example files.
#' @examples
#' # Using data read from disk
#' data_path <- covid_example(path="exampledata.csv")
#' dictionary.path <- covid_example(path="dictionaryexample.xls")
#' covid.df <- read_covid(microdata=data_path, vars="C002")
#' covid.df <- covid_labeller(data_covid=covid.df, dictionary.file=dictionary.path)
#' \donttest{
#' # Downloading data
#' covid.df2 <- get_covid(year=2020, month=5, vars="C002",
#'                        labels=FALSE, deflator=FALSE, design=FALSE, savedir=tempdir())
#' dictionary.path2 <- covid_example(path="dictionaryexample.xls")
#' covid.df2 <- covid_labeller(data_covid=covid.df2, dictionary.file=dictionary.path2)}
#' @export

covid_labeller <- function(data_covid, dictionary.file) {
  if (sum(class(data_covid) == "tbl_df") > 0) {
    dictionary <- suppressMessages(readxl::read_excel(dictionary.file))
    X__2 = X__5 = X__6 = NULL
    colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
    dictionary %<>% subset(!is.na(X__5))
    codcurrent <- dictionary$X__2
    for (i in 1:dim(dictionary)[1]) {
      if (is.na(dictionary$X__2[i])) {
        dictionary$X__2[i] <- codcurrent
      }
      else {
        codcurrent <- dictionary$X__2[i]
      }
    }
    label <- c("UF", "CAPITAL", "RM_RIDE", "V1022", "V1023",
               "A001A", "A003", "A004", "A005", "A006", "A006A", "A006B", "A006C", "A007", "A007A", "A007B", "A007C", "A008", "A009",
               "B0011", "B0012", "B0013", "B0014", "B0015", "B0016",
               "B0017", "B0018", "B0019", "B00110", "B00111", "B00112", "B00113",
               "B002", "B0031", "B0032", "B0033", "B0034", "B0035", "B0036", "B0037",
               "B0041", "B0042", "B0043", "B0044", "B0045", "B0046", "B005", "B006", "B007",
               "B008", "B009", "B009A", "B009B", "B009C", "B009D", "B009E", "B009F",
               "B0101", "B0102", "B0103", "B0104", "B0105", "B0106", "B011",
               "C001", "C002", "C003", "C004", "C005", "C0051", "C0052", "C0053", "C006", "C007",
               "C007A", "C007B", "C007C", "C007D", "C007E", "C007F", "C009A", "C010", "C0101", "C0102", "C0103",
               "C0104", "C011A", "C011A1", "C011A2", "C012", "C013", "C014", "C015", "C016", "C017A",
               "D0011", "D0021", "D0031", "D0041",
               "D0051", "D0061", "D0071", "E001", "E0021", "E0022", "E0023", "E0024",
               "F002A1", "F002A2", "F002A3", "F002A4", "F002A5", "F001", "F0061")
    label <- intersect(names(data_covid), label)
    data_covid[label] <- lapply(data_covid[label], as.character)
    notlabel <- c("Ano", "UPA", "Estrato", "V1008", "V1012", "V1013", "V1016",
                  "V1030", "V1031", "V1032", "posest",
                  "A001", "A0011", "A001B1", "A001B2", "A001B3", "A002", "A006A1", "A006B1", "A006C1", "A007A1", "A007B1", "A007C1",
                  "B00371", "C0031", "C007C1", "C007D1", "C007E1", "C007E2", "C008", "C009", "C01011", "C01012", "C01021", "C01022",
                  "C011A11", "C011A12", "C011A21", "C011A22", "C0161",
                  "D0012", "D0013", "D0022", "D0023", "D0032", "D0033", "D0042", "D0043", "D0052", "D0053", "D0062", "D0063", "D0072", "D0073", "D0074",
                  "E00241", "F0011", "F0021", "F0022", "F006",
                  "Habitual", "Efetivo")
    vars <- names(data_covid)
    varsc <- vars[sapply(data_covid, class) == "character"]
    varsf <- setdiff(varsc, notlabel)
    for (i in 1:length(varsf)) {
      if (i > 0 & varsf[i] %in% (dictionary$X__2)) {
        data_covid[varsf[i]] <- factor(suppressWarnings(as.numeric(unlist(data_covid[varsf[i]]))),
                                       levels=suppressWarnings(as.numeric(unlist(dictionary %>% subset(X__2 == varsf[i]) %>% select(X__5)))),
                                       labels=unlist(dictionary %>% subset(X__2 == varsf[i]) %>% select(X__6)))
      }
    }
  }
  else {
    warning("Sample design was already defined for microdata, so labelling categorical variables is not possible.")
  }
  return(data_covid)
}
