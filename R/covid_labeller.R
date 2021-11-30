#' Label categorical variables from PNAD COVID19 microdata
#' @description This function labels categorical variables from PNAD COVID19 microdata.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_covid A tibble of PNAD COVID19 microdata read with \code{read_covid} function.
#' @param dictionary.file The dictionary file for selected survey available on official website: (select a dictionary xls file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Documentacao/}.
#' @return A tibble with the data provided from PNAD COVID19 survey and its categorical variables as factors with related labels.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{get_covid} for downloading, labeling, deflating and creating survey design object for PNAD COVID19 microdata.\cr \link[COVIDIBGE]{read_covid} for reading PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 toy example files.
#' @examples
#' # Using data read from disk
#' data_path <- covid_example(path="exampledata.csv")
#' dictionary.path <- covid_example(path="dictionaryexample.xls")
#' covid.df <- read_covid(microdata=data_path, vars=c("C001","C002"))
#' covid.df <- covid_labeller(data_covid=covid.df, dictionary.file=dictionary.path)
#' \donttest{
#' # Downloading data
#' covid.df2 <- get_covid(year=2020, month=5, vars=c("C001","C002"),
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
    notlabel <- c("Ano", "UPA", "ID_DOMICILIO", "Estrato", "V1008", "V1012", "V1013", "V1016",
                  "V1030", "V1031", "V1032", "posest",
                  "A001", "A0011", "A001B1", "A001B2", "A001B3", "A002", "A006A1", "A006B1", "A006C1", "A007A1", "A007B1", "A007C1",
                  "B00371", "C0031", "C0051", "C0052", "C0053", "C007C1", "C007D1", "C007E1", "C007E2", "C008", "C009",
                  "C01011", "C01012", "C01021", "C01022", "C011A11", "C011A12", "C011A21", "C011A22", "C0161",
                  "D0012", "D0013", "D0022", "D0023", "D0032", "D0033", "D0042", "D0043", "D0052", "D0053", "D0062", "D0063", "D0072", "D0073", "D0074",
                  "E00241", "F0011", "F0021", "F0022", "F006",
                  "Habitual", "Efetivo", "CO3")
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
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so labeling categorical variables is not possible.")
  }
  return(data_covid)
}
