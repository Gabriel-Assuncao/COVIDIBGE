#' Create PNAD COVID19 survey object with its sample design
#' @description This function creates PNAD COVID19 survey object with its sample design for analysis using \code{survey} package functions.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param data_covid A tibble of PNAD COVID19 microdata read with \code{read_covid} function.
#' @return An object of class \code{survey.design} with the data from PNAD COVID19 and its sample design.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{get_covid} for downloading, labelling, deflating and creating survey design object for PNAD COVID19 microdata.\cr \link[COVIDIBGE]{read_covid} for reading PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_labeller} for labelling categorical variables from PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_example} for getting the path of the PNAD COVID19 example files.
#' @examples
#' # Using data read from disk
#' data_path <- covid_example(path="exampledata.csv")
#' dictionary.path <- covid_example(path="dictionaryexample.xls")
#' deflator.path <- covid_example(path="deflatorexample.xls")
#' covid.df <- read_covid(microdata=data_path, vars="C002")
#' covid.df <- covid_labeller(data_covid=covid.df, dictionary.file=dictionary.path)
#' covid.df <- covid_deflator(data_covid=covid.df, deflator.file=deflator.path)
#' \donttest{
#' covid.svy <- covid_design(data_covid=covid.df)
#' # Calculating temporarily away from work rate
#' if (!is.null(covid.svy)) survey::svymean(x=~C002, design=covid.svy, na.rm=TRUE)}
#' \donttest{
#' # Downloading data
#' covid.df2 <- get_covid(year=2020, month=5, vars="C002",
#'                        labels=TRUE, deflator=TRUE, design=FALSE, savedir=tempdir())
#' covid.svy2 <- covid_design(data_covid=covid.df2)
#' # Calculating temporarily away from work rate
#' if (!is.null(covid.svy2)) survey::svymean(x=~C002, design=covid.svy2, na.rm=TRUE)}
#' @export

covid_design <- function(data_covid) {
  if (sum(class(data_covid) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("UPA", "Estrato", "V1030", "V1031", "posest") %in% names(data_covid)))) {
      options(survey.lonely.psu="adjust")
      data_prior <- survey::svydesign(ids=~UPA, strata=~Estrato, data=data_covid, weights=~V1031, nest=TRUE)
      popc.types <- data.frame(posest=as.character(unique(data_covid$posest)), Freq=as.numeric(unique(data_covid$V1030)))
      popc.types <- (popc.types[order(popc.types$posest),])
      data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)
    }
    else {
      message("Weight variables required for sample design are missing.")
      data_posterior <- data_covid
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so applying another design is not possible.")
    data_posterior <- data_covid
  }
  return(data_posterior)
}
