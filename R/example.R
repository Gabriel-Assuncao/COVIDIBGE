#' Get the path of the PNAD COVID19 example files
#' @description This function provides the path of the microdata from month 5 of year 2020 of the PNAD COVID19 example files, loaded with this package.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param path Name of file. If \code{NULL}, the PNAD COVID19 example files names will be listed.
#' @return A vector with names of all the available PNAD COVID19 example files or the path for specific requested PNAD COVID19 example file.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[COVIDIBGE]{get_covid} for downloading, labelling, deflating and creating survey design object for PNAD COVID19 microdata.\cr \link[COVIDIBGE]{read_covid} for reading PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_labeller} for labelling categorical variables from PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_deflator} for adding deflator variables to PNAD COVID19 microdata.\cr \link[COVIDIBGE]{covid_design} for creating PNAD COVID19 survey design object.
#' @examples
#' covid_example()
#' covid_example(path="exampledata.csv")
#' covid_example(path="dictionaryexample.xls")
#' covid_example(path="deflatorexample.xls")
#' @export

covid_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package="COVIDIBGE"))
  }
  else {
    system.file("extdata", path, package="COVIDIBGE", mustWork=TRUE)
  }
}
