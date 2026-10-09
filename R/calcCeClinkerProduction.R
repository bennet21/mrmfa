#' Calculates global clinker production as from Andrew's 2019 paper.
#' @author Bennet Weiss
calcCeClinkerProduction <- function() {
  x <- readSource("Andrew2019")

  # convert to tonnes
  x <- x * 1e3

  x[is.na(x)] <- 0

  unit <- "tonnes (t)"
  description <- paste(
    "Annual clinker production as from",
    "Andrew, R.M., 2019. Global CO2 emissions from cement production, 1928-2018.",
    "Earth System Science Data 11, 1675-1710. https://doi.org/10.5194/essd-11-1675-2019.",
    "Data reported on https://zenodo.org/records/20397304."
  )
  note <- "dimensions: (Historic Time,Region,value)"
  output <- list(x = x, weight = NULL, unit = unit, description = description, note = note)
  return(output)
}
