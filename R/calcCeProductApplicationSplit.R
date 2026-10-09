#' Calculate strength class distribution of concrete.
#'
#' Concrete and mortar applications are each normalized to sum to 1.
#'
#' @author Bennet Weiss
calcCeProductApplicationSplit <- function() {
  concrete <- readSource("Cao2024", subtype = "concrete_application_split")
  concrete <- toolCeNormalize(toolCeDistributionMean(concrete, "Weibull"), dim = "Product Application")
  mortar <- readSource("Cao2024", subtype = "mortar_application_split")
  mortar <- toolCeNormalize(toolCeDistributionMean(mortar, "Weibull"), dim = "Product Application")
  x <- mbind(concrete, mortar)

  weight <- toolCeCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Split of product materials by application.",
    "Data from Cao2024."
  )
  note <- "dimensions: (Region,Product Application,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
