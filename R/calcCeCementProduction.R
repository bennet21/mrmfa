#' Calculates global cement production as from Andrew's 2019 paper.
#' @author Bennet Weiss
calcCeCementProduction <- function() {
  x <- readSource("Andrew2019Cement")

  # convert to tonnes
  x <- x * 1e3

  x <- toolInterpolate(x, type = "spline", extrapolate = FALSE, maxgap = 10)

  # Values below threshold (from spline interpolation or backcasting) are set to zero
  threshold <- 100 # tonnes (544 t (USA) is the lowest value reported in Andrew2019)
  x[!is.na(x) & x < threshold] <- 0

  # Countries whose oldest historical value is zero had no production before: fill all preceding NAs with zero
  # This is necessary as the backcasting uses the oldest n values which may not all be zero.
  years <- getYears(x, as.integer = TRUE)
  for (ctry in getItems(x, dim = 1)) {
    ctry_x <- x[ctry, ]
    non_na_years <- years[!is.na(ctry_x)]
    oldest_non_na_year <- if (length(non_na_years) > 0) non_na_years[1] else max(years)
    if (!is.na(ctry_x[, oldest_non_na_year, ]) && ctry_x[, oldest_non_na_year, ] == 0) {
      fill_years <- years[is.na(ctry_x) & years <= oldest_non_na_year]
      if (length(fill_years) > 0) x[ctry, fill_years, ] <- 0
    }
  }

  # TODO I should  base this on global available cement intensity, not just US
  # Backcast missing data in early 20th century using regional GDP and US cement intensity
  gdp <- calcOutput("CoGDP", years = getYears(x), aggregate = FALSE)
  # Complete data available for US: t cement production per unit of GDP
  us_cement_intensity <- x["USA", ] / gdp["USA", ]
  getItems(us_cement_intensity, dim = 1) <- "GLO"
  reference_cement_production <- us_cement_intensity * gdp
  x <- toolBackcastByReference(x, reference_cement_production)

  # Apply threshold again after backcasting
  x[!is.na(x) & x < threshold] <- 0

  unit <- "tonnes (t)"
  description <- paste(
    "Annual cement production as from",
    "Andrew, R. (2026). Annual global cement production data (Version 260831) [Data set]. Zenodo.",
    "https://doi.org/10.5281/zenodo.22207852."
  )
  note <- "dimensions: (Historic Time,Region,value)"
  output <- list(x = x, weight = NULL, unit = unit, description = description, note = note)
  return(output)
}
