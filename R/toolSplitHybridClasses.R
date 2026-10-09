#' Split hybrid GEM building classes into their materials.
#'
#' GEM hybrid classes combine several materials, for example HYB(W;M) (wood and masonry). They have no structure code
#' of their own, so each hybrid row is replaced by one row per material, with equal shares of the area.
#' Each row gets the GEM macro taxonomy code of its material, so that the structure mapping applies later.
#' All other rows are returned unchanged.
#'
#' @param data Dataframe with columns Structure (GEM macro taxonomy, hybrids are HYB), TAXONOMY and TOTAL_AREA_SQM.
#' @return Dataframe with the same columns. Total area is conserved.
#' @author Bennet Weiss
toolSplitHybridClasses <- function(data) {
  # GEM macro taxonomy code of each material that occurs in hybrid classes
  materialToCode <- c(
    C = "CR", CR = "CR", M = "MUR", MUR = "MUR", MCF = "MR|MCF", MR = "MR|MCF",
    W = "W", S = "S", EU = "ADO|ST|E", ME = "OT"
  )

  # "HYB(MUR+STRUB;W)/LWAL/..." -> materials "MUR+STRUB", "W" -> main materials "MUR", "W" -> codes "MUR", "W"
  hybridCodes <- function(taxonomy) {
    materials <- strsplit(sub("^HYB\\(([^)]*)\\).*$", "\\1", taxonomy), ";")[[1]]
    mainMaterials <- sub("\\+.*$", "", materials)
    unknown <- setdiff(mainMaterials, names(materialToCode))
    if (length(unknown) > 0) {
      stop("Unknown material in hybrid class ", taxonomy, ": ", paste(unknown, collapse = ", "))
    }
    return(unname(materialToCode[mainMaterials]))
  }

  # One list entry per row: the codes of its structures.
  codes <- as.list(data$Structure)
  isHybrid <- data$Structure %in% "HYB"
  codes[isHybrid] <- lapply(data$TAXONOMY[isHybrid], hybridCodes)

  # Repeat each row once per material and divide its area.
  nMaterials <- lengths(codes)
  rowOfOutput <- rep(seq_len(nrow(data)), nMaterials)
  out <- data[rowOfOutput, , drop = FALSE]
  out$Structure <- unlist(codes)
  out$TOTAL_AREA_SQM <- out$TOTAL_AREA_SQM / nMaterials[rowOfOutput]
  return(out)
}
