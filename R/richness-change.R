#' Calculate Net change in Richness From A Percent Reduction In Flow
#' @description function for calculating change in richness from streamflow reduction
#' @param stats a dataframe of ELF model statistics
#' @param z decrease in flow as a percent
#' @param xval x-value for assessing percent change in richness
#' @return richness.change
#' @export richness_change
richness_change <- function(stats, z, xval = FALSE) {
  m <- stats$m
  b <- stats$b
  z <- z * 0.01

  richness.loss <- m * (log(1 / (1 - z)))

  #print(xval)
  if (missing(xval) == FALSE){
    richness.change.percent <- richness.loss / ((m * log(xval)) + b)
    richness.change.percent <- richness.change.percent * 100
    richness.change <- richness.change.percent
  } else {
    richness.change <- richness.loss
  }


  return(richness.change)
}
