#' Generic ELF generation
#' @description function for calculating change in richness from streamflow reduction
#' @param stats a dataframe of ELF model statistics
#' @param z decrease in flow as a percent
#' @return richness.loss
#' @export elf_change
elf_change <- function(stats, z) {
  m <- stats$m
  z <- z * 0.01

  richness.loss <- m * (log(1 / (1 - z)))
  return(richness.loss)
}
