#' Generate an ELF plot for testing using compatible dataset
#' @description Given compatible dataframe of MAF and NT Total, generates
#' a ggplot for testing purposes
#' @param watershed.df dataframe containing nhdplus MAF and NT Total values
#' @return the ggplot of NT Total vs. MAF
#' @import ggplot2
#' @export elfgen_testplot
elfgen_testplot <- function (watershed.df) {

  ggplot(watershed.df, aes(x = MAF, y = NT.TOTAL.UNIQUE)) +
    geom_point(data = watershed.df) +
    scale_x_log10(
      limits = c(0.001, 15000),
      breaks = c(0.001, 0.01, 0.1, 1.0, 10, 100, 1000, 10000),
      labels = c("0.001", "0.01", "0.1", "1.0", "10", "100", "1,000", "10,000")
    )

}





