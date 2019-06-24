#' Aggregate to the maximum richness value at each flow value
#' @description Generic Aggregate to the maximum richness value at each flow value
#' @param watershed.df a dataframe of sites with ecological and hydrologic data
#' @return watershed.df
#' @export aggmax
aggmax <- function(watershed.df) {

  #RENAME COLUMNS TO HAVE GENERIC NAMES
  colnames(watershed.df)[1] <- "x_var"
  colnames(watershed.df)[2] <- "y_var"
  colnames(watershed.df)[3] <- "watershed"

  watershed.df <- watershed.df[order(watershed.df$x_var, watershed.df$y_var, decreasing=TRUE),]
  watershed.df <- watershed.df[!duplicated(watershed.df$x_var),]
  watershed.df <- watershed.df[order(watershed.df$x_var, watershed.df$y_var),]
  return(watershed.df)
}
