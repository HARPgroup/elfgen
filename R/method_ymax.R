#' Generic ELF generation
#' @description YMAX method function
#' @param watershed.df
#' @return breakpt
#' @export method_ymax
method_ymax <- function(watershed.df) {

  watershed.df.raw <- watershed.df

  #RENAME FLOW AND RICHNESS COLUMNS TO HAVE GENERIC NAMES
  colnames(watershed.df)[1] <- "x_var"
  colnames(watershed.df)[2] <- "y_var"

  x <- watershed.df$x_var
  y <- watershed.df$y_var

  ymax <- max(y) #finds the max y-value
  x.ymax <- subset(watershed.df, watershed.df$y_var == ymax)
  breakpt <- min(x.ymax$x_var)

  return(breakpt)
}
