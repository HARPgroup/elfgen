#' Generic ELF generation
#' @description YMAX method function
#' @param watershed.df a dataframe of sites with ecological and hydrologic data
#' @return breakpt
#' @export bkpt_ymax
bkpt_ymax <- function(watershed.df) {

  watershed.df.raw <- watershed.df

  #RENAME COLUMNS TO HAVE GENERIC NAMES
  colnames(watershed.df)[1] <- "x_var"
  colnames(watershed.df)[2] <- "y_var"
  colnames(watershed.df)[3] <- "watershed"

  x <- watershed.df$x_var
  y <- watershed.df$y_var

  ymax <- max(y) #finds the max y-value
  x.ymax <- subset(watershed.df, watershed.df$y_var == ymax)
  breakpt <- min(x.ymax$x_var)

  print(paste("Breakpoint identified at",breakpt,sep=" "))

  return(breakpt)
}
