#' Generic ELF generation
#' @description PIECEWISE ITERATIVE method function
#' @param watershed.df a dataframe of sites with ecological and hydrologic data
#' @param quantile a specified value for the quantile of interest - 0.95 equals the 95th percentile
#' @param glo a "guess low" value, or the lower bound of the piecewise range
#' @param ghi a "guess high" value, or the upper bound of the piecewise range
#' @return breakpt
#' @import quantreg
#' @import stats
#' @export bkpt_pwit
bkpt_pwit <- function(watershed.df,quantile,glo,ghi) {

  watershed.df.raw <- watershed.df

  #RENAME FLOW AND RICHNESS COLUMNS TO HAVE GENERIC NAMES
  colnames(watershed.df)[1] <- "x_var"
  colnames(watershed.df)[2] <- "y_var"

  # default glo if none provided
  if(missing(glo)) {
    glo <- 0
  }

  upper.quant.data <- rq(y_var ~ log(x_var),data = watershed.df, tau = quantile)
  newy <- c(log(watershed.df$x_var)*coef(upper.quant.data)[2]+coef(upper.quant.data)[1])
  upper.quant <- subset(watershed.df, watershed.df$y_var > newy)

  x <- upper.quant$x_var
  y <- upper.quant$y_var

  #set initial guess range
  breaks <- x[which(x >= glo & x <= ghi)]
  as.numeric(breaks)
  #print(breaks)

  #This is necessary in case no breaks are found
  if(length(breaks) != 0) {
    #Needed in case pwit function only locates a single break in the data
    if(length(breaks) == 1) {
      breakpt <- breaks
    }else{

      #mse <- numeric(length(breaks))
      mse <- as.numeric(length(breaks))

      for(n in 1:length(breaks)){
        piecewise1 <- lm(y ~ log(x)*(x < breaks[n]) + log(x)*(x >= breaks[n]))
        mse[n] <- summary(piecewise1)[6]
      }
      mse <- as.numeric(mse)
      #remove any breaks that are NaN
      mse[is.na(mse)] <- 100000
      breakpt <- breaks[which(mse==min(mse))]
      breakpt <- breakpt[1]
    } #end of breaks == 1 loop

    print(paste("Breakpoint identified at",breakpt,sep=" "))

  } else {
    print(paste("... Skipping (No breakpoint identified using this set of inputs for ", watershed.df$watershed.code[1],")", sep=''))
    breakpt <- "NONE IDENTIFIED"
  }
  return(breakpt)
}
