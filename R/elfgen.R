#' Generic ELF generation
#' @description Generic plotting function
#' @param watershed.df a dataframe of sites with ecological and hydrologic data
#' @param quantile a specified value for the quantile of interest - 0.95 equals the 95th percentile
#' @param breakpt a breakpoint - either user-defined or derived using elfgen breakpoint functions
#' @param yaxis_thresh y-axis threshold used for plotting maximum y-axis limit
#' @param xlabel used to overwrite default x-axis label
#' @param ylabel used to overwrite default y-axis label
#' @return the pdf image of ELF
#' @import ggplot2
#' @import quantreg
#' @export elfgen
elfgen <- function(watershed.df,quantile,breakpt,yaxis_thresh,xlabel = FALSE,ylabel = FALSE) {

  #-----------------------------------
   # watershed.df <- test.watershed.df
   # quantile <- 0.60
   # breakpt <- 526
   # yaxis_thresh <- 53
  #-----------------------------------

  # default method if none provided
  if(missing(breakpt)) {
    breakpt <- 500
  }

  watershed.df.raw <- watershed.df

  #RENAME FLOW AND RICHNESS COLUMNS TO HAVE GENERIC NAMES
  colnames(watershed.df)[1] <- "x_var"
  colnames(watershed.df)[2] <- "y_var"

  full_dataset <- watershed.df

  data <- watershed.df[!(watershed.df$x_var > breakpt),]

  #------------------------------
  # UPPER QUANTILE SUBSET
  upper.quant.data <- rq(y_var ~ log(x_var),data = data, tau = quantile)


 # upper.quant.data <- rq(y_var ~ log(x_var),data = data, tau = 0.51)

  #library(testit)
  #has_warning(rq(y_var ~ log(x_var),data = data, tau = quantile))




 # print(upper.quant.data)
#
   newy <- c(log(data$x_var)*coef(upper.quant.data)[2]+coef(upper.quant.data)[1])



#   #print(newy)
   upper.quant <- subset(data, data$y_var > newy)

   #return(upper.quant)

#   #------------------------------
#   #print(upper.quant)
#   #print(length(upper.quant[,1]))

   #Prevents: Error in ru$coefficients[2, 1] : subscript out of bound
   if(length(upper.quant[,1]) <= 2) {
     stop("Upper quantile subset contains fewer than 3 datapoints")
   }

   regupper <- lm(y_var ~ log(x_var),data = upper.quant)
   ru <- summary(regupper)

   ruint <- round(ru$coefficients[1,1], digits = 3)                         #intercept
   ruslope <- round(ru$coefficients[2,1], digits = 3)                       #slope of regression
   rurs <- round(ru$r.squared, digits = 3)                                  #r squared of upper quantile
   rursadj <- round(ru$adj.r.squared, digits = 3)                           #adjusted r squared of upper quantile
   rup <- round(ru$coefficients[2,4], digits = 3)                           #p-value of upper quantile
   rucount <- length(upper.quant$y_var)

   subset_n <- length(data$y_var)

#   ######################################################################################
#   ######################################################################################

#


   # default ymax if none provided
   if (missing(yaxis_thresh)) {
      yaxis_thresh <- max(full_dataset$y_var)
   }


#
#
#   #Plot titles
   plot_title <- paste("Watershed: ",watershed.df$watershed.code[1],"\n",
                       #startdate," to ",
                       #enddate,"\n\nQuantile Regression: (breakpoint at ",ghi_var," = ", ghi,")",
                       #"\nBreakpoint at ",breakpt,
                       sep="");



#   #is.na(xaxis_title)
#
#  #if (exists("xaxis_title") == FALSE){xaxis_title <- paste(flow_title,"\n","\n","m: ",ruslope,"    b: ",ruint,"    r^2: ",rurs,"    adj r^2: ",rursadj,"    p: ",rup,"\n","    Upper ",((1 - quantile)*100),"% n: ",rucount,"    Data Subset n: ",subset_n,"    Full Dataset n: ",length(full_dataset$y_var),sep="")}
#  #if (exists("yaxis_title") == FALSE){yaxis_title <- paste(biometric_title)}
#
#
#   #xlabel <- "Something Custom"

   #
   flow_title <- colnames(watershed.df.raw[1])
   biometric_title <- colnames(watershed.df.raw[2])

   if (xlabel != FALSE) {flow_title <- xlabel}
   if (ylabel != FALSE) {biometric_title <- ylabel}


#
   xaxis_title <- paste(flow_title,"\n","\n","m: ",ruslope,"    b: ",ruint,"    r^2: ",rurs,"    adj r^2: ",rursadj,"    p: ",rup,"\n","    Upper ",((1 - quantile)*100),"% n: ",rucount,"    Data Subset n: ",subset_n,"    Full Dataset n: ",length(full_dataset$y_var),sep="");
   yaxis_title <- paste(biometric_title);
   EDAS_upper_legend <- paste("Data Subset (Upper ",((1 - quantile)*100),"%)",sep="");
   Reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");
   Quantile_Legend <- paste(quantile," Quantile (Data Subset)",sep="");
   EDAS_lower_legend <- paste("Data Subset (Lower ",(100-((1 - quantile)*100)),"%)",sep="");

  # print(full_dataset)
   #print(quantile)
   x_var <- NULL # Fixes NOTE: no visible binding for global variable
   y_var <- NULL # Fixes NOTE: no visible binding for global variable
   #result <- ggplot(watershed.df, aes_string(x='x_var',y='y_var')) +
   #.data[[var]]
      #result <- ggplot(watershed.df, aes(x=x_var,y=y_var)) +
      result <- ggplot(full_dataset, aes(x=x_var,y=y_var)) +

       geom_point(data = full_dataset,aes(x=x_var,y=y_var, color="aliceblue")) +
       geom_point(data = data,aes(x=x_var,y=y_var, colour="blue")) +
       stat_smooth(method = "lm",fullrange=FALSE,level = .95, data = upper.quant, aes(x=x_var,y=y_var,color = "red")) +
       geom_point(data = upper.quant, aes(x=x_var,y=y_var,color = "black")) +
       geom_quantile(data = data, quantiles= quantile,show.legend = TRUE,aes(x=x_var,y=y_var, color="red")) +
       geom_smooth(data = data, method="lm",formula=y ~ x,show.legend = TRUE, aes(x=x_var,y=y_var, color="yellow"),se=FALSE) +
       geom_smooth(data = upper.quant, formula = y ~ x, method = "lm", show.legend = TRUE, aes(x=x_var,y=y_var,color = "green"),se=FALSE) +
#
      ylim(0,yaxis_thresh) +
      ggtitle(plot_title) +

      theme(
         plot.title = element_text(size = 12, face = "bold"),
         axis.text = element_text(colour = "blue"),
         panel.grid.minor.x = element_blank()
      ) +

      labs(x = xaxis_title, y = yaxis_title) +

       scale_x_log10(
          limits = c(0.001, 15000),
          breaks = c(0.001, 0.01, 0.1, 1.0, 10, 100, 1000, 10000),
          labels = c("0.001", "0.01", "0.1", "1.0", "10", "100", "1,000", "10,000")
       ) +

      annotation_logticks(sides = "b")+


      theme(legend.key = element_rect(fill = 'white')) +


#    #  #Add legend
    scale_color_manual(
      "Legend",
      values=c("gray66","forestgreen","blue","orange","black","red"),
      #values=c("gray66","gray66","gray66","gray66","gray66","gray66"),
      #labels=c("Full Dataset",EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Data Subset)")
      labels=c("Full Dataset",EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Data Subset)")
       ) +


       guides(colour = guide_legend(
          override.aes = list(
             size = c(1, 1, 1, 1, 1, 1),
             linetype = c(0, 0, 0, 1, 1, 1),
             shape = c(16, 16, 16, NA, NA, NA)
          ),
          label.position = "right"
       ))


#   filename <- paste(watershed.df$watershed.code[1],"elf.pdf", sep="_")
#  # ggsave(file=filename, path = "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/plots", width=8, height=6)

  return(result)
}
