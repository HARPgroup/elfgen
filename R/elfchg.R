#' Calculate Net change in Richness From A Percent Reduction In Flow
#' @description function for calculating change in richness from streamflow reduction
#' @param stats a dataframe of ELF model statistics
#' @param yaxis_thresh y-axis threshold used for plotting maximum y-axis limit
#' @param xlabel used to overwrite default x-axis label
#' @param ylabel used to overwrite default y-axis label
#' @return richness.change
#' @import scales
#' @export elfchg
elfchg <- function(stats,yaxis_thresh,xlabel = FALSE,ylabel = FALSE) {
  its <- seq(1, 500, 0.01)
  #its <- c(100, 200, 300, 400, 500)
  pct_list <- c(5, 10, 20, 30, 40, 50)

  table <- data.frame("xvalues" = its,
                      stringsAsFactors = FALSE)


  #i <- 1
  for (i in 1:length(pct_list)) {
    pct <- pct_list[i]
    elfchg.percent <- richness_change(stats, pct, its)

    table_i = data.frame(elfchg.percent)
    names(table_i) <- c(paste("pct_chg_", pct_list[i], sep = ""))
    table <- cbind(table, table_i)
  }


  # default ymax if none provided
  if (missing(yaxis_thresh)) {
    yaxis_thresh <- 100
  }

  print(yaxis_thresh)

 ####################################################
  #library(scales)


 # ptitle <- paste("Watershed: ",stats$watershed,"\n",sep="");
 # ptitle <- paste("Change in ",ylabel,sep="")


  xaxis_title <- paste("Mean Annual Flow (ft3/s)",sep="")
  yaxis_title <- paste("Fish Species Richness",sep="")
  if (xlabel != FALSE) {xaxis_title <- xlabel}
  if (ylabel != FALSE) {yaxis_title <- ylabel}

  ptitle <- paste("Change in ",yaxis_title," at Various Percent Flow Reductions","\n","Watershed: ",stats$watershed,"\n",sep="")

  xaxis_title <- paste("\n",xaxis_title,sep="")
  yaxis_title <- paste("Percent Decrease in","\n",yaxis_title,"\n",sep="")



  print("Generating Plot Image...")

  xvalues <- NULL # Fixes NOTE: no visible binding for global variable
  pct_chg_50 <- NULL # Fixes NOTE: no visible binding for global variable
  pct_chg_40 <- NULL # Fixes NOTE: no visible binding for global variable
  pct_chg_30 <- NULL # Fixes NOTE: no visible binding for global variable
  pct_chg_20 <- NULL # Fixes NOTE: no visible binding for global variable
  pct_chg_10 <- NULL # Fixes NOTE: no visible binding for global variable
  pct_chg_5 <- NULL # Fixes NOTE: no visible binding for global variable

  result <- ggplot(table , aes(x=xvalues, y=pct_chg_50)) +

    geom_line(data = table , aes(x=xvalues,y=pct_chg_50,color = "black")) +
    geom_line(data = table , aes(x=xvalues,y=pct_chg_40,color = "blue")) +
    geom_line(data = table , aes(x=xvalues,y=pct_chg_30,color = "green"))+
    geom_line(data = table , aes(x=xvalues,y=pct_chg_20,color = "red"))+
    geom_line(data = table , aes(x=xvalues,y=pct_chg_10,color = "violet"))+
    geom_line(data = table , aes(x=xvalues,y=pct_chg_5,color = "wheat"))+

    scale_color_manual(
      "Flow Reduction",
      values=c("black","blue","forestgreen","red","darkmagenta","sienna4"),
      labels=c("50%","40%","30%","20%","10%","5%")
    ) +

    ylim(0,yaxis_thresh) +

    scale_x_log10(
      #limits = c(0.1,500),
      limits = c(1,500),
      breaks = trans_breaks("log", function(x) {10^x})
      #breaks = trans_breaks("log10", function(x) {10^x})#,
      #labels = trans_format("log10", math_format(10^.x))
      #labels = round(x,digits = 0)
    ) +
    annotation_logticks(sides = "b")+

    #scale_y_continuous(limits = c(0, yaxis_thresh))+

    ggtitle(ptitle)+
    labs(x=xaxis_title,y=yaxis_title)+
    theme(axis.text.x = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"))

  ####################################################



 # return(elfchg.percent)
  return(result)
}
