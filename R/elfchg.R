#' Calculate Net change in Richness From A Percent Reduction In Flow
#' @description function for calculating change in richness from streamflow reduction
#' @param stats a dataframe of ELF model statistics
#' @param z decrease in flow as a percent
#' @param xval x-value for assessing percent change in richness
#' @return richness.change
#' @export elfchg
elfchg <- function(stats) {
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


 ####################################################
  library(scales)

  #ptitle <- paste("Change in ",biometric_title," at Various % Flow Reductions","\n", Feature.Name," (",startdate," to ",enddate,")\n",title_projname," grouping",sep="")
  #xaxis_title <- paste("\n",flow_title,sep="");
  #yaxis_title <- paste("% Decrease in ", biometric_title,"\n", sep="");
  ptitle <- "TEST"
  xaxis_title <- "TEST"
  yaxis_title <- "TEST"

  plt2 <- ggplot(table , aes(x=xvalues, y=pct_chgs_20)) +

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

    scale_x_log10(
      #limits = c(0.1,500),
      limits = c(1,500),
      breaks = trans_breaks("log", function(x) {10^x})
      #breaks = trans_breaks("log10", function(x) {10^x})#,
      #labels = trans_format("log10", math_format(10^.x))
      #labels = round(x,digits = 0)
    ) +
    annotation_logticks(sides = "b")+

    scale_y_continuous(limits = c(0, 100))+

    ggtitle(ptitle)+
    labs(x=xaxis_title,y=yaxis_title)+
    theme(axis.text.x = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"))

  ####################################################



  return(elfchg.percent)
}
