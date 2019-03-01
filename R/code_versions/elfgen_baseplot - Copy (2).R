elfgen_baseplot <- function(watershed.df,quantile,ghi) {

  # geom, data, full_dataset, upper.quant,
  # yaxis_thresh, quantile,
  # plot_title, xaxis_title, yaxis_title,
  # EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend
  watershed.df.raw <- watershed.df
  
  colnames(watershed.df)[3] <- "y_var"
  colnames(watershed.df)[4] <- "x_var"
  
  library(quantreg) #has rq function

  
  full_dataset <- watershed.df
  
  
  ######################################################################################
  # QUANTREG SPECIFIC
  ######################################################################################
  data <- watershed.df[!(watershed.df$x_var > (ghi)),]
  
  #------------------------------
  # UPPER QUANTILE SUBSET
  upper.quant.data <- rq(y_var ~ log(x_var),data = data, tau = quantile)
  newy <- c(log(data$x_var)*coef(upper.quant.data)[2]+coef(upper.quant.data)[1])   
  upper.quant <- subset(data, data$y_var > newy)
  #------------------------------
  
  regupper <- lm(y_var ~ log(x_var),data = upper.quant) 
  
  ru <- summary(regupper) 
  
  ruint <- round(ru$coefficients[1,1], digits = 6)                         #intercept 
  ruslope <- round(ru$coefficients[2,1], digits = 6)                       #slope of regression
  rurs <- round(ru$r.squared, digits = 6)                                  #r squared of upper quantile
  rursadj <- round(ru$adj.r.squared, digits = 6)                           #adjusted r squared of upper quantile
  rup <- round(ru$coefficients[2,4], digits = 6)                           #p-value of upper quantile
  rucount <- length(upper.quant$y_var)
  
  subset_n <- length(data$y_value)
  ######################################################################################
  ######################################################################################
  
  flow_title <- colnames(watershed.df.raw[4]) 
  biometric_title <- colnames(watershed.df.raw[3]) 
  
  yaxis_thresh <- max(full_dataset$y_var)
  
  #Plot titles
  plot_title <- paste("Watershed: ",watershed.df$watershed.code[1],"\n",
                      #startdate," to ",
                      #enddate,"\n\nQuantile Regression: (breakpoint at ",ghi_var," = ", ghi,")",
                      sep="");
  xaxis_title <- paste(flow_title,"\n","\n","m: ",ruslope,"    b: ",ruint,"    r^2: ",rurs,"    adj r^2: ",rursadj,"    p: ",rup,"\n","    Upper ",((1 - quantile)*100),"% n: ",rucount,"    Data Subset n: ",subset_n,"    Full Dataset n: ",length(full_dataset$y_value),sep="");
  yaxis_title <- paste(biometric_title);
  EDAS_upper_legend <- paste("Data Subset (Upper ",((1 - quantile)*100),"%)",sep="");
  Reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");       
  Quantile_Legend <- paste(quantile," Quantile (Data Subset)",sep=""); 
  EDAS_lower_legend <- paste("Data Subset (Lower ",(100-((1 - quantile)*100)),"%)",sep="");
  
  result <- ggplot(watershed.df, aes(x=x_var,y=y_var)) + 
    ylim(0,yaxis_thresh) + 
    
     geom_point(data = full_dataset,aes(colour="aliceblue")) +
     geom_point(data = data,aes(colour="blue")) + 
     stat_smooth(method = "lm",fullrange=FALSE,level = .95, data = upper.quant, aes(x=x_var,y=y_var,color = "red")) +
     geom_point(data = upper.quant, aes(x=x_var,y=y_var,color = "black")) + 
     geom_quantile(data = data, quantiles= quantile,show.legend = TRUE,aes(color="red")) + 
     geom_smooth(data = data, method="lm",formula=y ~ x,show.legend = TRUE, aes(colour="yellow"),se=FALSE) + 
     geom_smooth(data = upper.quant, formula = y ~ x, method = "lm", show.legend = TRUE, aes(x=x_var,y=y_var,color = "green"),se=FALSE) + 
    # 
    
    ggtitle(plot_title) + 
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(colour = "blue"),
      panel.grid.minor.x = element_blank()
    ) +
    labs(x=xaxis_title,y=yaxis_title) + 
    scale_x_log10(
      limits = c(0.001,15000),
      breaks = c(0.001,0.01,0.1,1.0,10,100,1000,10000),
      labels =c("0.001","0.01","0.1","1.0","10","100","1,000","10,000")
    ) + 
    annotation_logticks(sides = "b")+
    theme(legend.key=element_rect(fill='white')) +
    #Add legend
     scale_color_manual(
       "Legend",
       values=c("gray66","forestgreen","blue","orange","black","red"),
       #labels=c("Full Dataset",EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Data Subset)")
       labels=c("Full Dataset",EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Data Subset)")
        ) + 
     guides(
       colour = guide_legend(
         override.aes = list(
           size=c(1,1,1,1,1,1),
           linetype=c(0,0,0,1,1,1), 
           shape=c(16,16,16,NA,NA,NA)
         ),
         label.position = "right"
       )
     ); 
  filename <- paste(watershed.df$watershed.code[1],"elf.pdf", sep="_")
  ggsave(file=filename, path = "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R", width=8, height=6)
  
  return(result)
}