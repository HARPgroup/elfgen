library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);
#library(smwrStats);

elf_quantreg <- function(
  inputs, data, x_metric_code, y_metric_code, ws_ftype_code, 
  Feature.Name_code, Hydroid_code, search_code, token, 
  startdate, enddate, geom){

  a = FALSE
  
  #Load inputs
  x_metric <- x_metric_code
  y_metric <- y_metric_code
  Feature.Name <- Feature.Name_code
  Hydroid <- Hydroid_code
  ws_ftype <- ws_ftype_code
  pct_chg <- inputs$pct_chg 
  save_directory <- inputs$save_directory 
  target_hydrocode <- inputs$target_hydrocode
  quantile <- inputs$quantile  
  xaxis_thresh <- inputs$xaxis_thresh
  send_to_rest <- inputs$send_to_rest
  offset <- inputs$offset
  analysis_timespan <- inputs$analysis_timespan
  station_agg <- inputs$station_agg
  site <- inputs$site
  sampres <- inputs$sampres
  ghi <- inputs$ghi
  ghi_var = inputs$ghi_var
  dataset_tag <- inputs$dataset_tag
  
  #Retain a copy of the full dataset in order to include grey points on plot
  full_dataset <- data

  #remove datapoints greater than the ghi threshold
  if (ghi_var == '' || ghi_var == 'auto') {
    print (ghi_var)
    if (x_metric == 'nhdp_drainage_sqmi') {
      ghi_var <- 'drainage_area_sqmi'
    } else {
      ghi_var <- 'qmean_annual'
    }
  }
  
  data<-data[!(data[ghi_var] > (ghi)),]
  subset_n <- length(data$y_value)

  #If statement needed in case there are fewer than 4 datapoints to the left of x-axis inflection point, or if there are more than 3 points but all have the same x_value
  duplicates <- unique(data$x_value, incomparables = FALSE)
  if(nrow(data) && length(duplicates) > 3) {  
    
          up90 <- rq(y_value ~ log(x_value),data = data, tau = quantile) #calculate the quantile regression
          newy <- c(log(data$x_value)*coef(up90)[2]+coef(up90)[1])            #find the upper quantile values of y for each value of DA based on the quantile regression
          upper.quant <- subset(data, data$y_value > newy)                        #create a subset of the data that only includes the stations with NT values higher than the y values just calculated
          
print(paste("Upper quantile has ", nrow(upper.quant), "values"));
          #If statement needed in case there ae fewer than 4 datapoints in upper quantile of data set
          if (nrow(upper.quant) > 3) {

            regupper <- lm(y_value ~ log(x_value),data = upper.quant)  
            ru <- summary(regupper)                                                  #regression for upper quantile
            #print(ru)
            #print(ru$coefficients)
            #print(nrow(ru$coefficients))
            
            ru_pred <- predict(regupper)
            ####################################################
            #############JLR#######################################
            #Added by JLR to include prediction intervals
#            upper.quant_tab <- data.frame(upper.quant)
#            predict(regupper, interval="confidence") 
#            a <- predict(regupper, interval="confidence")
            
#            predict(regupper, interval="prediction") 
#            p <- predict(regupper, interval="prediction")
            
#            conf_table = data.frame(a)
#            names(conf_table) [1] <- "conf_fit"
#            names(conf_table) [2] <-  'conf_lwr'
#            names(conf_table) [3] <-  'conf_upr'
            
#            pred_table = data.frame(p)
#            names(pred_table) [1] <- "pred_fit"
#            names(pred_table) [2] <-  'pred_lwr'
#            names(pred_table) [3] <-  'pred_upr'
            
#            plus_minus <- round(((pred_table$pred_upr - pred_table$pred_lwr)/2), 1) #plus or minus this value
#            plus_minus_table = data.frame(plus_minus)
            
            # press <- press(regupper) 
            # press_table = data.frame(press)
            # names(press_table)[1] <- "PRESS"
            # 
            # Conf_Pred_table <- cbind(upper.quant_tab, conf_table, pred_table, plus_minus_table, press_table) #
            # 
            # out_name <- paste(search_code,"fe_quantreg",x_metric,y_metric,quantile,station_agg,sampres,analysis_timespan, sep='_');
            # print(paste("Exporting Prediction interval table "));
            # write.csv(Conf_Pred_table, file = paste(save_directory,"/",out_name,"_Conf_Pred_information",".csv", sep=""), row.names = F, quote = FALSE)
            # 

          ##################################
            # #REG UPPER Diagnostics code
            # library(relimp, pos=28)
            # outlierTest(regupper)
            # qqPlot(regupper, simulate=TRUE, id.method="y", id.n=2)
            # 
            # oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
            # diagPlot <- plot(regupper)
            # par(oldpar)
            # 
            # ShapResult<- shapiro.test(residuals (regupper))
            # print(ShapResult)
            ####################JLR###############
            
            
            #If statement needed in case slope is "NA"
            if (nrow(ru$coefficients) > 1) {
            
            ruint <- round(ru$coefficients[1,1], digits = 6)                         #intercept 
            ruslope <- round(ru$coefficients[2,1], digits = 6)                       #slope of regression
            rurs <- round(ru$r.squared, digits = 6)                                  #r squared of upper quantile
            rursadj <- round(ru$adj.r.squared, digits = 6)                           #adjusted r squared of upper quantile
            rup <- round(ru$coefficients[2,4], digits = 6)                           #p-value of upper quantile
            rucor <-round(cor.test(log(upper.quant$x_value),upper.quant$y_value)$estimate, digits = 6) #correlation coefficient of upper quantile
            rucount <- length(upper.quant$y_value)
            regfull <- lm(y_value ~ log(x_value),data = data)            
            rf <- summary(regfull)                                                   #regression for full dataset
            rfint <- round(rf$coefficients[1,2], digits = 6)                         #intercept 
            rfslope <- round(rf$coefficients[2,1], digits = 6)                       #slope of regression
            rfrs <- round(rf$r.squared, digits = 6)                                  #r squared of full dataset linear regression
            rfp <- round(rf$coefficients[2,4], digits = 6)                           #p-value of full dataset
            rfcor <- round(cor.test(log(data$x_value),data$y_value)$estimate, digits = 6) #correlation coefficient of full dataset
            rfcount <- length(data$y_value) 
            
            #Set yaxis threshold = to maximum biometric value in database 
            yaxis_thresh <- paste(site,"/elfgen_maxy_export/",y_metric, sep="")
            yaxis_thresh <- read.csv(yaxis_thresh , header = TRUE, sep = ",")
            yaxis_thresh <- yaxis_thresh$y_value
            print (paste("Setting ymax = ", yaxis_thresh));

            #retreive metric varids and varnames
            metric_definitions <- paste(site,"/?q=/fe_metric_export",sep="");
            metric_table <- read.table(metric_definitions,header = TRUE, sep = ",")
            
            biometric_row <- which(metric_table$varkey == y_metric)
            biomeric_name <- metric_table[biometric_row,]
            biometric_title <- biomeric_name$varname                #needed for human-readable plot titles

            flow_row <- which(metric_table$varkey == x_metric)
            flow_name <- metric_table[flow_row,]
            flow_title <- flow_name$varname                         #needed for human-readable plot titles

admincode <-paste(Hydroid,"_fe_quantreg",sep="");

        # stash the regression statistics using REST  
           if (send_to_rest == 'YES') {

            qd <- list(
              featureid = Hydroid,
              admincode = admincode,
              name = paste( "Quantile Regression, ", y_metric, ' = f( ', x_metric, ' ), upper ',quantile * 100, '%', sep=''),
              ftype = 'fe_quantreg',
              site = site,
              x = x_metric,
              y = y_metric,
              stats = list(
                stat_quantreg_m = ruslope,
                stat_quantreg_b = ruint,
                stat_quantreg_n = rucount,
                stat_quantreg_p = rup,
                stat_quantreg_rsq = rurs,
                stat_quantreg_adj_rsq = rursadj,
                stat_quantreg_qu = quantile,
                stat_quantreg_x = x_metric,
                stat_quantreg_y = y_metric,
                station_agg =station_agg,
                sampres = sampres,
                stat_quantreg_bkpt = ghi,
                stat_quantreg_glo = 0,
                stat_quantreg_ghi = ghi,
                analysis_timespan = analysis_timespan,
                dataset_tag = dataset_tag
              )
            );
print("Storing quantile regression.");
            adminid <- elf_store_data(qd, token, inputs, adminid)
           } else {
            #Plot images are stored using watershed hydrocode when NOT performing REST 
            adminid <- paste(dataset_tag,search_code,"fe_quantreg",x_metric,y_metric,quantile,station_agg,sampres,analysis_timespan,"0",ghi,ghi_var, sep='_');
           }

            #Display only 3 significant digits on plots
            plot_ruslope <- signif(ruslope, digits = 3)
            plot_ruint <- signif(ruint, digits = 3)
            plot_rurs <- signif(rurs, digits = 3)
            plot_rursadj <- signif(rursadj, digits = 3)
            plot_rup <- signif(rup, digits = 3)

            #Plot titles
            plot_title <- paste(Feature.Name," (",sampres," grouping)\n",
                                startdate," to ",
                                enddate,"\n\nQuantile Regression: (breakpoint at ",ghi_var," = ", ghi,")",
                                sep="");
            xaxis_title <- paste(flow_title,"\n","\n","m: ",plot_ruslope,"    b: ",plot_ruint,"    r^2: ",plot_rurs,"    adj r^2: ",plot_rursadj,"    p: ",plot_rup,"\n","    Upper ",((1 - quantile)*100),"% n: ",rucount,"    Data Subset n: ",subset_n,"    Full Dataset n: ",length(full_dataset$y_value),sep="");
            yaxis_title <- paste(biometric_title);
            EDAS_upper_legend <- paste("Data Subset (Upper ",((1 - quantile)*100),"%)",sep="");
            Reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");       
            Quantile_Legend <- paste(quantile," Quantile (Data Subset)",sep=""); 
            EDAS_lower_legend <- paste("Data Subset (Lower ",(100-((1 - quantile)*100)),"%)",sep="");

print (paste("Plotting ELF"));
            
            my.plot <- function() {
            # START - plotting function
              result <- base.plot(geom, data, full_dataset, upper.quant,
                                  yaxis_thresh, quantile,
                                  plot_title, xaxis_title, yaxis_title,
                                  EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend)
            
              #UNCOMMENT THE FOLLOWING TO INCLUDE CI AND PI
              #result <- result + 
              #  geom_point(data = Conf_Pred_table, aes(x=x_value,y=conf_upr,color = "yellow1"), shape=43, size=1.5) + 
              #  geom_point(data = Conf_Pred_table, aes(x=x_value,y=conf_lwr,color = "yellow2"), shape=43, size=1.5) + 
              #  geom_point(data = Conf_Pred_table, aes(x=x_value,y=pred_upr,color = "yellow3"), shape=2, size=0.5) + 
              #  geom_point(data = Conf_Pred_table, aes(x=x_value,y=pred_lwr,color = "yellow4"), shape=2, size=0.5) +
              #  scale_color_manual("Legend",values=c("gray66","forestgreen","blue","orange","black","red","orange3","orange3","turquoise1","turquoise1"),
              #                              labels=c("Full Dataset",EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend,"Regression (Data Subset)","conf_upr","conf_lwr","pred_upr","pred_lwr")) + 
              #  guides(colour = guide_legend(override.aes = list(size=c(1,1,1,1,1,1,2,2,1,1),linetype=c(0,0,0,1,1,1,0,0,0,0),shape=c(16,16,16,NA,NA,NA,43,43,2,2)),label.position = "right")); 
              }
            
            a <- my.plot()
            print(class(a)) 
            
            # END plotting function
              filename <- paste(adminid,"elf.png", sep="_")
              ggsave(file=filename, path = save_directory, width=8, height=6)

              ###########################################################################################
              ###########################################################################################
              # result <- base.map(geom, data, full_dataset, upper.quant,
              #                     yaxis_thresh, quantile,
              #                     plot_title, xaxis_title, yaxis_title,
              #                     EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend)
              # 
              # filename <- paste(adminid,"LOCATION_MAP.png", sep="_")
              # ggsave(file=filename, path = save_directory, width=9, height=6)
              
              ###########################################################################################
              ###########################################################################################
              
      print (paste("Plotting Barplot"));
      print (paste("ELF Slope: ",ruslope,sep="")); 
      print (paste("ELF Y-Intercept: ",ruint,sep="")); 
      if (ruslope >= 0){
        if (ruint >= 0){
          
          #slope barplot  
          # pct_inputs<- list(ruslope = ruslope, 
          #                   ruint = ruint,
          #                   biometric_title = biometric_title, 
          #                   flow_title = flow_title,
          #                   Feature.Name = Feature.Name,
          #                   pct_chg = pct_chg,
          #                   startdate = startdate,
          #                   sampres =  sampres,
          #                   enddate = enddate,
          #                   x_metric = x_metric,  #aded by jen
          #                   y_metric = y_metric,  #added by Jen
          #                   search_code = search_code) #added by Jen
          # elf_pct_chg (pct_inputs)
          # 
          # filename <- paste(adminid,"pctchg.png", sep="_")
          # ggsave(file=filename, path = save_directory, width=8, height=5)
        } else {
          print (paste("Y-Intercept is negative, not generating barplot"));        
        }  
      } else {
        print (paste("Slope is negative, not generating barplot"));        
      } 
            
            } else {
              print(paste("... Skipping slope is 'NA' for ", search_code,")", sep=''));
            }   
      
          } else {
              print(paste("... Skipping (fewer than 4 datapoints in upper quantile of ", search_code,")", sep=''));
          }   
                   
        } else {
          print(paste("... Skipping (fewer than 4 datapoints to the left of x-axis inflection point in ", search_code,")", sep=''));
        }   
  return(a)
} #close function
