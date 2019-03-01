library(quantreg);
library(ggplot2);
library(ggrepel);
library(ggpmisc);
library(grid);
library(httr);
library(data.table);
library(scales);
library(rgeos); #used for geospatial processing 
library(sp); #contains SpatialPolygonsDataFrame()
library(ggsn); #used for adding scale bar and north arrow to map

library(tidyverse)
library(sf)
library(maps)

elf_run_method <- function( method, inputs, data, x_metric_code, y_metric_code, ws_ftype_code, 
                            Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate, geom
) {
  
  if(method == "quantreg") {
    print(paste("PLOTTING - method quantreg breakpoint ...",sep="")) 
    plt <- elf_quantreg (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, 
      Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "ymax") {
    print(paste("PLOTTING - method ymax quantreg breakpoint at y-max...",sep="")) 
    plt <- elf_ymax (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, 
      Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "pwit") {
    print(paste("PLOTTING - method quantreg breakpoint using piecewise function...",sep="")) 
    plt <- elf_pw_it (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, 
      Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "twopoint") {
    print(paste("PLOTTING - method two-point function...",sep=""))
    plt <- elf_twopoint (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, 
      Hydroid_code, search_code, token, startdate, enddate
    )
    return;
  }
  
  if(method == "pwit_RS") {
    print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
    plt <-  elf_pw_it_RS (
      inputs, data, x_metric_code, y_metric_code, ws_ftype_code, 
      Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate, geom
    )
    return;
  }
  
  if(method == "pw_it_RS_IFIM") {
    print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
    plt <- elf_pw_it_RS_IFIM (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)
    return(plt)
  }
  
}

elf_cleandata <- function (data, inputs, startdate = FALSE, enddate = FALSE) {
  
  #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
  data$y_value <- as.numeric(data$y_value)
  #Subset by date range 
  data$tstime <- as.Date(data$tstime,origin="1970-01-01")
  
  if (typeof(startdate) != 'logical') {
    data <- subset(data, tstime > startdate)
  }
  if (typeof(enddate) != 'logical') {
    data <- subset(data, tstime < enddate)
  }
  
  #ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW 
  data["ratio"] <- (data$drainage_area)/(data$qmean_annual)
  #REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
  data<-data[!(data$ratio > 1000),]
  
  #USE ONLY MAX NT VALUE FOR EACH STATION
  if(inputs$station_agg == "max"){ 
    aa <- data[order(data$hydrocode, data$y_value, decreasing=TRUE),]
    aa <- aa[!duplicated(aa$hydrocode),]
    aa <- aa[order(aa$hydrocode, aa$y_value),]
    data <- aa
  }
  
  #subsets data to exclude anything with a flowmetric value greater than the "xaxis_thresh" specified in the user inputs file
  data <- subset(data, x_value >= .001 & x_value < inputs$xaxis_thresh);
  
  #Export data as spreadsheet
  ##write.table(data, paste(save_directory,"data.tsv",sep=""), sep="\t")
  
  print(paste("Found ", nrow(data), sep=''));
  #If statement needed in case geographic region does not contain more than 3 points
  if(nrow(data) <= 3) {
    print("... Skipping (fewer than 3 datapoints)")
    return(FALSE)
  } 
  
  
  #Skip if there is only 1 or 2 unique flow metric values for this watershed (either only a single EDAS station, or multiple with the same flow metric, which would result in a vertical bar of points in the plot)
  station_x_value <- data$x_value
  remove_da_duplicates <- unique(station_x_value, incomparables = FALSE)
  if(length(remove_da_duplicates) == 1 | length(remove_da_duplicates) == 2) {
    print("... Skipping (the points are all organized in 1 or 2 vertical lines in )");
    return(FALSE) 
  } #closes bar of points skip if-statement (rare)
  
  #Skip if there is only 1 or 2 unique biometric values for this watershed
  station_y_value <- data$y_value
  remove_metric_duplicates <- unique(station_y_value, incomparables = FALSE)
  if(length(remove_metric_duplicates) == 1 | length(remove_metric_duplicates) == 2) {
    print("... Skipping (the points are all organized in 1 or 2 horizontal lines in )");
    return(FALSE) 
  } #closes bar of points skip if-statement (rare)
  return(data)
  
}

elf_upper <- function(data, quantile) {
  upper <- rq(y_value ~ log(x_value),data = data, tau = quantile) #calculate the quantile regression
  newy <- c(log(data$x_value)*coef(upper)[2]+coef(upper)[1])            #find the upper quantile values of y for each value of DA based on the quantile regression
  upper.quant <- subset(data, data$y_value > newy)                        #create a subset of the data that only includes the stations with NT values higher than the y values just calculated
  return(upper.quant)
} 

elf_assemble_batch <- function(inputs = list()){
  batchlist = FALSE;
  #Load inputs
  x_metric <- inputs$x_metric 
  y_metric <- inputs$y_metric 
  ws_ftype <- inputs$ws_ftype
  target_hydrocode <- inputs$target_hydrocode
  offset_x_metric <- inputs$offset_x_metric
  offset_y_metric <- inputs$offset_y_metric
  offset_ws_ftype <- inputs$offset_ws_ftype
  offset_hydrocode <- inputs$offset_hydrocode
  site <- inputs$site
  xaxis_thresh <- inputs$xaxis_thresh
  sampres <- inputs$sampres
  analysis_timespan <- inputs$analysis_timespan
  station_agg <- inputs$station_agg
  quantreg <- inputs$quantreg 
  ymax <- inputs$ymax   
  pw_it <- inputs$pw_it  
  pw_it_RS <- inputs$pw_it_RS 
  pw_it_RS_IFIM <- inputs$pw_it_RS_IFIM  
  twopoint <- inputs$twopoint
  token <- inputs$token
  
  for (l in offset_ws_ftype:length(ws_ftype)) {
    
    print(paste("ws_ftype ",l,". of ",length(ws_ftype),". ",ws_ftype[l],sep=""))
    #Automatic bundle specification (WILL BE ELIMINATED ONCE WE UPDATE VAHYDRO STORAGE SCHEME)
    if(ws_ftype[l] == "hwi_region"){
      bundle <- "ecoregion"
    } else if(ws_ftype[l] == "state") {
      bundle <- "landunit" 
    } else if(ws_ftype[l] == "ecoregion_iii") {
      bundle <- "ecoregion" 
    } else if(ws_ftype[l] == "ecoregion_iv") {
      bundle <- "ecoregion" 
    } else if(ws_ftype[l] == "ecoiii_huc6") {
      bundle <- "ecoregion" 
    } else {
      bundle <- "watershed" 
    }
    #Pull in full list of Virginia watersheds for the specified ftype
    #If we define a hydrocode > 'XXXXXX' it will retrieve that single one
    HUClist_url_base <- paste(site,"/?q=elfgen_regions_export/",bundle, sep = "");
    if (!(target_hydrocode == '')) {
      HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], target_hydrocode, sep = "/");
    } else {
      HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], sep = "/");
    }
    print(paste("Searching ", HUClist_url_full, " for target_hydrocode ", target_hydrocode, sep=''))
    #print(HUClist_url_full)
    HUClist <- read.table(HUClist_url_full,header = TRUE, sep = ",")
    Watershed_Hydrocode <- HUClist$Hydrocode
    Feature.Name <- HUClist$Feature.Name
    Hydroid <- HUClist$HydroID
    
    for (k in offset_y_metric:length(y_metric)) {
      print(paste("y_metric ", k, ". of ",length(y_metric),". Beginning loop for ", y_metric[k], sep=''));
      for (j in offset_x_metric:length(x_metric)) {
        print(paste("x_metric ", j, ". of 14. Beginning loop for ", x_metric[j], sep=''));
        for (i in offset_hydrocode:length(Watershed_Hydrocode)) {
          print(paste("Feature ", i, ". of ",length(Watershed_Hydrocode),". Searching for stations from ", Watershed_Hydrocode[i], sep=''));
          search_code <- Watershed_Hydrocode[i];
          Feature.Name_code <- as.character(Feature.Name[i]);
          Hydroid_code <- Hydroid[i];
          ws_ftype_code <- ws_ftype[l]
          x_metric_code <-  x_metric[j];
          y_metric_code <-  y_metric[k];
          if (typeof(data) == 'logical') {
            next
          }  
          
          # now, add this to a master list to return
          if (batchlist == FALSE) {
            batchlist = data.frame(
              target_hydrocode = search_code, 
              hydroid = Hydroid_code,
              name = Feature.Name_code, 
              method = inputs$method,
              ws_ftype = ws_ftype_code, 
              bundle = bundle, 
              dataset_tag = inputs$dataset_tag, 
              x_metric = x_metric_code, 
              y_metric = y_metric_code,
              sampres = sampres
            )
          } else {
            batchlist <- rbind(
              batchlist, data.frame(
                target_hydrocode = search_code, 
                hydroid = Hydroid_code,
                name = Feature.Name_code, 
                method = inputs$method,
                ws_ftype = ws_ftype_code, 
                bundle = bundle, 
                dataset_tag = inputs$dataset_tag, 
                x_metric = x_metric_code, 
                y_metric = y_metric_code,
                sampres = sampres
              )
            )
          }
        } #closes watershed for loop  
      } #closes x_metric for loop
    } #closes y_metric for loop
  } #closes ws_ftype for loop
  return(batchlist)
} #close function

elf_plot_distribution <- function(
  data,
  x_metric_code, 
  y_metric_code, 
  ws_ftype_code, 
  Feature.Name_code, 
  Hydroid_code, 
  search_code) {
  
  hist(data$y_value / log(data$x_value))
  
}

base.plot <- function(geom, data, full_dataset, upper.quant,
                      yaxis_thresh, quantile,
                      plot_title, xaxis_title, yaxis_title,
                      EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend
) {
  
  
  # SPECIFY BOUNDING BOX: *should really calculate bb from the VADF shape, but for now hard code
  bb=readWKT("POLYGON((-85 35, -74 35,  -74 41, -85 41, -85 35))")
  bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
  bbProjected@data$id <- rownames(bbProjected@data)
  bbPoints <- fortify(bbProjected, region = "id")
  bbDF <- merge(bbPoints, bbProjected@data, by = "id")
  
  #-------------------------------------------------------------------------------------------- 
  # Geoprocess edas stations 
  STATIONS_data <- full_dataset
  split_1 <- read.table(text = as.character(STATIONS_data$geom), sep = "(", colClasses = "character")
  split_2 <- read.table(text = split_1$V2, sep = ")", colClasses = "character")
  split_3 <- read.table(text = split_2$V1, sep = " ", colClasses = "character")
  STATIONSDF <- data.frame(x=as.numeric(split_3$V1),y=as.numeric(split_3$V2),X.id.="id",id="1")
  
  BLUSTATIONS_data <- data
  BLUsplit_1 <- read.table(text = as.character(BLUSTATIONS_data$geom), sep = "(", colClasses = "character")
  BLUsplit_2 <- read.table(text = BLUsplit_1$V2, sep = ")", colClasses = "character")
  BLUsplit_3 <- read.table(text = BLUsplit_2$V1, sep = " ", colClasses = "character")
  BLUSTATIONSDF <- data.frame(x=as.numeric(BLUsplit_3$V1),y=as.numeric(BLUsplit_3$V2),X.id.="id",id="1")
  
  GRNSTATIONS_data <- upper.quant
  GRNsplit_1 <- read.table(text = as.character(GRNSTATIONS_data$geom), sep = "(", colClasses = "character")
  GRNsplit_2 <- read.table(text = GRNsplit_1$V2, sep = ")", colClasses = "character")
  GRNsplit_3 <- read.table(text = GRNsplit_2$V1, sep = " ", colClasses = "character")
  GRNSTATIONSDF <- data.frame(x=as.numeric(GRNsplit_3$V1),y=as.numeric(GRNsplit_3$V2),X.id.="id",id="1")
  #--------------------------------------------------------------------------------------------
  
  # CLIP WATERSHED GEOMETRY TO BOUNDING BOX
  watershed_geom <- readWKT(geom)
  watershed_geom_clip <- gIntersection(bb, watershed_geom)
  if (is.null(watershed_geom_clip)) {
    watershed_geom_clip = watershed_geom
  }
  wsdataProjected <- SpatialPolygonsDataFrame(watershed_geom_clip,data.frame("id"), match.ID = FALSE)
  #class(dataProjected)
  wsdataProjected@data$id <- rownames(wsdataProjected@data)
  watershedPoints <- fortify(wsdataProjected, region = "id")
  watershedDF <- merge(watershedPoints, wsdataProjected@data, by = "id")
  
  #LOAD STATE GEOMETRY
  STATES <- read.table(file=paste(fxn_locations,"STATES.tsv",sep=""), header=TRUE, sep="\t")
  
  VA <- STATES[which(STATES$state == "VA"),]
  VA_geom <- readWKT(VA$geom)
  VA_geom_clip <- gIntersection(bb, VA_geom)
  VAProjected <- SpatialPolygonsDataFrame(VA_geom_clip,data.frame("id"), match.ID = TRUE)
  VAProjected@data$id <- rownames(VAProjected@data)
  VAPoints <- fortify( VAProjected, region = "id")
  VADF <- merge(VAPoints,  VAProjected@data, by = "id")
  
  TN <- STATES[which(STATES$state == "TN"),]
  TN_geom <- readWKT(TN$geom)
  TN_geom_clip <- gIntersection(bb, TN_geom)
  TNProjected <- SpatialPolygonsDataFrame(TN_geom_clip,data.frame("id"), match.ID = TRUE)
  TNProjected@data$id <- rownames(TNProjected@data)
  TNPoints <- fortify( TNProjected, region = "id")
  TNDF <- merge(TNPoints,  TNProjected@data, by = "id")
  
  NC <- STATES[which(STATES$state == "NC"),]
  NC_geom <- readWKT(NC$geom)
  NC_geom_clip <- gIntersection(bb, NC_geom)
  NCProjected <- SpatialPolygonsDataFrame(NC_geom_clip,data.frame("id"), match.ID = TRUE)
  NCProjected@data$id <- rownames(NCProjected@data)
  NCPoints <- fortify( NCProjected, region = "id")
  NCDF <- merge(NCPoints,  NCProjected@data, by = "id")
  
  KY <- STATES[which(STATES$state == "KY"),]
  KY_geom <- readWKT(KY$geom)
  KY_geom_clip <- gIntersection(bb, KY_geom)
  KYProjected <- SpatialPolygonsDataFrame(KY_geom_clip,data.frame("id"), match.ID = TRUE)
  KYProjected@data$id <- rownames(KYProjected@data)
  KYPoints <- fortify( KYProjected, region = "id")
  KYDF <- merge(KYPoints,  KYProjected@data, by = "id")
  
  WV <- STATES[which(STATES$state == "WV"),]
  WV_geom <- readWKT(WV$geom)
  WV_geom_clip <- gIntersection(bb, WV_geom)
  WVProjected <- SpatialPolygonsDataFrame(WV_geom_clip,data.frame("id"), match.ID = TRUE)
  WVProjected@data$id <- rownames(WVProjected@data)
  WVPoints <- fortify( WVProjected, region = "id")
  WVDF <- merge(WVPoints,  WVProjected@data, by = "id")
  
  MD <- STATES[which(STATES$state == "MD"),]
  MD_geom <- readWKT(MD$geom)
  MD_geom_clip <- gIntersection(bb, MD_geom)
  MDProjected <- SpatialPolygonsDataFrame(MD_geom_clip,data.frame("id"), match.ID = TRUE)
  MDProjected@data$id <- rownames(MDProjected@data)
  MDPoints <- fortify( MDProjected, region = "id")
  MDDF <- merge(MDPoints,  MDProjected@data, by = "id")
  
  DE <- STATES[which(STATES$state == "DE"),]
  DE_geom <- readWKT(DE$geom)
  DE_geom_clip <- gIntersection(bb, DE_geom)
  DEProjected <- SpatialPolygonsDataFrame(DE_geom_clip,data.frame("id"), match.ID = TRUE)
  DEProjected@data$id <- rownames(DEProjected@data)
  DEPoints <- fortify( DEProjected, region = "id")
  DEDF <- merge(DEPoints,  DEProjected@data, by = "id")
  
  PA <- STATES[which(STATES$state == "PA"),]
  PA_geom <- readWKT(PA$geom)
  PA_geom_clip <- gIntersection(bb, PA_geom)
  PAProjected <- SpatialPolygonsDataFrame(PA_geom_clip,data.frame("id"), match.ID = TRUE)
  PAProjected@data$id <- rownames(PAProjected@data)
  PAPoints <- fortify( PAProjected, region = "id")
  PADF <- merge(PAPoints,  PAProjected@data, by = "id")
  
  NJ <- STATES[which(STATES$state == "NJ"),]
  NJ_geom <- readWKT(NJ$geom)
  NJ_geom_clip <- gIntersection(bb, NJ_geom)
  NJProjected <- SpatialPolygonsDataFrame(NJ_geom_clip,data.frame("id"), match.ID = TRUE)
  NJProjected@data$id <- rownames(NJProjected@data)
  NJPoints <- fortify( NJProjected, region = "id")
  NJDF <- merge(NJPoints,  NJProjected@data, by = "id")
  
  OH <- STATES[which(STATES$state == "OH"),]
  OH_geom <- readWKT(OH$geom)
  OH_geom_clip <- gIntersection(bb, OH_geom)
  OHProjected <- SpatialPolygonsDataFrame(OH_geom_clip,data.frame("id"), match.ID = TRUE)
  OHProjected@data$id <- rownames(OHProjected@data)
  OHPoints <- fortify( OHProjected, region = "id")
  OHDF <- merge(OHPoints,  OHProjected@data, by = "id")
  
  map <- ggplotGrob(ggplot(data = VADF, aes(x=long, y=lat, group = group))+
                      geom_polygon(data = VADF, color="gray46", fill = "gray")+
                      geom_polygon(data = TNDF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = NCDF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = KYDF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = WVDF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = MDDF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = DEDF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = PADF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = NJDF, color="gray46", fill = NA, lwd=0.5)+
                      geom_polygon(data = OHDF, color="gray46", fill = NA, lwd=0.5)+
                      
                      geom_polygon(data = watershedDF, color="khaki4", fill = "yellow",alpha = 0.25,lwd=0.5)+
                      geom_point(aes(x = x, y = y, group = id), data = STATIONSDF, color="gray66", size = 0.025)+
                      geom_point(aes(x = x, y = y, group = id), data = BLUSTATIONSDF, color="blue", size = 0.025)+
                      geom_point(aes(x = x, y = y, group = id), data = GRNSTATIONSDF, color="forestgreen", size = 0.025)+
                      
                      
                      geom_polygon(data = bbDF, color="black", fill = NA,lwd=0.5)+
                      
                      #ADD NORTH ARROW AND SCALE BAR
                      north(bbDF, location = 'topleft', symbol = 12, scale=0.2)+
                      #scalebar(bbDF, dist = 100, dd2km = TRUE, model = 'WGS84',st.bottom=FALSE,st.size=1.5,st.dist=0.04)+ #text too small to read 
                      scalebar(bbDF, dist = 100, dd2km = TRUE, model = 'WGS84',st.bottom=TRUE,st.size=1.5,st.dist=0.04)+
                      
                      scale_x_continuous(limits = c(-85, -74))+
                      scale_y_continuous(limits = c(35, 41))+
                      
                      theme(axis.title.x=element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(),
                            axis.title.y=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank(),
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            panel.border = element_blank())
  )
  
  result <- ggplot(data, aes(x=x_value,y=y_value)) + ylim(0,yaxis_thresh) + 
    geom_point(data = full_dataset,aes(colour="aliceblue")) +
    geom_point(data = data,aes(colour="blue")) + 
    stat_smooth(method = "lm",fullrange=FALSE,level = .95, data = upper.quant, aes(x=x_value,y=y_value,color = "red")) +
    geom_point(data = upper.quant, aes(x=x_value,y=y_value,color = "black")) + 
    geom_quantile(data = data, quantiles= quantile,show.legend = TRUE,aes(color="red")) + 
    geom_smooth(data = data, method="lm",formula=y ~ x,show.legend = TRUE, aes(colour="yellow"),se=FALSE) + 
    geom_smooth(data = upper.quant, formula = y ~ x, method = "lm", show.legend = TRUE, aes(x=x_value,y=y_value,color = "green"),se=FALSE) + 
    
    #add map to upper right of plot
    annotation_custom(
      grob = map,
      xmin = 4.54,
      xmax = 7.72,
      ymin = yaxis_thresh-(0.1*yaxis_thresh),
      ymax = yaxis_thresh+(0.3*yaxis_thresh)
    )+
    
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
  return(result)
}


base.map <- function(geom, data, full_dataset, upper.quant,
                      yaxis_thresh, quantile,
                      plot_title, xaxis_title, yaxis_title,
                      EDAS_upper_legend,EDAS_lower_legend,Reg_upper_legend,Quantile_Legend
) {
  
  
  # SPECIFY BOUNDING BOX: 
  #extent <- data.frame(x = c(-84, -75), 
  #                     y = c(35, 41))  
  extent <- data.frame(x = c(-85.5, -74.5), 
                       y = c(35, 41))  
#    extent <- data.frame(x = c(-90, -60), 
#                         y = c(20, 60))    
  
  bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
  
  bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
  bbProjected@data$id <- rownames(bbProjected@data)
  bbPoints <- fortify(bbProjected, region = "id")
  bbDF <- merge(bbPoints, bbProjected@data, by = "id")
  
  #-------------------------------------------------------------------------------------------- 
  # Geoprocess edas stations 
  STATIONS_data <- full_dataset
  split_1 <- read.table(text = as.character(STATIONS_data$geom), sep = "(", colClasses = "character")
  split_2 <- read.table(text = split_1$V2, sep = ")", colClasses = "character")
  split_3 <- read.table(text = split_2$V1, sep = " ", colClasses = "character")
  STATIONSDF <- data.frame(x=as.numeric(split_3$V1),y=as.numeric(split_3$V2),X.id.="id",id="1")
  
  BLUSTATIONS_data <- data
  BLUsplit_1 <- read.table(text = as.character(BLUSTATIONS_data$geom), sep = "(", colClasses = "character")
  BLUsplit_2 <- read.table(text = BLUsplit_1$V2, sep = ")", colClasses = "character")
  BLUsplit_3 <- read.table(text = BLUsplit_2$V1, sep = " ", colClasses = "character")
  BLUSTATIONSDF <- data.frame(x=as.numeric(BLUsplit_3$V1),y=as.numeric(BLUsplit_3$V2),X.id.="id",id="1")
  
  GRNSTATIONS_data <- upper.quant
  GRNsplit_1 <- read.table(text = as.character(GRNSTATIONS_data$geom), sep = "(", colClasses = "character")
  GRNsplit_2 <- read.table(text = GRNsplit_1$V2, sep = ")", colClasses = "character")
  GRNsplit_3 <- read.table(text = GRNsplit_2$V1, sep = " ", colClasses = "character")
  GRNSTATIONSDF <- data.frame(x=as.numeric(GRNsplit_3$V1),y=as.numeric(GRNsplit_3$V2),X.id.="id",id="1")
  #--------------------------------------------------------------------------------------------
  
  # CLIP WATERSHED GEOMETRY TO BOUNDING BOX
  watershed_geom <- readWKT(geom)
  watershed_geom_clip <- gIntersection(bb, watershed_geom)
  if (is.null(watershed_geom_clip)) {
    watershed_geom_clip = watershed_geom
  }
  wsdataProjected <- SpatialPolygonsDataFrame(watershed_geom_clip,data.frame("id"), match.ID = FALSE)
  #class(dataProjected)
  wsdataProjected@data$id <- rownames(wsdataProjected@data)
  watershedPoints <- fortify(wsdataProjected, region = "id")
  watershedDF <- merge(watershedPoints, wsdataProjected@data, by = "id")
  
  #LOAD STATE AND River GEOMETRY
  STATES <- read.table(file=paste(hydro_tools,"GIS_LAYERS","STATES.tsv",sep="\\"), header=TRUE, sep="\t") #Load state geometries
  RIVDF <- read.table(file=paste(hydro_tools,"GIS_LAYERS","RIVDF.csv",sep="/"), header=TRUE, sep=",") #Load river geometries
  WBDF <- read.table(file=paste(hydro_tools,"GIS_LAYERS","WBDF.csv",sep="/"), header=TRUE, sep=",") #Load waterbody geometries
  
  VA <- STATES[which(STATES$state == "VA"),]
  VA_geom <- readWKT(VA$geom)
  VA_geom_clip <- gIntersection(bb, VA_geom)
  VAProjected <- SpatialPolygonsDataFrame(VA_geom_clip,data.frame("id"), match.ID = TRUE)
  VAProjected@data$id <- rownames(VAProjected@data)
  VAPoints <- fortify( VAProjected, region = "id")
  VADF <- merge(VAPoints,  VAProjected@data, by = "id")
  
  TN <- STATES[which(STATES$state == "TN"),]
  TN_geom <- readWKT(TN$geom)
  TN_geom_clip <- gIntersection(bb, TN_geom)
  TNProjected <- SpatialPolygonsDataFrame(TN_geom_clip,data.frame("id"), match.ID = TRUE)
  TNProjected@data$id <- rownames(TNProjected@data)
  TNPoints <- fortify( TNProjected, region = "id")
  TNDF <- merge(TNPoints,  TNProjected@data, by = "id")
  
  NC <- STATES[which(STATES$state == "NC"),]
  NC_geom <- readWKT(NC$geom)
  NC_geom_clip <- gIntersection(bb, NC_geom)
  NCProjected <- SpatialPolygonsDataFrame(NC_geom_clip,data.frame("id"), match.ID = TRUE)
  NCProjected@data$id <- rownames(NCProjected@data)
  NCPoints <- fortify( NCProjected, region = "id")
  NCDF <- merge(NCPoints,  NCProjected@data, by = "id")
  
  KY <- STATES[which(STATES$state == "KY"),]
  KY_geom <- readWKT(KY$geom)
  KY_geom_clip <- gIntersection(bb, KY_geom)
  KYProjected <- SpatialPolygonsDataFrame(KY_geom_clip,data.frame("id"), match.ID = TRUE)
  KYProjected@data$id <- rownames(KYProjected@data)
  KYPoints <- fortify( KYProjected, region = "id")
  KYDF <- merge(KYPoints,  KYProjected@data, by = "id")
  
  WV <- STATES[which(STATES$state == "WV"),]
  WV_geom <- readWKT(WV$geom)
  WV_geom_clip <- gIntersection(bb, WV_geom)
  WVProjected <- SpatialPolygonsDataFrame(WV_geom_clip,data.frame("id"), match.ID = TRUE)
  WVProjected@data$id <- rownames(WVProjected@data)
  WVPoints <- fortify( WVProjected, region = "id")
  WVDF <- merge(WVPoints,  WVProjected@data, by = "id")
  
  MD <- STATES[which(STATES$state == "MD"),]
  MD_geom <- readWKT(MD$geom)
  MD_geom_clip <- gIntersection(bb, MD_geom)
  MDProjected <- SpatialPolygonsDataFrame(MD_geom_clip,data.frame("id"), match.ID = TRUE)
  MDProjected@data$id <- rownames(MDProjected@data)
  MDPoints <- fortify( MDProjected, region = "id")
  MDDF <- merge(MDPoints,  MDProjected@data, by = "id")
  
  DE <- STATES[which(STATES$state == "DE"),]
  DE_geom <- readWKT(DE$geom)
  DE_geom_clip <- gIntersection(bb, DE_geom)
  DEProjected <- SpatialPolygonsDataFrame(DE_geom_clip,data.frame("id"), match.ID = TRUE)
  DEProjected@data$id <- rownames(DEProjected@data)
  DEPoints <- fortify( DEProjected, region = "id")
  DEDF <- merge(DEPoints,  DEProjected@data, by = "id")
  
  PA <- STATES[which(STATES$state == "PA"),]
  PA_geom <- readWKT(PA$geom)
  PA_geom_clip <- gIntersection(bb, PA_geom)
  PAProjected <- SpatialPolygonsDataFrame(PA_geom_clip,data.frame("id"), match.ID = TRUE)
  PAProjected@data$id <- rownames(PAProjected@data)
  PAPoints <- fortify( PAProjected, region = "id")
  PADF <- merge(PAPoints,  PAProjected@data, by = "id")
  
  NJ <- STATES[which(STATES$state == "NJ"),]
  NJ_geom <- readWKT(NJ$geom)
  NJ_geom_clip <- gIntersection(bb, NJ_geom)
  NJProjected <- SpatialPolygonsDataFrame(NJ_geom_clip,data.frame("id"), match.ID = TRUE)
  NJProjected@data$id <- rownames(NJProjected@data)
  NJPoints <- fortify( NJProjected, region = "id")
  NJDF <- merge(NJPoints,  NJProjected@data, by = "id")
  
  OH <- STATES[which(STATES$state == "OH"),]
  OH_geom <- readWKT(OH$geom)
  OH_geom_clip <- gIntersection(bb, OH_geom)
  OHProjected <- SpatialPolygonsDataFrame(OH_geom_clip,data.frame("id"), match.ID = TRUE)
  OHProjected@data$id <- rownames(OHProjected@data)
  OHPoints <- fortify( OHProjected, region = "id")
  OHDF <- merge(OHPoints,  OHProjected@data, by = "id")
  
  SC <- STATES[which(STATES$state == "SC"),]
  SC_geom <- readWKT(SC$geom)
  SC_geom_clip <- gIntersection(bb, SC_geom)
  SCProjected <- SpatialPolygonsDataFrame(SC_geom_clip,data.frame("id"), match.ID = TRUE)
  SCProjected@data$id <- rownames(SCProjected@data)
  SCPoints <- fortify( SCProjected, region = "id")
  SCDF <- merge(SCPoints,  SCProjected@data, by = "id")
  
  DC <- STATES[which(STATES$state == "DC"),]
  DC_geom <- readWKT(DC$geom)
  DC_geom_clip <- gIntersection(bb, DC_geom)
  DCProjected <- SpatialPolygonsDataFrame(DC_geom_clip,data.frame("id"), match.ID = TRUE)
  DCProjected@data$id <- rownames(DCProjected@data)
  DCPoints <- fortify( DCProjected, region = "id")
  DCDF <- merge(DCPoints,  DCProjected@data, by = "id")
  
  IN <- STATES[which(STATES$state == "IN"),]
  IN_geom <- readWKT(IN$geom)
  IN_geom_clip <- gIntersection(bb, IN_geom)
  INProjected <- SpatialPolygonsDataFrame(IN_geom_clip,data.frame("id"), match.ID = TRUE)
  INProjected@data$id <- rownames(INProjected@data)
  INPoints <- fortify( INProjected, region = "id")
  INDF <- merge(INPoints,  INProjected@data, by = "id")
  
  #########################################################
  #########################################################
  
  map <- ggplotGrob(ggplot(data = VADF, aes(x=long, y=lat, group = group))+
                      geom_polygon(data = bbDF, color="black", fill = "powderblue",lwd=0.5)+
                      geom_polygon(data = VADF, color="gray46", fill = "gray")+
                      geom_polygon(data = TNDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = NCDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = SCDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = KYDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = WVDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = MDDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = DEDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = PADF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = NJDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = OHDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = DCDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = INDF, color="gray46", fill = "gray", lwd=0.5)+
                      geom_polygon(data = INDF, color="gray46", fill = "gray", lwd=0.5)+

                    
                      #Plot watershed outline
                      geom_polygon(data = watershedDF, color="khaki4", fill = "yellow",alpha = 0.25,lwd=0.5)+
                      
                      
                      # ADD RIVERS ####################################################################
                      geom_point(data = RIVDF, aes(x = long, y = lat), color="steelblue1", size=0.09)+
                      #################################################################################
                      # ADD WATERBODIES ###############################################################
                      geom_point(data = WBDF, aes(x = long, y = lat), color="steelblue1", size=0.09)+
                      #################################################################################
                      
                      #geom_point(aes(x = x, y = y, group = id), data = STATIONSDF, color="gray66", size = 0.025)+
                      #geom_point(aes(x = x, y = y, group = id), data = BLUSTATIONSDF, color="blue", size = 0.025)+
                      #geom_point(aes(x = x, y = y, group = id), data = GRNSTATIONSDF, color="forestgreen", size = 0.025)+
                      
                      #larger points attempt
                      geom_point(aes(x = x, y = y, group = id), data = STATIONSDF, color="gray66", size = 0.3)+
                      geom_point(aes(x = x, y = y, group = id), data = BLUSTATIONSDF, color="blue", size = 0.3)+
                      geom_point(aes(x = x, y = y, group = id), data = GRNSTATIONSDF, color="forestgreen", size = 0.3)+
                      
                      #bold outter border
                      geom_polygon(data = bbDF, color="black", fill = "NA",lwd=0.5)+
                      
                      #ADD NORTH ARROW AND SCALE BAR
                      north(bbDF, location = 'topleft', symbol = 12, scale=0.1)+
                      scalebar(bbDF, location = 'bottomleft', dist = 100, dd2km = TRUE, model = 'WGS84',st.bottom=FALSE,st.size=3.5, 
                               anchor = c(
                                 x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-1.1,
                                 y = extent$y[1]+(extent$y[1])*0.001
                               ))+
                      
                      scale_x_continuous(limits = c(extent$x[1], extent$x[2]))+
                      scale_y_continuous(limits = c(extent$y[1], extent$y[2]))+
                      
                      theme(axis.title.x=element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(),
                            axis.title.y=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank(),
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            panel.border = element_blank())
  )
  
  result <- map
  return(result)
}
