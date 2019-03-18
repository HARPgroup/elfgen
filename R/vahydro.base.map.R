#' BASE MAP
#' @description
#' @param inputs
#' @param watershed.df
#' @param elfgen.location
#' @return
#' @import rgeos
#' @import sp
#' @import ggsn
#' @export vahydro.base.map
vahydro.base.map <- function(inputs, watershed.df, quantile, breakpt, elfgen.location) {

#  library(rgeos)
#  library(sp)
#  library(ggsn)

  geom <- watershed.df$geom
  target_hydrocode <- inputs$target_hydrocode
  vahydro_site <- inputs$vahydro_site
  target_ftype <- inputs$target_ftype

  #Automatic bundle specification (WILL BE ELIMINATED ONCE WE UPDATE VAHYDRO STORAGE SCHEME)
  if(target_ftype == "hwi_region"){
    bundle <- "ecoregion"
  } else if(target_ftype == "state") {
    bundle <- "landunit"
  } else if(target_ftype == "ecoregion_iii") {
    bundle <- "ecoregion"
  } else if(target_ftype == "ecoregion_iv") {
    bundle <- "ecoregion"
  } else if(target_ftype == "ecoiii_huc6") {
    bundle <- "ecoregion"
  } else {
    bundle <- "watershed"
  }


watershed.vahydro.url <- paste(vahydro_site,"?q=elfgen_regions_export",bundle, target_ftype, target_hydrocode, sep = "/");
print(paste("Using ", watershed.vahydro.url, sep=''))
watershed.vahydro.export  <- read.table(watershed.vahydro.url ,header = TRUE, sep = ",")
geom <- watershed.vahydro.export$geom

#library(rgeos);
#library(sp);
#library(ggsn);

#-----------------------------------------------------------
print(paste("PROCESSING QUANTILE DATA...", sep = ''))


# default method if none provided
if(missing(breakpt)) {
  breakpt <- 500
}

#watershed.df.raw <- watershed.df

#RENAME FLOW AND RICHNESS COLUMNS TO HAVE GENERIC NAMES
colnames(watershed.df)[1] <- "x_var"
colnames(watershed.df)[2] <- "y_var"

#full_dataset <- watershed.df

data <- watershed.df[!(watershed.df$x_var > breakpt),]

#------------------------------
# UPPER QUANTILE SUBSET
upper.quant.data <- rq(y_var ~ log(x_var),data = data, tau = quantile)
newy <- c(log(data$x_var)*coef(upper.quant.data)[2]+coef(upper.quant.data)[1])
upper.quant <- subset(data, data$y_var > newy)
#-----------------------------------------------------------




print(paste("PROCESSING GIS DATA...", sep = ''))

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
  STATIONS_data <- watershed.df
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
  # #--------------------------------------------------------------------------------------------

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
  STATES <- read.table(file=paste(elfgen.location,"GIS_LAYERS","STATES.tsv",sep="\\"), header=TRUE, sep="\t") #Load state geometries
#  RIVDF <- read.table(file=paste(elfgen.location,"GIS_LAYERS","RIVDF.csv",sep="/"), header=TRUE, sep=",") #Load river geometries
#  WBDF <- read.table(file=paste(elfgen.location,"GIS_LAYERS","WBDF.csv",sep="/"), header=TRUE, sep=",") #Load waterbody geometries

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

  print(paste("PLOTTING GIS DATA...", sep = ''))

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
               #     geom_point(data = RIVDF, aes(x = long, y = lat), color="steelblue1", size=0.09)+
                      #################################################################################
                    # ADD WATERBODIES ###############################################################
               #     geom_point(data = WBDF, aes(x = long, y = lat), color="steelblue1", size=0.09)+
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

  print(paste("OUTPUTTING PDF FILE...", sep = ''))

  filename <- paste(target_hydrocode,"map.pdf", sep="_")
  ggsave(file=filename, path = paste(elfgen.location,"plots",sep=""), width=9, height=6)


  #result <- map
  #return(result)
}
