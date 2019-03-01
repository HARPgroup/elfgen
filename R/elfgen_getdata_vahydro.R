#' Retrieve and format data for ELF generation
#' @description Given a HUC code, provides a dataframe of
#' all contained nhdplus segments and their individual NT Total
#' and Mean Annual Flow MAF values
#' @param watershed.code a hydrologic unit code, either HUC6, HUC8, HUC10, or HUC12
#' @param ichthy.localpath local path for storing downloaded ichthy data. Default to temp directory
#' @return the watershed.df dataframe containing nhdplus MAF and NT Total values
#' @export elfgen_getdata_vahydro
elfgen_getdata_vahydro <- function (watershed.code,ichthy.localpath = tempdir()) {

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
  datasite <- inputs$datasite
  xaxis_thresh <- inputs$xaxis_thresh
  sampres <- inputs$sampres
  analysis_timespan <- inputs$analysis_timespan
  station_agg <- inputs$station_agg
  method <- inputs$method
  token <- inputs$token

  #l <- 1
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
    HUClist_url_base <- paste(datasite,"/?q=elfgen_regions_export/",bundle, sep = "");
    if (!(target_hydrocode == '')) {
      HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], target_hydrocode, sep = "/");
    } else {
      HUClist_url_full <- paste(HUClist_url_base, ws_ftype[l], sep = "/");
    }
    #print(HUClist_url_full)
    HUClist <- read.table(HUClist_url_full,header = TRUE, sep = ",")
    Watershed_Hydrocode <- HUClist$Hydrocode
    Feature.Name <- HUClist$Feature.Name
    Hydroid <- HUClist$HydroID

    #k<-1
    for (k in offset_y_metric:length(y_metric)) {
      print(paste("y_metric ", k, ". of ",length(y_metric),". Beginning loop for ", y_metric[k], sep=''));
      #j<-1
      for (j in offset_x_metric:length(x_metric)) {
        print(paste("x_metric ", j, ". of 14. Beginning loop for ", x_metric[j], sep=''));
        #i<-1
        for (i in offset_hydrocode:length(Watershed_Hydrocode)) {
          print(paste("Feature ", i, ". of ",length(Watershed_Hydrocode),". Searching for stations from ", Watershed_Hydrocode[i], sep=''));
          search_code <- Watershed_Hydrocode[i];
          Feature.Name_code <- as.character(Feature.Name[i]);
          Hydroid_code <- Hydroid[i];
          ws_ftype_code <- ws_ftype[l]
          x_metric_code <-  x_metric[j];
          y_metric_code <-  y_metric[k];
          data <- vahydro_fe_data(
            Watershed_Hydrocode[i],
            x_metric_code,y_metric_code,

            bundle,ws_ftype_code,sampres,data #datasite   old line without datasite

            #bundle,ws_ftype_code,sampres, data, datasite

          );

          #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
          data$y_value <- as.numeric(data$y_value)
          #Subset by date range
          data$tstime <- as.Date(data$tstime,origin="1970-01-01")

          if (analysis_timespan != 'full') {
            #Need to convert timespan paramteter into startdate and endate format for subsetting data
            startdate <- paste(unlist(strsplit(analysis_timespan, "[-]"))[[1]],"-01-01",sep="")
            enddate <- paste(unlist(strsplit(analysis_timespan, "[-]"))[[2]],"-12-31",sep="")
            print(paste("startdate: ", startdate))
            print(paste("enddate: ", enddate))
            data <- subset(data, tstime > startdate & tstime < enddate)
            startdate <- paste("subset: ",startdate,sep="")
          } else {
            startdate <- paste("full timespan: ",min(data$tstime),sep="") #if plotting for full timespan, display start and end dates above plot
            enddate <- max(data$tstime)   #no dates set with REST, only "full" for analysis_timespan propcode
          }


          #ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW
          data["ratio"] <- (data$drainage_area)/(data$qmean_annual)
          #REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
          data<-data[!(data$ratio > 1000),]

          #USE ONLY MAX NT VALUE FOR EACH STATION
          if(station_agg == "max"){
            aa <- data[order(data$hydrocode, data$y_value, decreasing=TRUE),]
            aa <- aa[!duplicated(aa$hydrocode),]
            aa <- aa[order(aa$hydrocode, aa$y_value),]
            data <- aa
          }

          #subsets data to exclude anything with a flowmetric value greater than the "xaxis_thresh" specified in the user inputs file
          data <- subset(data, x_value >= .001 & x_value < xaxis_thresh);

          #Export data as spreadsheet
          ##write.table(data, paste(save_directory,"data.tsv",sep=""), sep="\t")

          print(paste("Found ", nrow(data), sep=''));
          #If statement needed in case geographic region does not contain more than 3 points
          if(nrow(data) <= 3) {
            print(paste("... Skipping (fewer than 3 datapoints in ", Watershed_Hydrocode[i],")",sep=''))
            next
          }


          #Skip if there is only 1 or 2 unique flow metric values for this watershed (either only a single EDAS station, or multiple with the same flow metric, which would result in a vertical bar of points in the plot)
          station_x_value <- data$x_value
          remove_da_duplicates <- unique(station_x_value, incomparables = FALSE)
          if(length(remove_da_duplicates) == 1 | length(remove_da_duplicates) == 2) {
            print(paste("... Skipping (the points are all organized in 1 or 2 vertical lines in ", Watershed_Hydrocode[i],")", sep=''));
            next
          } #closes bar of points skip if-statement (rare)

          #Skip if there is only 1 or 2 unique biometric values for this watershed
          station_y_value <- data$y_value
          remove_metric_duplicates <- unique(station_y_value, incomparables = FALSE)
          if(length(remove_metric_duplicates) == 1 | length(remove_metric_duplicates) == 2) {
            print(paste("... Skipping (the points are all organized in 1 or 2 horizontal lines in ", Watershed_Hydrocode[i],")", sep=''));
            next
          } #closes bar of points skip if-statement (rare)

          #---------------------------------------------------------------------

          #Load Functions
          source(paste(fxn_locations,"elf_quantreg.R", sep = ""));       #loads elf_quantreg function
          source(paste(fxn_locations,"elf_ymax.R", sep = ""));           #loads elf_ymax function
          source(paste(fxn_locations,"elf_pw_it.R", sep = ""));          #loads ef_pw_it function
          source(paste(fxn_locations,"elf_twopoint.R", sep = ""));       #loads elf_twopoint function
          source(paste(fxn_locations,"elf_pw_it_RS.R", sep = ""));       #loads ef_pw_it_RS function
          source(paste(fxn_locations,"elf_pct_chg.R", sep =""));         #loads percent change barplot function
          source(paste(fxn_locations,"elf_store_data.R", sep = ""));     #loads function used to store ELF stats to VAHydro

          #source(paste(fxn_locations,"elf_DA_Flow.R", sep = ""));            #loads function used to plot DA and Flow
          source(paste(fxn_locations,"elf_pw_it_RS_IFIM.R", sep = ""));  #loads elf_pw_it_RS_IFIM function for overlaying WUA curves on ELFs
          source(paste(fxn_locations,"elf_pct_chg_hab.R", sep =""));

          if(method == "quantreg") {print(paste("PLOTTING - method quantreg breakpoint ...",sep=""))
            plt <- elf_quantreg (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
          if(method == "ymax") {print(paste("PLOTTING - method quantreg breakpoint at y-max...",sep=""))
            plt <- elf_ymax (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
          if(method == "pwit") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function...",sep=""))
            plt <- elf_pw_it (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "twopoint") {print(paste("PLOTTING - method two-point function...",sep=""))

            plt <- elf_twopoint (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "pwit_RS") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
            plt <-  elf_pw_it_RS (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "pw_it_RS_IFIM") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
            plt <- elf_pw_it_RS_IFIM (inputs, data, x_metric_code, y_metric_code, ws_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "pw_it_RS_IFIM") {return(plt)}
          #print(plt)

          #return(plt)

        } #closes watershed for loop
      } #closes x_metric for loop
    } #closes y_metric for loop
  } #closes ws_ftype for loop
} #close function
