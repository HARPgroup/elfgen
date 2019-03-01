#' GET DATA FROM VAHYDRO
#' @description
#' @param inputs
#' @return
#' @export elfgen_getdata_vahydro
elfgen_getdata_vahydro <- function (inputs) {

  #Load inputs
  vahydro_site <- inputs$vahydro_site
  x_var <- inputs$x_var
  y_var <- inputs$y_var
  sampres <- inputs$sampres
  target_ftype <- inputs$target_ftype
  target_hydrocode <- inputs$target_hydrocode




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


    vahydro_query <- paste(vahydro_site,
                              "?q=elfgen_regions_export",
                              bundle,
                              target_ftype,
                              target_hydrocode,
                              sep = "/");

    vahydro_watershed <- read.table(vahydro_query,header = TRUE, sep = ",")
    vahydro_hydrocode <- vahydro_watershed$Hydrocode
    vahydro_name <- vahydro_watershed$Feature.Name
    vahydro_hydroid <- vahydro_watershed$HydroID
#
#
#         search_code <- Watershed_Hydrocode[i];
#           Feature.Name_code <- as.character(Feature.Name[i]);
#           Hydroid_code <- Hydroid[i];
#           target_ftype_code <- target_ftype[l]
#           x_var_code <-  x_var[j];
#           y_var_code <-  y_var[k];
#           data <- vahydro_fe_data(
#             Watershed_Hydrocode[i],
#             x_var_code,y_var_code,
#
#             bundle,target_ftype_code,sampres,data #datasite   old line without datasite

            #bundle,target_ftype_code,sampres, data, datasite

          #);

          #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
          data$y_var <- as.numeric(data$y_var)
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
            aa <- data[order(data$hydrocode, data$y_var, decreasing=TRUE),]
            aa <- aa[!duplicated(aa$hydrocode),]
            aa <- aa[order(aa$hydrocode, aa$y_var),]
            data <- aa
          }

          #subsets data to exclude anything with a flowmetric value greater than the "xaxis_thresh" specified in the user inputs file
          data <- subset(data, x_var >= .001 & x_var < xaxis_thresh);

          #Export data as spreadsheet
          ##write.table(data, paste(save_directory,"data.tsv",sep=""), sep="\t")

          print(paste("Found ", nrow(data), sep=''));
          #If statement needed in case geographic region does not contain more than 3 points
          if(nrow(data) <= 3) {
            print(paste("... Skipping (fewer than 3 datapoints in ", Watershed_Hydrocode[i],")",sep=''))
            next
          }


          #Skip if there is only 1 or 2 unique flow metric values for this watershed (either only a single EDAS station, or multiple with the same flow metric, which would result in a vertical bar of points in the plot)
          station_x_var <- data$x_var
          remove_da_duplicates <- unique(station_x_var, incomparables = FALSE)
          if(length(remove_da_duplicates) == 1 | length(remove_da_duplicates) == 2) {
            print(paste("... Skipping (the points are all organized in 1 or 2 vertical lines in ", Watershed_Hydrocode[i],")", sep=''));
            next
          } #closes bar of points skip if-statement (rare)

          #Skip if there is only 1 or 2 unique biometric values for this watershed
          station_y_var <- data$y_var
          remove_metric_duplicates <- unique(station_y_var, incomparables = FALSE)
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
            plt <- elf_quantreg (inputs, data, x_var_code, y_var_code, target_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
          if(method == "ymax") {print(paste("PLOTTING - method quantreg breakpoint at y-max...",sep=""))
            plt <- elf_ymax (inputs, data, x_var_code, y_var_code, target_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}
          if(method == "pwit") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function...",sep=""))
            plt <- elf_pw_it (inputs, data, x_var_code, y_var_code, target_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "twopoint") {print(paste("PLOTTING - method two-point function...",sep=""))

            plt <- elf_twopoint (inputs, data, x_var_code, y_var_code, target_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "pwit_RS") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
            plt <-  elf_pw_it_RS (inputs, data, x_var_code, y_var_code, target_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "pw_it_RS_IFIM") {print(paste("PLOTTING - method quantreg breakpoint using piecewise function (Including regression to the right of breakpoint)...",sep=""))
            plt <- elf_pw_it_RS_IFIM (inputs, data, x_var_code, y_var_code, target_ftype_code, Feature.Name_code, Hydroid_code, search_code, token, startdate, enddate)}

          if(method == "pw_it_RS_IFIM") {return(plt)}
          #print(plt)

          #return(plt)



} #close function
