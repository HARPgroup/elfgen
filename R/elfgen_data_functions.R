#' Prepare data to avoid common QA pitfalls
#'
#' This function runs a set of common QA routines.
#' @param data The dataframe to QA.
#' @param inputs A list of configuration options including x_variable, y_variable, x_max.
#' @param startdate A date filter. Default FALSE
#' @param enddate A date filter. Default FALSE
#' @return The modified dataframe data or FALSE if something went terribly wrong.
#' @seealso
#' @export elfgen_cleandata
#' @examples
elfgen_cleandata <- function (data, inputs, startdate = FALSE, enddate = FALSE) {
  # @todo: port this function fully, and critically examine the role of inputs list
  #        for now - do nothing
  print("This function needs to be enabled!")
  return(data)
  
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
