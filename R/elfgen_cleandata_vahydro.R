#' GET watershed.df FROM VAHYDRO
#' @description
#' @param watershed.df
#' @param startdate
#' @param enddate
#' @return
#' @export elfgen_cleandata_vahydro
#elfgen_cleanwatershed.df_vahydro <- function (inputs) {
#elf_cleanwatershed.df <- function (watershed.df, inputs, startdate = FALSE, enddate = FALSE) {
elfgen_cleandata_vahydro <- function (watershed.df, startdate = FALSE, enddate = FALSE) {

  #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
  #watershed.df$y_var <- as.numeric(watershed.df$y_var)
  watershed.df[,2] <- as.numeric(watershed.df[,2])

  #Subset by date range
  watershed.df$tstime <- as.Date(watershed.df$tstime,origin="1970-01-01")

  if (typeof(startdate) != 'logical') {
    watershed.df <- subset(watershed.df, tstime > startdate)
  }
  if (typeof(enddate) != 'logical') {
    watershed.df <- subset(watershed.df, tstime < enddate)
  }

  #ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW
  watershed.df["ratio"] <- (watershed.df$drainage_area)/(watershed.df$qmean_annual)
  #REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
  watershed.df<-watershed.df[!(watershed.df$ratio > 1000),]

  #USE ONLY MAX NT VALUE FOR EACH STATION
    aa <- watershed.df[order(watershed.df$hydrocode, watershed.df[,2], decreasing=TRUE),]
    aa <- aa[!duplicated(aa$hydrocode),]
    aa <- aa[order(aa$hydrocode, aa[,2]),]
    watershed.df <- aa


  #subsets watershed.df to exclude anything with a flowmetric value greater than the "xaxis_thresh" specified in the user inputs file
  #watershed.df <- subset(watershed.df, x_var >= .001 & x_var < inputs$xaxis_thresh);

  #Export watershed.df as spreadsheet
  ##write.table(watershed.df, paste(save_directory,"watershed.df.tsv",sep=""), sep="\t")

  # print(paste("Found ", nrow(watershed.df), sep=''));
  # #If statement needed in case geographic region does not contain more than 3 points
  # if(nrow(watershed.df) <= 3) {
  #   print("... Skipping (fewer than 3 watershed.dfpoints)")
  #   return(FALSE)
  # }


  #Skip if there is only 1 or 2 unique flow metric values for this watershed (either only a single EDAS station, or multiple with the same flow metric, which would result in a vertical bar of points in the plot)
  station_x_var <- watershed.df[,1]
  remove_da_duplicates <- unique(station_x_var, incomparables = FALSE)
  if(length(remove_da_duplicates) == 1 | length(remove_da_duplicates) == 2) {
    print("... Skipping (the points are all organized in 1 or 2 vertical lines in )");
    return(FALSE)
  } #closes bar of points skip if-statement (rare)

  #Skip if there is only 1 or 2 unique biometric values for this watershed
  station_y_var <- watershed.df[,2]
  remove_metric_duplicates <- unique(station_y_var, incomparables = FALSE)
  if(length(remove_metric_duplicates) == 1 | length(remove_metric_duplicates) == 2) {
    print("... Skipping (the points are all organized in 1 or 2 horizontal lines in )");
    return(FALSE)
  } #closes bar of points skip if-statement (rare)
  return(watershed.df)

}
