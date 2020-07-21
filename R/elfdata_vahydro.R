#' Retrieve data from DEQ VAHydro database and format data for ELF generation
#' @description Given a set of VAHydro input parameters, outputs a dataframe of
#' flow metric and richness metric data for hydrologic unit supplied
#' @param watershed.code a hydrologic unit code, either HUC6, HUC8, HUC10, or HUC12
#' @param watershed.bundle dH bundle of hydrologic unit
#' @param watershed.ftype dH ftype of hydrologic unit
#' @param x.metric x metric - streamflow or drainage area
#' @param y.metric y metric - most commonly species richness
#' @param y.sampres sample resolution of y.metric
#' @param datasite vahydro datasite URL
#' @param EDAS.localpath local path for storing downloaded EDAS data. Default to temp directory
#' @return the watershed.df dataframe containing streamflow and species richness data
#' @export elfdata_vahydro
elfdata_vahydro <- function (watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,datasite,EDAS.localpath = tempdir()) {

  EDAS_item <- paste(
    datasite,"elfgen_data_export",x.metric,y.metric,
    watershed.bundle,watershed.ftype,y.sampres,watershed.code,sep="/"
  )

  EDAS_filename <- paste("EDAS_data_",watershed.code,"_",x.metric,"_",y.metric,"_",y.sampres,".csv",sep="")

  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(EDAS.localpath, EDAS_filename, sep = '/')) == FALSE) {
    print(paste("DOWNLOADING EDAS DATASET", sep = ''))

    destfile <- paste(EDAS.localpath,EDAS_filename,sep="\\")
    download.file(EDAS_item, destfile = destfile, method = "libcurl")

  } else {
    print(paste("EDAS DATASET PREVIOUSLY DOWNLOADED", sep = ''))
  }

  #read csv from local directory
  EDAS.dataframe <- read.csv(file=paste(EDAS.localpath,EDAS_filename,sep="\\"), header=TRUE, sep=",")

  if (length(EDAS.dataframe[,1]) == 0) {
    stop("No VAHydro EDAS Data for Hydrologic Unit")
  }

  #reformat dataframe to conform to elfgen format
  watershed.df <- data.frame("MAF" = EDAS.dataframe$qmean_annual,
                             "NT.TOTAL.UNIQUE" = EDAS.dataframe$y_value,
                             "watershed.code" = watershed.code,
                             "hydrocode" = EDAS.dataframe$hydrocode,
                             "DA_SQMI" = EDAS.dataframe$drainage_area_sqmi,
                             "x.metric" = EDAS.dataframe$x_value

  )

  return(watershed.df)
} #close function
