#' Retrieve and format data for ELF generation
#' @description Given a HUC code, provides a dataframe of
#' all contained nhdplus segments and their individual NT Total
#' and Mean Annual Flow MAF values
#' @param watershed.code Hydrologic unit code, either HUC6, HUC8, HUC10, or HUC12 (e.g. HUC10 code '0208020101').
#' @param ichthy.localpath Local file path for storing downloaded ichthy data. Defaults to a temp directory.
#' @return A dataframe of nhdplus segments containing species richness data (NT Total values) and mean annual flow (MAF) data.
#' @import utils
#' @import RJSONIO
#' @import stringr
#' @importFrom curl has_internet handle_reset
#' @importFrom httr http_error
#' @export elfdata
#' @examples
#' \donttest{
#' # Retrieve dataset of interest
#' # You may enter either a 6, 8, 10, or 12-digit HUC code.
#' # By default the ichthy dataset is downloaded to a temp directory, however this may be overridden by
#' # supplying a local path of interest using the input parameter 'ichthy.localpath'
#' watershed.df <- elfdata('02080201')
#' }
elfdata <- function (watershed.code,ichthy.localpath = tempdir()) {

  if (class(watershed.code) == 'data.frame') {
    ichthy.dataframe <- watershed.code
    watershed.code <- '02080106'
  } else {

  HUCRES.df <- data.frame(HUCRES = c(12, 10, 8, 6))
  if (length(which(HUCRES.df$HUCRES == nchar(watershed.code))) < 1) {
    stop("Invalid Length of Hydrologic Unit Code")
  }

    #using direct sciencebase file link
    ichthy_item <- "https://www.sciencebase.gov/catalog/file/get/5446a5a1e4b0f888a81b816d?f=__disk__25%2Fed%2F4a%2F25ed4a840a109d160d081bf144a66f615cb765cd"
    ichthy_filename <- "IchthyMaps_v1_20150520.csv"

    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(paste(ichthy.localpath, ichthy_filename, sep = '/')) == FALSE) {
      message(paste("DOWNLOADING ICHTHY DATASET:", sep = ''))

      #using direct sciencebase file link
      destfile <- paste(ichthy.localpath,ichthy_filename,sep="\\")
      download.file(ichthy_item, destfile = destfile, method = "libcurl")

    } else {
      message(paste("ICHTHY DATASET PREVIOUSLY DOWNLOADED",sep = ''))
    }
    message(paste("DATASET DOWNLOAD LOCATION: ",ichthy.localpath,sep = ''))

    #read csv from local directory
    ichthy.dataframe <- read.csv(file=paste(ichthy.localpath,ichthy_filename,sep="\\"), header=TRUE, sep=",")

  }

  #pad HUC12 column to ensure leading "0", generate columns for HUC10, HUC8, HUC6
  ichthy.dataframe$HUC12 <- as.character(str_pad(gsub(" ", "", format(ichthy.dataframe$HUC12, scientific=F), fixed = TRUE), 12, pad = "0"))
  ichthy.dataframe$HUC10 <- substr(ichthy.dataframe$HUC12, start=1, stop=10)
  ichthy.dataframe$HUC8 <- substr(ichthy.dataframe$HUC12, start=1, stop=8)
  ichthy.dataframe$HUC6 <- substr(ichthy.dataframe$HUC12, start=1, stop=6)

  #determine resolution of watershed.code and take subset of rows for that HUC
  if (nchar(watershed.code) == 12) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC12 == watershed.code),] }
  if (nchar(watershed.code) == 10) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC10 == watershed.code),] }
  if (nchar(watershed.code) == 8) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC8 == watershed.code),] }
  if (nchar(watershed.code) == 6) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC6 == watershed.code),] }

   if (length(watershed.rows[,1]) == 0) {
     stop("NO ICHTYMAP DATA FOR HYDROLOGIC UNIT CODE")
   }

  #initialize watershed.df dataframe
  watershed.df <- data.frame(WATERSHED=character(),
                           COMID=character(),
                           NT_TOTAL=character(),
                           stringsAsFactors=FALSE)

  #Loop through all COMIDs, summing the number of unique taxa present
  # to generate an NT Total value for each nhdplus segment
  message(paste("PROCESSING NHD FEATURES:",sep = ''))
  for (i in 1:length(watershed.rows$COMID_NHDv2)) {
    message(paste(i," OF ",length(watershed.rows$COMID_NHDv2),sep = ''))

    COMID <- watershed.rows[i,]$COMID
    COMID.rows <- watershed.rows[which(watershed.rows$COMID_NHDv2 == COMID),] #single comid

    COMID.Taxa.All <- COMID.rows$Name_Taxa
    NT.TOTAL.ALL <- length(COMID.Taxa.All)
    NT.TOTAL.UNIQUE <- length(unique(COMID.Taxa.All))

    watershed.df.i <- data.frame(watershed.code,COMID_NHDv2 = COMID,NT.TOTAL.UNIQUE)
    watershed.df <- rbind(watershed.df,watershed.df.i)

  }
  watershed.df <- unique(watershed.df[, 1:3]) #remove duplicates (so each comid appears once)
  message(paste(length(watershed.df$COMID_NHDv2)," NHD FEATURES FOUND CONTAINING RICHNESS DATA:",sep = ''))

  #Loop through each COMID retrieving MAF value
  message(paste("PROCESSING NHD FEATURE MEAN ANNUAL FLOW:",sep = ''))
  for (j in 1:length(watershed.df$COMID_NHDv2)) {
    message(paste(j," OF ",length(watershed.df$COMID_NHDv2),sep = ''))

    COMID <- watershed.df[j,]$COMID_NHDv2
    COMID.URL <- paste('https://ofmpub.epa.gov/waters10/nhdplus.jsonv25?ppermanentidentifier=',COMID,'&pFilenameOverride=AUTO',sep="")

    #handle if epa resource is not available or has changed
    if (!curl::has_internet()) {
      message("INTERNET CONNECTION BROKEN")
      return(NULL)
    } else if (httr::http_error(COMID.URL)) {
      message("DATA SOURCE BROKEN")
      return(NULL)
    } else {
      json_file <- fromJSON(COMID.URL)
    }

    COMID.MAF <- json_file$`output`$header$attributes[[4]]$value #MAF in cfs

    #Skip COMID if MAF is NULL
    if(is.null(COMID.MAF)==TRUE){next}

    watershed.df[j,"MAF"] <- COMID.MAF
  }

  #reformat dataframe to conform to elfgen format
  watershed.df <- data.frame("MAF" = watershed.df$MAF,
                             "NT.TOTAL.UNIQUE" = watershed.df$NT.TOTAL.UNIQUE,
                             "watershed.code" = watershed.df$watershed.code,
                             "COMID_NHDv2" = watershed.df$COMID_NHDv2

  )

  return(watershed.df)
} #close function



