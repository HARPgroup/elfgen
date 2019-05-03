#' Retrieve and format data for ELF generation
#' @description Given a HUC code, provides a dataframe of
#' all contained nhdplus segments and their individual NT Total
#' and Mean Annual Flow MAF values
#' @param watershed.code a hydrologic unit code, either HUC6, HUC8, HUC10, or HUC12
#' @param ichthy.localpath local path for storing downloaded ichthy data. Default to temp directory
#' @return the watershed.df dataframe containing nhdplus MAF and NT Total values
#' @import utils
#' @import sbtools
#' @import RJSONIO
#' @import stringr
#' @export elfgen_getdata
elfgen_getdata <- function (watershed.code,ichthy.localpath = tempdir()) {

  HUCRES.df <- data.frame(HUCRES = c(12, 10, 8, 6))
  if (length(which(HUCRES.df$HUCRES == nchar(watershed.code))) < 1) {
    stop("Invalid Length of Hydrologic Unit Code")
  }

  print(ichthy.localpath)

  ichthy_item = item_get("5446a5a1e4b0f888a81b816d") #Get item using its ScienceBase unique identifier
  ichthy_filename <- item_list_files(ichthy_item)[1,1] #Obtain filename from ichthy item

  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(ichthy.localpath, ichthy_filename, sep = '/')) == FALSE) {
    print(paste("DOWNLOADING ICHTHY DATASET", sep = ''))
    ichthy_download = item_file_download(ichthy_item,
                                         dest_dir = ichthy.localpath,
                                         overwrite_file = FALSE,
                                         timeout = 200)
  } else {
    print(paste("ICHTHY DATASET PREVIOUSLY DOWNLOADED", sep = ''))
  }

  #read csv from local directory
  ichthy.dataframe <- read.csv(file=paste(ichthy.localpath,ichthy_filename,sep="\\"), header=TRUE, sep=",")

  #pad HUC12 column to ensure leading "0", genrate columns for HUC10, HUC8, HUC6
  ichthy.dataframe$HUC12 <- as.character(str_pad(gsub(" ", "", format(ichthy.dataframe$HUC12, scientific=F), fixed = TRUE), 12, pad = "0"))
  ichthy.dataframe$HUC10 <- substr(ichthy.dataframe$HUC12, start=1, stop=10)
  ichthy.dataframe$HUC8 <- substr(ichthy.dataframe$HUC12, start=1, stop=8)
  ichthy.dataframe$HUC6 <- substr(ichthy.dataframe$HUC12, start=1, stop=6)

  #determine resolution of watershed.code and take subset of rows for that HUC
  if (nchar(watershed.code) == 12) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC12 == watershed.code),] }
  if (nchar(watershed.code) == 10) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC10 == watershed.code),] }
  if (nchar(watershed.code) == 8) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC8 == watershed.code),] }
  if (nchar(watershed.code) == 6) {watershed.rows <- ichthy.dataframe[which(ichthy.dataframe$HUC6 == watershed.code),] }

  #initialize watershed.df dataframe
  watershed.df <- data.frame(WATERSHED=character(),
                           COMID=character(),
                           NT_TOTAL=character(),
                           stringsAsFactors=FALSE)

  #Loop through all COMIDs, summing the number of unique taxa present
  # to generate an NT Total value for each nhdplus segment
  for (i in 1:length(watershed.rows$COMID_NHDv2)) {
    print(paste("PROCESSING COMID ",i," OF ",length(watershed.rows$COMID_NHDv2), sep = ''))

    COMID <- watershed.rows[i,]$COMID
    COMID.rows <- watershed.rows[which(watershed.rows$COMID_NHDv2 == COMID),] #single comid

    # SKIP COMID IF THAT NHDPlusV2 SEGMENT ISNT IN ICHTHY DATASET
    #if (length(COMID.rows$ID) == 0) {
    #  print(paste("NO ICHTHY DATA FOR COMID ", COMID, " (SKIPPING)", sep = ''))
    #  next
    #}

    COMID.Taxa.All <- COMID.rows$Name_Taxa
    NT.TOTAL.ALL <- length(COMID.Taxa.All)
    NT.TOTAL.UNIQUE <- length(unique(COMID.Taxa.All))
    print(paste("ICHTHY DATA FOR COMID ",COMID," (NT TOTAL = ",NT.TOTAL.UNIQUE,")",sep = ''))

    watershed.df.i <- data.frame(watershed.code,COMID_NHDv2 = COMID,NT.TOTAL.UNIQUE)
    watershed.df <- rbind(watershed.df,watershed.df.i)

  }

  watershed.df <- unique(watershed.df[, 1:3]) #remove duplicates (so each comid appears once)

  #Loop through each COMID retrieving MAF value
  for (j in 1:length(watershed.df$COMID_NHDv2)) {
    COMID <- watershed.df[j,]$COMID_NHDv2
    COMID.URL <- paste('https://ofmpub.epa.gov/waters10/nhdplus.jsonv25?ppermanentidentifier=',COMID,'&pFilenameOverride=AUTO',sep="")
    json_file <- fromJSON(COMID.URL)

    COMID.MAF <- json_file$`output`$header$attributes[[4]]$value #MAF in cfs
    watershed.df[j,"MAF"] <- COMID.MAF

    print(paste("PROCESSING ",j," OF ",length(watershed.df$COMID_NHDv2)," (COMID ",COMID,"), MAF = ",COMID.MAF,sep = ''))

  }

  #reformat dataframe to conform to elfgen format
  watershed.df <- data.frame("MAF" = watershed.df$MAF,
                             "NT.TOTAL.UNIQUE" = watershed.df$NT.TOTAL.UNIQUE,
                             "watershed.code" = watershed.df$watershed.code,
                             "COMID_NHDv2" = watershed.df$COMID_NHDv2

  )

  return(watershed.df)
} #close function



