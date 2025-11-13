#' Retrieve and format data for ELF generation
#' @description Given a HUC code, provides a dataframe of all contained nhdplus
#'   segments and their individual NT Total and Mean Annual Flow MAF values
#' @param watershed.code Hydrologic unit code, either HUC6, HUC8, HUC10, or
#'   HUC12 (e.g. HUC10 code '0208020101').
#' @param ichthy.localpath Local file path for storing downloaded ichthy data.
#'   Defaults to a temp directory.
#' @param use_cache Should the function look for a file with the same name in
#'   the file directory? This allows users to use the same Icthy dataset each
#'   time rather than needing to download separately when running multiple
#'   analyses
#' @param update_cache Should the file be written out to use for future caching?
#' @return A dataframe of nhdplus segments containing species richness data (NT
#'   Total values) and mean annual flow (MAF) data.
#' @import utils
#' @import stringr
#' @import curl
#' @import sbtools
#' @import nhdplusTools
#' @export elfdata
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' # Retrieve dataset of interest
#' # You may enter either a 6, 8, 10, or 12-digit HUC code.
#' # By default the ichthy dataset is downloaded to a temp directory, however this may be overridden by
#' # supplying a local path of interest using the input parameter 'ichthy.localpath'
#' watershed_df <- elfdata(watershed.code = '0208020104',
#'  ichthy.localpath = tempdir(), use_cache = FALSE)
#' head(watershed_df)
#' }
elfdata <- function (watershed.code,ichthy.localpath,use_cache=TRUE, update_cache=FALSE) {
  watershed_code = watershed.code # rename to avoid janky dot notation
  # set up file names to retrieve ichthymaps file from sciencebase or use custom value
  ichthy_filename <- "IchthyMaps_v1_20150520.csv"
  ichthy.localpath <- gsub("\\", "/", ichthy.localpath, fixed=TRUE)
  if (substr(ichthy.localpath,str_length(ichthy.localpath)-3,str_length(ichthy.localpath)) == ".csv") {
    destfile = ichthy.localpath
  } else {
    destfile <- file.path(ichthy.localpath,ichthy_filename,fsep="/")
  }
  if (use_cache) {
    # this is for caching. We assume that destfile is now a full CSV with path
    # and if a cache has been done for the given watershed, it will be saved in the same
    # path as watershed.code + '.csv'
    cachefile <- paste0(dirname(destfile), '/', watershed_code, '.csv.')
    if (file.exists(cachefile)) {
      message(paste("Reading watershed", watershed_code,"from cached", cachefile))
      watershed_df = read.csv(file=cachefile, header=TRUE, sep=",")
      return(watershed_df)
    } else {
      message(paste("Could not find cached", cachefile, "querying full Ichthy database"))
    }
  }
  if (inherits(watershed_code,"data.frame") == TRUE) {
    ichthy_dataframe <- watershed_code
    watershed_code <- '02080106'
  } else {

    HUCRES.df <- data.frame(HUCRES = c(12, 10, 8, 6))
    if (length(which(HUCRES.df$HUCRES == nchar(watershed_code))) < 1) {
      stop("Invalid length of hydrologic unit code")
    }

    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(destfile) == FALSE) {
      #test if you have internet connection
      if (curl::has_internet() == FALSE) {
        return("Internet resource not available, check internet connection and try again")
      }

      #ping ScienceBase to see if it is available
      if (sbtools::sb_ping() == FALSE) {
        return("Connection to ScienceBase can not be established, Check internet connection and try again")
      }
      message(paste("Downloading ichthy dataset:", sep = ''))
      invisible(sbtools::item_file_download(sb_id = '5446a5a1e4b0f888a81b816d', dest_dir = ichthy.localpath))
    } else {
      message(paste("Ichthy dataset previously downloaded",sep = ''))
    }

    # message(paste("Dataset download location: ",ichthy.localpath,sep = ''))

    #read csv from local directory
    if ((file.size(destfile) == 0L) == TRUE) {
      stop('Ichthymaps resource not available')
    } else {
      ichthy_dataframe <- read.csv(file=destfile, header=TRUE, sep=",")
    }

  }

  #pad HUC12 column to ensure leading "0", generate columns for HUC10, HUC8, HUC6
  ichthy_dataframe$HUC12 <- as.character(str_pad(gsub(" ", "", format(ichthy_dataframe$HUC12, scientific=F), fixed = TRUE), 12, pad = "0"))
  ichthy_dataframe$HUC10 <- substr(ichthy_dataframe$HUC12, start=1, stop=10)
  ichthy_dataframe$HUC8 <- substr(ichthy_dataframe$HUC12, start=1, stop=8)
  ichthy_dataframe$HUC6 <- substr(ichthy_dataframe$HUC12, start=1, stop=6)

  #determine resolution of watershed_code and take subset of rows for that HUC
  if (nchar(watershed_code) == 12) {watershed_rows <- ichthy_dataframe[which(ichthy_dataframe$HUC12 == watershed_code),] }
  if (nchar(watershed_code) == 10) {watershed_rows <- ichthy_dataframe[which(ichthy_dataframe$HUC10 == watershed_code),] }
  if (nchar(watershed_code) == 8) {watershed_rows <- ichthy_dataframe[which(ichthy_dataframe$HUC8 == watershed_code),] }
  if (nchar(watershed_code) == 6) {watershed_rows <- ichthy_dataframe[which(ichthy_dataframe$HUC6 == watershed_code),] }

   if (length(watershed_rows[,1]) == 0) {
     stop("No ichtymap data for hydrologic unit code")
   }

  #initialize watershed_df dataframe
  watershed_df <- data.frame(WATERSHED=character(),
                           COMID=character(),
                           NT_TOTAL=character(),
                           stringsAsFactors=FALSE)

  #Loop through all COMIDs, summing the number of unique taxa present
  # to generate an NT Total value for each nhdplus segment
  message(paste("Processing NHD features:",sep = ''))
  pbr <- txtProgressBar(min = 0, max = length(watershed_rows$COMID_NHDv2), initial = 0)
  watershed_df = sqldf(
    "
    select COMID_NHDv2,
      count(DISTINCT Name_Taxa) as NT_TOTAL_UNIQUE
    from watershed_rows
    group by COMID_NHDv2
    "
  )
  watershed_df$watershed_code <- watershed_code
  message(paste("\n",length(watershed_df$COMID_NHDv2)," NHD features found containing richness data",sep = ''))

  #Loop through each COMID retrieving MAF value (qe_ma is Mean annual flow from gage adjustment)
  message(paste("Processing NHD feature mean annual flow:",sep = ''))
  pbm <- txtProgressBar(min = 0, max = length(watershed_df$COMID_NHDv2), initial = 0)
  for (j in 1:length(watershed_df$COMID_NHDv2)) {
    COMID <- watershed_df[j,]$COMID_NHDv2
    COMID.dat <- nhdplusTools::get_nhdplus(comid = COMID)
    COMID.MAF <- COMID.dat$qe_ma

    #Skip COMID if MAF is NULL
    if(is.null(COMID.MAF)==TRUE){next}

    watershed_df[j,"MAF"] <- COMID.MAF
    watershed_df$Q01[j] <- COMID.dat$qe_01
    watershed_df$Q02[j] <- COMID.dat$qe_02
    watershed_df$Q03[j] <- COMID.dat$qe_03
    watershed_df$Q04[j] <- COMID.dat$qe_04
    watershed_df$Q05[j] <- COMID.dat$qe_05
    watershed_df$Q06[j] <- COMID.dat$qe_06
    watershed_df$Q07[j] <- COMID.dat$qe_07
    watershed_df$Q08[j] <- COMID.dat$qe_08
    watershed_df$Q09[j] <- COMID.dat$qe_09
    watershed_df$Q10[j] <- COMID.dat$qe_10
    watershed_df$Q11[j] <- COMID.dat$qe_11
    watershed_df$Q12[j] <- COMID.dat$qe_12
    if(interactive()) setTxtProgressBar(pbm, j)
  }

  #reformat dataframe to conform to elfgen format
  watershed_df <- data.frame("MAF" = watershed_df$MAF,
                             "NT.TOTAL.UNIQUE" = watershed_df$NT_TOTAL_UNIQUE,
                             "watershed_code" = watershed_df$watershed_code,
                             "Q01" = watershed_df$Q01,
                             "Q02" = watershed_df$Q02,
                             "Q03" = watershed_df$Q03,
                             "Q04" = watershed_df$Q04,
                             "Q05" = watershed_df$Q05,
                             "Q06" = watershed_df$Q06,
                             "Q07" = watershed_df$Q07,
                             "Q08" = watershed_df$Q08,
                             "Q09" = watershed_df$Q09,
                             "Q10" = watershed_df$Q10,
                             "Q11" = watershed_df$Q11,
                             "Q12" = watershed_df$Q12

  )
  names(watershed_df) <- c("MAF",
                           "NT.TOTAL.UNIQUE", "watershed.code",
                           "Q01", "Q02", "Q03", "Q04", "Q05", "Q06",
                           "Q07", "Q08", "Q09", "Q10", "Q11", "Q12")
  if (update_cache) {
    # this is for caching. We assume that destfile is now a full CSV with path
    # and if a cache has been done for the given watershed, it will be saved in the same
    # path as watershed.code + '.csv'
    cachefile <- paste0(dirname(destfile), '/', watershed_code, '.csv.')
    write.csv(watershed_df,cachefile,row.names = FALSE)
  }
  return(watershed_df)
} #close function



