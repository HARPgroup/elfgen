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

  #note: add a 0 for the HUC6's or else the url doesn't work
  if (target_ftype == 'nhd_huc6') {
    target_hydrocode <- str_pad(target_hydrocode, 6, "left", pad = "0");
  }
  if (target_ftype == 'nhd_huc10') {
    target_hydrocode <- str_pad(target_hydrocode, 10, "left", pad = "0");
  }

  uri <- paste(
    vahydro_site,"elfgen_data_export",x_var,y_var,
    bundle,target_ftype,sampres,target_hydrocode,sep="/"
  )
  print(paste("Using ", uri, sep=''));
  data <- read.csv(uri, header = TRUE, sep = ",")

  #reformat datafram3 to conform to elfgen format
  data <- data.frame(x_value = data$x_value,
                     y_value = data$y_value,
                     tstime = data$tstime,
                     geom = data$geom,
                     hydrocode = data$hydrocode,
                     drainage_area_sqmi = data$drainage_area_sqmi,
                     qmean_annual = data$qmean_annual,
                     watershed.code = target_hydrocode)
  colnames(data)[1] <- x_var
  colnames(data)[2] <- y_var

  data <- data

}
