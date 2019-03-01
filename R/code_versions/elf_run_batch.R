rm(list = ls())  #clear variables
options(timeout=240); # set timeout to twice default level to avoid abort due to high traffic
 
#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
datasite <- "http://deq2.bse.vt.edu/d.dh" # where to get the raw data to analyze
#----------------------------------------------
base_url <- datasite



#basepath='D:\\Jkrstolic\\R\\deqEcoflows\\GitHub\\r-dh-ecohydro\\';
basepath='C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro\\'

#----Change Basepath here to point to your global config file:

# set your local directory paths in config.local.private located in filepath above
# this file will NOT be sent to git, so it should persist
# so, edit config.local.private once and you should be good to go
source(paste(basepath,'config.local.private',sep='/'));
#source('/var/www/R/config.local.private');


#Load Functions               
source(paste(fxn_locations,"elf_retrieve_data.R", sep = ""));  #loads function used to retrieve F:E data from VAHydro
source(paste(hydro_tools,"VAHydro-2.0/rest_functions.R", sep = "/")); 
rest_uname = FALSE;
rest_pw = FALSE;
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw);

source(paste(fxn_locations,"huc8_groupings.txt", sep = "")); 

# Load Default inputs
source(paste(fxn_locations,"elf_default_inputs.R", sep = ""));
source(paste(fxn_locations,"elf_assemble_batch.R", sep = ""));
source(paste(fxn_locations,"elf_quantreg.R", sep = ""));
source(paste(fxn_locations,"elf_ymax.R", sep = ""));
source(paste(fxn_locations,"elf_pw_it.R", sep = ""));
source(paste(fxn_locations,"elf_pw_it_RS.R", sep = ""));
source(paste(fxn_locations,"elf_twopoint.R", sep = ""));
source(paste(fxn_locations,"elf_pct_chg.R", sep = ""));
source(paste(fxn_locations,"elf_store_data.R", sep = ""));
source(paste(basepath,"Analysis/query_elf_statistics.R", sep = "/")); 
#####

use_icthy_data <- 'YES' #Toggle "YES" to ulitize icthy dataset, otherwise "NO" to use EDAS 

# Now add custom local settings here
inputs$x_metric = c(
  #'nhdp_drainage_sqmi',
  #'erom_q0001e_mean',
  'erom_q0001e_jan',
  'erom_q0001e_feb',
  'erom_q0001e_mar', 
  'erom_q0001e_apr', 
  'erom_q0001e_may',
  'erom_q0001e_june',
  #'erom_q0001e_july',
  #'erom_q0001e_aug',
  #'erom_q0001e_sept',
  'erom_q0001e_oct',
  'erom_q0001e_nov',
  'erom_q0001e_dec'
);

inputs$y_metric = 'aqbio_nt_total';
inputs$sampres = 'species';
inputs$ws_ftype = c('nhd_huc10'); #nhd_huc6, nhd_huc10
inputs$target_hydrocode = '0208020112';# 030102, 060102, 020700

inputs$quantile = .80;

inputs$send_to_rest = "YES";



inputs$glo = 0;
inputs$ghi = 530;
inputs$method = "quantreg"; #quantreg, pwit, ymax, twopoint, pwit_RS
inputs$dataset_tag = 'bpj-530-icthy';

inputs$token = token;

#------------------------------------------------------------------------------------------------
# 1. Get data list - expects header line format with at least target_hydrocode
#    and optional any of the following
# target_hydrocode,name,ghi,glo,


#   ** Use this if you want a batch list to be generated from the inputs array
batchlist = elf_assemble_batch(inputs) 
#   ** or, Use this if you want to load the batch list from a file, with defaults from inputs()


#batchlist = read.csv(file=paste(fxn_locations,"HUC10_RegDiag.csv",sep="/"),header=TRUE)  #RCC_HUC8taxaloss.csv



# 2. check for x_metric in batch list, if not there we merge from inputs$x_metric
bnames = colnames(batchlist)
if (!('x_metric' %in% bnames)) {
  batchlist <- merge(batchlist,data.frame(x_metric = inputs$x_metric))
}

# Batch Start

batch_start = 1; # use 911 after finishing fist one # if we want to skip ahead, do so here.

batch_len = nrow(batchlist)
batch_end = batch_len; #default = batch_len; # if we want to stop early, do so here
# 3. Iterate through each item in the list
for (row in batch_start:batch_end) {
  tin = inputs
  target <- batchlist[row,];
  # 3.1 Check for custom inputs in list
  eligible <- c(
    'glo', 'ghi', 'target_hydrocode', 'x_metric', 'y_metric', 'startdate', 'enddate',
    'bundle', 'ws_ftype', 'name', 'method', 'sampres', 'dataset_tag'
    )
  for (col in eligible) {
    if (col %in% colnames(target)) {
      tin[col] <- target[col]
    }
  }
  print(paste("Record ", row, " out of ", batch_len, " = ", tin$target_hydrocode, " (targetting records ", batch_start, " to ", batch_end, ")",sep=''))
  
  
  # get the raw data
  if (use_icthy_data == 'YES') {
      mydata <- vahydro_fe_data_icthy(
        Watershed_Hydrocode = tin$target_hydrocode, x_metric_code = tin$x_metric, 
        y_metric_code = tin$y_metric, bundle = tin$bundle,  
        ws_ftype_code = tin$ws_ftype, sampres = tin$sampres, datasite = datasite
      );
  }else{ 
      mydata <- vahydro_fe_data(
        Watershed_Hydrocode = tin$target_hydrocode, x_metric_code = tin$x_metric, 
        y_metric_code = tin$y_metric, bundle = tin$bundle,  
        ws_ftype_code = tin$ws_ftype, sampres = tin$sampres, datasite = datasite
      );
  }
  
  #note: add a 0 for the HUC6/HUC10's or else rest feature retrieval doesnt work 
  if (inputs$ws_ftype == 'nhd_huc6') {
    tin$target_hydrocode <- str_pad(tin$target_hydrocode, 6, "left", pad = "0");
  }
  if (inputs$ws_ftype == 'nhd_huc10') {
    tin$target_hydrocode <- str_pad(tin$target_hydrocode, 10, "left", pad = "0");
  }

  # filter out stuff we don't want (can be controlled via tin)
  data <- elf_cleandata(mydata, inputs = tin);
  # 3.2 Run selected routine 
  if (!(is.data.frame(data))) {
    print("No Data Found") 
  } else {
    if (is.null(tin$hydroid) || is.null(tin$geom)) {
      feature <- getFeature(
        list(
          ftype = tin$ws_ftype,
          bundle = tin$bundle,
          hydrocode = tin$target_hydrocode
        )
        , token, base_url);
      
      tin$name = feature$name
      tin$hydroid = feature$hydroid
    }
    if (is.null(tin$name)) {
      tin$name = tin$target_hydrocode
    }
    
    startdate = min(data$tstime)
    enddate = max(data$tstime)
    elf_run_method(
      method = tin$method, inputs = tin, data = data, 
      x_metric_code = as.character(tin$x_metric), y_metric_code = as.character(tin$y_metric), 
      ws_ftype_code = tin$ws_ftype, Feature.Name_code = tin$name, 
      Hydroid_code = tin$hydroid, search_code = tin$target_hydrocode, 
      token = token, startdate = startdate, enddate = enddate, geom = feature$geom 
    )
  }
#  lnz_data = subset(data, log(x_value) > 0)
#  elf_plot_distribution(
#    data = lnz_data,
#    x_metric_code = as.character(tin$x_metric), 
#    y_metric_code = as.character(tin$y_metric), 
#    ws_ftype_code = tin$ws_ftype, 
#    Feature.Name_code = tin$name, 
#    Hydroid_code = tin$hydroid, 
#    search_code = tin$target_hydrocode 
#  )
#  upper.quant <- elf_upper(lnz_data, 0.8)
#  elf_plot_distribution(
#    data = upper.quant,
#    x_metric_code = as.character(tin$x_metric), 
#    y_metric_code = as.character(tin$y_metric), 
#    ws_ftype_code = tin$ws_ftype, 
#    Feature.Name_code = tin$name, 
#    Hydroid_code = tin$hydroid, 
#    search_code = tin$target_hydrocode 
#  )
}
##############################################################################
