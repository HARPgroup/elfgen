#library(Rtools)
#library(devtools)
library(elfgen)

watershed.df <- elfgen_getdata('02070008') #020700080403, 02070008
#watershed.df.orig <- watershed.df <- watershed.df.orig

#########################################################

# custom inputs
inputs <- list()
inputs$vahydro_site = 'http://deq2.bse.vt.edu/d.dh';
inputs$x_var = 'erom_q0001e_mean';
inputs$y_var = 'aqbio_nt_total';
inputs$sampres = 'species';
inputs$target_ftype = 'nhd_huc6'; #nhd_huc6, nhd_huc10
inputs$target_hydrocode = '020700';# 030102, 060102, 020700



watershed.df <- elfgen_getdata_vahydro(inputs)
watershed.df <- elfgen_cleandata_vahydro(watershed.df)

#write.table(watershed.df, file = paste("C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/sample_data/",inputs$target_hydrocode,"_vahydro.csv",sep=""),row.names=FALSE, na="",col.names=TRUE, sep=",")
watershed.df <- read.csv(file = "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/sample_data/nhd_huc8_02070008_vahydro.csv")

#########################################################
# BREAKPOINT IDENTIFICATION (CHOOSE 1 of 3 OPTIONS)
#########################################################
# USER DEFINED
breakpt <- 800

# PIECEWISE ITERATIVE
breakpt <- method_pwit("watershed.df" = watershed.df,
                "quantile" = 0.80,
                "glo" = 290,
                "ghi" = 350)

# YMAX
breakpt <- method_ymax("watershed.df" = watershed.df)
#########################################################
#########################################################

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.80,
                "breakpt" = breakpt,
                "yaxis_thresh" = 55)

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97)

