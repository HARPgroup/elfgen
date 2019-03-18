#library(Rtools)
#library(devtools) #load_all()
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
inputs$target_hydrocode = '020700';# 030102, 060102, 020700, nhd_huc8_02080201



watershed.df <- elfgen_getdata_vahydro(inputs)
watershed.df <- elfgen_cleandata_vahydro(watershed.df)

#write.table(watershed.df, file = paste("C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/sample_data/",inputs$target_hydrocode,"_vahydro.csv",sep=""),row.names=FALSE, na="",col.names=TRUE, sep=",")
#watershed.df <- read.csv(file = "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/sample_data/nhd_huc8_02070008_vahydro.csv")

#########################################################
# BREAKPOINT IDENTIFICATION (CHOOSE 1 of 3 OPTIONS)
#########################################################
# USER DEFINED
breakpt <- 307
#breakpt <- 10000

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
                "yaxis_thresh" = 53)

# have option to send custom axis labels?
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.90,
                "breakpt" = breakpt,
                "yaxis_thresh" = 53,
                "xlabel" = "Mean Annual Flow (ft3/s)",
                "ylabel" = "Fish Species Richness")

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.8)


#########################################################
#########################################################
# vahydro.base.map("watershed.df" = watershed.df,
#                 "quantile" = 0.8,
#                 "yaxis_thresh" = 53)


elfgen.location <- "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/"

vahydro.base.map(inputs = inputs,
                 "watershed.df" = watershed.df,
                 "quantile" = 0.80,
                 "breakpt" = breakpt,
                 elfgen.location = elfgen.location)


