#library(Rtools)
#library(devtools) #load_all()
library(elfgen)

watershed.df <- elfgen_getdata('02080201') #020700080403, 02070008, 02080201
#watershed.df.orig <- watershed.df <- watershed.df.orig

#########################################################

# custom inputs
inputs <- list()
inputs$vahydro_site = 'http://deq2.bse.vt.edu/d.dh';
inputs$x_var = 'nhdp_drainage_sqmi';                #erom_q0001e_mean, nhdp_drainage_sqmi
inputs$y_var = 'aqbio_benthic_nt_total';          #aqbio_nt_total, aqbio_benthic_nt_total
inputs$sampres = 'maj_fam_gen';               #species, maj_fam_gen_spec
inputs$target_ftype = 'nhd_huc6'; #nhd_huc6, nhd_huc10, state
inputs$target_hydrocode = '020700';# 030102, 060102, 020700, nhd_huc8_02080201, nhd_huc8_03010103, usa_state_virginia
                                    #020801


#020801
#02080103
#0208010302



watershed.df <- elfgen_getdata_vahydro(inputs)
watershed.df <- elfgen_cleandata_vahydro(watershed.df)

#write.table(watershed.df, file = paste("C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/sample_data/",inputs$target_hydrocode,"_vahydro.csv",sep=""),row.names=FALSE, na="",col.names=TRUE, sep=",")
#watershed.df <- read.csv(file = "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/sample_data/nhd_huc8_02070008_vahydro.csv")

#########################################################
# BREAKPOINT IDENTIFICATION (CHOOSE 1 of 3 OPTIONS)
#########################################################
# USER DEFINED
#breakpt <- 307
breakpt <- 90 #200
#breakpt <- 15000

# PIECEWISE ITERATIVE
breakpt <- method_pwit("watershed.df" = watershed.df,
                "quantile" = 0.95,
                "glo" = 50,   #50  #290 200
                "ghi" = 1000)  #500  #350 1000

# YMAX
breakpt <- method_ymax("watershed.df" = watershed.df)
#########################################################
#########################################################
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.8)


elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.80,
                "breakpt" = breakpt,
                "yaxis_thresh" = 53)

# have option to send custom axis labels?
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.95,
                "breakpt" = breakpt,
                "yaxis_thresh" = 53,
                "xlabel" = "Mean Annual Flow (ft3/s)",
                "ylabel" = "Fish Species Richness")

# Benthic defaults
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.8,
                "breakpt" = breakpt,
                "yaxis_thresh" = 90,
                "xlabel" = "Mean Annual Flow (ft3/s)",
                "ylabel" = "Benthic Species Richness")

#########################################################
#########################################################
# vahydro.base.map("watershed.df" = watershed.df,
#                 "quantile" = 0.8,
#                 "yaxis_thresh" = 53)


elfgen.location <- "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/elfgen/R/"

vahydro.base.map(inputs = inputs,
                 "watershed.df" = watershed.df,
                 "quantile" = 0.8,
                 "breakpt" = breakpt,
                 elfgen.location = elfgen.location)


#########################################################
extent <- data.frame(x = c(-84, -75),
                     y = c(35, 41))

vahydro.base.map.custom(inputs = inputs,
                        "watershed.df" = watershed.df,
                        "quantile" = 0.8,
                        "breakpt" = breakpt,
                        elfgen.location = elfgen.location,
                        extent = extent)
