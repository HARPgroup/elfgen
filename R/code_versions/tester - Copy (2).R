#library(Rtools)
#library(devtools)
library(elfgen)

watershed.df <- elfgen_getdata('02070008') #020700080403
#watershed.df.orig <- watershed.df <- watershed.df.orig

#########################################################
inputs <- list()

# custom inputs
inputs$vahydro_site = 'http://deq2.bse.vt.edu/d.dh';
inputs$x_var = 'erom_q0001e_mean';
inputs$y_var = 'aqbio_nt_total';
inputs$sampres = 'species';
inputs$target_ftype = 'nhd_huc8'; #nhd_huc6, nhd_huc10
inputs$target_hydrocode = 'nhd_huc8_02070008';# 030102, 060102, 020700



watershed.df <- elfgen_getdata_vahydro(inputs)


#########################################################
# BREAKPOINT IDENTIFICATION (CHOOSE 1 of 3 OPTIONS)
#########################################################
# USER DEFINED
breakpt <- 800

# PIECEWISE ITERATIVE
breakpt <- method_pwit("watershed.df" = watershed.df,
                "quantile" = 0.97,
                "glo" = 10,
                "ghi" = 1000)

# YMAX
breakpt <- method_ymax("watershed.df" = watershed.df)
#########################################################
#########################################################

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.80,
                "breakpt" = breakpt)

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97)

