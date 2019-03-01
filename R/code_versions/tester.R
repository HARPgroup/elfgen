#library(Rtools)
#library(devtools)
library(elfgen)

watershed.df <- elfgen_getdata('02070008') #020700080403


#########################################################
inputs <- list()

# custom inputs
inputs$x_metric = c('erom_q0001e_mean','erom_q0001e_aug');
inputs$y_metric = 'aqbio_nt_total';
inputs$sampres = 'species';
inputs$ws_ftype = c('nhd_huc10'); #nhd_huc6, nhd_huc10
inputs$target_hydrocode = '0208020112';# 030102, 060102, 020700
#inputs$quantile = .80;
#inputs$send_to_rest = "YES";
#inputs$glo = 0;
#inputs$ghi = 530;
#inputs$method = "quantreg"; #quantreg, pwit, ymax, twopoint, pwit_RS
#inputs$dataset_tag = 'bpj-530-icthy';
#inputs$token = token;



#########################################################
# BREAKPOINT IDENTIFICATION (CHOOSE 1 of 3 OPTIONS)
#########################################################
# USER DEFINED
breakpt <- 800

# PIECEWISE ITERATIVE
breakpt <- method_pwit("watershed.df" = watershed.df,
                "quantile" = 0.97,
                "glo" = 100,
                "ghi" = 1000)

# YMAX
breakpt <- method_ymax("watershed.df" = watershed.df)
#########################################################
#########################################################

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97,
                "breakpt" = breakpt)

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97)

