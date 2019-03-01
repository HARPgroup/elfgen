#library(Rtools)
#library(devtools)
library(elfgen)

watershed.df <- elfgen_getdata('02070008') #020700080403


#library(ggplot2)

elfgen_testplot(watershed.df)
#elfgen_quantreg(watershed.df)
#------------------------------------------------------



#elfgen_baseplot(watershed.df,0.97,1000)
#elfgen_baseplot(watershed.df,0.97,1000,"PWIT")
#seq(0.8,0.99,0.01)

#PWIT example
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97,
                #"glo" = 50,
                "ghi" = 1000,
                "method" = "PWIT")

#quantreg example
elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97,
                "ghi" = 1000)

#------------------------------------------------------

#z <- method_pwit(watershed.df,0.97,100,300)
z <- method_pwit("watershed.df" = watershed.df,
                "quantile" = 0.97,
                "glo" = 50,
                "ghi" = 1000)

#------------------------------------------------------

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97,
                "breakpt" = z)

elfgen_baseplot("watershed.df" = watershed.df,
                "quantile" = 0.97)

