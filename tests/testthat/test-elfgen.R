context("elfgen")
library(elfgen)


 test.watershed.df <- data.frame(
   MAF = c(100, 100, 200, 200, 300, 300, 400, 400, 526, 526, 600, 600, 700, 700, 800, 800, 900, 900, 1000, 1000),
   NT.TOTAL.UNIQUE = c(5, 10, 10, 20, 15, 30, 20, 40, 25, 50, 20, 40, 15, 30 , 10, 20, 5, 10, 5, 10),
   watershed.code = "testcode"
 )


 test_that("Testing breakpt default when none is provided", {
    expect_equal(class(elfgen("watershed.df" = test.watershed.df,
                        "quantile" = 0.60,
                        "yaxis_thresh"= 53)), c("gg", "ggplot"))
 })

 test_that("Testing yaxis_thresh default when none is provided", {
   expect_equal(class(elfgen("watershed.df" = test.watershed.df,
                             "quantile" = 0.60,
                             "breakpt" = 526)), c("gg", "ggplot"))
 })

 test_that("Checking for successful upper.quant subset", {
    expect_error(elfgen(test.watershed.df, 0.80, 526, 56), "Upper quantile subset contains fewer than 3 datapoints")
    expect_error(elfgen(test.watershed.df, 0.70, 526, 53), "Upper quantile subset contains fewer than 3 datapoints")
  })

 test_that("Testing xlabel and ylabel overrides", {
   expect_equal(class(elfgen("watershed.df" = test.watershed.df,
                             "quantile" = 0.60,
                             "breakpt" = 526,
                             "yaxis_thresh"= 53,
                             "xlabel" = "Mean Annual Flow (ft3/s)",
                             "ylabel" = "Fish Species Richness")), c("gg", "ggplot"))
 })

 #---------------------------------------------------
# c(5.00000,  5.00000 ,20.77324, 20.77324, 30.00000, 30.00000, 36.54649, 36.54649, 42.77791, 42.77791)
#  elfgen(test.watershed.df, 0.50, 526, 56)
#  elfgen("watershed.df" = watershed.df,
#                  "quantile" = 0.60, #for this one: DONT USE 0.5 for quantile AKA the median - this will blow up rq() function
#                  "breakpt" = 526,
#                  "yaxis_thresh" = 53)



 test_that("Function returns a ggplot object", {
    expect_equal(class(elfgen(test.watershed.df, 0.60, 526, 53)), c("gg", "ggplot"))

 })

# watershed.df <- read.csv(file = "C:/Users/nrf46657/Desktop/elfgen_pre_4.29.19/R/sample_data/nhd_huc8_02070008_vahydro.csv")
# plt <- elfgen(watershed.df, 0.8, 526, 53)
#



