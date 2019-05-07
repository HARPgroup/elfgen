context("elfgen")
library(elfgen)

# test.watershed.df <- data.frame(
#   MAF = c(100, 200, 300, 400, 526, 600, 700, 800, 900, 1000),
#   NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 10, 10),
#   watershed.code = "testcode"
# )

 test.watershed.df <- data.frame(
   MAF = c(100, 100, 200, 200, 300, 300, 400, 400, 526, 526, 600, 600, 700, 700, 800, 800, 900, 900, 1000, 1000),
   NT.TOTAL.UNIQUE = c(5, 10, 10, 20, 15, 30, 20, 40, 25, 50, 20, 40, 15, 30 , 10, 20, 5, 10, 5, 10),
   watershed.code = "testcode"
 )

 # test_that("Testing breakpt default when none is provided", {
 #   expect_equal(elfgen("watershed.df" = test.watershed.df,
 #                        "quantile" = 0.60,
 #                        "yaxis_thresh"= 53), "Data Subset (Lower 60%)")
 # })

 test_that("Testing breakpt default when none is provided", {
    expect_equal(class(elfgen("watershed.df" = test.watershed.df,
                        "quantile" = 0.60,
                        "yaxis_thresh"= 53)), c("gg", "ggplot"))
 })

# test_that("Function returns a plot figure", {
#   # expect_equal(bkpt_pwit(test.watershed.df, 0.80, 50, 1000), 526)
#   # expect_equal(bkpt_pwit(test.watershed.df, 0.60, 50, 1000), 400)
#   # expect_equal(bkpt_pwit(test.watershed.df, 0.80, "ghi"= 1000), 526)
#   # expect_equal(bkpt_pwit(test.watershed.df, 0.80, 600, 1000), "NONE IDENTIFIED")
#   expect_equal(elfgen(test.watershed.df, 0.80, 526, 56), 526)
# })

#LOOK BACK AT THIS
#elfgen(test.watershed.df, 0.80, 526, 56)  #Error in ru$coefficients[2, 1] : subscript out of bound
#elfgen(test.watershed.df, 0.70, 526, 56) #Computation failed in `stat_quantile()`: (rursadj and rup = NaN)


#elfgen(test.watershed.df, 0.50, 526, 56)



 test_that("Checking for successful upper.quant subset", {
    expect_error(elfgen(test.watershed.df, 0.80, 526, 56), "Upper quantile subset contains fewer than 2 datapoints")
  })


 #---------------------------------------------------
# c(5.00000,  5.00000 ,20.77324, 20.77324, 30.00000, 30.00000, 36.54649, 36.54649, 42.77791, 42.77791)
#
#  elfgen("watershed.df" = watershed.df,
#                  "quantile" = 0.60, #DONT USE 0.5 for quantile AKA the median - this will blow up rq() function
#                  "breakpt" = 526,
#                  "yaxis_thresh" = 53)


 # test_that("Function returns a newy", {
 #   expect_equal(elfgen(test.watershed.df, 0.60, 526, 53), "Data Subset (Lower 60%)")
 #
 # })

 test_that("Function returns a ggplot object", {
    expect_equal(class(elfgen(test.watershed.df, 0.60, 526, 53)), c("gg", "ggplot"))

 })

#  watershed.df <- read.csv(file = "C:/Users/nrf46657/Desktop/elfgen_pre_4.29.19/R/sample_data/nhd_huc8_02070008_vahydro.csv")
# plt <- elfgen(watershed.df, 0.8, 526, 53)
#
#
#  elfgen(test.watershed.df, 0.7, 526, 53) #Warning message:
                                           #In qt((1 - level)/2, df) : NaNs produced


