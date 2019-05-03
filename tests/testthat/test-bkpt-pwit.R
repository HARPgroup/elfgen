context("bkpt-pwit")
library(elfgen)

test.watershed.df <- data.frame(
  MAF = c(100, 200, 300, 400, 526, 600, 700, 800, 900, 1000),
  NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 10, 10),
  watershed.code = "testcode"
)


test_that("Function returns a dataframe", {
  expect_equal(bkpt_pwit(test.watershed.df, 0.80, 50, 1000), 526)
  expect_equal(bkpt_pwit(test.watershed.df, 0.60, 50, 1000), 400)
  expect_equal(bkpt_pwit(test.watershed.df, 0.80, "ghi"= 1000), 526)
  expect_equal(bkpt_pwit(test.watershed.df, 0.80, 600, 1000), "NONE IDENTIFIED")
})

test_that("Checking for quantile input parameter", {
  expect_error(bkpt_pwit("watershed.df" = test.watershed.df,
                         "glo" = 50,
                         "ghi"= 1000), "Missing quantile parameter")
})

