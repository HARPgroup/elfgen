context("aggmax")
library(elfgen)

test.watershed.df <- data.frame(
  MAF = c(100, 200, 300, 400, 526, 600, 700, 800, 400, 900, 1000, 100, 100),
  NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 50, 10, 10,99999,87),
  watershed.code = "testcode"
)


test_that("Function returns a dataframe", {
  expect_equal((aggmax(test.watershed.df)[which(aggmax(test.watershed.df)$x_var==100),])$y_var, 99999)
  expect_equal((aggmax(test.watershed.df)[which(aggmax(test.watershed.df)$x_var==400),])$y_var, 50)
})

