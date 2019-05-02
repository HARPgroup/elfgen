context("bkpt-pwit")
library(elfgen)



test_that("Function returns a dataframe", {
  test.watershed.df <- data.frame(
    MAF = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
    NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 10, 10),
    watershed.code = "testcode"
  )

  expect_equal(bkpt_pwit(test.watershed.df, 0.80, 50, 1000), 500)
})







