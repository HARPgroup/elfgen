context("elfgen-getdata")
library(elfgen)

test_that("Checking length of input watershed.code",
          {
            expect_error(elfgen_getdata("02080"), "Invalid Length of Hydrologic Unit Code")
          })

test_that("Checking for valid watershed.code",
          {
            expect_error(elfgen_getdata("999999999999"), "No IchthyMap Data for Hydrologic Unit Code")
          })

  # test_that("Function returns a dataframe", {
  #   expect_equal(is.data.frame(elfgen_getdata("020700080403")), TRUE)
  # })

