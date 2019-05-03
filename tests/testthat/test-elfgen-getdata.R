context("elfgen-getdata")
library(elfgen)

test_that("Checking length of input watershed.code",
          {
            expect_error(elfgen_getdata("9999999999999"), "Invalid Length of Hydrologic Unit Code")
          })


 test_that("Function returns a dataframe", {
   expect_equal(is.data.frame(elfgen_getdata("020700080403")), TRUE)
   #expect_equal(is.data.frame(elfgen_getdata("0207000804")), TRUE)
   #expect_equal(is.data.frame(elfgen_getdata("02070008")), TRUE)
   #expect_equal(is.data.frame(elfgen_getdata("020700")), TRUE)
 })

