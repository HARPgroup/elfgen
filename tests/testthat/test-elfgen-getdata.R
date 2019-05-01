context("elfgen-getdata")

test_that("Checking length of input watershed.code",
          {
            expect_error(elfgen_getdata(9999999999999), "Invalid Length of watershed.code")
          })
