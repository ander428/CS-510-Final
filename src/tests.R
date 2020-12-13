library(testthat)

source(knitr::purl("../final.Rmd", quiet=TRUE))

test_df <- data.frame(
  y = c(6300, 5800, 5700, 4500, 4500, 4200, 4100, 3100, 2100, 2500, 2200),
  x = c(4, 4, 5, 5, 7, 7, 8, 9, 10, 11, 12)
)

test_that("Intercept",{
  expect_equal(round(linear_regression(y~x, test_df)[1], 1), 7836.259, tolerance = 0.01)
})

test_that("B_1",{
  expect_equal(round(linear_regression(y~x, test_df)[2], 1), -502.4, tolerance = 0.01)
})

test_that("Summary DF w/ wrong model", {
  expect_error(lm_summary(lm(y~x, test_df)), "unimplemented type 'list' in 'greater'\n")
})

test_that("Summary DF w/ correct model", {
  expect_equal(lm_summary(linear_regression(y~x, test_df)), data.frame(mean=c(7836.2587, -502.4), 
                                                                       median=c(7836.2587, -502.4249),                                                                      sd=c(NaN,NaN)), tolerance = 0.01)
})

