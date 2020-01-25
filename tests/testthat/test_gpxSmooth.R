context("gtxSmooth")

test_that("make_odd_k",{
  expect_equal(tcxAnalyser:::makeKOdd(11), 11)
  expect_equal(tcxAnalyser:::makeKOdd(12), 13)
  expect_equal(tcxAnalyser:::makeKOdd(12.1), 13)
  
})

test_that('wrongK',{
  expect_error(tcxAnalyser:::validateK(NULL))
  expect_error(tcxAnalyser:::validateK(0))
  expect_error(tcxAnalyser:::validateK(0.5))
  expect_error(tcxAnalyser:::validateK(-2))
  expect_error(tcxAnalyser:::validateK(NA))
  expect_error(tcxAnalyser:::validateK('hi'))
  expect_error(tcxAnalyser:::validateK(c(1,2)))
})

test_that('right_results',{
  inputData <- data.frame(a=seq(1,10))
  expected <- data.frame(a=c(2, 2, 3, 4, 5, 6, 7, 8, 9, 9))
  actual <- gpxSmooth(inputData, k=3)
  expect_equal(actual, expected)
})
