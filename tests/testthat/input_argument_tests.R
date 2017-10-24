library(spartan)
context("Test of Error Checking of Spartan Function Arguments")

test_that("paramvals_length_equals_parameter_length", {
  input_arguments <- as.list(PARAMETERS=c("A","B","C"),PARAMVALS=c("0.1,0.2","1,2,3","1,2"))
  expect_true(check_paramvals_length_equals_parameter_length(input_arguments,TRUE))
})
