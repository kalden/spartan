library(spartan)
context("Test of Error Checking of Spartan Function Arguments")

test_that("paramvals_length_equals_parameter_length", {
  # Same length:
  input_arguments <- make_input_arguments_object(PARAMETERS=c("A","B","C"),PARAMVALS=c("0.1,0.2","1,2,3","1,2"))
  expect_true(check_paramvals_length_equals_parameter_length(input_arguments,TRUE))
  # Parameters shorter
  input_arguments <- make_input_arguments_object(PARAMETERS=c("A","B"),PARAMVALS=c("0.1,0.2","1,2,3","1,2"))
  expect_false(check_paramvals_length_equals_parameter_length(input_arguments,TRUE))
  # Param values shorter
  input_arguments <- make_input_arguments_object(PARAMETERS=c("A","B","C"),PARAMVALS=c("0.1,0.2","1,2,3"))
  expect_false(check_paramvals_length_equals_parameter_length(input_arguments,TRUE))
  # Parameters null
  input_arguments <- make_input_arguments_object(PARAMETERS=NULL,PARAMVALS=c("0.1,0.2","1,2,3"))
  expect_false(check_paramvals_length_equals_parameter_length(input_arguments,TRUE))
  # Parameters a string
  input_arguments <- make_input_arguments_object(PARAMETERS="A",PARAMVALS=c("0.1,0.2","1,2,3"))
  expect_false(check_paramvals_length_equals_parameter_length(input_arguments,TRUE))
  # Parameters an undeclared variable
  input_arguments <- make_input_arguments_object(PARAMETERS=A,PARAMVALS=c("0.1,0.2","1,2,3"))
  expect_message(check_paramvals_length_equals_parameter_length(input_arguments,TRUE),"PARAMVALS or PARAMETERS has been declared incorrectly")
})

test_that("check_robustness_range_or_values", {
  # Using method where min, max, and inc are specified, and not PARAMVALS
  input_arguments <- make_input_arguments_object(PMIN=c(1,1,1),PMAX=c(10,10,10), PINC=c(1,1,1),PARAMVALS=NULL)
  expect_true(check_robustness_range_or_values(input_arguments,TRUE))
  # Using method where PARAMVALS is specified, not min,max,inc
  input_arguments <- make_input_arguments_object(PMIN=NULL,PMAX=NULL, PINC=NULL,PARAMVALS=c("0.1,0.2","0.1,0.2","0.1,0.2"))
  expect_true(check_robustness_range_or_values(input_arguments,TRUE))
  # What if we declare both:
  input_arguments <- make_input_arguments_object(PMIN=c(1,1,1),PMAX=c(10,10,10), PINC=c(1,1,1),PARAMVALS=c("0.1,0.2","0.1,0.2","0.1,0.2"))
  expect_message(check_robustness_range_or_values(input_arguments,TRUE),"You need to specify either PMIN,PMAX,and PINC, or the values to sample in PARAMVALS")
  # What if we specified min, max, and paramvals
  input_arguments <- make_input_arguments_object(PMIN=c(1,1,1),PMAX=c(10,10,10), PINC=NULL,PARAMVALS=c("0.1,0.2","0.1,0.2","0.1,0.2"))
  expect_message(check_robustness_range_or_values(input_arguments,TRUE),"You need to specify either PMIN,PMAX,and PINC, or the values to sample in PARAMVALS")
  # What if we specified only pmin and pmax?
  input_arguments <- make_input_arguments_object(PMIN=c(1,1,1),PMAX=c(10,10,10), PINC=NULL,PARAMVALS=NULL)
  expect_message(check_robustness_range_or_values(input_arguments,TRUE),"You need to specify either PMIN,PMAX,and PINC, or the values to sample in PARAMVALS")
})
