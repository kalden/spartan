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

test_that("check_robustness_paramvals_contains_baseline", {
  # Check method works as it should, where baseline is contained in paramvals
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4,0.5),PARAMVALS=c("2,3,4","1,4,7","0.1,0.2,0.3,0.4,0.5,0.6,0.7"))
  expect_true(check_robustness_paramvals_contains_baseline(input_arguments,TRUE))
  # Where baseline does not contain the value
  input_arguments <- make_input_arguments_object(BASELINE=c(1,4,0.5),PARAMVALS=c("2,3,4","1,4,7","0.1,0.2,0.3,0.4,0.5,0.6,0.7"))
  expect_message(check_robustness_paramvals_contains_baseline(input_arguments,"PARAMVALS should contain the BASELINE value, else behaviours cannot be compared"))
  # Test lengths npt equal, although this should be picked up in other tests
  input_arguments <- make_input_arguments_object(BASELINE=c(1,4,0.5),PARAMVALS=c("2,3,4","1,4,7"))
  expect_message(check_robustness_paramvals_contains_baseline(input_arguments,"PARAMVALS should contain the BASELINE value, else behaviours cannot be compared"))
  # Test a null baseline & paramvals
  input_arguments <- make_input_arguments_object(BASELINE=NULL,PARAMVALS=c("2,3,4","1,4,7"))
  expect_message(check_robustness_paramvals_contains_baseline(input_arguments,"Error in declaring BASELINE or PARAMVALS"))
  input_arguments <- make_input_arguments_object(BASELINE=c(1,4,0.5),PARAMVALS=NULL)
  expect_message(check_robustness_paramvals_contains_baseline(input_arguments,"Error in declaring BASELINE or PARAMVALS"))
  # Whether baseline contains a string
  input_arguments <- make_input_arguments_object(BASELINE=c(1,"!",0.5),PARAMVALS=NULL)
  expect_message(check_robustness_paramvals_contains_baseline(input_arguments,"Error in declaring BASELINE or PARAMVALS"))
  # Whether baseline contains a reference to an undeclared argument
  input_arguments <- make_input_arguments_object(BASELINE=c(1,B,0.5),PARAMVALS=NULL)
  expect_message(check_robustness_paramvals_contains_baseline(input_arguments,"Error in declaring BASELINE or PARAMVALS"))
})

test_that("check_robustness_range_contains_baseline", {
  # Similar to above, but this checks the baseline is in the sequence defined by PMIN, PMAX, and PINC
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4,0.5),PMIN=c(2,1,0.1), PMAX=c(4,7,0.7), PINC=c(1,3,0.1))
  expect_true(check_robustness_range_contains_baseline(input_arguments,TRUE))
  # What if baseline is not contained:
  input_arguments <- make_input_arguments_object(BASELINE=c(5,4,0.5),PMIN=c(2,1,0.1), PMAX=c(4,7,0.7), PINC=c(1,3,0.1))
  expect_message(check_robustness_range_contains_baseline(input_arguments,TRUE),"Range specified by PMIN and PMAX should contain the BASELINE value, else behaviours cannot be compared")
  # Lengths not equal:
  input_arguments <- make_input_arguments_object(BASELINE=c(5,4),PMIN=c(2,1,0.1), PMAX=c(4,7,0.7), PINC=c(1,3,0.1))
  expect_message(check_robustness_range_contains_baseline(input_arguments,TRUE),"Range specified by PMIN and PMAX should contain the BASELINE value, else behaviours cannot be compared")
  # Test nulls
  input_arguments <- make_input_arguments_object(BASELINE=c(5,4),PMIN=c(2,1,0.1), PMAX=NULL, PINC=c(1,3,0.1))
  expect_message(check_robustness_range_contains_baseline(input_arguments,TRUE),"Error in declaring BASELINE in range specified by PMIN, PMAX, and PINC")
  # Contains string or reference to undeclared argument
  input_arguments <- make_input_arguments_object(BASELINE=c(5,4),PMIN=c(2,1,0.1), PMAX=NULL, PINC=c(1,"A",0.1))
  expect_message(check_robustness_range_contains_baseline(input_arguments,TRUE),"Error in declaring BASELINE in range specified by PMIN, PMAX, and PINC")
  input_arguments <- make_input_arguments_object(BASELINE=c(5,4),PMIN=c(2,1,0.1), PMAX=NULL, PINC=c(1,A,0.1))
  expect_message(check_robustness_range_contains_baseline(input_arguments,TRUE),"Error in declaring BASELINE in range specified by PMIN, PMAX, and PINC")
})

test_that("check_robustness_parameter_and_ranges_lengths", {
  # PMIN, PMAX, PINC, BASELINE, and PARAMETERS should all be the same length
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4,0.5),PMIN=c(2,1,0.1), PMAX=c(4,7,0.7), PINC=c(1,3,0.1), PARAMETERS=c("A","B","C"))
  expect_true(check_robustness_parameter_and_ranges_lengths(input_arguments,TRUE))
  # If lengths differ:
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4),PMIN=c(2,1,0.1), PMAX=c(4,7,0.7), PINC=c(1,3,0.1), PARAMETERS=c("A","B","C"))
  expect_message(check_robustness_parameter_and_ranges_lengths(input_arguments,TRUE),"Number of entries in PARAMETERS, BASELINE, PMIN, PMAX, and PINC should be equal")
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4,5),PMIN=c(2,1,0.1), PMAX=c(4,7,0.7), PINC=c(1,3,0.1), PARAMETERS=c("A","B"))
  expect_message(check_robustness_parameter_and_ranges_lengths(input_arguments,TRUE),"Number of entries in PARAMETERS, BASELINE, PMIN, PMAX, and PINC should be equal")
  # Test nulls
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4,5),PMIN=NULL, PMAX=c(4,7,0.7), PINC=c(1,3,0.1), PARAMETERS=c("A","B"))
  expect_message(check_robustness_parameter_and_ranges_lengths(input_arguments,TRUE),"Number of entries in PARAMETERS, BASELINE, PMIN, PMAX, and PINC should be equal")
  # Contains reference to undeclared arguments
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4,5),PMIN=NULL, PMAX=c(4,7,A), PINC=c(1,3,0.1), PARAMETERS=c("A","B","C"))
  expect_message(check_robustness_parameter_and_ranges_lengths(input_arguments,TRUE),"Error in declaring PARAMETERS, BASELINE, PMIN, PMAX, or PINC. Check all lengths are equal and all are numeric")
  # Rare, as this comes from a match call to a function, but what if an argument is missing
  input_arguments <- make_input_arguments_object(BASELINE=c(3,4,5),PMIN=NULL, PMAX=c(4,7,0.7), PARAMETERS=c("A","B","C"))
  expect_message(check_robustness_parameter_and_ranges_lengths(input_arguments,TRUE),"Number of entries in PARAMETERS, BASELINE, PMIN, PMAX, and PINC should be equal")

  })

test_that("check_filepath_exists", {

  # Filepath should exist
  input_arguments <- make_input_arguments_object(FILEPATH="/home/kja505/Desktop")
  skip_on_travis()
  expect_true(check_filepath_exists(input_arguments,TRUE))
  # Filepath does not exist:
  input_arguments <- make_input_arguments_object(FILEPATH="/home/kja505/Desktop2")
  expect_message(check_filepath_exists(input_arguments,TRUE),paste("FILEPATH does not seem to exist:", input_arguments$FILEPATH))
  # Filepath stated as an undelared variable
  input_arguments <- make_input_arguments_object(FILEPATH=VAR)
  expect_message(check_filepath_exists(input_arguments,TRUE),paste("FILEPATH does not seem to exist:", input_arguments$FILEPATH))
  # Filepath is null
  input_arguments <- make_input_arguments_object(FILEPATH=NULL)
  expect_message(check_filepath_exists(input_arguments,TRUE),paste("FILEPATH does not seem to exist:", input_arguments$FILEPATH))
})

test_that("check_lhs_algorithm", {
  # Test accepts optimal and normal
  input_arguments <- make_input_arguments_object(ALGORITHM="optimal")
  expect_true(check_lhs_algorithm(input_arguments,TRUE))
  input_arguments <- make_input_arguments_object(ALGORITHM="normal")
  expect_true(check_lhs_algorithm(input_arguments,TRUE))
  input_arguments <- make_input_arguments_object(ALGORITHM="NORMAL")
  expect_true(check_lhs_algorithm(input_arguments,TRUE))
  input_arguments <- make_input_arguments_object(ALGORITHM="OPTIMAL")
  expect_true(check_lhs_algorithm(input_arguments,TRUE))
  # Should not accept anything else
  input_arguments <- make_input_arguments_object(ALGORITHM="OTHER")
  expect_message(check_lhs_algorithm(input_arguments,TRUE),"LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
  input_arguments <- make_input_arguments_object(ALGORITHM="")
  expect_message(check_lhs_algorithm(input_arguments,TRUE),"LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
  # Should not be an unreferenced variable
  input_arguments <- make_input_arguments_object(ALGORITHM=A)
  expect_message(check_lhs_algorithm(input_arguments,TRUE),"LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
  # Or a number
  input_arguments <- make_input_arguments_object(ALGORITHM=1)
  expect_message(check_lhs_algorithm(input_arguments,TRUE),"LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
  # Or null
  input_arguments <- make_input_arguments_object(ALGORITHM=NULL)
  expect_message(check_lhs_algorithm(input_arguments,TRUE),"LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
})

test_that("check_package_installed", {
  # Test that the lhs package is installed
  expect_true(check_package_installed("lhs",TRUE))
  # Check one that isn't, e.g. dplyr
  expect_message(check_package_installed("dplyr",TRUE),"Looking for package dplyr, which is not installed or does not exist")
  # Check a package that does not exist
  expect_message(check_package_installed("York",TRUE),"Looking for package York, which is not installed or does not exist")
  # Check null
  expect_message(check_package_installed(NULL,TRUE),"Looking for package , which is not installed or does not exist")

})

test_that("check_lengths_parameters_ranges", {
  # Test that the parameters, PMIN, and PMAX are the same length
  input_arguments <- make_input_arguments_object(PMIN=c(2,1,0.1), PMAX=c(4,7,0.7), PARAMETERS=c("A","B","C"))
  expect_true(check_lengths_parameters_ranges(input_arguments,TRUE))
  # Check not the same lengths
  input_arguments <- make_input_arguments_object(PMIN=c(2,1), PMAX=c(4,7,0.7), PARAMETERS=c("A","B","C"))
  expect_message(check_lengths_parameters_ranges(input_arguments,TRUE),"Number of parameters must match the numbers of entries in PMIN and PMAX")
  input_arguments <- make_input_arguments_object(PMIN=c(2,1), PMAX=c(7,0.7), PARAMETERS=c("A","B","C"))
  expect_message(check_lengths_parameters_ranges(input_arguments,TRUE),"Number of parameters must match the numbers of entries in PMIN and PMAX")
  # Check for nulls
  input_arguments <- make_input_arguments_object(PMIN=c(2,1,0.1), PMAX=NULL, PARAMETERS=c("A","B","C"))
  expect_message(check_lengths_parameters_ranges(input_arguments,TRUE),"Number of parameters must match the numbers of entries in PMIN and PMAX")
  # Check for reference to undeclared variables
  input_arguments <- make_input_arguments_object(PMIN=A, PMAX=c(4,7,0.7), PARAMETERS=c("A","B","C"))
  expect_message(check_lengths_parameters_ranges(input_arguments,TRUE),"Value error in PMIN or PMAX. Check these are numeric")
  # Check for strings in PMIN and PMAX
  input_arguments <- make_input_arguments_object(PMIN=c("A",1,0.1), PMAX=c(4,7,0.7), PARAMETERS=c("A","B","C"))
  expect_message(check_lengths_parameters_ranges(input_arguments,TRUE),"Value error in PMIN or PMAX. Check these are numeric")

})

test_that("check_numeric_list_values", {
  # Min should be less than max, for all values
  expect_true(check_numeric_list_values(smallList=c(2,1,0.1), largerList=c(4,7,0.7),"PMIN","PMAX",TRUE))
  # Should return an error and false if not
  expect_false(check_numeric_list_values(smallList=c(12,1,0.1), largerList=c(4,7,0.7),"PMIN","PMAX",TRUE))
  expect_message(check_numeric_list_values(smallList=c(12,1,0.1), largerList=c(4,7,0.7),"PMIN","PMAX",TRUE),"PMIN must be less than PMAX for all parameters, and must be numeric")
  # If one is null
  expect_message(check_numeric_list_values(smallList=NULL, largerList=c(4,7,0.7),"PMIN","PMAX",TRUE),"PMIN must be less than PMAX for all parameters, and must be numeric")
  # If not numeric
  expect_message(check_numeric_list_values(smallList=c(2,1,0.1), largerList=c(4,7,"a"),"PMIN","PMAX",TRUE),"PMIN must be less than PMAX for all parameters, and must be numeric")
  # Reference to an undeclared variable
  expect_message(check_numeric_list_values(smallList=NULL, largerList=A, "PMIN","PMAX",TRUE),"Value error in PMIN or PMAX. Check these are numeric")

})

test_that("check_argument_positive_int", {
  # Positive integers should be accepted
  expect_true(check_argument_positive_int(50,TRUE,"NUMSAMPLES"))
  # If negative:
  expect_message(check_argument_positive_int(-50,TRUE,"NUMSAMPLES"),"NUMSAMPLES must be a positive integer. Terminated")
  # Double
  expect_message(check_argument_positive_int(50.234,TRUE,"NUMSAMPLES"),"NUMSAMPLES must be a positive integer. Terminated")
  # String
  expect_message(check_argument_positive_int("A",TRUE,"NUMSAMPLES"),"NUMSAMPLES must be a positive integer. Terminated")
  # Referenced variable that doesn't exist
  expect_message(check_argument_positive_int(A,TRUE,"NUMSAMPLES"),"NUMSAMPLES must be a positive integer. Terminated")
  # Null
  expect_message(check_argument_positive_int(NULL,TRUE,"NUMSAMPLES"),"NUMSAMPLES must be a positive integer. Terminated")

})
