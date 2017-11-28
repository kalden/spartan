library(spartan)
context("Test of Spartan Robustness Analysis")

test_that("oat_processParamSubsets", {
  # Below tests examine each internal function
  # Here we just need to test that we get some output
  # All input has also been checked in test input argument

  setup_multiple_parameter_result_analysis()

  oat_processParamSubsets(getwd(), c("chemoLowerLinearAdjust","chemoUpperLinearAdjust"),1,
                          c("Velocity","Displacement"), RESULTFILENAME="Test_Robustness_Result.csv",
                          ALTERNATIVEFILENAME=NULL, OUTPUTFILECOLSTART=1, OUTPUTFILECOLEND=2, "Test_Summary_File.csv",
                          c(0.05,2),PMIN=c(0.05,1),PMAX=c(0.1,2),
                          PINC=c(0.05,1), PARAMVALS=NULL, TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL)

  expect_true(file.exists(file.path(getwd(),"Test_Summary_File.csv")))

  cleanup()
  file.remove(file.path(getwd(),"Test_Summary_File.csv"))


})

test_that("generate_summary_stats_for_all_param_sets", {

  setup_multiple_parameter_result_analysis()

  summary_two_param <- generate_summary_stats_for_all_param_sets(getwd(),c("chemoLowerLinearAdjust","chemoUpperLinearAdjust"),c(0.05,2),PMIN=c(0.05,1),PMAX=c(0.1,2),
                                                                 PINC=c(0.05,1), PARAMVALS=NULL, 1, c("Velocity","Displacement"), "Test_Robustness_Result.csv", NULL, 1,2)

  # Should be three rows (if baseline has not be evaluated twice)
  expect_equal(nrow(summary_two_param),3)
  # Four columns - 2 param values and two results
  expect_equal(ncol(summary_two_param),4)
  expect_equal(toString(colnames(summary_two_param)),"chemoLowerLinearAdjust, chemoUpperLinearAdjust, Velocity, Displacement")
  # Check values (just makes sure no NAs)
  expect_equal(as.numeric(summary_two_param[1,1]),0.05)
  expect_equal(as.numeric(summary_two_param[2,1]),0.10)
  expect_equal(as.numeric(summary_two_param[3,1]),0.05)
  expect_equal(as.numeric(summary_two_param[1,2]),2)
  expect_equal(as.numeric(summary_two_param[3,2]),1)
  expect_equal(round(as.numeric(summary_two_param[2,3]),digits=6),2.156870)
  expect_equal(round(as.numeric(summary_two_param[2,4]),digits=5),43.18424)


  unlink(paste(getwd(),"/chemoLowerLinearAdjust/",sep=""), recursive = TRUE)
  unlink(paste(getwd(),"/chemoUpperLinearAdjust/",sep=""), recursive = TRUE)
})

test_that("produce_summary_for_all_values_of_parameter", {

  # Make test file, in correct directory structure
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.05"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.05","1"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.1"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.1","1"))
  make_test_sim_result_file(paste(getwd(),"/chemoLowerLinearAdjust/0.05/1/Test_Robustness_Result.csv",sep=""),1)
  make_test_sim_result_file(paste(getwd(),"/chemoLowerLinearAdjust/0.1/1/Test_Robustness_Result.csv",sep=""),2)

  PARAMETERS<-c("chemoLowerLinearAdjust")
  param_val_list <-
    as.numeric(prepare_parameter_value_list(PMIN=c(0.05), PMAX=c(0.10), PINC=c(0.05),
                                            NULL, 1))
  EXP_PARAMS <- as.character(c(0.05))

  param_result <- produce_summary_for_all_values_of_parameter(getwd(),1, param_val_list, BASELINE=c(0.05), FALSE, PARAMETERS, EXP_PARAMS,
    1, c("Velocity","Displacement"),"Test_Robustness_Result.csv", NULL, 1,2)

  # Test the baseline has been evaluated
  expect_true(param_result$baseline_evaluated)
  expect_equal(nrow(param_result$parameter_result),2)
  expect_equal(ncol(param_result$parameter_result),3)
  # Check the values
  expect_equal(round(as.numeric(param_result$parameter_result[1,2]),digits=6),2.101808)
  expect_equal(round(as.numeric(param_result$parameter_result[2,2]),digits=6),2.156870)
  expect_equal(round(as.numeric(param_result$parameter_result[1,3]),digits=5),44.09774)
  expect_equal(round(as.numeric(param_result$parameter_result[2,3]),digits=5),43.18424)

  unlink(paste(getwd(),"/chemoLowerLinearAdjust/",sep=""), recursive = TRUE)


})

test_that("process_parameter_value_if_exists", {

  # Make test file, in correct directory structure
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.05"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.05","1"))
  make_test_sim_result_file(paste(getwd(),"/chemoLowerLinearAdjust/0.05/1/Test_Robustness_Result.csv",sep=""),1)

  # Requires exp_params for labelling results, but not too much of a worry here
  BASELINE<-c(50, 0.3, 0.2, 0.04, 0.60, 1.0)
  EXP_PARAMS <- as.character(BASELINE)

  summary_stats <- process_parameter_value_if_exists(getwd(),1,c("Velocity","Displacement"),"Test_Robustness_Result.csv",NULL,1,2,"chemoLowerLinearAdjust", 0.05, EXP_PARAMS)

  # Return in this case should be the same as the test below (which calls the test below function)
  expect_equal(nrow(summary_stats),1)
  expect_equal(ncol(summary_stats),8)
  # Check the median values are correct
  expect_equal(round(as.numeric(summary_stats[1,7]),digits=6),2.101808)
  expect_equal(round(as.numeric(summary_stats[1,8]),digits=5),44.09774)

  # What if the file does not exist
  expect_message(process_parameter_value_if_exists(getwd(),1,c("Velocity","Displacement"),"Test_Robustness_Result.csv",NULL,1,2,"chemoLowerLinearAdjust", 0.10, EXP_PARAMS),"No results can be found for parameter: chemoLowerLinearAdjust Value: 0.1")

  unlink(paste(getwd(),"/chemoLowerLinearAdjust/",sep=""), recursive = TRUE)
})

test_that("generate_medians_for_param_set" , {

  # Write and place in the correct directory
  dir.create(file.path(getwd(),1))
  make_test_sim_result_file(paste(getwd(),"/1/Test_Robustness_Result.csv",sep=""),1)

  # Requires exp_params for labelling results, but not too much of a worry here
  BASELINE<-c(50, 0.3, 0.2, 0.04, 0.60, 1.0)
  EXP_PARAMS <- as.character(BASELINE)

  # Now to test the result produces the correct structure:
  summary_stats <- generate_medians_for_param_set(getwd(), 1, c("Velocity","Displacement"), "Test_Robustness_Result.csv",
    NULL,1, 2, EXP_PARAMS, "chemoLowerLinearAdjust",0.05)

  # examine the structure created
  expect_equal(nrow(summary_stats),1)
  expect_equal(ncol(summary_stats),8)
  # Check the median values are correct
  expect_equal(round(as.numeric(summary_stats[1,7]),digits=6),2.101808)
  expect_equal(round(as.numeric(summary_stats[1,8]),digits=5),44.09774)

  # Remove the created structure
  unlink(paste(getwd(),"/1/",sep=""), recursive = TRUE)

})
