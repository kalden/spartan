library(spartan)
context("Test of Spartan eFAST Analysis")

test_that("efast_generate_medians_for_all_parameter_subsets", {
  # Fortunate here as this function can reuse much of the LHC analysis code, that has
  # already been tested
  setup_efast_result_analysis()

  efast_generate_medians_for_all_parameter_subsets(getwd(), 2, c("A","B"), 2, 2,
             c("Velocity","Displacement"), "Test_eFAST_Result.csv", NULL, 1,
             2)

  # check for output
  expect_true(file.exists(file.path(getwd(),"Curve1_Parameter1_Results.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve1_Parameter2_Results.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Parameter1_Results.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Parameter2_Results.csv")))

  # Check structure
  result<-read_from_csv(file.path(getwd(),"Curve1_Parameter1_Results.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve1_Parameter2_Results.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve2_Parameter1_Results.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve2_Parameter2_Results.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  cleanup_efast()

})

test_that("efast_generate_medians_for_all_parameter_subsets_overTime", {

  setup_efast_result_analysis(overTime=TRUE)
  efast_generate_medians_for_all_parameter_subsets(getwd(), 2, c("A","B"), 2, 2,
                                                   c("Velocity","Displacement"), "Test_eFAST_Result.csv", NULL, 1,
                                                   2,TIMEPOINTS=c(12,36),TIMEPOINTSCALE="Hours")

  # check for output
  expect_true(file.exists(file.path(getwd(),"Curve1_Parameter1_Results_12.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve1_Parameter2_Results_12.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Parameter1_Results_12.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Parameter2_Results_12.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve1_Parameter1_Results_36.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve1_Parameter2_Results_36.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Parameter1_Results_36.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Parameter2_Results_36.csv")))

  # Check structure
  result<-read_from_csv(file.path(getwd(),"Curve1_Parameter1_Results_12.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))
  result<-read_from_csv(file.path(getwd(),"Curve1_Parameter1_Results_36.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve1_Parameter2_Results_12.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))
  result<-read_from_csv(file.path(getwd(),"Curve1_Parameter2_Results_36.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve2_Parameter1_Results_12.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))
  result<-read_from_csv(file.path(getwd(),"Curve2_Parameter1_Results_36.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve2_Parameter2_Results_12.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))
  result<-read_from_csv(file.path(getwd(),"Curve2_Parameter2_Results_36.csv"))
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  cleanup_efast(overTime=TRUE)

})

test_that("efast_get_overall_medians", {

  # Setup:
  setup_efast_result_analysis(overTime=FALSE)
  efast_generate_medians_for_all_parameter_subsets(getwd(), 2, c("A","B"), 2, 2,
                                                   c("Velocity","Displacement"), "Test_eFAST_Result.csv", NULL, 1,
                                                   2)

  # Now run the summary method:
  efast_get_overall_medians(getwd(),2,c("A","B"),2,c("Velocity","Displacement"))

  # Now check for existance of results
  expect_true(file.exists(file.path(getwd(),"Curve1_Results_Summary.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Results_Summary.csv")))

  # Test structures:
  result<-read_from_csv(file.path(getwd(),"Curve1_Results_Summary.csv"))
  expect_true(nrow(result)==2)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve2_Results_Summary.csv"))
  expect_true(nrow(result)==2)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  # Cleanup
  cleanup_efast()
  file.remove(file.path(getwd(),"Curve1_Results_Summary.csv"))
  file.remove(file.path(getwd(),"Curve2_Results_Summary.csv"))
})

test_that("efast_get_overall_medians_overTime", {

  # Setup:
  setup_efast_result_analysis(overTime=TRUE)
  efast_generate_medians_for_all_parameter_subsets(getwd(), 2, c("A","B"), 2, 2,
                                                   c("Velocity","Displacement"), "Test_eFAST_Result.csv", NULL, 1,
                                                   2,TIMEPOINTS=c(12,36),TIMEPOINTSCALE="Hours")

  # Now run the summary method:
  efast_get_overall_medians(getwd(),2,c("A","B"),2,c("Velocity","Displacement"), TIMEPOINTS=c(12,36),TIMEPOINTSCALE="Hours")

  # Now check for existance of results
  expect_true(file.exists(file.path(getwd(),"Curve1_Results_Summary_12.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Results_Summary_12.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve1_Results_Summary_36.csv")))
  expect_true(file.exists(file.path(getwd(),"Curve2_Results_Summary_36.csv")))

  # Test structures:
  result<-read_from_csv(file.path(getwd(),"Curve1_Results_Summary_12.csv"))
  expect_true(nrow(result)==2)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve2_Results_Summary_12.csv"))
  expect_true(nrow(result)==2)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve1_Results_Summary_36.csv"))
  expect_true(nrow(result)==2)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  result<-read_from_csv(file.path(getwd(),"Curve2_Results_Summary_36.csv"))
  expect_true(nrow(result)==2)
  expect_true(ncol(result)==4)
  expect_false(any(is.na(result)))

  # Cleanup
  cleanup_efast(overTime=TRUE)
  file.remove(file.path(getwd(),"Curve1_Results_Summary_12.csv"))
  file.remove(file.path(getwd(),"Curve2_Results_Summary_12.csv"))
  file.remove(file.path(getwd(),"Curve1_Results_Summary_36.csv"))
  file.remove(file.path(getwd(),"Curve2_Results_Summary_36.csv"))
})
