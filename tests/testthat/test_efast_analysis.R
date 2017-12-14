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

test_that("read_all_curve_results", {

  # Setup:
  setup_efast_result_analysis(overTime=FALSE)
  efast_generate_medians_for_all_parameter_subsets(getwd(), 2, c("A","B"), 2, 2,
                                                   c("Velocity","Displacement"), "Test_eFAST_Result.csv", NULL, 1,
                                                   2)
  # Summary method:
  efast_get_overall_medians(getwd(),2,c("A","B"),2,c("Velocity","Displacement"))

  # Now see if we can read these results in:
  efast_sim_results <- read_all_curve_results(getwd(), GRAPHTIME=NULL, 2,
                                              2,  c("Velocity","Displacement"), c("A","B"))

  # Check the structure - should be a multi-dimensional array
  expect_true(all(dim(efast_sim_results)==c(2,4,2)))
  # Each of the curves should have 2 rows and 4 columns in this case:
  expect_true(nrow(efast_sim_results[,,1])==2)
  expect_true(nrow(efast_sim_results[,,2])==2)
  expect_true(ncol(efast_sim_results[,,1])==4)
  expect_true(ncol(efast_sim_results[,,2])==4)
  expect_false(any(is.na(efast_sim_results[,,1])))
  expect_false(any(is.na(efast_sim_results[,,2])))

  cleanup_efast()
  file.remove(file.path(getwd(),"Curve1_Results_Summary.csv"))
  file.remove(file.path(getwd(),"Curve2_Results_Summary.csv"))

})

test_that("construct_result_filename", {
  expect_equal(construct_result_filename(getwd(), "Test.csv", timepoint=NULL),file.path(getwd(),"Test.csv"))
  expect_equal(construct_result_filename(getwd(), "Test.csv", timepoint=12),file.path(getwd(),"Test_12.csv"))
})

test_that("generate_sensitivity_indices", {
  # Load in the exemplar summaries
  load(file.path("efast_curve1_summary.Rda"))
  load(file.path("efast_curve2_summary.Rda"))
  # write these to file
  write_data_to_csv(efast_curve1_summary,"Curve1_Results_Summary.csv")
  write_data_to_csv(efast_curve2_summary,"Curve2_Results_Summary.csv")
  # Read these in
  efast_sim_results <- read_all_curve_results(
    getwd(), GRAPHTIME=NULL, 2, 65,  c("Velocity","Displacement"),
    c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy"))

  # Now generate sensitivity indexes
  # omi is floor( ( (wanted_n / NUMCURVES) - 1) / (2 * MI) / length(PARAMETERS)), or 12, MI is 4
  sensitivities <- generate_sensitivity_indices(efast_sim_results, 12, 4,
                                                c("Velocity","Displacement"),
                                                c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy"), 2)

  # Check structures:
  # Should be a list of 6 elements
  expect_true(length(sensitivities)==8)
  expect_true(ncol(sensitivities$si)==1)
  expect_true(ncol(sensitivities$sti)==1)
  expect_true(nrow(sensitivities$si)==7)
  expect_true(nrow(sensitivities$sti)==7)
  expect_false(any(is.na(sensitivities$sti)))
  expect_false(any(is.na(sensitivities$si)))
  # All should be between 0 and 1
  expect_false(any(sensitivities$si<0))
  expect_false(any(sensitivities$si>1))
  expect_false(any(sensitivities$sti<0))
  expect_false(any(sensitivities$sti>1))

  expect_true(ncol(sensitivities$cv_si_coeffs)==2)
  expect_true(ncol(sensitivities$cv_sti_coeffs)==2)
  expect_true(nrow(sensitivities$cv_si_coeffs)==7)
  expect_true(nrow(sensitivities$cv_sti_coeffs)==7)
  expect_false(any(is.na(sensitivities$cv_si_coeffs)))
  expect_false(any(is.na(sensitivities$cv_sti_coeffs)))
  expect_false(any(sensitivities$cv_sti_coeffs<0))
  expect_false(any(sensitivities$cv_si_coeffs<0))

  expect_true(ncol(sensitivities$errors_si)==2)
  expect_true(ncol(sensitivities$errors_sti)==2)
  expect_true(nrow(sensitivities$errors_si)==7)
  expect_true(nrow(sensitivities$errors_sti)==7)
  expect_false(any(is.na(sensitivities$errors_si)))
  expect_false(any(is.na(sensitivities$errors_sti)))
  expect_false(any(sensitivities$errors_si<0))
  expect_false(any(sensitivities$errors_si>1))
  expect_false(any(sensitivities$errors_sti<0))
  expect_false(any(sensitivities$errors_sti>1))

  file.remove("Curve1_Results_Summary.csv")
  file.remove("Curve2_Results_Summary.csv")

})

test_that("format_efast_result_for_output", {

  # Load in the exemplar summaries
  load(file.path("efast_curve1_summary.Rda"))
  load(file.path("efast_curve2_summary.Rda"))
  # write these to file
  write_data_to_csv(efast_curve1_summary,"Curve1_Results_Summary.csv")
  write_data_to_csv(efast_curve2_summary,"Curve2_Results_Summary.csv")
  # Read these in
  efast_sim_results <- read_all_curve_results(
    getwd(), GRAPHTIME=NULL, 2, 65,  c("Velocity","Displacement"),
    c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy"))

  # Now generate sensitivity indexes
  # omi is floor( ( (wanted_n / NUMCURVES) - 1) / (2 * MI) / length(PARAMETERS)), or 12, MI is 4
  sensitivities <- generate_sensitivity_indices(efast_sim_results, 8, 4,
                                                c("Velocity","Displacement"),
                                                c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy"), 2)

  t_tests  <-  efast_ttest(sensitivities$si, sensitivities$range_si, sensitivities$sti, sensitivities$range_sti, 1:2, 7,2,0.95)

  formatted_results <- format_efast_result_for_output(
    sensitivities, t_tests, 1:2, c("Velocity","Displacement"),
    c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy"))

  # Check structure:
  expect_true(nrow(formatted_results)==7)
  expect_true(ncol(formatted_results)==18)
  # Can check data here to make sure the calculation was right, for an influential and not influential parameter
  expect_equal(toString(formatted_results[5,]),"0.578785503097756, 0.00582696668896769, 0.777133954645919, 0.0152168713573869, 0.222866045354081, 0.599460745968321, 5.28405158348795, 0.00245334733047453, 0.0290393146958406, 0.434647015238436, 0.00413740344881465, 0.7706584490614, 0.0258989741045774, 0.2293415509386, 5.30797631521571, 5.29939040773517, 0.0163420849315433, 0.02882827231962")
  expect_equal(toString(formatted_results[1,]),"0.088130856214053, 0.0569981227030553, 0.247844101345894, 0.0246890788521741, 0.752155898654106, 26.8685666768394, 3.71013457159303, 0.0171719523663099, 0.00647979183056818, 0.225106348126848, 0.129146980550748, 0.544969237508613, 0.0627598583903683, 0.455030762491387, 51.9194041701801, 6.5027876097207, 0.0777707300726425, 0.024863517908328")

  # Cleanup
  file.remove("Curve1_Results_Summary.csv")
  file.remove("Curve2_Results_Summary.csv")

})

test_that("efast_run_Analysis_overTime", {

  # Again we have tested underlying functions, we just need to test for output
  # Load in the exemplar summaries
  load(file.path("efast_curve1_summary.Rda"))
  load(file.path("efast_curve2_summary.Rda"))
  load(file.path("efast_curve1_summary60.Rda"))
  load(file.path("efast_curve2_summary60.Rda"))
  # write these to file
  write_data_to_csv(efast_curve1_summary,"Curve1_Results_Summary_12.csv")
  write_data_to_csv(efast_curve2_summary,"Curve2_Results_Summary_12.csv")
  write_data_to_csv(efast_curve1_summary_60,"Curve1_Results_Summary_60.csv")
  write_data_to_csv(efast_curve2_summary_60,"Curve2_Results_Summary_60.csv")

  efast_run_Analysis(getwd(),c("Velocity","Displacement"),
                     c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy"),
                     2,65,1:2,0.95,GRAPH_FLAG=TRUE, "efast_summary.csv",TIMEPOINTS=c(12,60))

  expect_true(file.exists(file.path(getwd(),"efast_summary_12.csv")))
  # Read in the result
  result <- read.csv("efast_summary_12.csv",header=T,check.names=F)

  # Can check strucure as we did in test above
  expect_true(nrow(result)==7)
  expect_true(ncol(result)==19)
  # Can check data here to make sure the calculation was right, for an influential and not influential parameter
  expect_equal(toString(result[5,]),"6, 0.578785503097756, 0.00582696668896769, 0.777133954645919, 0.0152168713573869, 0.222866045354081, 0.599460745968321, 5.28405158348795, 0.00245334733047453, 0.0290393146958406, 0.434647015238436, 0.00413740344881465, 0.7706584490614, 0.0258989741045774, 0.2293415509386, 5.30797631521571, 5.29939040773517, 0.0163420849315433, 0.02882827231962")
  expect_equal(toString(result[1,]),"1, 0.088130856214053, 0.0569981227030553, 0.247844101345894, 0.0246890788521741, 0.752155898654106, 26.8685666768394, 3.71013457159303, 0.0171719523663099, 0.00647979183056818, 0.225106348126848, 0.129146980550748, 0.544969237508613, 0.0627598583903683, 0.455030762491387, 51.9194041701801, 6.5027876097207, 0.0777707300726425, 0.024863517908328")

  # Test the plots exist
  expect_true(file.exists(file.path("Displacement_12.pdf")))
  expect_true(file.exists(file.path("Velocity_12.pdf")))

  # Now for hour 60:
  expect_true(file.exists(file.path(getwd(),"efast_summary_60.csv")))
  # Read in the result
  result <- read_from_csv(file.path(getwd(),"efast_summary_60.csv"))

  # Can check strucure as we did in test above
  expect_true(nrow(result)==7)
  expect_true(ncol(result)==19)
  # Can check data here to make sure the calculation was right, for an influential and not influential parameter
  expect_equal(toString(result[5,]),"6, 0.856393926951934, 0.040311205295597, 0.920798017919624, 0.00471547309563304, 0.0792019820803763, 18.0227039960324, 8.2344739964822, 0.108617377625795, 0.0534975658619736, 0.334459026526191, 0.149381805023321, 0.408554982195318, 0.134120265853752, 0.591445017804682, 71.27568190278, 58.2186856479987, 0.20046032207808, 0.193311934225992")
  expect_equal(toString(result[1,]),"1, 0.0979949007937029, 0.0138302915953678, 0.199535712977272, 0.120782235540079, 0.800464287022728, 13.9798082495274, 24.0199173684648, 0.00968159959495171, 0.0338579328718696, 0.0727862128868466, 0.0079511388747055, 0.294466353569997, 0.016105046525698, 0.705533646430003, 3.9875244528475, 1.48492816708813, 0.00206253141642716, 0.00309763603049745")

  # Test the plots exist
  expect_true(file.exists(file.path("Displacement_60.pdf")))
  expect_true(file.exists(file.path("Velocity_60.pdf")))

  # Cleanup
  file.remove("Curve1_Results_Summary_12.csv")
  file.remove("Curve2_Results_Summary_12.csv")
  file.remove("Curve1_Results_Summary_60.csv")
  file.remove("Curve2_Results_Summary_60.csv")
  file.remove("efast_summary_12.csv")
  file.remove("efast_summary_60.csv")
  file.remove("Displacement_12.pdf")
  file.remove("Displacement_60.pdf")
  file.remove("Velocity_12.pdf")
  file.remove("Velocity_60.pdf")

})

test_that("efast_run_Analysis", {

  # Load in the exemplar summaries
  load("efast_curve1_summary.Rda")
  load("efast_curve2_summary.Rda")
  # write these to file
  write.csv(efast_curve1_summary,file="Curve1_Results_Summary.csv",row.names=F,quote=F)
  write.csv(efast_curve2_summary,file="Curve2_Results_Summary.csv",row.names=F,quote=F)

  # With all internal methods tested, we just need to make sure we are getting CSV and graph output
  efast_run_Analysis(getwd(),c("Velocity","Displacement"),
                     c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy"),
                     2,65,1:2,0.95,GRAPH_FLAG=TRUE, "efast_summary.csv")

  expect_true(file.exists(file.path("efast_summary.csv")))
  # Read in the result
  result <- read.csv(file.path("efast_summary.csv"),header=T,check.names=F)

  # Can check strucure as we did in test above
  expect_true(nrow(result)==7)
  expect_true(ncol(result)==19)
  # Can check data here to make sure the calculation was right, for an influential and not influential parameter
  expect_equal(toString(result[5,]),"6, 0.578785503097756, 0.00582696668896769, 0.777133954645919, 0.0152168713573869, 0.222866045354081, 0.599460745968321, 5.28405158348795, 0.00245334733047453, 0.0290393146958406, 0.434647015238436, 0.00413740344881465, 0.7706584490614, 0.0258989741045774, 0.2293415509386, 5.30797631521571, 5.29939040773517, 0.0163420849315433, 0.02882827231962")
  expect_equal(toString(result[1,]),"1, 0.088130856214053, 0.0569981227030553, 0.247844101345894, 0.0246890788521741, 0.752155898654106, 26.8685666768394, 3.71013457159303, 0.0171719523663099, 0.00647979183056818, 0.225106348126848, 0.129146980550748, 0.544969237508613, 0.0627598583903683, 0.455030762491387, 51.9194041701801, 6.5027876097207, 0.0777707300726425, 0.024863517908328")

  # Test the plots exist
  expect_true(file.exists(file.path("Displacement.pdf")))
  expect_true(file.exists(file.path("Velocity.pdf")))

  # Cleanup
  file.remove("Curve1_Results_Summary.csv")
  file.remove("Curve2_Results_Summary.csv")
  file.remove("efast_summary.csv")
  file.remove("Displacement.pdf")
  file.remove("Velocity.pdf")

})

test_that("efast_netlogo_get_overall_medians", {
  expect_warning(efast_netlogo_get_overall_medians(getwd(), 3, c("A"), 2, c("B")),"'efast_netlogo_get_overall_medians' is deprecated");
})

test_that("efast_netlogo_run_Analysis", {
  expect_warning(efast_netlogo_run_Analysis(getwd(), c("A"),c("B","C"), 3, 2, 1:2, 0.95, GRAPH_FLAG=TRUE, "NoResult.csv", TIMEPOINTS=NULL, TIMEPOINTSCALE=NULL));
})

test_that("efast_process_netlogo_result", {
  unzip(file.path(getwd(),"netlogo_efast.zip"))
  params <- rbind(cbind(75.243533999874,20.8659777539209),
                  cbind(72.7819955383356,19.7890546769978))
  write.csv(params,file.path(getwd(),"test_data","Curve1_duration.csv"))
  params <- rbind(cbind(60.9800740342874,17.0678570512324),
                  cbind(58.5185355727489,18.1447801281555))
  write.csv(params,file.path(getwd(),"test_data","Curve1_infectiousness.csv"))

  params <- rbind(cbind(28.9835400518316,13.8958840938643),
                  cbind(26.5220015902932,12.8189610169412))
  write.csv(params,file.path(getwd(),"test_data","Curve2_duration.csv"))
  params <- rbind(cbind(83.0367824074167,32.0177571749888),
                  cbind(80.5752439458783,33.0946802519119))
  write.csv(params,file.path(getwd(),"test_data","Curve2_infectiousness.csv"))
  efast_process_netlogo_result(file.path(getwd(),"test_data"), "efast_result_set", c("infectiousness","duration"), 2,
             2, c("death-thru-sickness","death-but-immune","death-old-age","death-old-and-sick"), "ParamValResponses", 5200)

  expect_true(file.exists(file.path(getwd(),"test_data","Curve1_Parameter1_Results.csv")))
  expect_true(file.exists(file.path(getwd(),"test_data","Curve1_Parameter2_Results.csv")))
  expect_true(file.exists(file.path(getwd(),"test_data","Curve2_Parameter1_Results.csv")))
  expect_true(file.exists(file.path(getwd(),"test_data","Curve2_Parameter2_Results.csv")))

  unlink(file.path(getwd(),"test_data"),recursive = TRUE)

})



