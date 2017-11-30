library(spartan)
context("Test of Spartan LHC Analysis")

test_that("summarise_lhc_sweep_responses", {

  setup_lhc_result_analysis()
  lhctable <- rbind(cbind(63.8870291068684,0.738687975020334,0.150462160198018,0.0299383036779426,0.866686868487718,0.569160230072564),
                    cbind(97.4719759861007,0.241536056525633,0.127347518739663,0.070033726479502,0.725104774672724,3.19126452945347))
  colnames(lhctable) <-c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")

  # Utilises many of spartan's internal functions that have been tested elsewhere,
  # so need to check return value. 2 runs per sample, here only 2 samples
  summary_stats <- summarise_lhc_sweep_responses(getwd(), 2, c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope"),
                                c("Velocity","Displacement"), "Test_LHC_Result.csv", NULL, 2, lhctable, 1,2)

  # Can now check the make up of these stats
  expect_true(nrow(summary_stats)==4)
  expect_true(ncol(summary_stats)==8)
  expect_equal(toString(summary_stats[,7]),"2.1018076004, 2.15687041465, 2.15687041465, 4.15687041465")
  expect_equal(toString(summary_stats[,8]),"44.0977369078, 43.1842375117, 43.1842375117, 30.6842375117")

  unlink(file.path(getwd(),"1"),recursive = TRUE)
  unlink(file.path(getwd(),"2"),recursive = TRUE)

})

test_that("lhc_process_sample_run_subsets", {

  # All input has already been checked and those functions tested. The internal functions have been tested
  # We just need to make sure we get sensible output
  # Setup:
  setup_lhc_result_analysis()
  lhctable <- rbind(cbind(63.8870291068684,0.738687975020334,0.150462160198018,0.0299383036779426,0.866686868487718,0.569160230072564),
                    cbind(97.4719759861007,0.241536056525633,0.127347518739663,0.070033726479502,0.725104774672724,3.19126452945347))
  colnames(lhctable) <-c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
  write.csv(lhctable,"Test_Param_File.csv",quote=F,row.names=F)


  summary_stats <- lhc_process_sample_run_subsets(getwd(), "Test_Param_File.csv",  c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope"),
                                                  2,2,c("Velocity","Displacement"),"Test_LHC_Result.csv",NULL,1,2,"LHC_Summary_File.csv")

  # read in the result file
  expect_true(file.exists(file.path(getwd(),"LHC_Summary_File.csv")))
  result <- read.csv(file.path(getwd(),"LHC_Summary_File.csv"),header=T,sep=",",check.names=F)

  # Can now check the make up of these stats
  expect_true(nrow(result)==4)
  expect_true(ncol(result)==8)
  expect_equal(toString(result[,7]),"2.1018076004, 2.15687041465, 2.15687041465, 4.15687041465")
  expect_equal(toString(result[,8]),"44.0977369078, 43.1842375117, 43.1842375117, 30.6842375117")
  expect_false(any(is.na(result)))

  unlink(file.path(getwd(),"1"),recursive = TRUE)
  unlink(file.path(getwd(),"2"),recursive = TRUE)
  file.remove(file.path(getwd(),"Test_Param_File.csv"))
  file.remove(file.path(getwd(),"LHC_Summary_File.csv"))
})

test_that("lhc_process_sample_run_subsets_overTime", {
  setup_lhc_result_analysis_overtime()
  lhctable <- rbind(cbind(63.8870291068684,0.738687975020334,0.150462160198018,0.0299383036779426,0.866686868487718,0.569160230072564),
                    cbind(97.4719759861007,0.241536056525633,0.127347518739663,0.070033726479502,0.725104774672724,3.19126452945347))
  colnames(lhctable) <-c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
  write.csv(lhctable,"Test_Param_File.csv",quote=F,row.names=F)

  summary_stats <- lhc_process_sample_run_subsets(getwd(), "Test_Param_File.csv",  c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope"),
                                                  2,2,c("Velocity","Displacement"),"Test_LHC_Result.csv",NULL,1,2,"LHC_Summary_File.csv",c(12,36),"Hours")

  expect_true(file.exists(file.path(getwd(),"LHC_Summary_File_12.csv")))
  result <- read.csv(file.path(getwd(),"LHC_Summary_File_12.csv"),header=T,sep=",",check.names=F)

  expect_true(nrow(result)==4)
  expect_true(ncol(result)==8)
  expect_equal(toString(result[,7]),"2.1018076004, 2.15687041465, 2.15687041465, 4.15687041465")
  expect_equal(toString(result[,8]),"44.0977369078, 43.1842375117, 43.1842375117, 30.6842375117")
  expect_false(any(is.na(result)))

  expect_true(file.exists(file.path(getwd(),"LHC_Summary_File_36.csv")))
  result <- read.csv(file.path(getwd(),"LHC_Summary_File_36.csv"),header=T,sep=",",check.names=F)

  expect_true(nrow(result)==4)
  expect_true(ncol(result)==8)
  expect_equal(toString(result[,7]),"2.15687041465, 4.15687041465, 4.15687041465, 2.1018076004")
  expect_equal(toString(result[,8]),"43.1842375117, 30.6842375117, 30.6842375117, 44.0977369078")
  expect_false(any(is.na(result)))

  unlink(file.path(getwd(),"1"),recursive = TRUE)
  unlink(file.path(getwd(),"2"),recursive = TRUE)
  file.remove(file.path(getwd(),"Test_Param_File.csv"))
  file.remove(file.path(getwd(),"LHC_Summary_File_12.csv"))
  file.remove(file.path(getwd(),"LHC_Summary_File_36.csv"))
})

test_that("calculate_prcc_for_all_measures", {

  load(file.path("Coeff_data_for_test.Rda"))
  load(file.path("LHC_Result_for_Test.Rda"))
  PARAMETERS<- c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
                 "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope")
  COEFFPARAMCOL <- LHCRESULTFILE[, PARAMETERS[1]]
  prccs <- calculate_prcc_for_all_measures(c("Velocity","Displacement"), COEFFPARAMCOL, coeff_data_for_test, LHCRESULTFILE)

  # Check structure:
  expect_true(ncol(prccs)==4)
  expect_true(nrow(prccs)==1)
  # In this case we know what the result should be, so we can check:
  expect_equal(toString(prccs[1,]),"-0.612573243351365, 1.04590080937115e-65, -0.496004647238542, 1.66467817317124e-36")

})

test_that("calculate_prccs_all_parameters", {

  load(file.path("LHC_Result_for_Test.Rda"))
  PARAMETERS<- c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
                 "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope")
  results <- calculate_prccs_all_parameters(PARAMETERS, LHCRESULTFILE, c("Velocity","Displacement"))

  # Check structure of results
  expect_true(ncol(results)==4)
  expect_true(nrow(results)==6)
  expect_false(any(is.na(results)))

  # We can check the data to known results
  expect_equal(toString(results[,1]),"-0.612573243351365, -0.23544238910024, -0.0352194687577137, -0.38036610430444, -0.965612015969247, -0.105381260719843")
  expect_equal(toString(results[,2]),"1.04590080937115e-65, 8.72122168326143e-08, 0.436270488971339, 1.03259665068273e-19, 0, 0.0192330437767813")
  expect_equal(toString(results[,3]),"-0.496004647238542, -0.675884592581191, 0.016675452545702, -0.790094708254836, -0.820138812445728, -0.064631794993301")
  expect_equal(toString(results[,4]),"1.66467817317124e-36, 2.98004421745699e-91, 0.712557093834158, 2.50135510062331e-178, 4.74933607297012e-220, 0.15250059663425")
})

test_that("lhc_generatePRCoEffs_overTime", {

  # Setup:
  load(file.path("LHC_Result_Hour12.Rda"))
  load(file.path("LHC_Result_Hour36.Rda"))
  write.csv(lhc_result_hour12,file="LHC_Results_12.csv",row.names=T,quote=F)
  write.csv(lhc_result_hour36,file="LHC_Results_36.csv",row.names=T,quote=F)

  # All internal functions have been checked - we now just need to check output
  lhc_generatePRCoEffs(
    getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
               "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"),
    c("Velocity","Displacement"), "LHC_Results.csv", "Prcc_Out.csv",c(12,36),"Hours")

  # Check files exist
  expect_true(file.exists(file.path("Prcc_Out_12.csv")))
  expect_true(file.exists(file.path("Prcc_Out_36.csv")))

  # Read in file and check structure
  resultIn<- read.csv(file.path("Prcc_Out_12.csv"),header=T,sep=",")

  # Check structure of results
  expect_true(ncol(resultIn)==5)
  expect_true(nrow(resultIn)==6)
  expect_false(any(is.na(resultIn)))

  resultIn<- read.csv(file.path("Prcc_Out_36.csv"),header=T,sep=",")

  # Check structure of results
  expect_true(ncol(resultIn)==5)
  expect_true(nrow(resultIn)==6)
  expect_false(any(is.na(resultIn)))

  file.remove(file.path("LHC_Results_12.csv"))
  file.remove(file.path("LHC_Results_36.csv"))
  file.remove(file.path("Prcc_Out_36.csv"))
  file.remove(file.path("Prcc_Out_12.csv"))

})

test_that("lhc_generatePRCoEffs", {

  # Setup:
  load(file.path("LHC_Result_for_Test.Rda"))
  # Write to file so can be read in
  write.csv(LHCRESULTFILE,file="LHC_Results.csv",row.names=T,quote=F)

  # All internal functions have been checked - we now just need to check output
  lhc_generatePRCoEffs(
    getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
               "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"), c("Velocity","Displacement"), "LHC_Results.csv", "Prcc_Out.csv")

  # Read in file and check structure
  resultIn<- read.csv("Prcc_Out.csv",header=T,sep=",")

  # Check structure of results
  expect_true(ncol(resultIn)==5)
  expect_true(nrow(resultIn)==6)
  expect_false(any(is.na(resultIn)))

  # We can check the data to known results
  expect_equal(toString(resultIn[,2]),"-0.612573243351365, -0.23544238910024, -0.0352194687577137, -0.38036610430444, -0.965612015969247, -0.105381260719843")
  expect_equal(toString(resultIn[,3]),"1.04590080937115e-65, 8.72122168326143e-08, 0.436270488971339, 1.03259665068273e-19, 0, 0.0192330437767813")
  expect_equal(toString(resultIn[,4]),"-0.496004647238542, -0.675884592581191, 0.016675452545702, -0.790094708254836, -0.820138812445728, -0.064631794993301")
  expect_equal(toString(resultIn[,5]),"1.66467817317124e-36, 2.98004421745699e-91, 0.712557093834158, 2.50135510062331e-178, 4.74933607297012e-220, 0.15250059663425")

  file.remove(file.path(getwd(),"LHC_Results.csv"))
  file.remove(file.path(getwd(),"Prcc_Out.csv"))
})
