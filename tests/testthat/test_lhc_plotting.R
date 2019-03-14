library(spartan)
context("Testing Spartan LHC Plotting Utilities Class")

test_that("lhc_graphMeasuresForParameterChange", {

  # Setup:
  load(file.path("LHC_Summary.Rda"))
  # Write to file so can be read in
  write.csv(LHCRESULTFILE,file="LHC_Results.csv",row.names=F,quote=F)
  # Load Correlation Coefficients
  load(file.path("test_cor_coeffs.Rda"))
  write.csv(test_cor_coeffs,file="Test_Cor_Coeffs.csv",row.names=F,quote=F)

  # All internals have been tested - we now need to check for output
  lhc_graphMeasuresForParameterChange(getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
                                                 "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"),
                                      c("Velocity","Displacement"), c("microns/min","microns"), "Test_Cor_Coeffs.csv",
                                      "LHC_Results.csv", OUTPUT_TYPE = c("PDF","PNG"))

  # Check for existence of graphs
  expect_true(file.exists(file.path(getwd(),"thresholdBindProbability_Velocity.pdf")))
  expect_true(file.exists(file.path(getwd(),"chemoThreshold_Velocity.pdf")))
  expect_true(file.exists(file.path(getwd(),"chemoUpperLinearAdjust_Velocity.pdf")))
  expect_true(file.exists(file.path(getwd(),"chemoLowerLinearAdjust_Velocity.pdf")))
  expect_true(file.exists(file.path(getwd(),"maxVCAMeffectProbabilityCutoff_Velocity.pdf")))
  expect_true(file.exists(file.path(getwd(),"vcamSlope_Velocity.pdf")))
  expect_true(file.exists(file.path(getwd(),"thresholdBindProbability_Displacement.pdf")))
  expect_true(file.exists(file.path(getwd(),"chemoThreshold_Displacement.pdf")))
  expect_true(file.exists(file.path(getwd(),"chemoUpperLinearAdjust_Displacement.pdf")))
  expect_true(file.exists(file.path(getwd(),"chemoLowerLinearAdjust_Displacement.pdf")))
  expect_true(file.exists(file.path(getwd(),"maxVCAMeffectProbabilityCutoff_Displacement.pdf")))
  expect_true(file.exists(file.path(getwd(),"vcamSlope_Displacement.pdf")))

  expect_true(file.exists(file.path(getwd(),"thresholdBindProbability_Velocity.png")))
  expect_true(file.exists(file.path(getwd(),"chemoThreshold_Velocity.png")))
  expect_true(file.exists(file.path(getwd(),"chemoUpperLinearAdjust_Velocity.png")))
  expect_true(file.exists(file.path(getwd(),"chemoLowerLinearAdjust_Velocity.png")))
  expect_true(file.exists(file.path(getwd(),"maxVCAMeffectProbabilityCutoff_Velocity.png")))
  expect_true(file.exists(file.path(getwd(),"vcamSlope_Velocity.png")))
  expect_true(file.exists(file.path(getwd(),"thresholdBindProbability_Displacement.png")))
  expect_true(file.exists(file.path(getwd(),"chemoThreshold_Displacement.png")))
  expect_true(file.exists(file.path(getwd(),"chemoUpperLinearAdjust_Displacement.png")))
  expect_true(file.exists(file.path(getwd(),"chemoLowerLinearAdjust_Displacement.png")))
  expect_true(file.exists(file.path(getwd(),"maxVCAMeffectProbabilityCutoff_Displacement.png")))
  expect_true(file.exists(file.path(getwd(),"vcamSlope_Displacement.png")))


  file.remove("LHC_Results.csv")
  file.remove("Test_Cor_Coeffs.csv")
  file.remove("thresholdBindProbability_Velocity.pdf")
  file.remove("chemoThreshold_Velocity.pdf")
  file.remove("chemoUpperLinearAdjust_Velocity.pdf")
  file.remove("chemoLowerLinearAdjust_Velocity.pdf")
  file.remove("maxVCAMeffectProbabilityCutoff_Velocity.pdf")
  file.remove("vcamSlope_Velocity.pdf")
  file.remove("thresholdBindProbability_Displacement.pdf")
  file.remove("chemoThreshold_Displacement.pdf")
  file.remove("chemoUpperLinearAdjust_Displacement.pdf")
  file.remove("chemoLowerLinearAdjust_Displacement.pdf")
  file.remove("maxVCAMeffectProbabilityCutoff_Displacement.pdf")
  file.remove("vcamSlope_Displacement.pdf")

  file.remove("thresholdBindProbability_Velocity.png")
  file.remove("chemoThreshold_Velocity.png")
  file.remove("chemoUpperLinearAdjust_Velocity.png")
  file.remove("chemoLowerLinearAdjust_Velocity.png")
  file.remove("maxVCAMeffectProbabilityCutoff_Velocity.png")
  file.remove("vcamSlope_Velocity.png")
  file.remove("thresholdBindProbability_Displacement.png")
  file.remove("chemoThreshold_Displacement.png")
  file.remove("chemoUpperLinearAdjust_Displacement.png")
  file.remove("chemoLowerLinearAdjust_Displacement.png")
  file.remove("maxVCAMeffectProbabilityCutoff_Displacement.png")
  file.remove("vcamSlope_Displacement.png")

})

test_that("lhc_graphMeasuresForParameterChange_overTime", {

  # Setup:
  load(file.path("LHC_Summary.Rda"))
  load(file.path("LHC_Summary_36.Rda"))
  write.csv(LHCRESULTFILE,file="LHC_Results_12.csv",row.names=F,quote=F)
  write.csv(LHCRESULTFILE_36,file="LHC_Results_36.csv",row.names=F,quote=F)
  # Load Correlation Coefficients
  load(file.path("test_cor_coeffs.Rda"))
  load(file.path("test_cor_coeffs_36.Rda"))
  write.csv(test_cor_coeffs,file="Test_Cor_Coeffs_12.csv",row.names=F,quote=F)
  write.csv(test_cor_coeffs_36,file="Test_Cor_Coeffs_36.csv",row.names=F,quote=F)

  # All internals have been tested - we now need to check for output
  lhc_graphMeasuresForParameterChange(getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
                                                 "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"),
                                      c("Velocity","Displacement"), c("microns/min","microns"), "Test_Cor_Coeffs.csv",
                                      "LHC_Results.csv", OUTPUT_TYPE = c("PDF","PNG"),TIMEPOINTS=c(12,36),TIMEPOINTSCALE="Hours")

  file.remove("LHC_Results_12.csv")
  file.remove("LHC_Results_36.csv")
  file.remove("Test_Cor_Coeffs_12.csv")
  file.remove("Test_Cor_Coeffs_36.csv")
  file.remove("thresholdBindProbability_Velocity12.pdf")
  file.remove("chemoThreshold_Velocity12.pdf")
  file.remove("chemoUpperLinearAdjust_Velocity12.pdf")
  file.remove("chemoLowerLinearAdjust_Velocity12.pdf")
  file.remove("maxVCAMeffectProbabilityCutoff_Velocity12.pdf")
  file.remove("vcamSlope_Velocity12.pdf")
  file.remove("thresholdBindProbability_Displacement12.pdf")
  file.remove("chemoThreshold_Displacement12.pdf")
  file.remove("chemoUpperLinearAdjust_Displacement12.pdf")
  file.remove("chemoLowerLinearAdjust_Displacement12.pdf")
  file.remove("maxVCAMeffectProbabilityCutoff_Displacement12.pdf")
  file.remove("vcamSlope_Displacement12.pdf")

  file.remove("thresholdBindProbability_Velocity36.pdf")
  file.remove("chemoThreshold_Velocity36.pdf")
  file.remove("chemoUpperLinearAdjust_Velocity36.pdf")
  file.remove("chemoLowerLinearAdjust_Velocity36.pdf")
  file.remove("maxVCAMeffectProbabilityCutoff_Velocity36.pdf")
  file.remove("vcamSlope_Velocity36.pdf")
  file.remove("thresholdBindProbability_Displacement36.pdf")
  file.remove("chemoThreshold_Displacement36.pdf")
  file.remove("chemoUpperLinearAdjust_Displacement36.pdf")
  file.remove("chemoLowerLinearAdjust_Displacement36.pdf")
  file.remove("maxVCAMeffectProbabilityCutoff_Displacement36.pdf")
  file.remove("vcamSlope_Displacement36.pdf")


})

test_that("lhc_netlogo_graphMeasuresForParameterChange", {
  expect_message(lhc_netlogo_graphMeasuresForParameterChange(getwd(), c("A","B"),c("R","W"),c("m","m"),"Co_File.csv","lhc_File.csv",NULL,NULL),"Deprecated. Use the lhc_graphMeasuresForParameterChange method instead")
})


#test_that("plotPRCCSFromTimepointFiles", {

  # This test needs a rewrite to fit new spartan 4

  # Again just testing we get some output from the plotting functions:
  #load(file.path("test_cor_coeffs.Rda"))
  #load(file.path("test_cor_coeffs_36.Rda"))
  #write.csv(test_cor_coeffs,file="Test_Cor_Coeffs_12.csv",row.names=F,quote=F)
  #write.csv(test_cor_coeffs_36,file="Test_Cor_Coeffs_36.csv",row.names=F,quote=F)


  #plotPRCCSFromTimepointFiles(getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
  #                                                   "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"),
  #                                        c("Velocity","Displacement"), "Test_Cor_Coeffs.csv", c(12,36),"Hours")

  # Check file existence
  #expect_true(file.exists("thresholdBindProbability_OverTime.pdf"))
  #expect_true(file.exists("chemoThreshold_OverTime.pdf"))
  #expect_true(file.exists("chemoUpperLinearAdjust_OverTime.pdf"))
  #expect_true(file.exists("chemoLowerLinearAdjust_OverTime.pdf"))
  #expect_true(file.exists("maxVCAMeffectProbabilityCutoff_OverTime.pdf"))
  #expect_true(file.exists("vcamSlope_OverTime.pdf"))

  #file.remove("thresholdBindProbability_OverTime.pdf")
  #file.remove("chemoThreshold_OverTime.pdf")
  #file.remove("chemoUpperLinearAdjust_OverTime.pdf")
  #file.remove("chemoLowerLinearAdjust_OverTime.pdf")
  #file.remove("maxVCAMeffectProbabilityCutoff_OverTime.pdf")
  #file.remove("vcamSlope_OverTime.pdf")

  #plotPRCCSFromTimepointFiles(getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
  #                                       "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"),
  #                            c("Velocity","Displacement"), "Test_Cor_Coeffs.csv", c(12,36),"Hours", DISPLAYPVALS = TRUE)

  #expect_true(file.exists("thresholdBindProbability_OverTime.pdf"))
  #expect_true(file.exists("chemoThreshold_OverTime.pdf"))
  #expect_true(file.exists("chemoUpperLinearAdjust_OverTime.pdf"))
  #expect_true(file.exists("chemoLowerLinearAdjust_OverTime.pdf"))
  #expect_true(file.exists("maxVCAMeffectProbabilityCutoff_OverTime.pdf"))
  #expect_true(file.exists("vcamSlope_OverTime.pdf"))

  #file.remove("thresholdBindProbability_OverTime.pdf")
  #file.remove("chemoThreshold_OverTime.pdf")
  #file.remove("chemoUpperLinearAdjust_OverTime.pdf")
  #file.remove("chemoLowerLinearAdjust_OverTime.pdf")
  #file.remove("maxVCAMeffectProbabilityCutoff_OverTime.pdf")
  #file.remove("vcamSlope_OverTime.pdf")
  #file.remove("Test_Cor_Coeffs_12.csv")
  #file.remove("Test_Cor_Coeffs_36.csv")

#})

test_that("lhc_polarplot", {
  # Test graphic creation

  load(file.path("test_cor_coeffs.Rda"))
  write.csv(test_cor_coeffs,file="Test_Cor_Coeffs.csv",row.names=F,quote=F)

  lhc_polarplot(getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
                           "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"),
                c("Velocity","Displacement"), "Test_Cor_Coeffs.csv",
                            TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL)

  expect_true(file.exists("polarPlot_Velocity.pdf"))
  expect_true(file.exists("polarPlot_Velocity.png"))
  expect_true(file.exists("polarPlot_Displacement.pdf"))
  expect_true(file.exists("polarPlot_Displacement.png"))

  file.remove("polarPlot_Velocity.pdf")
  file.remove("polarPlot_Velocity.png")
  file.remove("polarPlot_Displacement.pdf")
  file.remove("polarPlot_Displacement.png")
  file.remove("Test_Cor_Coeffs.csv")

  # Timepoints:
  load(file.path("test_cor_coeffs.Rda"))
  load(file.path("test_cor_coeffs_36.Rda"))
  write.csv(test_cor_coeffs,file="Test_Cor_Coeffs_12.csv",row.names=F,quote=F)
  write.csv(test_cor_coeffs_36,file="Test_Cor_Coeffs_36.csv",row.names=F,quote=F)

  lhc_polarplot(getwd(), c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
                           "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope"),
                c("Velocity","Displacement"), "Test_Cor_Coeffs.csv",
                TIMEPOINTS = c(12,36), TIMEPOINTSCALE = "Hours")

  expect_true(file.exists("polarPlot_Velocity_12.pdf"))
  expect_true(file.exists("polarPlot_Velocity_12.png"))
  expect_true(file.exists("polarPlot_Velocity_36.pdf"))
  expect_true(file.exists("polarPlot_Velocity_36.png"))
  expect_true(file.exists("polarPlot_Displacement_12.pdf"))
  expect_true(file.exists("polarPlot_Displacement_12.png"))
  expect_true(file.exists("polarPlot_Displacement_36.pdf"))
  expect_true(file.exists("polarPlot_Displacement_36.png"))

  file.remove("polarPlot_Velocity_12.pdf")
  file.remove("polarPlot_Velocity_12.png")
  file.remove("polarPlot_Velocity_36.pdf")
  file.remove("polarPlot_Velocity_36.png")
  file.remove("polarPlot_Displacement_12.pdf")
  file.remove("polarPlot_Displacement_12.png")
  file.remove("polarPlot_Displacement_36.pdf")
  file.remove("polarPlot_Displacement_36.png")
  file.remove("Test_Cor_Coeffs_12.csv")
  file.remove("Test_Cor_Coeffs_36.csv")

})
