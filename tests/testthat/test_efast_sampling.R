library(spartan)
context("Test of Spartan eFAST Sampling")

test_that("efast_generate_sample", {

  # Input to this function has been checked by others - what we need to test is whether the CSV files are produced
  # Run the method
  efast_generate_sample(getwd(), 2, 65, c("BindProbability", "ChemoThreshold", "Dummy"),
                          c(0, 0.10, 1),
                        PMAX <- c(100, 0.9, 10))

  # Check for existance of sample files
  expect_true(file.exists(paste(getwd(),"/Curve1_BindProbability.csv",sep="")))
  expect_true(file.exists(paste(getwd(),"/Curve1_ChemoThreshold.csv",sep="")))
  expect_true(file.exists(paste(getwd(),"/Curve1_Dummy.csv",sep="")))

  expect_true(file.exists(paste(getwd(),"/Curve2_BindProbability.csv",sep="")))
  expect_true(file.exists(paste(getwd(),"/Curve2_ChemoThreshold.csv",sep="")))
  expect_true(file.exists(paste(getwd(),"/Curve2_Dummy.csv",sep="")))

  # Check a few error conditions
  expect_message(efast_generate_sample(getwd(), "A", 65, c("BindProbability", "ChemoThreshold", "Dummy"),
                        c(0, 0.10, 1),
                        PMAX <- c(100, 0.9, 10)),"NUMCURVES must be a positive integer. Terminated")

  expect_message(efast_generate_sample(getwd(), 3, 65, c("BindProbability", "ChemoThreshold", "Dummy"),
                                       c(120, 0.10, 1),
                                       PMAX <- c(100, 0.9, 10)),"PMIN must be less than PMAX for all parameters, both must be numeric, and declared in capitals: e.g. PMIN, PMAX, PINC")

  # Cleanup

  file.remove(paste(getwd(),"/Curve1_BindProbability.csv",sep=""))
  file.remove(paste(getwd(),"/Curve1_ChemoThreshold.csv",sep=""))
  file.remove(paste(getwd(),"/Curve1_Dummy.csv",sep=""))

  file.remove(paste(getwd(),"/Curve2_BindProbability.csv",sep=""))
  file.remove(paste(getwd(),"/Curve2_ChemoThreshold.csv",sep=""))
  file.remove(paste(getwd(),"/Curve2_Dummy.csv",sep=""))

})
