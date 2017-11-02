library(spartan)
context("Test of Spartan Robustness Sampling")

test_that("generate_parameter_table", {
  PARAMETERS<-c("A","B")
  BASELINE<-c(0.5,1.75)
  PMIN<-c(0,1)
  PMAX<-c(1,2)
  PINC<-c(0.1,0.25)
  PARAMVALS<-NULL

  # Now we can test the function for parameter 1: nrow should equal the number of samples

  val_list <- as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                      PARAMVALS,
                                                      1))

  # 1 is th parameter of interest
  expect_equal(nrow(generate_parameter_table(PARAMETERS, BASELINE, 1, val_list)),11)
  # This should have two columns, one for each parameter
  expect_equal(ncol(generate_parameter_table(PARAMETERS, BASELINE, 1, val_list)),2)
  # Column two should all be identical, the baseline value of parameter 2
  expect_true(all(generate_parameter_table(PARAMETERS, BASELINE, 1, val_list)[,2] == BASELINE[2]))

  val_list <- as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                      PARAMVALS,
                                                      2))

  # Now try parameter 2
  expect_equal(nrow(generate_parameter_table(PARAMETERS, BASELINE, 2, val_list)),5)
  # This should have two columns, one for each parameter
  expect_equal(ncol(generate_parameter_table(PARAMETERS, BASELINE, 2, val_list)),2)
  # For some reason, putting this in expect_true did not work
  expect_true(all(generate_parameter_table(PARAMETERS, BASELINE, 2, val_list)[,1] == BASELINE[1]))

})
