library(spartan)
context("Test of Spartan Latin-Hypercube Sampling")

test_that("sample_parameter_space", {
  # Test that the correct number of samples are designed:
  PARAMETERS<-c("A","B")
  lhcSample <- sample_parameter_space("normal", 500, PARAMETERS)
  expect_equal(nrow(lhcSample),500)
  expect_equal(ncol(lhcSample),2)
  # At this stage all the values are normalised between 0 and 1, check that
  expect_equal(sum(lhcSample>1),0)
  expect_equal(sum(lhcSample<0),0)
  expect_equal(sum(lhcSample<=1),1000)

  # Optimal
  lhcSample <- sample_parameter_space("optimum", 500, PARAMETERS)
  expect_equal(nrow(lhcSample),500)
  expect_equal(ncol(lhcSample),2)
  # At this stage all the values are normalised between 0 and 1, check that
  expect_equal(sum(lhcSample>1),0)
  expect_equal(sum(lhcSample<0),0)
  expect_equal(sum(lhcSample<=1),1000)

  # No need to test any others, as the input check will take care of that
})

test_that("scale_lhc_sample", {
  # Test that the sample is scaled correctly in the specified range
  PARAMETERS<-c("A","B")
  PMIN<-c(10,0.1)
  PMAX<-c(100,0.9)
  NUMSAMPLES<-500
  design <- sample_parameter_space("normal", 500, PARAMETERS)
  scaledSample <- scale_lhc_sample(PARAMETERS, PMIN, PMAX, NUMSAMPLES, design)

  # Test the characteristics
  expect_equal(nrow(scaledSample),500)
  expect_equal(ncol(scaledSample),2)
  # Now test all fall within the range
  expect_equal(sum(scaledSample[,1]<PMIN[1]),0)
  expect_equal(sum(scaledSample[,1]>PMAX[1]),0)
  expect_equal(sum(scaledSample[,1]<=PMAX[1]),500)
  expect_equal(sum(scaledSample[,2]<PMIN[2]),0)
  expect_equal(sum(scaledSample[,2]>PMAX[2]),0)
  expect_equal(sum(scaledSample[,2]<=PMAX[2]),500)

})

# Now can test the return of the function, given the utilities above were tested
test_that("lhc_generate_lhc_sample", {
  # We don't specify a FILEPATH, to return an R object
  lhcSample <- lhc_generate_lhc_sample(FILEPATH=NULL, c("A","B"), 500, c(10,0.1), c(100,0.9), "normal")
  # Do the checks as before:
  expect_equal(nrow(lhcSample),500)
  expect_message(lhc_generate_lhc_sample(FILEPATH=NULL, c("A","B"), 500, c(10,0.1), c(100,0.9), "normal"),"No FILEPATH specified. Returning sample as R Object")
  expect_equal(ncol(lhcSample),2)
})

