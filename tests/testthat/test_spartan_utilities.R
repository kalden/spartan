library(spartan)
context("Testing Spartan Utilities Class")

test_that("partition_dataset", {
    arguments <- load_lhc_training_data()
    partitionedData <- partition_dataset(arguments$dataset,arguments$parameters,percent_train = 75, percent_test = 15,
                                       percent_validation = 10, seed = NULL,
                                       normalise = FALSE, sample_mins = arguments$sample_mins, sample_maxes = arguments$sample_maxes,
                                       timepoint = NULL)
    expect_equal(nrow(partitionedData$training),375)
    expect_equal(nrow(partitionedData$testing),75)
    expect_equal(nrow(partitionedData$validation),50)

    expect_message(partition_dataset(arguments$dataset,arguments$parameters,percent_train = A, percent_test = 15,
                                         percent_validation = 10, seed = NULL,
                                         normalise = FALSE, sample_mins = arguments$sample_mins, sample_maxes = arguments$sample_maxes,
                                         timepoint = NULL),"Training, Testing, and Validation percentages have been declared incorrectly")

    expect_message(partition_dataset(arguments$dataset,arguments$parameters,percent_train = "STRING", percent_test = 15,
                                     percent_validation = 10, seed = NULL,
                                     normalise = FALSE, sample_mins = arguments$sample_mins, sample_maxes = arguments$sample_maxes,
                                     timepoint = NULL),"Training, Testing, and Validation percentages have been declared incorrectly")

    expect_message(partition_dataset(arguments$dataset,arguments$parameters,percent_train = 10, percent_test = 15,
                                     percent_validation = 10, seed = NULL,
                                     normalise = FALSE, sample_mins = arguments$sample_mins, sample_maxes = arguments$sample_maxes,
                                     timepoint = NULL),"Partition percentages do not add up to 100%. Terminated")

})

test_that("normalise_dataset" , {
  arguments <- load_lhc_training_data()
  normalised_set <- normalise_dataset(arguments$dataset, arguments$sample_mins, arguments$sample_maxes, arguments$parameters)
  expect_gte(min(normalised_set$scaled),0)
  expect_lte(max(normalised_set$scaled),1)

  # Build a dataset of two columns between -50 and 50
  x<- -50:50
  testDat <- cbind(sample(x,100),sample(x,100))
  colnames(testDat) <- c("A","B")
  normalised_set <- normalise_dataset(testDat, c(-50,-50), c(50,50), c("A","B"))
  expect_gte(min(normalised_set$scaled),0)
  expect_lte(max(normalised_set$scaled),1)

})
