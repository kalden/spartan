library(spartan)
context("Testing Spartan Utilities Class")

test_that("partition_dataset", {
    arguments <- load_lhc_training_data()
    partitionedData <- partition_dataset(arguments$dataset,arguments$parameters,percent_train = 75, percent_test = 15,
                                       percent_validation = 10, seed = NULL,
                                       normalise = FALSE, sample_mins = arguments$sampleMins, sample_maxes = arguments$sampleMaxes,
                                       timepoint = NULL)
    expect_equal(nrow(partitionedData$training),375)
    expect_equal(nrow(partitionedData$testing),75)
    expect_equal(nrow(partitionedData$validation),50)

    expect_message(partition_dataset(arguments$dataset,arguments$parameters,percent_train = A, percent_test = 15,
                                         percent_validation = 10, seed = NULL,
                                         normalise = FALSE, sample_mins = arguments$sampleMins, sample_maxes = arguments$sampleMaxes,
                                         timepoint = NULL),"Training, Testing, and Validation percentages have been declared incorrectly")

    expect_message(partition_dataset(arguments$dataset,arguments$parameters,percent_train = "STRING", percent_test = 15,
                                     percent_validation = 10, seed = NULL,
                                     normalise = FALSE, sample_mins = arguments$sampleMins, sample_maxes = arguments$sampleMaxes,
                                     timepoint = NULL),"Training, Testing, and Validation percentages have been declared incorrectly")

    expect_message(partition_dataset(arguments$dataset,arguments$parameters,percent_train = 10, percent_test = 15,
                                     percent_validation = 10, seed = NULL,
                                     normalise = FALSE, sample_mins = arguments$sampleMins, sample_maxes = arguments$sampleMaxes,
                                     timepoint = NULL),"Partition percentages do not add up to 100%. Terminated")



})
