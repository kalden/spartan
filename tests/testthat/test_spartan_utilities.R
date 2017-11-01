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

test_that("denormalise_dataset", {

  # Load in the required data and parameter arguments
  arguments <- load_lhc_training_data()
  # Normalise dataset
  normalised_set <- normalise_dataset(arguments$dataset, arguments$sample_mins, arguments$sample_maxes, arguments$parameters)

  # Denormalise the set and it should be the same as the original
  transformedData <- denormalise_dataset(normalised_set$scaled[arguments$parameters], arguments$sample_mins, arguments$sample_maxes)

  expect_equal(transformedData, arguments$dataset[arguments$parameters])
})

test_that("make_path", {
  expect_match(make_path(c("/home/kja505/Desktop","outputFile.csv")),"/home/kja505/Desktop/outputFile.csv")
})

test_that("make_filename", {
  expect_match(make_filename(c("Curve","1","Parameter","2")),"Curve_1_Parameter_2")
})

test_that("make_extension", {
  expect_match(make_extension("outputFile","csv"),"outputFile.csv")
})

test_that("join_strings_nospace", {
  expect_match(join_strings_nospace(c("Curve","1")),"Curve1")
  })

test_that("prepare_parameter_value_list", {
  # Test that mins, max, and increment produces the correct sequences
  PMIN<-c(0,1)
  PMAX<-c(1,2)
  PINC<-c(0.1,0.25)
  PARAMVALS<-NULL
  PARAM_OF_INT<-1
  # First parameter should generate 11 samples:
  expect_length(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT),11)
  # Check values have been calculated correctly
  expect_equal(as.numeric(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT)),seq(PMIN[1],PMAX[1],by=PINC[1]))
  # Second should generate 5
  PARAM_OF_INT<-2
  expect_length(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT),5)
  # Check values have been calculated correctly
  expect_equal(as.numeric(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT)),seq(PMIN[2],PMAX[2],by=PINC[2]))
  # Test Paramvals
  PMIN<-NULL
  PMAX<-NULL
  PINC<-NULL
  PARAMVALS<-c("0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1","0, 0.25, 0.5, 0.75, 1")
  # Now repeat tests
  PARAM_OF_INT<-1
  expect_length(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT),11)
  expect_equal(as.numeric(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT)),as.numeric(strsplit(PARAMVALS[1],split=",")[[1]]))
  PARAM_OF_INT<-2
  expect_length(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT),5)
  expect_equal(as.numeric(prepare_parameter_value_list(PMIN,PMAX,PINC,PARAMVALS,PARAM_OF_INT)),as.numeric(strsplit(PARAMVALS[2],split=",")[[1]]))
})


