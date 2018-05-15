#' Function used to calculate the median results for those run under a set of
#' parameter samples
#'
#' Internal function used to calculate the median set where a simulation has
#' been run a number of times for a given parameter set. Used in all of
#' Techniques 1-4 detailed in the R Journal
#'
#' @keywords internal
#'
#' @param FILEPATH Where the simulation results can be found
#' @param NUMRUNSPERSAMPLE Number of times the simulation has been run for a
#' parameter set
#' @param MEASURES Names of Simulation output responses
#' @param RESULTFILENAME Name of the simulation result file
#' @param ALTFILENAME Where necessary, name of the alternative result file
#' @param outputfilecolstart Number of the column of the CSV file where results
#' commence
#' @param outputfilecolend Number of the column of the CSV file where results
#' end
#' @return Median Simulation responses under the parameter set in the
#' result file
getMediansSubset <- function(FILEPATH, NUMRUNSPERSAMPLE, measures,
                             resultfilename, altfilename = NULL,
                             outputfilecolstart, outputfilecolend) {

  all_results <- NULL

  for (i in 1:NUMRUNSPERSAMPLE) {

    #print(i)
    fileaddress <- file.path(FILEPATH, toString(i))
    if (file.exists(fileaddress)) {

      model_result <- import_model_result(fileaddress, resultfilename,
                                      altfilename, outputfilecolstart,
                                      outputfilecolend)

      if(nrow(model_result)>0) {
        all_results <- rbind(all_results,
                             get_median_results_for_all_measures(model_result,
                                                                 measures))
      }
    } else {
      message(paste("File ", fileaddress, " does not exist", sep = ""))
    }
  }

  if(!is.null(all_results) & nrow(all_results) > 0)
    colnames(all_results) <- measures

  # Now we return this set of medians
  return(all_results)
}


#' Import a model result from either a CSV or XML file
#' @param fileaddress Directory where the file is
#' @param resultfilename Name of the results file
#' @param altfilename If no results in resultfile, can read an alternative
#' @param outputfilecolstart Start column of output in CSV file
#' @param outputfilecolend End column of output in CSV file
#' @return Results for this simulation run
import_model_result <- function(fileaddress, resultfilename,
                                altfilename, outputfilecolstart = NULL,
                                outputfilecolend = NULL) {

  model_result <- read_model_result_file(fileaddress, resultfilename,
                                           outputfilecolstart,
                                           outputfilecolend)

  if(nrow(model_result) == 0 & !is.null(altfilename))
    model_result <- read_model_result_file(fileaddress, altfilename,
                                           outputfilecolstart,
                                           outputfilecolend)

  return(model_result)


}

#' Reads a model result file, either CSV or XML
#' @param fileaddress Folder where the result file can be found
#' @param resultfilename Name of the result file
#' @param outputfilecolstart Start column of output in CSV file
#' @param outputfilecolend End column of output in CSV file
#' @return Results for this simulation run
read_model_result_file <- function(fileaddress, resultfilename,
                                   outputfilecolstart = NULL,
                                   outputfilecolend = NULL) {

  filepath <- file.path(fileaddress, resultfilename)
  if(file.exists(filepath)) {
    if (check_file_extension(resultfilename) == "csv") {
      # import model result
      if (outputfilecolstart > 1) {
        col_diff <- outputfilecolend - outputfilecolstart
        import <- read.csv(filepath,
                           colClasses = c(rep("NULL", outputfilecolstart - 1),
                                          rep(NA, col_diff + 1)),
                           header = TRUE, check.names = FALSE)
      } else {
        import <- read.csv(filepath,
                           colClasses = c(rep(NA, outputfilecolend)),
                           header = TRUE, check.names = FALSE)
      }

      return(data.frame(import, check.names = FALSE))

    } else if (check_file_extension(resultfilename) == "xml")
      return(XML::xmlToDataFrame(filepath))
  }
  else
  {
    ## Return an empty dataframe - no rows in result file
    return(data.frame())
  }
}

#' For a model result, calculate the medians of the desired measures
#' @param model_result Simulation results
#' @param measures Measures to summarise
#' @return Median summary statistics for all measures
get_median_results_for_all_measures <- function(model_result, measures) {

  medians_all_measures <- NULL

  # Calculate the median response for each measure
  for (q in 1:length(measures)) {
    measure_result <- as.matrix(model_result[measures[q]])
    measure_median <- median(as.numeric(measure_result))
    medians_all_measures <- cbind(medians_all_measures,
                                   measure_median[[1]])
  }
  return(medians_all_measures)

}


#' Check the file extension of a file and return it
#'
#' @keywords internal
check_file_extension <- function(filename) {
  if (substr(filename, (nchar(filename) + 1 ) - 3,
         nchar(filename)) == "csv")
    return("csv")
  else if (substr(filename, (nchar(filename) + 1 ) - 3,
            nchar(filename)) == "xml")
    return("xml")
  else if (substr(filename, (nchar(filename) + 1 ) - 3,
                  nchar(filename)) == "pdf")
    return("pdf")
  else
    return("NULL")
}


#' Prepares the parameter value list, as either an interval range or specific
#' values can be supplied
#'
#' When iterating through parameter values, we need to do a bit of prep, as
#' there are two ways these values can be specified (increment and specified
#' in a list). This returns a list of the values for both specifications, so
#' these can be iterated through without issue
#'
#' @param PMIN Vector of the minimum values for each parameter
#' @param PMAX Vector of the maximum values for each parameter
#' @param PINC Vector of the sample increment value for each parameter
#' @param PARAMVALS Vector of the sampling values specified for each parameter,
#' rather than an incremental sequence
#' @param PARAM_OF_INT The current parameter being analysed
#'
#' @keywords internal
prepare_parameter_value_list <- function(PMIN, PMAX, PINC, PARAMVALS,
                                         PARAM_OF_INT) {


  if (is.null(PARAMVALS)) {
    # MUST HAVE SPECIFIED AS MIN, MAX, AND INC
    val_list <- seq(PMIN[PARAM_OF_INT], PMAX[PARAM_OF_INT], PINC[PARAM_OF_INT])

    # BUT WE ADDED A CHECK HERE - DUE TO POTENTIAL OF ROUNDING ERRORS IN SEQ,
    # THE VALUE MAY NOT MATCH THE PARAMETER IN THE OUTPUT, SO ROUND THE VALUE
    # TO THE NUMBER OF DECIMAL PLACES IN THE INCREMENT
    for (i in 1:length(val_list)) {
      dp <- num.decimals(PINC[PARAM_OF_INT])
      val_list[i] <- round(val_list[i], digits = dp + 2)
    }
    # Convert to char - stops any trailing zeros that may lead to results
    # not being found
    val_list <- as.character(val_list)
  } else {
    # WILL HAVE SPECIFIED A STRING LIST OF VALUES, SPLIT AND CONVERT TO NUMBERS
    val_list <- strsplit(PARAMVALS[PARAM_OF_INT], ",")[[1]]
  }

  # NOTE THE RETURN AS CHARACTER - STOPS ANY TRAILING ZEROS WHICH MAY MAKE THE
  # RESULTS IMPOSSIBLE TO FIND IN THE OUTPUT FILE
  return(val_list)
}

subset_results_by_param_value_set <- function(PARAMETERS, RESULT_SET,
                                              PARAMETER_VALUES_TO_FILTER_BY) {
  # TAKES A CSV FILE OF MEDIAN RESULTS OR SIMULATION RESULTS AND FILTERS BY A
  # SPECIFIED SET OF PARAMETER VALUES RETURNING THE FILTERED SET

  # TAKE A COPY OF THE RESULT SET
  PARAM_RESULT <- RESULT_SET

  # NOW EXTRACT THE DATA FOR THIS PARAM VALUE
  for (P in 1:length(PARAMETERS))
    PARAM_RESULT <- subset(PARAM_RESULT, PARAM_RESULT[[PARAMETERS[P]]]
                           == as.numeric(PARAMETER_VALUES_TO_FILTER_BY[P]))

  return(PARAM_RESULT)
}

#' Used to diagnose skew in a training dataset before use in emulation
#'
#' Useful for determining how useful a simulation dataset is for training
#' the range of emulators available in this package. This is output as a
#' PDF.
#' @param dataset Dataset being visualised
#' @param measure Simulation response measure to visualise
#' @param graphname Name of the graph to produce (as a PDF)
#' @param num_bins Number of bins to use in the histogram
#' @export
#'
visualise_data_distribution <- function(dataset, measure, graphname,
                                        num_bins=30) {
  kurt <- psych::describe(dataset[measure])
  #dataset <- data.frame(dataset[measure])
  ggplot(dataset[measure],
         aes(dataset[measure])) + geom_histogram(bins=num_bins) + ggtitle(
             paste("Diagnostic Plot for ", measure, "\nKurtosis:",
                    round(kurt$kurtosis, 3), "Skew", round(kurt$skew, 3),
                    sep = " ")) +  scale_x_continuous(name = "Dataset") +
    scale_y_continuous(name = "Frequency")

  ggsave(paste(graphname, ".pdf", sep = ""), device = "pdf")
}

check_data_partitions <-function(train,test,validate)
{
  if((train+test+validate) == 100)
    return(TRUE)
  else
    return(FALSE)
}

#' Partition latin-hypercube summary file to training, testing, and validation
#'
#' Used in the development of emulations of a simulation using a
#' latin-hypercube summary file
#'
#' @param dataset LHC summary file to partition
#' @param percent_train Percent of the dataset to use as training
#' @param percent_test Percent of the dataset to use as testing
#' @param percent_validation Percent of the dataset to use as validation
#' @param seed For specifying a particular seed when randomly splitting the set
#' @param normalise Whether the data needs to be normalised (to be between 0
#' and 1). For emulation creation to be successful, all data must be normalised
#' prior to use in training and testing
#' @param parameters Simulation parameters the emulation will be fed as input
#' @param sample_mins The minimum value used for each parameter in generating
#' the latin-hypercube sample
#' @param sample_maxes The maximum value used for each parameter in generating
#' the latin-hypercube sample
#' @param timepoint Simulation timepoint for which this summary file was created
#' @return Partitioned dataset containing training, testing, and validation
#' sets, in addition to the sample mins and maxes such that any predictions
#' that are generated using this normalised data can be rescaled correctly
#' @examples
#' data("sim_data_for_emulation")
#' parameters<-c("stableBindProbability","chemokineExpressionThreshold",
#' "initialChemokineExpressionValue","maxChemokineExpressionValue",
#' "maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
#' measures<-c("Velocity","Displacement","PatchArea")
#' sampleMaxes <- cbind(100,0.9,0.5,0.08,1,5)
#' sampleMins <-cbind(0,0.1,0.1,0.015,0.1,0.25)
#' partitionedData <- partition_dataset(sim_data_for_emulation, parameters,
#' percent_train=75, percent_test=15, percent_validation=10, normalise=TRUE,
#' sample_mins = sampleMins, sample_maxes = sampleMaxes)
#'
#' @export
partition_dataset <- function(dataset, parameters, percent_train = 75, percent_test = 15,
                              percent_validation = 10, seed = NULL,
                              normalise = FALSE,
                              sample_mins = NULL, sample_maxes = NULL,
                              timepoint = NULL) {

  tryCatch(
  {
    if(check_data_partitions(percent_train, percent_test, percent_validation))
    {
      if (!is.null(seed)) set.seed(seed)

      # If we normalise, we need to have the mins and maxes for parameters and
      # measures for denormalisation of results. If we don't normalise then
      # there will be no denormalisation, but the values being passed will not
      # be initialised, so we need to cope with both here
      pre_normed_data_mins <- NULL
      pre_normed_data_maxes <- NULL


      if (normalise == TRUE) {
        if (is.null(sample_mins) | is.null(sample_maxes) | is.null(parameters))
          message("You need to specify sampling mins and maxes for each parameter,
                and parameter names, for correct normalisation. Terminated.")
        else {
          normed_data <- normalise_dataset(dataset, sample_mins, sample_maxes,
                                           parameters)
          dataset <- normed_data$scaled
          pre_normed_data_mins <- normed_data$mins
          pre_normed_data_maxes <- normed_data$maxs
        }
      }

      positions <- sample(nrow(dataset), size = floor( (nrow(dataset) / 100)
                                                      * percent_train))
      training <- dataset[positions, ]
      remainder <- dataset[ -positions, ]

      testing_positions <- sample(
        nrow(remainder), size = floor(
          (nrow(remainder) / 100) * ( (percent_test / (percent_test +
                                                       percent_validation))
                                     * 100)))
      testing <- remainder[testing_positions, ]
      validation <- remainder[-testing_positions, ]

      partitioned_data <- list("training" = training, "testing" = testing,
                              "validation" = validation,
                              "pre_normed_mins" = pre_normed_data_mins,
                              "pre_normed_maxes" = pre_normed_data_maxes)

      if (is.null(timepoint))
        save(partitioned_data, file = "partitioned_data.Rda")
      else
        save(partitioned_data, file = paste("partitioned_data_", timepoint, ".Rda",
                                           sep = ""))

      return(partitioned_data)
    }
    else
    {
      message("Partition percentages do not add up to 100%. Terminated")
      return(NULL)
    }
  },
  error=function(cond) {
  message("Training, Testing, and Validation percentages have been declared incorrectly")
  message("Spartan Function Terminated")
  return(NULL)
})
}

#' Normalise a dataset such that all values are between 0 and 1
#'
#' @param dataset LHC Summary file being used in emulator creation
#' @param sample_mins The minimum value used for each parameter in generating
#' the latin-hypercube sample
#' @param sample_maxes The maximum value used for each parameter in generating
#' the latin-hypercube sample
#' @param parameters Simulation parameters the emulation will be fed as input
#' @return List of the scaled data and the mimumum/maximum sample values for
#' each, to aid rescale of the data and any predictions made using it.
#'
#' @export
normalise_dataset <- function(dataset, sample_mins, sample_maxes, parameters) {

  mins <- apply(dataset, 2, min)
  maxs <- apply(dataset, 2, max)

  # we want to override the parameter bounds with those used in sampling
  mins[parameters] <- sample_mins
  maxs[parameters] <- sample_maxes

  # Normalise data
  scaled <- as.data.frame(scale(dataset, center = mins, scale = maxs - mins))

  return(list("scaled" = scaled, "mins" = mins, "maxs" = maxs))
}

#' Rescale normalised data back to it's original scale
#'
#' @param normalised_data Dataset that has been normalised
#' @param scaled_mins The minimum value used for each parameter in generating
#' the latin-hypercube sample
#' @param scaled_maxes The maximum value used for each parameter in generating
#' the latin-hypercube sample
#'
#' @keywords internal
denormalise_dataset <- function(normalised_data, scaled_mins, scaled_maxes) {

  for (c in 1:ncol(normalised_data)) {
    normalised_data[, c] <- (normalised_data[, c] *
                              (scaled_maxes[, c] -
                                 scaled_mins[, c])) + scaled_mins[, c]
  }
  return(normalised_data)
}


#' Combines a list of elements into a filepath separated by "/"
#' @param string_list List of text elements to combine
#' @return Combined list as string separated by "/"
#' @keywords internal
make_path <- function(string_list) {
  return(paste(string_list, collapse = "/"))
}

#' Adds an extension to a filename
#' @param file Name of the file being created
#' @param extenstion Type of file being created
#' @return Filename with extension
#' @keywords internal
make_extension <- function(file, extension) {
  return(paste(file, extension, sep = "."))
}

#' Makes a filename from a list of strings, separated by "_"
#' @param string_list List of text elements to combine
#' @return Combined list as a string separated by "_"
#' @keywords internal
make_filename <- function(string_list) {
  return(paste(string_list, collapse = "_"))
}

#' Join strings and separate by specified character
#' @param string_list List of text elements to combine
#' @param sepr Character to separate elements by
#' @return Combined list as string separated by specified character
#' @keywords internal
join_strings <- function(string_list, sepr) {
  return(paste(string_list, collapse = sepr))
}

#' Join strings and separate by a space
#' @param string_list List of text elements to combine
#' @return Combined list as string separated by a space
#' @keywords internal
join_strings_space <- function(string_list) {
  return(paste(string_list, collapse = " "))
}

#' Join strings and separate with no space
#' @param string_list List of text elements to combine
#' @return Combined list as string separated by no spaces
#' @keywords internal
join_strings_nospace <- function(string_list) {
  return(paste(string_list, collapse = ""))
}

#' Shortcut function for writing data to CSV file
#' @param outputData Data to write to CSV file
#' @param outputFile Name of the output file
#' @param row_names Boolean as to whether to print row names
write_data_to_csv <- function(outputData, outputFile, row_names = FALSE)
{
  write.csv(outputData, outputFile, row.names = row_names, quote = FALSE)
}

#' To save retyping all options, function to read CSV data
#' @param filepath Path to CSV file to read
#' @return Data from CSV file
read_from_csv <- function(filepath)
{
  return(read.csv(filepath, sep = ",", header = TRUE, check.names = FALSE))
}

#' Check whether a file exists
#' @param filepath File path to check
#' @return Boolean stating whether this file exists or not
check_file_exists <- function(filepath) {
  if(file.exists(filepath))
    return(TRUE)
  else
  {
    message(paste(filepath, " does not exist",sep=""))
    return(FALSE)
  }
}

#' Output a ggplot graph in the requested formats
#' @param GRAPHFILE Path and name of the file to output
#' @param OUTPUT_TYPE List of the output types to produce
#' @param output_graph Graph to output
output_ggplot_graph <-function(GRAPHFILE, OUTPUT_TYPE, output_graph) {
  for (file_type in 1:length(OUTPUT_TYPE)) {
    # Save the graphs in the requested format
    if (OUTPUT_TYPE[file_type] == "PDF") {
      ggsave(paste(GRAPHFILE, ".pdf", sep = ""),
             plot = output_graph, width = 4, height = 4)
    } else if (OUTPUT_TYPE[file_type] == "PNG") {
      ggsave(paste(GRAPHFILE, ".png", sep = ""),
             plot = output_graph, width = 4, height = 4)
    } else if (OUTPUT_TYPE[file_type] == "TIFF") {
      ggsave(paste(GRAPHFILE, ".tiff", sep = ""),
             plot = output_graph, width = 4, height = 4)
    } else if (OUTPUT_TYPE[file_type] == "BMP") {
      ggsave(paste(GRAPHFILE, ".bmp", sep = ""),
             plot = output_graph, width = 4, height = 4)
    }
  }
}
