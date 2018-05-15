#' Summarise results in set folder structure into one single CSV file
#'
#' Only to be applied in cases where simulation responses are supplied in the
#' folder structure (as in all previous versions of Spartan), useful for cases
#' where the simulation is non-deterministic. Iterates through simulation runs
#' for each sample size creating a CSV file containing results for all sample
#' sizes and all subsets (in the same format as the new CSV file format
#' discussed above). Where a simulation response is comprised of a number of
#' records (for example a number of cells), the median value will be recorded
#' as the response for this subset of the sample size being analysed. This file
#' is output to a CSV file, named as stated by the parameter
#' SUMMARYFILENAME. If doing this analysis over multiple timepoints, the
#' timepoint will be appended to the filename given in SUMMARYFILENAME
#'
#' @param FILEPATH Directory where the results of the simulation runs, in folders or in single CSV file format, can be found
#' @param SAMPLESIZES The sample sizes chosen (i.e. in our case, this would be an array containing 1,5,50,100,300,800
#' @param MEASURES An array containing the names of the simulation output measures to be analysed.
#' @param RESULTFILENAME Name of the simulation results file. In the current version, XML and CSV files can be processed. If performing this analysis over multiple timepoints, it is assumed that the timepoint follows the file name, e.g. trackedCells_Close_12.csv.
#' @param ALTFILENAME In some cases, it may be relevant to read from a further results file if the initial file contains no results. This filename is set here. In the current version, XML and CSV files can be processed.
#' @param OUTPUTFILECOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTFILECOLEND Column number in the simulation results file where the last output measure is.
#' @param SUMMARYFILENAME Name of the file generated that lists the maximum and median A-Test results for each sample size.
#' @param NUMSUBSETSPERSAMPLESIZE Number of subsets of simulation runs for each sample size. Defaults to 20
#' @param TIMEPOINTS Implemented so this method can be used when analysing multiple simulation timepoints. If only analysing one timepoint, this should be set to NULL. If not, this should be an array of timepoints, e.g. c(12,36,48,60)
#' @param TIMEPOINTSCALE Implemented so this method can be used when analysing multiple simulation timepoints. Sets the scale of the timepoints being analysed, e.g. "Hours"
#' @param check_done If multiple timepoints, whether the input has been checked
#'
#' @export
aa_summariseReplicateRuns <- function(FILEPATH, SAMPLESIZES, MEASURES,
                                      RESULTFILENAME, ALTFILENAME = NULL,
                                      OUTPUTFILECOLSTART, OUTPUTFILECOLEND,
                                      SUMMARYFILENAME, NUMSUBSETSPERSAMPLESIZE=20, TIMEPOINTS = NULL,
                                      TIMEPOINTSCALE = NULL, check_done=FALSE) {

  # Version 3.1 adds pre-execution check functions as part of refactoring:
  # Get the provided function arguments
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:
  if(check_input_args(input_check$names, input_check$arguments)) {

    if (is.null(TIMEPOINTS)) {

      SAMPLE_SIZE_RESULTS <- NULL

      for (SAMPLESIZE in 1:length(SAMPLESIZES)) {
        subset_medians <- get_medians_for_size_subsets(
          FILEPATH, NUMSUBSETSPERSAMPLESIZE, SAMPLESIZES[SAMPLESIZE], MEASURES, RESULTFILENAME,
          ALTFILENAME, OUTPUTFILECOLSTART, OUTPUTFILECOLEND)

        if(!is.null(subset_medians))
          SAMPLE_SIZE_RESULTS <- rbind(SAMPLE_SIZE_RESULTS, subset_medians)

      }

      colnames(SAMPLE_SIZE_RESULTS) <- c("SampleSize", "Set", MEASURES)

      message(join_strings_space(c("Writing Median Results to CSV File:",
                               SUMMARYFILENAME)))
      write_data_to_csv(SAMPLE_SIZE_RESULTS, file.path(FILEPATH,SUMMARYFILENAME))


    } else {
      aa_summariseReplicateRuns_overTime(FILEPATH, SAMPLESIZES, MEASURES,
                                         RESULTFILENAME, ALTFILENAME,
                                         OUTPUTFILECOLSTART, OUTPUTFILECOLEND,
                                         SUMMARYFILENAME, NUMSUBSETSPERSAMPLESIZE,
                                         TIMEPOINTS, TIMEPOINTSCALE)
    }
  }
}

#' Calculate summary responses for consistency analysis simulations at multiple timepoints
#' @inheritParams aa_summariseReplicateRuns
aa_summariseReplicateRuns_overTime <- function(
  FILEPATH, SAMPLESIZES, MEASURES, RESULTFILENAME, ALTFILENAME = NULL,
  OUTPUTFILECOLSTART, OUTPUTFILECOLEND, SUMMARYFILENAME, NUMSUBSETSPERSAMPLESIZE,
  TIMEPOINTS, TIMEPOINTSCALE) {

  for (n in 1:length(TIMEPOINTS)) {
    current_time <- TIMEPOINTS[n]
    message(join_strings_space(c("Processing Timepoint:", current_time)))

    simresultfilename <- append_time_to_argument(
      RESULTFILENAME, current_time,
      check_file_extension(RESULTFILENAME))

    altfilename_full <- NULL
    if (!is.null(ALTFILENAME))
      altfilename_full <- append_time_to_argument(
        ALTFILENAME, current_time,
        check_file_extension(ALTFILENAME))

    summaryfilename_full <- append_time_to_argument(
      SUMMARYFILENAME, current_time,
      check_file_extension(SUMMARYFILENAME))

    aa_summariseReplicateRuns(
      FILEPATH, SAMPLESIZES, MEASURES, simresultfilename, altfilename_full,
      OUTPUTFILECOLSTART, OUTPUTFILECOLEND, summaryfilename_full,
      NUMSUBSETSPERSAMPLESIZE, TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL,
      check_done=TRUE)

  }
}


#' Calculates the A-Test scores observed for all sets, for each sample size
#'
#' Examines the summary CSV file produced either by the method
#' \code{aa_summariseReplicateRuns} or provided by the user, analysing each
#' sample size independently, to determine how 'different' the results of
#' each of the subsets are. For each sampple size, the distribution of
#' responses for each subset are compared with the first subset using the
#' Vargha-Delaney A-Test. These scores are stored in a CSV file, with
#' filename as stated in parameter ATESTRESULTSFILENAME. The A-Test results
#' for a sample size are then graphed, showing how different each of the
#' subsets are. If doing this analysis over multiple timepoints, the
#' timepoint will be appended to the filename given in
#' ATESTRESULTSFILENAME and appended to the name of the graph.
#'
#' @inheritParams aa_summariseReplicateRuns
#' @param NUMSUBSETSPERSAMPLESIZE The number of subsets for each sample size (i.e in the tutorial case, 20)
#' @param ATESTRESULTSFILENAME Name of the file that will contain the A-Test scores for each sample size
#' @param LARGEDIFFINDICATOR The A-Test determines there is a large difference between two sets if the result is greater than 0.2 either side of the 0.5 line.  Should this not be suitable, this can be changed here
#' @param GRAPHNAME Used internally by the getATestResults method when producing graphs for multiple timepoints. Should not be set in function call.
#' @param AA_SIM_RESULTS_FILE The name of the CSV file containing the simulation responses, if reading from a CSV file
#' @param AA_SIM_RESULTS_OBJECT The name of the R object containing the simulation responses, if not reading from a CSV file
#' @param check_done If multiple timepoints, has the input been checked
#'
#' @export
#'
#' @importFrom utils read.csv write.csv
aa_getATestResults <- function(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE,
                               MEASURES, ATESTRESULTSFILENAME,
                               LARGEDIFFINDICATOR, AA_SIM_RESULTS_FILE = NULL,
                               AA_SIM_RESULTS_OBJECT = NULL,
                               TIMEPOINTS = NULL,
                               TIMEPOINTSCALE = NULL, GRAPHNAME = NULL, check_done=FALSE) {

  # Version 3.1 adds pre-execution check functions as part of refactoring:
  # Get the provided function arguments
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])

  # Run if all checks pass:
  if(check_input_args(input_check$names, input_check$arguments)) {

    if (is.null(TIMEPOINTS)) {

      sim_results <- read_simulation_results(FILEPATH, AA_SIM_RESULTS_FILE, AA_SIM_RESULTS_OBJECT)
      message("Generating A-Test Scores for Consistency Analysis")

      analysis_result <- generate_scores_for_all_sample_sizes(
        FILEPATH, SAMPLESIZES, sim_results, NUMSUBSETSPERSAMPLESIZE, MEASURES,
        LARGEDIFFINDICATOR, GRAPHNAME)

      # Write out the result
      write_data_to_csv(analysis_result, make_path(c(FILEPATH, ATESTRESULTSFILENAME)))

      # From 3.0 we're going to return the results too.
      # Future additions may remove the CSV writing altogether
      return(analysis_result)

    } else {
      # Process each timepoint
      aa_getATestResults_overTime(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE,
                                  MEASURES, ATESTRESULTSFILENAME,
                                  LARGEDIFFINDICATOR, AA_SIM_RESULTS_FILE,
                                  AA_SIM_RESULTS_OBJECT, TIMEPOINTS, TIMEPOINTSCALE,
                                  GRAPHNAME = NULL)

    }
  }
}

#' Get A-Test results for multiple simulation timepoints
#'
#' @inheritParams aa_summariseReplicateRuns
#' @param NUMSUBSETSPERSAMPLESIZE The number of subsets for each sample size (i.e in the tutorial case, 20)
#' @param ATESTRESULTSFILENAME Name of the file that will contain the A-Test scores for each sample size
#' @param LARGEDIFFINDICATOR The A-Test determines there is a large difference between two sets if the result is greater than 0.2 either side of the 0.5 line.  Should this not be suitable, this can be changed here
#' @param GRAPHNAME Used internally by the getATestResults method when producing graphs for multiple timepoints. Should not be set in function call.
#' @param AA_SIM_RESULTS_FILE The name of the CSV file containing the simulation responses, if reading from a CSV file
#' @param AA_SIM_RESULTS_OBJECT The name of the R object containing the simulation responses, if not reading from a CSV file
aa_getATestResults_overTime <- function(
  FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE, MEASURES,
  ATESTRESULTSFILENAME, LARGEDIFFINDICATOR, AA_SIM_RESULTS_FILE = NULL,
  AA_SIM_RESULTS_OBJECT = NULL, TIMEPOINTS, TIMEPOINTSCALE, GRAPHNAME = NULL) {

  for (n in 1:length(TIMEPOINTS)) {
    current_time <- TIMEPOINTS[n]
    message(join_strings_space(c("Processing Timepoint:",
                               current_time)))

    aa_sim_results_full <- append_time_to_argument(
      AA_SIM_RESULTS_FILE, current_time,
      check_file_extension(AA_SIM_RESULTS_FILE))

    atest_results_full <- append_time_to_argument(
      ATESTRESULTSFILENAME, current_time,
      check_file_extension(ATESTRESULTSFILENAME))

    graph_output_name <- current_time

    aa_getATestResults(
      FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE,
      MEASURES, atest_results_full, LARGEDIFFINDICATOR,
      AA_SIM_RESULTS_FILE = aa_sim_results_full,
      AA_SIM_RESULTS_OBJECT = NULL, TIMEPOINTS = NULL,
      TIMEPOINTSCALE = NULL, GRAPHNAME = graph_output_name, check_done=TRUE)
  }
}


#' Determines the median and maximum A-Test score observed for each sample size
#'
#' This takes each sample size to be examined in turn, and iterates through all
#' the subsets, determining the median and maximum A-Test score observed for
#' each sample size. A CSV file is created summarising the median and maximum
#' A-Test scores for all sample sizes, named as stated in parameter
#' SUMMARYFILENAME. If doing this analysis over multiple timepoints, the
#' timepoint will be appended to the filename given in SUMMARYFILENAME.
#'
#' @param ATESTRESULTS_FILE The name of a CSV file containing the A-Test
#' results calculated by aa_getATestResults, if reading from a CSV file.
#' @param ATESTRESULTS_OBJECT The name of an R object containing the A-Test
#' results calculated by aa_getATestResults, if not reading from a CSV file
#' @inheritParams aa_summariseReplicateRuns
#' @inheritParams aa_getATestResults
#'
#' @export
aa_sampleSizeSummary <- function(FILEPATH, SAMPLESIZES, MEASURES,
                                 SUMMARYFILENAME, ATESTRESULTS_FILE = NULL,
                                 ATESTRESULTS_OBJECT = NULL,
                                 TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL, check_done=FALSE) {

  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:
  if(check_input_args(input_check$names, input_check$arguments))
  {
    if (is.null(TIMEPOINTS)) {

      # Can use read_simulation_results to read CSV or R object
      allSubset_ATest_Scores <- read_simulation_results(FILEPATH,
                                                        ATESTRESULTS_FILE,
                                                        ATESTRESULTS_OBJECT)
      # These were checked in pre-execution check, so should exist

      message("Producing Analysis Summary (aa_sampleSizeSummary)")
      atest_summary <- produce_atest_score_summary(
        SAMPLESIZES, allSubset_ATest_Scores, MEASURES)

      # Output results
      write_data_to_csv(atest_summary,file.path(FILEPATH, SUMMARYFILENAME))

      message(join_strings(c("Summary file of all A-Test results output to ",
                             FILEPATH, "/", SUMMARYFILENAME), ""))

      return(atest_summary)
    } else {

    aa_sampleSizeSummary_overTime(FILEPATH, SAMPLESIZES, MEASURES,
                                  SUMMARYFILENAME, ATESTRESULTS_FILE,
                                  ATESTRESULTS_OBJECT,
                                  TIMEPOINTS, TIMEPOINTSCALE)
    }
  }
}

#' Determines median and maximum A-Test score for each sample size over time
#' @inheritParams aa_sampleSizeSummary
aa_sampleSizeSummary_overTime <- function(FILEPATH, SAMPLESIZES, MEASURES,
                                 SUMMARYFILENAME, ATESTRESULTS_FILE = NULL,
                                 ATESTRESULTS_OBJECT = NULL,
                                 TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL) {

  for (n in 1:length(TIMEPOINTS)) {

    current_time <- TIMEPOINTS[n]
    message(join_strings_space(c("Processing Timepoint:", current_time)))

    atestresultsfilename_full <- append_time_to_argument(
      ATESTRESULTS_FILE, current_time,
      check_file_extension(ATESTRESULTS_FILE))

    summaryfilename_full <- append_time_to_argument(
      SUMMARYFILENAME, current_time,
      check_file_extension(SUMMARYFILENAME))

    aa_sampleSizeSummary(FILEPATH, SAMPLESIZES, MEASURES,
                         summaryfilename_full, ATESTRESULTS_FILE = atestresultsfilename_full,
                         TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL, check_done=TRUE)

  }
}

#' For a given sample size, get the median results to summarise results for all sets
#' @inheritParams aa_summariseReplicateRuns
#' @param SAMPLESIZE Current sample size being processed
#' @param NUMSUBSETSPERSAMPLESIZE Number of run subsets for this sample size, typically 20
#' @return Summary statistics for all runs under this sample size
get_medians_for_size_subsets <- function(FILEPATH, NUMSUBSETSPERSAMPLESIZE,
                                         SAMPLESIZE, MEASURES, RESULTFILENAME,
                                         ALTFILENAME, OUTPUTFILECOLSTART,
                                         OUTPUTFILECOLEND) {

  sample_size_results <- NULL
  for (SET in 1:NUMSUBSETSPERSAMPLESIZE) {
    message(join_strings_space(c("Processing Sample Size ",
                             SAMPLESIZE, " Set ", SET)))

    SAMPLE_FILEPATH <- make_path(c(FILEPATH, SAMPLESIZE, SET))

    MEDIANS <- data.frame(getMediansSubset(SAMPLE_FILEPATH,
                                         SAMPLESIZE,
                                         MEASURES, RESULTFILENAME,
                                         ALTFILENAME,
                                         OUTPUTFILECOLSTART,
                                         OUTPUTFILECOLEND))

    # Add on the sample size and set this was generated from
    MEDIANS <- cbind(
      rep(SAMPLESIZE, nrow(MEDIANS)),
      rep(SET,nrow(MEDIANS)),
      MEDIANS)

    sample_size_results <- rbind(sample_size_results, MEDIANS)
  }

  return(sample_size_results)
}




#' Read in the simulation results either from a file, or R object
#' The existance of these results was checked in pre-execution checks
#' @param FILEPATH Where the results can be found
#' @param AA_SIM_RESULTS_FILE If the objects are in a file, the file name
#' @param AA_SIM_RESULTS_OBJECT If in an R object, the name of the object
#' @return Simulation results for processing
read_simulation_results <- function(FILEPATH, AA_SIM_RESULTS_FILE,
                                    AA_SIM_RESULTS_OBJECT) {

  if(!is.null(AA_SIM_RESULTS_FILE)) {
    # READ IN THE SUMMARY FILE
    return(read.csv(make_path(c(FILEPATH, AA_SIM_RESULTS_FILE)),
                       sep = ",", header = TRUE, check.names = FALSE))
  } else {
    return(AA_SIM_RESULTS_OBJECT)
  }
}

#' Generates the CSV file header for the A-Test results file
#' @param pre_measure_info Any header info to put before measure names
#' @param measures The simulation output responses
#' @return Header object for CSV file
generate_a_test_results_header <- function(pre_measure_info,measures) {

  atest_results_header <- pre_measure_info

  for (l in 1:length(measures)) {
    atest_results_header <- cbind(atest_results_header,
                                paste("ATest", measures[l], sep = ""),
                                paste("ATest", measures[l], "Norm",
                                      sep = ""))
  }

  return(atest_results_header)
}

#' Get the first result set, to which all others are compared
#' @param RESULT Simulation results
#' @param SAMPLESIZE Current sample size being examined
#' @return Results for set 1 of this sample size
retrieve_results_for_comparison_result_set <- function(RESULT, SAMPLESIZE)
{
  return(subset_results_by_param_value_set(c("SampleSize", "Set"), RESULT,
                                            c(SAMPLESIZE, 1)))
}

compare_result_sets_to_comparison_set <- function(NUMSUBSETSPERSAMPLESIZE,
                                                  RESULT, SET1, SAMPLESIZE,
                                                  MEASURES)
{
  SIZE_RESULTS <- NULL

  # Now process all the remaining subsets:
  for (m in 2:NUMSUBSETSPERSAMPLESIZE) {

    # Get this result set
    COMPAREDSET <- subset_results_by_param_value_set(c("SampleSize", "Set"),
                                                     RESULT,
                                                     c(SAMPLESIZE, m))

    # Append A-Test scores on to labels of sample size and measure:
    ALLATESTRESULTS <- generate_a_test_score(cbind(SAMPLESIZE, m), SET1,
                                             COMPAREDSET, MEASURES)

    # Add these tests to the results sheets
    SIZE_RESULTS <- rbind(SIZE_RESULTS, ALLATESTRESULTS)
    colnames(SIZE_RESULTS) <- c(generate_a_test_results_header(
      cbind("Sample Size", "Sample"),MEASURES))
  }

  return(SIZE_RESULTS)
}

generate_scores_for_all_sample_sizes <- function(FILEPATH, SAMPLESIZES, RESULT,
                                                 NUMSUBSETSPERSAMPLESIZE,
                                                 MEASURES, LARGEDIFFINDICATOR,
                                                 GRAPHNAME) {

  analysis_result <- NULL

  for (s in 1:length(SAMPLESIZES)) {
    message(join_strings_space(c("Processing Sample Size:", SAMPLESIZES[s])))

    # Get the first set, for comparison to all the other results
    # Subsetting on SampleSize and Set
    SET1 <- retrieve_results_for_comparison_result_set(RESULT, SAMPLESIZES[s])

    # Now do the comparisons for all other sets for this sample size
    SIZE_RESULTS <- compare_result_sets_to_comparison_set(
      NUMSUBSETSPERSAMPLESIZE, RESULT, SET1, SAMPLESIZES[s], MEASURES)


    # Plot the results
    graph_sample_size_results(FILEPATH, GRAPHNAME, SAMPLESIZES[s], SIZE_RESULTS,
                              MEASURES, LARGEDIFFINDICATOR)

    # Add to total analysis result
    analysis_result<-rbind(analysis_result, SIZE_RESULTS)
  }

  #print(head(analysis_result))
  #colnames(analysis_result) <- c(generate_a_test_results_header(c("Sample Size","Sample"),MEASURES))

  return(analysis_result)
}

#' Take the first set and compare it to a distribution from another set
#' using the A-Test
#'
#' @param ALLATESTRESULTS Current set of A-Test results to append to
#' @param SET1 Results from sample set one
#' @param COMPAREDSET Results from a second set being compared
#' @param MEASURES Simulation output measures
#' @return Appended result to ALLATESTRESULTS
generate_a_test_score <- function(ALLATESTRESULTS, SET1, COMPAREDSET,
                                  MEASURES) {

  if (nrow(COMPAREDSET) > 0) {
    # Now perform the analysis for each measure
    # Then normalise (put above 0.5) as direction does not matter
    for (l in 1:length(MEASURES)) {
      ATESTMEASURERESULT <- atest(as.numeric
                                  (as.matrix(SET1[MEASURES[l]][, 1])),
                                  as.numeric(
                                    as.matrix(COMPAREDSET[MEASURES[l]][, 1]
                                    )))
      # the [,1] is added so the data is extracted
      ATESTMEASURENORM <- normaliseATest(ATESTMEASURERESULT)
      # Bind to result set
      ALLATESTRESULTS <- cbind(ALLATESTRESULTS, ATESTMEASURERESULT,
                               ATESTMEASURENORM)
    }
  } else {
    for (l in 1:length(MEASURES)) {
      ALLATESTRESULTS <- cbind(ALLATESTRESULTS, 1, 1)
    }
  }

  return(ALLATESTRESULTS)
}

#' Graph the A-Test results for a sample size
#'
#' @param FILEPATH Where the results can be found
#' @param GRAPHNAME If multiple timepoints, holds the time so this can be appended to graph name
#' @param SAMPLESIZE Current sample size being analysed
#' @param SIZE_RESULTS Simulation results for this sample size
#' @param MEASURES Simulation output responses
#' @param LARGEDIFFINDICATOR Value either side of 0.5 that indicates a large difference
graph_sample_size_results <- function(FILEPATH, GRAPHNAME, SAMPLESIZE,
                                      SIZE_RESULTS, MEASURES,
                                      LARGEDIFFINDICATOR)
{
  # NOW GRAPH THIS SAMPLE SIZE
  if (is.null(GRAPHNAME))
    GRAPHOUTPUTNAME <- join_strings_nospace(c(SAMPLESIZE,
                                              "Samples.pdf"))
  else
    GRAPHOUTPUTNAME <- join_strings_nospace(c(SAMPLESIZE,
                                              "Samples_", GRAPHNAME,
                                              ".pdf"))

  aa_graphATestsForSampleSize(FILEPATH, SIZE_RESULTS, MEASURES,
                              LARGEDIFFINDICATOR,
                              GRAPHOUTPUTNAME, NULL, NULL)

  message(join_strings_space(c("Summary Graph for Sample Size of",
                             SAMPLESIZE, "Saved to",
                             join_strings_nospace(c(FILEPATH,"/",
                                                    GRAPHOUTPUTNAME)))))
}

#' Appends the time to an argument if processing timepoints
#' @param argument Argument to append the current time to
#' @param current_time Current time being processed
#' @param file_format Format of the file name being altered
#' @return Amended file name argument with time point
append_time_to_argument <- function(argument, current_time, file_format)
{
  return(paste(substr(argument, 0, nchar(argument)-4),
               "_", current_time, ".", file_format, sep = ""))
}


#' Return the max and median A-Test score for all measures for a sample size
#' @param sample_processing Sample size being analysed
#' @param measures Simulation output responses
#' @param sample_size_result all processed simulation responses
#' @return Summary of median and max A-Test scores for this sample size
get_max_and_median_atest_scores <- function(sample_processing,
                                            measures, sample_size_result) {

  sample_size_summary <- c(sample_processing)
  for (measure in 1:length(measures)) {
    measurelabel <- join_strings_nospace(c("ATest",
                                           measures[measure], "Norm"))

    median_atest_measure_val <- median(
      sample_size_result[measurelabel][, 1])
    max_atest_measure_val <- max(sample_size_result[measurelabel][, 1])
    sample_size_summary <- cbind(sample_size_summary, max_atest_measure_val,
                                 median_atest_measure_val)
  }
  return(sample_size_summary)
}

#' Generates headers for the A-Test summary CSV and R Object
#' @param measures Simulation output responses
#' @return header list for A-Test summary object
generate_headers_for_atest_file <- function(measures) {
  headers <- c("samplesize")

  for (l in 1:length(measures)) {
    headers <- cbind(headers, paste(measures[l],
                                                  "MaxA", sep = ""),
                            paste(measures[l], "MedianA", sep = ""))
  }
  return(headers)
}

#' Generates A-Test score summary for all sample sizes
#' @param sample_sizes Sample sizes being assessed
#' @param allSubset_ATest_Scores Scores from all subsets for all sample sizes
#' @param measures Simulation output responses
#' @return A-Test max and median summary for all sample sizes
produce_atest_score_summary <- function(sample_sizes, allSubset_ATest_Scores,
                                        measures) {

  atest_summary <- NULL
  for (k in 1:length(sample_sizes)) {
    sample_processing <- sample_sizes[k]
    message(join_strings_space(c("Processing Sample Size:",
                                 sample_processing)))

    # Subset by column 1, Sample Size
    # KA 3.0 Issue with processing by label "Sample Size" when handling
    # files and objects, so changed to reference column 1.
    sample_size_result <- as.data.frame(subset(allSubset_ATest_Scores,
                                               allSubset_ATest_Scores[,1] ==
                                                 as.numeric(sample_processing)))

    # Calculate max and median A-Test results for all sample sizes
    atest_summary <- rbind(atest_summary, get_max_and_median_atest_scores(
      sample_processing, measures, sample_size_result))
  }
  colnames(atest_summary) <- generate_headers_for_atest_file(measures)
  return(atest_summary)
}

