#' Summarises stochastic, repeated, simulations for all robustness parameter sets into a single file.
#'
#' This method should only be used for stochastic simulations where the data is
#' provided in the set folder structure (see the spartan R Journal paper). Each
#' parameter, and all values that it has been assigned, are examined in turn.
#' For each replicate run under those parameter conditions, the median of the
#' simulation response is calculated. These medians for each simulation replicate,
#' of each parameter set, are stored in a CSV file, creating the same single CSV
#' file format that can also be provided as Spartan input. This file is named
#' as stated in parameter CSV_FILE_NAME. This method can be performed for a number
#' of simulation timepoints, producing these statistics for each timepoint taken.
#'
#' @param FILEPATH Directory where either the simulation runs or single CSV file result can be found
#' @param PARAMETERS Array containing the names of the parameters for which local analyses are being conducted
#' @param NUMRUNSPERSAMPLE The number of runs performed for each parameter subset. This figure is generated through Aleatory Analysis
#' @param MEASURES Array containing the names of the output measures which are used to analyse the simulation
#' @param RESULTFILENAME Name of the simulation results file. In the current version, XML and CSV files can be processed. If performing this analysis over multiple timepoints, it is assumed that the timepoint follows the file name, e.g. trackedCells_Close_12.csv.
#' @param ALTERNATIVEFILENAME In some cases, it may be relevant to read from a further results file if he initial file contains no results. This filename is set here.
#' @param OUTPUTCOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTCOLEND Column number in the simulation results file where the last output measure is. Only required if running the first method.
#' @param CSV_FILE_NAME Name of the file created that summarises the median value of each measure for every run. This specifies what that file should be called (e.g. Medians.csv).
#' @param BASELINE Array containing the values assigned to each of these parameters in the calibrated baseline
#' @param PMIN Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space
#' @param PINC Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space
#' @param PARAMVALS Array containing a list of strings for each parameter, each string containing comma separated values that should be assigned to that parameter. Thus sampling can be performed for specific values for each parameter, rather than a uniform incremented value. This replaces the PMIN, PMAX, and PINC where this method is used
#' @param TIMEPOINTS Implemented so this method can be used when analysing multiple simulation timepoints. If only analysing one timepoint, this should be set to NULL. If not, this should be an array of timepoints, e.g. c(12,36,48,60)
#' @param TIMEPOINTSCALE Implemented so this method can be used when analysing multiple simulation timepoints. Sets the scale of the timepoints being analysed, e.g. "Hours"
#'
#' @export
oat_processParamSubsets <- function(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE,
                                    MEASURES, RESULTFILENAME,
                                    ALTERNATIVEFILENAME, OUTPUTFILECOLSTART,
                                    OUTPUTFILECOLEND, CSV_FILE_NAME,
                                    BASELINE, PMIN = NULL, PMAX = NULL,
                                    PINC = NULL, PARAMVALS = NULL,
                                    TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL) {

  # CREATE THE MEDIAN DISTRIBUTION OVER THE SET OF RUNS FOR EACH PARAMETER SET,
  # FOR EACH PARAMETER (AS THIS IS ONE AT A TIME)
  # NOTE FROM SPARTAN 2, THIS FILE CAN ONLY BE A CSV FILE - XML FILES OF THIS
  # SIZE TAKE A LARGE AMOUNT OF TIME TO PROCESS
  # Version 3.1 - added pre-execution error checks
  # Get the provided function arguments
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  #print(input_check)

  if(check_input_args(input_check$names, input_check$arguments))
  {

    if (is.null(TIMEPOINTS)) {

      message("Generating Median Response File (oat_processParamSubsets)")

      all_median_results <- generate_summary_stats_for_all_param_sets(
        FILEPATH, PARAMETERS, BASELINE, PMIN, PMAX, PINC, PARAMVALS, NUMRUNSPERSAMPLE,
        MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTFILECOLSTART,
        OUTPUTFILECOLEND)

      # Output if results not blank
      if (!is.null(all_median_results)) {
        write_data_to_csv(all_median_results,
                          paste(FILEPATH, "/", CSV_FILE_NAME, sep = ""))
      }
    } else {
      oat_processParamSubsets_overTime(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE,
                                       MEASURES, RESULTFILENAME,
                                       ALTERNATIVEFILENAME, OUTPUTFILECOLSTART,
                                       OUTPUTFILECOLEND, CSV_FILE_NAME, BASELINE,
                                       PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS,
                                       TIMEPOINTSCALE)
    }
  }
}

#' Summarises stochastic, repeated, simulations for all robustness parameter sets into a single file, for multiple timepoints
#'
#' @inheritParams oat_processParamSubsets
oat_processParamSubsets_overTime <- function(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE,
                                    MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME,
                                    OUTPUTCOLSTART, OUTPUTCOLEND, CSV_FILE_NAME,
                                    BASELINE, PMIN = NULL, PMAX = NULL,
                                    PINC = NULL, PARAMVALS = NULL,
                                    TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL) {

  # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
  for (n in 1:length(TIMEPOINTS)) {

    current_time <- TIMEPOINTS[n]
    message(join_strings_space(c("Processing Timepoint:", current_time)))

    simresultfilename <- append_time_to_argument(
      RESULTFILENAME, current_time,
      check_file_extension(RESULTFILENAME))

    altfilename_full <- NULL
    if (!is.null(ALTERNATIVEFILENAME))
      altfilename_full <- append_time_to_argument(
        ALTERNATIVEFILENAME, current_time,
        check_file_extension(ALTERNATIVEFILENAME))

    csvfilename_full <- append_time_to_argument(
      CSV_FILE_NAME, current_time,
      check_file_extension(CSV_FILE_NAME))

    # Now process this point, timepoints null so we don't end up back here

    oat_processParamSubsets(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE, MEASURES,
                            simresultfilename, altfilename_full,
                            OUTPUTCOLSTART, OUTPUTCOLEND, csvfilename_full,
                            BASELINE, PMIN, PMAX, PINC, PARAMVALS, NULL, NULL)
  }

}

#' Generate summary statistics for each value of all parameters in this analysis
#' @inheritParams oat_processParamSubsets
generate_summary_stats_for_all_param_sets <- function(
  FILEPATH, PARAMETERS, BASELINE, PMIN, PMAX, PINC, PARAMVALS, NUMRUNSPERSAMPLE,
  MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
  OUTPUTCOLEND) {

  # With multiple parameters, the baseline will be simulated multiple times
  # No need to analyse them all. This flag stops that happening.
  baseline_evaluated = FALSE

  # Store for all results
  all_median_results <- NULL

  for (param in 1:length(PARAMETERS)) {
    EXP_PARAMS <- as.character(BASELINE)

    # Now we can work with increments between min and max or specified values,
    # we get the values of the parameters we're analysing. Conversion gets rid
    # of trailing zeros
    param_val_list <-
      as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                              PARAMVALS, param))

    param_result <- produce_summary_for_all_values_of_parameter(
      FILEPATH, param, param_val_list, BASELINE, baseline_evaluated, PARAMETERS, EXP_PARAMS,
      NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
      OUTPUTCOLEND)

    all_median_results <- rbind(
      all_median_results, param_result$parameter_result)

    baseline_evaluated <- param_result$baseline_evaluated


  }

  colnames(all_median_results) <- c(PARAMETERS, MEASURES)
  return(all_median_results)

}

#' For one parameter, evaluate the results of all values that parameter can take
#' @param param Current index of parameter being evaluated
#' @param param_val_list List of values this parameter can take
#' @param BASELINE Array containing the values assigned to each of these parameters in the calibrated baseline
#' @param baseline_evaluated Whether results for the baseline have been calculated
#' @param PARAMETERS Array containing the names of the parameters for which local analyses are being conducted
#' @param all_median_results The current result set to append the parameter results to
#' @inheritParams oat_processParamSubsets
#' @return The results for this parameter, and whether the baseline has been evaluated
produce_summary_for_all_values_of_parameter <- function(
  FILEPATH, param, param_val_list, BASELINE, baseline_evaluated, PARAMETERS, EXP_PARAMS,
  NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
  OUTPUTCOLEND) {

  parameter_result <- NULL
  # iterate through all values in this list
  for (paramval in 1:length(param_val_list)) {
    # Set the value of the parameters being examined to include current examined value
    EXP_PARAMS[param] <- as.character(param_val_list[paramval])

    if (param_val_list[paramval] != BASELINE[param] ||
        !baseline_evaluated) {

      param_set_result <- process_parameter_value_if_exists(
        FILEPATH, NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME,
        OUTPUTCOLSTART, OUTPUTCOLEND, PARAMETERS[param], param_val_list[paramval],
        EXP_PARAMS)

      # Add to result set if results were found
      if(!is.null(param_set_result))
        parameter_result <- rbind(
          parameter_result, param_set_result)

      if (param_val_list[paramval] == BASELINE[param]) {
        baseline_evaluated <- TRUE
      }
    }
  }

  return(list("baseline_evaluated"=baseline_evaluated, "parameter_result"=parameter_result))
}


#' Generate the median responses for a set of parameter values
#'
#' @param SAMPLEFILEPATH
#' @param EXP_PARAMS Set of the value of all parameters being examined
#' @param PARAMETER Name of the parameter currently being analysed
#' @param PARAM_VAL Value of the parameter currently being analysed
#' @inheritParams oat_processParamSubsets
#' @return parameter set with median responses for all values
generate_medians_for_param_set <- function(SAMPLEFILEPATH, NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
                                           OUTPUTCOLEND, EXP_PARAMS, PARAMETER, PARAM_VAL)
{
  message(paste("Generating Median Results for Parameter: ",
              PARAMETER, ", Value: ",
              PARAM_VAL, sep = ""))

  # Get median distribution for all runs under these parameter conditions
  median_results <- getMediansSubset(SAMPLEFILEPATH, NUMRUNSPERSAMPLE,
                                     MEASURES, RESULTFILENAME,
                                     ALTERNATIVEFILENAME, OUTPUTCOLSTART,
                                     OUTPUTCOLEND)

  # Bind parameter set to calculated median results
  params <- NULL
  for (p in 1:length(EXP_PARAMS)) {
    params <- cbind(params, array(as.numeric(
      EXP_PARAMS[p]),dim = c(nrow(median_results))))
  }
  params_and_result <- cbind(params, median_results)

  return(params_and_result)

}

#' Process parameter value set if results exist
#'
#' @inheritParams oat_processParamSubsets
#' @param PARAMETER Name of the parameter currently being analysed
#' @param PARAM_VAL Value of the parameter currently being analysed
#' @param EXP_PARAMS Set of the value of all parameters being examined
#' @return medians for this parameter set
process_parameter_value_if_exists <- function(
  FILEPATH, NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME,
  OUTPUTCOLSTART, OUTPUTCOLEND, PARAMETER, PARAM_VAL, EXP_PARAMS)
{
  # Where results for this parameter and value should be:
  SAMPLEFILEPATH <- paste(FILEPATH, "/", PARAMETER, "/",
                          toString(PARAM_VAL), "/", sep = "")

  if (file.exists(SAMPLEFILEPATH)) {

    return(generate_medians_for_param_set(
        SAMPLEFILEPATH, NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME,
        ALTERNATIVEFILENAME, OUTPUTCOLSTART, OUTPUTCOLEND,
        EXP_PARAMS, PARAMETER, PARAM_VAL))


  } else {

    message(paste("No results can be found for parameter: ",
                PARAMETER, " Value: ", PARAM_VAL, sep = ""))
    return(NULL)
  }
}



#' Performs a robustness analysis for supplied simulation data, comparing simulation behaviour at different parameter values
#'
#' This method takes either the CSV file created in
#' \code{oat_processParamSubsets} or provided by the user and analyses the
#' impact that a change in a single parameter value has had on simulation
#' response. This is performed by comparing the distribution of responses
#' for a perturbed parameter condition with the distribution under
#' baseline/calibrated conditions. This produces a CSV file, in the directory
#' stated in FILEPATH, named as stated by parameter ATESTRESULTSFILENAME,
#' containing the A-Test scores for all parameter conditions under which the
#' simulation was run. This method can be performed for a number of simulation
#' timepoints, producing these statistics for each timepoint taken.
#'
#' @inheritParams oat_processParamSubsets
#' @param CSV_FILE_NAME Name of the CSV file in which the results of all simulations exist (or have been summarised)
#' @param ATESTRESULTFILENAME File name of the ATests result summary file that will be created For one timepoint, this could be ATests.csv. For additional timepoints, the time is added to the file name
#'
#' @export
oat_csv_result_file_analysis <- function(FILEPATH, CSV_FILE_NAME, PARAMETERS,
                                         BASELINE, MEASURES,
                                         ATESTRESULTFILENAME,
                                         PMIN = NULL, PMAX = NULL, PINC = NULL,
                                         PARAMVALS = NULL, TIMEPOINTS = NULL,
                                         TIMEPOINTSCALE = NULL) {
  if (is.null(TIMEPOINTS)) {

    # NEW TO SPARTAN VERSION 2
    # READS SIMULATION RESPONSES FROM A CSV FILE, IN THE FORMAT: PARAMETER
    # VALUES (COLUMNS), SIMULATION OUTPUT MEASURES
    # IN A CHANGE TO SPARTAN 1, THE FIRST FUNCTION THAT PROCESSES SIMULATION
    # RESPONSES CREATES THIS FILE, NOT MEDIANS FOR EACH PARAMETER AS IT USED TO
    # THIS WAY WE ARE NOT DEALING WITH TWO METHODS OF SIMULATION RESULT
    # SPECIFICATION
    RESULT <- read.csv(paste(FILEPATH, "/", CSV_FILE_NAME, sep = ""), sep = ",",
                       header = TRUE, check.names = FALSE)

    # FIRSTLY FILTER THE SIMULATION RESULTS WHEN AT BASELINE VALUES
    BASELINE_RESULT <- subset_results_by_param_value_set(PARAMETERS, RESULT,
                                                         BASELINE)

    if (nrow(BASELINE_RESULT) == 0) {
      print("No results in the CSV file for simulation at specified baseline
            values. No analysis performed")
    } else {
      # STORE ALL THE A-TEST SCORES FOR ALL EXPERIMENTS
      ALL_ATEST_SCORES <- NULL

      # DO THE BASELINE A-TEST FIRST - THIS WILL ALWAYS BE NO DIFFERENCE, BUT
      # NEEDS TO BE LISTED AS A RESULT FOR GRAPHING
      # THIS ALSO STOPS THE SAME TEST BEING DONE FOR EACH PARAMETER
      # (AS EACH WILL BE AT BASELINE VALUE AT SOME POINT)
      ALL_ATEST_SCORES <- rbind(
        ALL_ATEST_SCORES, perform_aTest_for_all_sim_measures(BASELINE,
                                                             BASELINE_RESULT,
                                                             BASELINE_RESULT,
                                                             MEASURES))

      # NOW PROCESS EACH PARAMETER
      for (PARAM in 1:length(PARAMETERS)) {
        # THE RESULTS OF THE OAT ANALYSIS IS IN ONE PLACE. THUS WE NEED TO
        # REFER TO THE CORRECT BASELINE RESULT FOR PARAMETERS THAT ARE
        # NOT BEING CHANGED SO WE USE THE VARIABLE EXP_PARAMS WHEN WE START
        # A NEW VARIABLE - WE SET THE PARAMS TO THE BASELINE AND THEN ONLY
        # ALTER THE ONE BEING CHANGED
        EXP_PARAMS <- as.character(BASELINE)

        # GET THE LIST OF PARAMETER VALUES BEING EXPLORED FOR THIS PARAMETER
        PARAM_VAL_LIST <- as.numeric(
          prepare_parameter_value_list(PMIN, PMAX, PINC, PARAMVALS, PARAM))

        # NOW WE ITERATE THROUGH THE VALUES IN THIS LIST
        for (PARAMVAL in 1:length(PARAM_VAL_LIST)) {
          print(paste("Processing Parameter: ", PARAMETERS[PARAM], " Value: ",
                      PARAM_VAL_LIST[PARAMVAL], sep = ""))

          # HERE WE STOP THE CASE OF THE BASELINE BEING PROCESSED SEVERAL
          #TIMES, AS IT WOULD BE FOR EACH PARAMETER
          if (PARAM_VAL_LIST[PARAMVAL] != BASELINE[PARAM]) {

            # SET THE VALUE OF THIS PARAMETER TO BE THAT WE ARE PROCESSING
            EXP_PARAMS[PARAM] <- as.character(PARAM_VAL_LIST[PARAMVAL])

            PARAM_RESULT <- subset_results_by_param_value_set(PARAMETERS,
                                                            RESULT, EXP_PARAMS)

            if (nrow(PARAM_RESULT) > 0) {
              # NOW WE CAN COMPARE THIS BEHAVIOUR TO THAT AT THE BASELINE
              # USING THE A-TEST. DO THIS FOR EACH MEASURE
              ALL_ATEST_SCORES <- rbind(
                ALL_ATEST_SCORES, perform_aTest_for_all_sim_measures(
                  EXP_PARAMS, BASELINE_RESULT, PARAM_RESULT, MEASURES))
            } else {
              print(paste("No Results for Parameter ", PARAMETERS[PARAM],
                          " Value: ", PARAM_VAL_LIST[PARAMVAL],
                          ". No A-Test Calculated", sep = ""))
            }
          }
        }
      }

      # LABEL THE SCORES
      LABELS <- NULL
      for (MEASURE in 1:length(MEASURES)) {
        LABELS <- cbind(LABELS, paste("ATest", MEASURES[MEASURE], sep = ""),
                        paste("ATest", MEASURES[MEASURE], "Norm", sep = ""))
      }

      colnames(ALL_ATEST_SCORES) <- cbind(t(PARAMETERS), LABELS)

      # WRITE THE FILE OUT
      RESULTSFILE <- paste(FILEPATH, "/", ATESTRESULTFILENAME, sep = "")

      write.csv(ALL_ATEST_SCORES, RESULTSFILE, quote = FALSE,
                row.names = FALSE)
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {

      current_time <- TIMEPOINTS[n]
      print(paste("Processing Timepoint: ", current_time, sep = ""))

      CSV_FILE_NAME_FORMAT <- check_file_extension(CSV_FILE_NAME)
      CSV_FILE_NAME_FULL <- paste(substr(CSV_FILE_NAME, 0,
                                         nchar(CSV_FILE_NAME) - 4),
                                  "_", current_time, ".",
                                  CSV_FILE_NAME_FORMAT,
                                  sep = "")

      ATESTRESULTFILENAME_FORMAT <- check_file_extension(ATESTRESULTFILENAME)
      ATESTRESULTFILENAME_FULL <- paste(substr(ATESTRESULTFILENAME, 0,
                                               nchar(ATESTRESULTFILENAME) - 4),
                                        "_", current_time, ".",
                                        ATESTRESULTFILENAME_FORMAT, sep = "")

      oat_csv_result_file_analysis(FILEPATH, CSV_FILE_NAME_FULL, PARAMETERS,
                                   BASELINE, MEASURES,
                                   ATESTRESULTFILENAME_FULL, PMIN, PMAX,
                                   PINC, PARAMVALS, TIMEPOINTS = NULL,
                                   TIMEPOINTSCALE = NULL)
    }
  }
}


