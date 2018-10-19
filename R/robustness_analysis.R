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
#' @param OUTPUTFILECOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTFILECOLEND Column number in the simulation results file where the last output measure is. Only required if running the first method.
#' @param CSV_FILE_NAME Name of the file created that summarises the median value of each measure for every run. This specifies what that file should be called (e.g. Medians.csv).
#' @param BASELINE Array containing the values assigned to each of these parameters in the calibrated baseline
#' @param PMIN Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space
#' @param PINC Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space
#' @param PARAMVALS Array containing a list of strings for each parameter, each string containing comma separated values that should be assigned to that parameter. Thus sampling can be performed for specific values for each parameter, rather than a uniform incremented value. This replaces the PMIN, PMAX, and PINC where this method is used
#' @param TIMEPOINTS Implemented so this method can be used when analysing multiple simulation timepoints. If only analysing one timepoint, this should be set to NULL. If not, this should be an array of timepoints, e.g. c(12,36,48,60)
#' @param TIMEPOINTSCALE Implemented so this method can be used when analysing multiple simulation timepoints. Sets the scale of the timepoints being analysed, e.g. "Hours"
#' @param check_done Whether the input has been checked (used when doing multiple timepoints)
#'
#' @export
oat_processParamSubsets <- function(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE,
                                    MEASURES, RESULTFILENAME,
                                    ALTERNATIVEFILENAME, OUTPUTFILECOLSTART,
                                    OUTPUTFILECOLEND, CSV_FILE_NAME,
                                    BASELINE, PMIN = NULL, PMAX = NULL,
                                    PINC = NULL, PARAMVALS = NULL,
                                    TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL,
                                    check_done = FALSE) {

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
#' @param OUTPUTCOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTCOLEND Column number in the simulation results file where the last output measure is. Only required if running the first method.
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
                            BASELINE, PMIN, PMAX, PINC, PARAMVALS, NULL, NULL,
                            check_done = TRUE)
  }

}

#' Generate summary statistics for each value of all parameters in this analysis
#'
#' @param OUTPUTCOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTCOLEND Column number in the simulation results file where the last output measure is. Only required if running the first method.
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
#' @param EXP_PARAMS Set of the value of all parameters being examined
#' @param OUTPUTCOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTCOLEND Column number in the simulation results file where the last output measure is. Only required if running the first method.
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
#' @param SAMPLEFILEPATH Path to the results currently being analysed (parameter and value)
#' @param EXP_PARAMS Set of the value of all parameters being examined
#' @param PARAMETER Name of the parameter currently being analysed
#' @param PARAM_VAL Value of the parameter currently being analysed
#' @param OUTPUTCOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTCOLEND Column number in the simulation results file where the last output measure is. Only required if running the first method.
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
#' @param OUTPUTCOLSTART Column number in the simulation results file where output begins - saves (a) reading in unnecessary data, and (b) errors where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTCOLEND Column number in the simulation results file where the last output measure is. Only required if running the first method.
#' @return medians for this parameter set
process_parameter_value_if_exists <- function(
  FILEPATH, NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME,
  OUTPUTCOLSTART, OUTPUTCOLEND, PARAMETER, PARAM_VAL, EXP_PARAMS)
{
  # Where results for this parameter and value should be:
  SAMPLEFILEPATH <- file.path(FILEPATH, PARAMETER,
                              toString(PARAM_VAL))

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
#' @param CSV_FILE_NAME Name of the CSV file in which the results of all simulations exist (or have been summarised)
#' @param ATESTRESULTFILENAME File name of the ATests result summary file that will be created For one timepoint, this could be ATests.csv. For additional timepoints, the time is added to the file name
#' @param check_done Whether the input has been checked (used when doing multiple timepoints)
#' @inheritParams oat_processParamSubsets
#'
#' @export
oat_csv_result_file_analysis <- function(FILEPATH, CSV_FILE_NAME, PARAMETERS,
                                         BASELINE, MEASURES,
                                         ATESTRESULTFILENAME,
                                         PMIN = NULL, PMAX = NULL, PINC = NULL,
                                         PARAMVALS = NULL, TIMEPOINTS = NULL,
                                         TIMEPOINTSCALE = NULL, check_done=FALSE) {

  # Version 3.1 - added pre-execution error checks
  # Get the provided function arguments
  #input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  #print(input_check)

  # If recursively called when doing timepoints, we don't need to do the check again
  #if(check_input_args(input_check$names, input_check$arguments)) {
      if (is.null(TIMEPOINTS)) {
        # From Spartan 2 simulation responses read from CSV in format: parameter
        # values (columns) followed by sim output measures. First function creates
        # this file if folder structure was used

        result <- read_from_csv(file.path(FILEPATH, CSV_FILE_NAME))

        # Firstly filter when the simulation results were at baseline values
        baseline_result <- subset_results_by_param_value_set(PARAMETERS, result,
                                                           BASELINE)

        if(nrow(baseline_result) > 0)
        {
          # Do baseline A-Test - will always be no difference, but must be in
          # result file for graphing
          all_atest_scores <- perform_aTest_for_all_sim_measures(
            BASELINE, baseline_result, baseline_result, MEASURES)

          # Now process each parameter
          for (PARAM in 1:length(PARAMETERS)) {
            # Exp_params is set as baseline, then the value of the parameter
            # being analysed is adjusted, thus we have a set of parameters with
            # which we can subset the result file
            exp_params <- as.character(BASELINE)

            # List of parameter values for this parameter
            parameter_value_list <- as.numeric(
              prepare_parameter_value_list(PMIN, PMAX, PINC, PARAMVALS, PARAM))

            # Now iterate through the values in this list
            ## Bug Here, fixed Sept 2018 - should not have been rbinding
            all_atest_scores <- compare_all_values_of_parameter_to_baseline (
                                        parameter_value_list, PARAMETERS, PARAM, BASELINE, result,
                                        exp_params, baseline_result, MEASURES, all_atest_scores)

            #all_atest_scores <- rbind(all_atest_scores,
            #                          compare_all_values_of_parameter_to_baseline (
            #                            parameter_value_list, PARAMETERS, PARAM, BASELINE, result,
            #                            exp_params, baseline_result, MEASURES, all_atest_scores))

          }

          # label the scores
          colnames(all_atest_scores) <- generate_a_test_results_header(t(PARAMETERS),MEASURES)

          # Write out the result
          write_data_to_csv (all_atest_scores, file.path(FILEPATH,ATESTRESULTFILENAME))
        } else {
          message("No results in the CSV file for simulation at specified baseline
              values. No analysis performed")
        }
      } else {
        oat_csv_result_file_analysis_overTime(
          FILEPATH, CSV_FILE_NAME, PARAMETERS, BASELINE, MEASURES,
          ATESTRESULTFILENAME, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS,
          TIMEPOINTSCALE)
      }
    #}
}


#' Performs a robustness analysis for simulation results stored in a database, comparing simulation behaviour at different parameter values
#'
#' This method takes results mined from a database (like spartanDB produces)
#' and analyses the impact that a change in a single parameter value has had
#' on simulation response. This is performed by comparing the distribution of
#' responses for a perturbed parameter condition with the distribution under
#' baseline/calibrated conditions. This produces a A-Test statistics that are
#' returned for storing in the results database by the spartanDB package.
#'
#' @param db_results Set of experimental results from a mysql database
#' @param parameters Simulation parameters of interest
#' @param baseline Baseline/Calibrated values for each of those parameters
#' @param measures Simulation output responses
#' @param PMIN Minimum value of each of the parameters in sampling
#' @param PMAX Maximum valuer of each of the parameters in sampling
#' @param PINC Increment value applied in sampling
#' @return A-Test scores for all parameter values and measure pairings
#'
#' @export
oat_csv_result_file_analysis_from_DB <- function(db_results, parameters, baseline,
                                                 measures,PMIN,PMAX,PINC)
{
  # Subset the database-mined results at baseline values
  # Here we have to be careful, as we could get multiple responses for the
  # baseline if multiple results in the database (multiples were not found in
  # original spartan)
  baseline_result <- subset_results_by_param_value_set(parameters, db_results,
                                                       baseline)

  if(nrow(baseline_result) > 0)
  {
    # Check this does not have more than one parameter of interest
    if(apply(cbind(baseline_result$paramOfInterest),2,function(x) length(unique(x)))>1)
    {
      # Only take the baseline for one parameter - the rest will be duplicates
      baseline_result<-subset(baseline_result,baseline_result$paramOfInterest==parameters[1])
    }

    all_atest_scores <- perform_aTest_for_all_sim_measures(
      baseline, baseline_result, baseline_result, measures)

    # Now process each parameter
    for (p in 1:length(parameters))
    {
      # Exp_params is set as baseline, then the value of the parameter
      # being analysed is adjusted, thus we have a set of parameters with
      # which we can subset the result file
      exp_params <- as.character(baseline)

      # List of parameter values for this parameter
      parameter_value_list <- as.numeric(
        prepare_parameter_value_list(PMIN, PMAX, PINC, NULL, p))

      # Now iterate through the values in this list
      #all_atest_scores <- rbind(all_atest_scores,
      #                          compare_all_values_of_parameter_to_baseline (
      #                            parameter_value_list, parameters, p, baseline, db_results,
      #                            exp_params, baseline_result, measures, all_atest_scores))

      all_atest_scores <- compare_all_values_of_parameter_to_baseline (
                                  parameter_value_list, parameters, p, baseline, db_results,
                                  exp_params, baseline_result, measures, all_atest_scores)
    }

    # label the scores
    colnames(all_atest_scores) <- generate_a_test_results_header(t(parameters),measures)

    return(all_atest_scores)
  } else {
    message("No results in the CSV file for simulation at specified baseline
              values. No analysis performed")
  }
}


#' Pre-process analysis settings if multiple timepoints are being considered
#'
#' @inheritParams oat_csv_result_file_analysis
oat_csv_result_file_analysis_overTime <- function(FILEPATH, CSV_FILE_NAME, PARAMETERS,
                                         BASELINE, MEASURES,
                                         ATESTRESULTFILENAME,
                                         PMIN = NULL, PMAX = NULL, PINC = NULL,
                                         PARAMVALS = NULL, TIMEPOINTS,
                                         TIMEPOINTSCALE)
{
  # Process each timepoint, amending filenames and calling above function
  for (n in 1:length(TIMEPOINTS)) {

    current_time <- TIMEPOINTS[n]
    message(paste("Processing Timepoint: ", current_time, sep = ""))

    csvfilename_full <- append_time_to_argument(
      CSV_FILE_NAME, current_time,
      check_file_extension(CSV_FILE_NAME))

    atestresultfilename_full <- append_time_to_argument(
      ATESTRESULTFILENAME, current_time,
      check_file_extension(ATESTRESULTFILENAME))


    oat_csv_result_file_analysis(FILEPATH, csvfilename_full, PARAMETERS,
                                 BASELINE, MEASURES,
                                 atestresultfilename_full, PMIN, PMAX,
                                 PINC, PARAMVALS, TIMEPOINTS = NULL,
                                 TIMEPOINTSCALE = NULL, check_done=TRUE)
  }
}

#' For one parameter, compare responses for all values with those at baseline
#' @param parameter_value_list List of values for this parameter
#' @param parameters Simulation parameters
#' @param parameter_index Index of the current parameter being analysed
#' @param baseline Baseline values of all parameters
#' @param result Simulation results, read in from CSV file, to subset
#' @param exp_params Current parameter set being analysed
#' @param baseline_result Simulation result under baseline conditions
#' @param measures Simulation output measures
#' @param all_atest_scores Store of output of this analysis, to append to
#' @return all_atest_scores with resuls for this parameter appended
compare_all_values_of_parameter_to_baseline <- function(
  parameter_value_list, parameters, parameter_index, baseline, result,
  exp_params, baseline_result, measures, all_atest_scores) {

  for (param_val in 1:length(parameter_value_list)) {
    message(paste("Processing Parameter: ", parameters[parameter_index], " Value: ",
                  parameter_value_list[param_val], sep = ""))

    # Baseline already processed, so ignore
    if (parameter_value_list[param_val] != baseline[parameter_index]) {

      # Set value in param list to that we are processing
      exp_params[parameter_index] <- as.character(parameter_value_list[param_val])

      parameter_result <- subset_results_by_param_value_set(parameters,
                                                            result, exp_params)

      score <- calculate_atest_score(
        parameter_result, exp_params,baseline_result, measures,
        parameters[parameter_index], parameter_value_list[param_val])

      if(ncol(score)>0)
        all_atest_scores <- rbind(all_atest_scores,score)
    }
  }
  return(all_atest_scores)
}

#' Calculate the A-Test score for a parameter set in comparison with baseline
#' @param parameter_result Simulation results under current parameter set
#' @param exp_params Parameter set that led to this behaviour
#' @param baseline_result Results obtained at baseline conditions
#' @param measures Simulation response measures
#' @param parameter Name of the parameter being processed
#' @param value Value of the parameter being processsed
#' @return A-Test scores, or NULL if result not found
calculate_atest_score <- function(parameter_result, exp_params,
                                  baseline_result, measures, parameter, value)
{
  # Need to do the bind back in the original calling function now
  if (nrow(parameter_result) > 0) {
    return(perform_aTest_for_all_sim_measures(
      exp_params, baseline_result, parameter_result, measures))
  } else {
    message(paste("No Results for Parameter ", parameter,
                " Value: ", value,
                ". No A-Test Calculated", sep = ""))
    return(NULL)
  }
}
