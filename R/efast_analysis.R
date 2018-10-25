#' Generates summary file for stochastic simulations stored in multiple files
#'
#' Only to be applied in cases where simulation responses are supplied in the
#' folder structure shown in the R Journal paper, useful for cases where the
#' simulation is agent-based. Iterates through the folder structure
#' analysing the result of each replicate run under the same parameter
#' conditions, creating a CSV file for each curve/parameter pair. This will
#' hold the parameters of the run and the median of each simulation response
#' for that run. As stated earlier, more than one run result can exist in this
#' file. Where a simulation is being analysed for multiple timepoints,
#' this will iterate through the results at all timepoints, creating
#' curve/parameter pair CSV files for all specified timepoints.
#'
#' @param FILEPATH Directory where the simulation runs can be found, in folders
#'  or in CSV file format
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated
#' @param NUMSAMPLES The number of parameter subsets that were generated in
#' the eFAST design
#' @param NUMRUNSPERSAMPLE The number of runs performed for each parameter
#' subset. This figure can be generated through Aleatory Analysis
#' @param MEASURES Array containing the names of the output measures which
#' are used to analyse the simulation
#' @param RESULTFILENAME {Name of the simulation results file. In the current
#' version, XML and CSV files can be processed. If performing this analysis
#' over multiple timepoints, it is assumed that the timepoint follows the
#' file name, e.g. trackedCells_Close_12.csv.}
#' @param ALTERNATIVEFILENAME In some cases, it may be relevant to read from
#' a further results file if the initial file contains no results. This
#' filename is set here. In the current version, XML and CSV files can be
#' processed.
#' @param OUTPUTCOLSTART Column number in the simulation results file where
#' output begins - saves (a) reading in unnecessary data, and (b) errors
#' where the first column is a label, and therefore could contain duplicates.
#' @param OUTPUTCOLEND Column number in the simulation results file where the
#' last output measure is. Only required if running the first method.
#' @param TIMEPOINTS Implemented so this method can be used when analysing
#' multiple simulation timepoints. If only analysing one timepoint, this
#' should be set to NULL. If not, this should be an array of timepoints,
#' e.g. c(12,36,48,60)
#' @param TIMEPOINTSCALE Sets the scale of the timepoints being analysed,
#' e.g. "Hours"
#' @param check_done If multiple timepoints, whether the input has been checked
#' @param current_time If multiple timepoints, the current timepoint being processed
#'
#' @export
efast_generate_medians_for_all_parameter_subsets  <-
  function(FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, NUMRUNSPERSAMPLE,
           MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
           OUTPUTCOLEND, TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL,
           check_done = FALSE, current_time = NULL) {

  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:
  if(check_input_args(input_check$names, input_check$arguments)) {

    if (is.null(TIMEPOINTS))  {

      message("Generating Simulation Median Response Sets for eFAST")

      for (CURVE in 1:NUMCURVES) {
        # Now look at this parameter of interest
        for (PARAM in 1:length(PARAMETERS)) {
          message(paste("Generating Summary Results for Curve ", CURVE, " Parameter: ", PARAM, sep=""))

          # Open the parameter file
          params <- read_from_csv(file.path(FILEPATH, paste("Curve",CURVE,"_Param",PARAM,".csv",sep="")))

          # Can use the LHC function to summarise the responses
          curve_param_result <- summarise_lhc_sweep_responses(
            file.path(FILEPATH,CURVE,PARAM), NUMRUNSPERSAMPLE, PARAMETERS, MEASURES,
            RESULTFILENAME, ALTERNATIVEFILENAME, NUMSAMPLES, params, OUTPUTCOLSTART, OUTPUTCOLEND)

          # Write this file out to the FILEPATH
          if(is.null(current_time))
            write_data_to_csv(curve_param_result, file.path(
              FILEPATH, paste("Curve", CURVE, "_Parameter", PARAM, "_Results.csv", sep = "")))
          else
            write_data_to_csv(curve_param_result, file.path(
              FILEPATH, paste("Curve", CURVE, "_Parameter", PARAM, "_Results_",current_time,".csv", sep = "")))
        }
      }
    } else {
    efast_generate_medians_for_all_parameter_subsets_overTime(
      FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, NUMRUNSPERSAMPLE,
      MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
      OUTPUTCOLEND, TIMEPOINTS, TIMEPOINTSCALE)
    }
  }
  }

#' Pre-process analysis settings if multiple timepoints are being considered
#'
#' @inheritParams efast_generate_medians_for_all_parameter_subsets
efast_generate_medians_for_all_parameter_subsets_overTime <-
  function(FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, NUMRUNSPERSAMPLE,
           MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
           OUTPUTCOLEND, TIMEPOINTS, TIMEPOINTSCALE) {

    # Process each timepoint
    for (n in 1:length(TIMEPOINTS)) {

      current_time <- TIMEPOINTS[n]
      message(paste("Processing Timepoint:", current_time, sep=" "))

      simresultfilename <- append_time_to_argument(
        RESULTFILENAME, current_time,
        check_file_extension(RESULTFILENAME))

      altfilename_full <- NULL
      if (!is.null(ALTERNATIVEFILENAME))
        altfilename_full <- append_time_to_argument(
          ALTERNATIVEFILENAME, current_time,
          check_file_extension(ALTERNATIVEFILENAME))

      efast_generate_medians_for_all_parameter_subsets(
        FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES,NUMRUNSPERSAMPLE, MEASURES,
        simresultfilename, altfilename_full, OUTPUTCOLSTART, OUTPUTCOLEND,
        TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL, check_done = TRUE,
        current_time = current_time)
    }
  }

#' Calculates the summary stats for each parameter set (median of any
#' replicates)
#'
#' This method produces a summary of the results for a particular resampling
#' curve.  This shows, for each parameter of interest, the median of each
#' simulation output measure for each of the 65 parameter value sets generated.
#' Here's an example. We examine resampling curve 1, and firstly examine
#' parameter 1. For this parameter of interest, a number of different parameter
#' value sets were generated from the frequency curves (lets say 65), thus we
#' have 65 different sets of simulation results. The method
#' \code{efast_generate_medians_for_all_parameter_subsets} produced a summary
#' showing the median of each output measure for each run. Now, this method
#' calculates the median of these medians, for each output measure, and stores
#' these in the summary. Thus, for each parameter of interest, the medians of
#' each of the 65 sets of results are stored. The next parameter is then
#' examined, until all have been analysed. This produces a snapshot showing
#' the median simulation output for all parameter value sets generated for
#' the first resample curve. These are stored with the file name
#' Curve[Number]_Results_Summary in the directory specified in FILEPATH.
#' Again this can be done recursively for a number of timepoints if required.
#'
#' @inheritParams efast_generate_medians_for_all_parameter_subsets
#'
#' @export
efast_get_overall_medians  <-  function(
  FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, MEASURES, TIMEPOINTS=NULL,
  TIMEPOINTSCALE=NULL, current_time=NULL, check_done=FALSE) {

  # Again we can use the LHC methods here not rewrite, once input checked
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:
  if(check_input_args(input_check$names, input_check$arguments)) {

    if (is.null(TIMEPOINTS)) {
      message("Calculating overall medians responses for each parameter set (efast_get_overall_medians)")

      results_file_end <- "_Results.csv"
      output_file_end <- "_Results_Summary.csv"
      if(!is.null(current_time))
      {
        results_file_end <- paste("_Results_",current_time,".csv",sep="")
        output_file_end <- paste("_Results_Summary_",current_time,".csv",sep="")
      }

      for (CURVE in 1:NUMCURVES) {

        message(paste("Generating results summary for Curve ", CURVE, sep = ""))

        summary_table <- NULL

        for (PARAM in 1:length(PARAMETERS)) {

          # Read result
          sim_responses <- read_from_csv(
            file.path(FILEPATH, paste("Curve", CURVE, "_Parameter", PARAM,
                                      results_file_end, sep = "")))

          # Summarise
          param_results <- summarise_replicate_runs(sim_responses, PARAMETERS, MEASURES, bind_params=FALSE)

          column_names <- NULL
          for (l in 1:length(MEASURES)) {
              column_names <- c(column_names,paste(PARAMETERS[PARAM],"_Median",MEASURES[l],sep=""))
          }
          colnames(param_results) <- column_names

          # NOW TO BIND THIS COLUMN ONTO THE RESULTS FOR ALL PARAMETERS
          summary_table <- cbind(summary_table, param_results)
        }

        # Write curve result to file
        write_data_to_csv(summary_table, file.path(
          FILEPATH,paste("Curve", CURVE, output_file_end , sep = "")))

        message(paste("eFAST Summary file output to ", file.path(
          FILEPATH,paste("Curve", CURVE, output_file_end , sep = "")), sep = ""))

      }
    } else {

      efast_get_overall_medians_overTime(
        FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, MEASURES, TIMEPOINTS,
        TIMEPOINTSCALE)

    }
  }
}

#' Pre-process analysis settings if multiple timepoints are being considered
#'
#' @inheritParams efast_get_overall_medians
efast_get_overall_medians_overTime  <-  function(
    FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, MEASURES, TIMEPOINTS,
    TIMEPOINTSCALE) {

  for (n in 1:length(TIMEPOINTS)) {
    current_time <- TIMEPOINTS[n]
    message(join_strings(c("Processing Timepoint:", current_time), " "))

    efast_get_overall_medians(FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES,
                              MEASURES, TIMEPOINTS = NULL,
                              TIMEPOINTSCALE = NULL, current_time = current_time,
                              check_done=TRUE)

  }
}


#' Runs the eFAST Analysis for the pre-generated summary file
#'
#' Produces a file summarising the analysis; partitioning the variance between
#' parameters and providing relevant statistics. These include, for each
#' parameter of interest, first-order sensitivity index (Si), total-order
#' sensitivity index (STi), complementary parameters sensitivity index (SCi),
#' and relevant p-values and error bar data calculated using a two-sample
#' t-test and standard error respectively. For a more detailed examination of
#'  this analysis, see the references in the R Journal paper. For ease of
#'  representation, the method also produces a graph showing this data for
#'  each simulation output measure. These graphs and summaries can be produced
#'  for multiple timepoints.
#'
#' @param FILEPATH Directory where the simulation runs can be found, in folders
#' or in CSV file format
#' @param MEASURES Array containing the names of the output measures which
#' are used to analyse the simulation
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples have been generated
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param NUMSAMPLES The number of parameter subsets that were generated in
#' the eFAST design
#' @param OUTPUTMEASURES_TO_TTEST Which measures in the range should be tested
#' to see if the result is statistically significant.  To do all, and if
#' there were 3 measures, this would be set to 1:3
#' @param TTEST_CONF_INT The level of significance to use for the T-Test
#' (e.g. 0.95)
#' @param GRAPH_FLAG Whether graphs should be produced summarising the output
#' - should be TRUE or FALSE
#' @param EFASTRESULTFILENAME File name under which the full eFAST analysis
#' should be stored. This will contain the partitioning of variance for each
#' parameter.
#' @param GRAPHTIME Value never needs stating, used internally to produce
#' graphs when processing multiple timepoints
#' @param TIMEPOINTS mplemented so this method can be used when analysing
#' multiple simulation timepoints. If only analysing one timepoint, this
#' should be set to NULL. If not, this should be an array of timepoints,
#' e.g. c(12,36,48,60)
#' @param TIMEPOINTSCALE Sets the scale of the timepoints being analysed,
#' e.g. "Hours"
#' @param check_done If using multiple timepoints, whether data entry has been
#' checked
#' @param output_types Files types of graph to produce (pdf,png,bmp etc)
#' @param csv_file_curve_summary Whether curve summaries are provided in
#' multiple CSV files (as in traditional spartan), or as an R object (as in
#' spartanDB)
#' @param write_csv_file_out Whether the analysis should output a CSV file
#' (TRUE) or return an R object (FALSE)
#' @param CURVE_SUMMARY If providing an R object (csv_file_curve_summary=FALSE),
#' this is the R object containing those summaries
#' @return Either NULL if a CSV file is returned, or an R object containing the
#' results of this analysis
#' @export
#'
efast_run_Analysis  <-  function(
  FILEPATH, MEASURES, PARAMETERS, NUMCURVES, NUMSAMPLES, OUTPUTMEASURES_TO_TTEST,
  TTEST_CONF_INT, GRAPH_FLAG, EFASTRESULTFILENAME, TIMEPOINTS = NULL,
  TIMEPOINTSCALE = NULL, GRAPHTIME = NULL, check_done=FALSE, output_types=c("pdf"),
  csv_file_curve_summary = TRUE, write_csv_file_out = TRUE, CURVE_SUMMARY=NULL) {

  #input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:
  #if(check_input_args(input_check$names, input_check$arguments)) {

    if (is.null(TIMEPOINTS)) {
      # maximum number of fourier coefficients that may be retained in
      # calculating the partial variances without interferences between the
      # assigned frequencies
      MI <- 4
      # wanted no. of sample points
      wanted_n <- NUMSAMPLES * length(PARAMETERS) * NUMCURVES
      # OMI changed October 2018 to accommodate greater number of parameters
      omi <- floor( ( (wanted_n / NUMCURVES) - 1) / (2 * MI) / length(PARAMETERS))

      # In sampling, this change caused undesirable effects (below), so changed back
      # omi <- floor(((wanted_n / NUMCURVES) - 1) / (2 * MI) / (length(PARAMETERS)/3))

      message("Producing eFAST Analysis (efast_run_analysis)")

      if(csv_file_curve_summary)
      {
        efast_sim_results <- read_all_curve_results(FILEPATH, GRAPHTIME, NUMCURVES,
                                                  NUMSAMPLES,  MEASURES, PARAMETERS)
      } else {

        efast_sim_results <- build_curve_results_from_r_object(CURVE_SUMMARY, NUMCURVES, NUMSAMPLES,
                                                  MEASURES, PARAMETERS)
      }

      # Sensitivity Indexes
      sensitivities <- generate_sensitivity_indices(efast_sim_results, omi, MI,
                                                      MEASURES, PARAMETERS, NUMCURVES)

      # T-Test to get P-Values against dummy parameter
      message("Generating measures of statistical significance")
      t_tests  <-  efast_ttest(sensitivities$si, sensitivities$range_si,
                               sensitivities$sti, sensitivities$range_sti,
                                 OUTPUTMEASURES_TO_TTEST, length(PARAMETERS),
                                 NUMCURVES, TTEST_CONF_INT)

      formatted_results <- format_efast_result_for_output(
        sensitivities, t_tests, OUTPUTMEASURES_TO_TTEST, MEASURES, PARAMETERS)

      # Output
      if(write_csv_file_out)
      {
        write_data_to_csv(formatted_results,file.path(FILEPATH, EFASTRESULTFILENAME), row_names=TRUE)

        message(paste("eFAST Results file generated. Output to",
                    file.path(FILEPATH, EFASTRESULTFILENAME)))
      }
      else
      {
        return(formatted_results)
        message(paste("eFAST Results generated and returned as R object"))
      }

        # GRAPH THE RESULTS IF REQUIRED
        if (GRAPH_FLAG)
          efast_graph_Results(
            FILEPATH, PARAMETERS, sensitivities$si, sensitivities$sti, sensitivities$errors_si,
            sensitivities$errors_sti, MEASURES, GRAPHTIME, TIMEPOINTSCALE, output_types)


    } else {
      efast_run_Analysis_overTime(
        FILEPATH, MEASURES, PARAMETERS, NUMCURVES, NUMSAMPLES, OUTPUTMEASURES_TO_TTEST,
        TTEST_CONF_INT, GRAPH_FLAG, EFASTRESULTFILENAME, TIMEPOINTS, TIMEPOINTSCALE, output_types)

    }
  #}
}

#' Runs the eFAST Analysis for a set of results stored in a database
#'
#' Produces a file summarising the analysis; partitioning the variance between
#' parameters and providing relevant statistics. These include, for each
#' parameter of interest, first-order sensitivity index (Si), total-order
#' sensitivity index (STi), complementary parameters sensitivity index (SCi),
#' and relevant p-values and error bar data calculated using a two-sample
#' t-test and standard error respectively. For a more detailed examination of
#'  this analysis, see the references in the R Journal paper. For ease of
#'  representation, the method also produces a graph showing this data for
#'  each simulation output measure. In this case, this function works with
#'  spartanDB to analyse data stored in a database
#'
#' @param efast_sim_results Set of simulation results mined from the database,
#' put into the format required by spartan
#' @param number_samples Number of samples taken per parameter
#' @param parameters Array containing the names of the parameters of which
#' parameter samples have been generated
#' @param number_curves The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param measures Array containing the names of the output measures which
#' are used to analyse the simulation
#' @param OUTPUTMEASURES_TO_TTEST Which measures in the range should be tested
#' to see if the result is statistically significant.  To do all, and if
#' there were 3 measures, this would be set to 1:3
#' @param TTEST_CONF_INT The level of significance to use for the T-Test
#' (e.g. 0.95)
#'
#' @export
efast_run_Analysis_from_DB<-function(efast_sim_results, number_samples, parameters,
                                     number_curves, measures,
                                     OUTPUTMEASURES_TO_TTEST=1:length(measures),
                                     TTEST_CONF_INT=0.95)
{
  MI <- 4
  # wanted no. of sample points
  wanted_n <- number_samples * length(parameters) * number_curves
  omi <- floor( ( (wanted_n / number_curves) - 1) / (2 * MI) / length(parameters))

  message("Producing eFAST Analysis (efast_run_analysis)")

  # Sensitivity Indexes
  sensitivities <- generate_sensitivity_indices(efast_sim_results, omi, MI,
                                                measures, parameters, number_curves)

  # T-Test to get P-Values against dummy parameter
  message("Generating measures of statistical significance")
  t_tests  <-  efast_ttest(sensitivities$si, sensitivities$range_si,
                           sensitivities$sti, sensitivities$range_sti,
                           OUTPUTMEASURES_TO_TTEST, length(parameters),
                           number_curves, TTEST_CONF_INT)

  formatted_results <- format_efast_result_for_output(
    sensitivities, t_tests, OUTPUTMEASURES_TO_TTEST, measures,parameters)

  return(formatted_results)


}

#' Pre-process analysis settings if multiple timepoints are being considered
#'
#' @inheritParams efast_run_Analysis
efast_run_Analysis_overTime  <- function(
  FILEPATH, MEASURES, PARAMETERS, NUMCURVES, NUMSAMPLES, OUTPUTMEASURES_TO_TTEST,
  TTEST_CONF_INT, GRAPH_FLAG, EFASTRESULTFILENAME, TIMEPOINTS, TIMEPOINTSCALE,
  GRAPHTIME = NULL) {

  for (n in 1:length(TIMEPOINTS)) {
    current_time <- TIMEPOINTS[n]
    message(paste("Processing Timepoint: ", current_time, sep = ""))

    efastresultfile_full <- append_time_to_argument(
      EFASTRESULTFILENAME, current_time,
      check_file_extension(EFASTRESULTFILENAME))

    efast_run_Analysis(
      FILEPATH, MEASURES, PARAMETERS, NUMCURVES, NUMSAMPLES, OUTPUTMEASURES_TO_TTEST,
      TTEST_CONF_INT, GRAPH_FLAG, efastresultfile_full, TIMEPOINTS = NULL,
      TIMEPOINTSCALE, GRAPHTIME = current_time, check_done=TRUE)
  }
}

#' Reads results from each curve into a multi-dimensional array
#' @param FILEPATH Filepath where simulation results can be found
#' @param GRAPHTIME Timepoint graph is being produced
#' @param NUMCURVES Number of resample curves used in this analysis
#' @param NUMSAMPLES Number of samples taken from each curve
#' @param MEASURES Simulation output measures of interest
#' @param PARAMETERS Simulation parameters of interest
#' @return R matrix containing the curve summaries in the format spartan requires
read_all_curve_results <- function(FILEPATH, GRAPHTIME, NUMCURVES, NUMSAMPLES,
                                   MEASURES, PARAMETERS) {
  # read in curve 1
  results <- read_from_csv(construct_result_filename(FILEPATH, "Curve1_Results_Summary.csv",
                                                     timepoint=GRAPHTIME))
  # Now read in any further curves
  if (NUMCURVES > 1) {
    for (CURVE in 2:NUMCURVES) {
      curve_results_file <- construct_result_filename(
        FILEPATH, paste("Curve",CURVE,"_Results_Summary.csv",sep=""), timepoint=GRAPHTIME)

      if (file.exists(curve_results_file)) {
        curve_results <- read_from_csv(curve_results_file)
        # Add to previous curves:
        results <- cbind(results, curve_results[1:length(curve_results)])
      } else {
        message(paste("No summary file for Curve ", CURVE, sep = ""))
      }
    }
  }

  # Convert to multi-dimensional array for ease of processing later
  # Each dimension contains one curve
  results <- as.matrix(results)
  return(array(results, dim = c(NUMSAMPLES, (length(PARAMETERS) * length(MEASURES)),
                                NUMCURVES)))

}

#' When developing spartanDB, it became clear curve results may not be in separate
#' files but in one R object. This takes an R object containing curve summaries and
#' builds these into the format spartan requires to perform the analysis
#' @param CURVE_SUMMARY R object containing summaries for all curves
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param NUMSAMPLES The number of parameter subsets that were generated in
#' the eFAST design
#' @param MEASURES Array containing the names of the output measures which
#' are used to analyse the simulation
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples have been generated
#' @return R matrix containing the curve summaries in the format spartan requires
build_curve_results_from_r_object<-function(CURVE_SUMMARY, NUMCURVES, NUMSAMPLES,
                                            MEASURES, PARAMETERS)
{
  results <- CURVE_SUMMARY[[1]]

  if(NUMCURVES > 1) {
    for (CURVE in 2:NUMCURVES) {
      results <- cbind(results, CURVE_SUMMARY[[CURVE]])
    }
  }

  # Convert to multi-dimensional array for ease of processing later
  # Each dimension contains one curve
  results <- as.matrix(results)
  return(array(results, dim = c(NUMSAMPLES, (length(PARAMETERS) * length(MEASURES)),
                                NUMCURVES)))
}


#' Generate eFAST Sensitivity Indices
#' @param results_array Results for all eFAST resample curves
#' @param omi floor( ( (wanted_n / NUMCURVES) - 1) / (2 * MI) / length(PARAMETERS))
#' @param MI maximum number of fourier coefficients, always 4
#' @inheritParams efast_run_Analysis
#' @return List of SI and STI coefficients and error bars
generate_sensitivity_indices <- function(results_array, omi, MI, MEASURES, PARAMETERS, NUMCURVES) {

  message("Generating Sensitivity Indexes")
  result_list <- efast_sd(results_array, omi, MI, length(MEASURES),
                          length(PARAMETERS), NUMCURVES)

  # Calculate coeffiecients of variance
  cv_si_coeff_results <- NULL
  cv_sti_coeff_results <- NULL
  errors_si <- NULL
  errors_sti <- NULL

  for (OUTPUTMEASURE in 1:length(MEASURES)) {
    # efast_cvmethod is within CVmethod.R
    output_measure_cvs  <-  efast_cvmethod(
      result_list$si, result_list$range_si, result_list$sti, result_list$range_sti,
      OUTPUTMEASURE, length(PARAMETERS), NUMCURVES, length(MEASURES))

    cv_si_coeff_results  <-  rbind(cv_si_coeff_results,
                                   output_measure_cvs$cv_si)
    cv_sti_coeff_results  <-  rbind(cv_sti_coeff_results,
                                    output_measure_cvs$cv_sti)
    errors_si  <-  cbind(errors_si, output_measure_cvs$error_si)
    errors_sti  <-  cbind(errors_sti, output_measure_cvs$error_sti)
  }

  # Transpose such that output order is same as rest of results
  # (measures columns, parameters rows)
  cv_si_coeff_results <- t(cv_si_coeff_results)
  cv_sti_coeff_results <- t(cv_sti_coeff_results)

  return(list("si"=result_list$si,"range_si"=result_list$range_si,"sti"=result_list$sti, "range_sti"=result_list$range_sti, "cv_si_coeffs"=cv_si_coeff_results,
              "cv_sti_coeffs"=cv_sti_coeff_results,
              "errors_si"=errors_si,"errors_sti"=errors_sti))
}

#' Joins the various results objects into an output ready format
#' @inheritParams efast_run_Analysis
#' @param efast_analysis_stats Sensitivity indices
#' @param t_tests T-Test scores for output measures
#' @return formatted results for output to csv file
format_efast_result_for_output <- function(efast_analysis_stats, t_tests, OUTPUTMEASURES_TO_TTEST, MEASURES, PARAMETERS) {

  formatted_results <- NULL
  labels <- c("_Si", "_Si_PVal", "_STi", "_STi_PVal", "_SCi",
              "_Si_CoEff_of_Var", "_STi_CoEff_of_Var",
              "_Si_ErrorBar", "_STi_ErrorBar")
  final_labels <- NULL

  for (MEASURE in seq(OUTPUTMEASURES_TO_TTEST)) {
    # OUTPUT FORMAT
    # COLUMNS ORDERED BY MEASURE
    # 5 COLUMNS PER MEASURE: Si, Si P Val, STi, STi P Val, SCi

    measure_results <- cbind(efast_analysis_stats$si[, , MEASURE],
                             t_tests$p_si[, , MEASURE],
                             efast_analysis_stats$sti[, , MEASURE],
                             t_tests$p_sti[, , MEASURE],
                             (1 - efast_analysis_stats$sti[, , MEASURE]),
                             efast_analysis_stats$cv_si_coeffs[, MEASURE],
                             efast_analysis_stats$cv_sti_coeffs[, MEASURE],
                             efast_analysis_stats$errors_si[, MEASURE],
                             efast_analysis_stats$errors_sti[, MEASURE])

    formatted_results <- cbind(formatted_results, measure_results)

    final_labels <- c(final_labels,paste(rep(MEASURES[MEASURE],9),labels,sep=""))
  }

  colnames(formatted_results) <- final_labels
  rownames(formatted_results) <- c(PARAMETERS)

  return(formatted_results)
}

#' Appends the time to an eFAST argument, if processing multiple timepoints
#' @param filepath Working directory
#' @param filename Name of the file to append time to (or not)
#' @param timepoint Timepoint being processed (if any)
#' @return Path to file with timepoint added if necessary
construct_result_filename <- function(filepath, filename, timepoint=NULL)
{
  if(is.null(timepoint))
    return(file.path(filepath,filename))
  else
    return(file.path(filepath,append_time_to_argument(
      filename, timepoint,
      check_file_extension(filename))))
}
