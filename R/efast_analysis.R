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
#'
#' @export
#'
efast_run_Analysis  <-  function(FILEPATH, MEASURES, PARAMETERS, NUMCURVES,
                                 NUMSAMPLES, OUTPUTMEASURES_TO_TTEST,
                                 TTEST_CONF_INT, GRAPH_FLAG,
                                 EFASTRESULTFILENAME, TIMEPOINTS = NULL,
                                 TIMEPOINTSCALE = NULL, GRAPHTIME = NULL) {

  if (is.null(TIMEPOINTS)) {
    if (file.exists(FILEPATH)) {

      NUMPARAMS <- length(PARAMETERS)

      NUMOUTMEASURES <- length(MEASURES)

      # maximum number of fourier coefficients
      # that may be retained in calculating the partial
      # variances without interferences between the
      # assigned frequencies
      MI <- 4
      # wanted no. of sample points
      wanted_n <- NUMSAMPLES * NUMPARAMS * NUMCURVES
      omi <- floor( ( (wanted_n / NUMCURVES) - 1) / (2 * MI) / NUMPARAMS)

      # READ IN THE MEDIAN RESULT SETS

      print("Producing eFAST Analysis (efast_run_analysis)")

      # CONSTRUCT THE FILE NAME,  TAKING ANY TIMEPOINT INTO ACCOUNT
      if (is.null(GRAPHTIME)) {
        CURVE1RESULTSFILENAME <- paste(FILEPATH,
                                       "/Curve1_Results_Summary.csv", sep = "")
      }
      else {
        CURVE1RESULTSFILENAME <- paste(FILEPATH, "/Curve1_", GRAPHTIME,
                                       "_Results_Summary.csv", sep = "")
        print(CURVE1RESULTSFILENAME)
      }


      # READ IN THE FIRST CURVE
      if (file.exists(CURVE1RESULTSFILENAME)) {
        RESULTS <- read.csv(CURVE1RESULTSFILENAME, sep = ",",
                            header = TRUE, check.names = FALSE)

        # NOW READ IN ANY FURTHER CURVES
        if (NUMCURVES > 1) {
          for (CURVE in 2:NUMCURVES) {
            # CONSTRUCT THE FILE NAME
            if (is.null(GRAPHTIME))
              CURVERESULTSFILENAME <- paste(FILEPATH, "/Curve", CURVE,
                                            "_Results_Summary.csv",
                                            sep = "")
            else
              CURVERESULTSFILENAME <- paste(FILEPATH, "/Curve", CURVE,
                                            "_", GRAPHTIME,
                                            "_Results_Summary.csv", sep = "")

            if (file.exists(CURVERESULTSFILENAME)) {
              CURVERESULTS <- read.csv(CURVERESULTSFILENAME, sep = ",",
                                       header = TRUE, check.names = FALSE)
              # NOTE THIS ASSUMES THAT THE RESULTS HAVE BEEN GENERATED WITH
              # THE FIRST COLUMN CONTAINING NO SAMPLE COUNT
              # ADD TO THE PREVIOUS CURVES
              RESULTS <- cbind(RESULTS, CURVERESULTS[1:length(CURVERESULTS)])
            } else {
              print(paste("No summary file for Curve ", CURVE, sep = ""))
            }
          }
        }

        # CONVERT THE RESULTS FILE FOR EASE OF PROCESSING LATER
        RESULTS <- as.matrix(RESULTS)
        # PUT IN MULTI DIMENSIONAL ARRAY
        # EACH DIMENSION CONTAINS ONE CURVE
        RESULTSARRAY <- array(RESULTS, dim = c(NUMSAMPLES,
                                             (NUMPARAMS * NUMOUTMEASURES),
                                             NUMCURVES))

        # NOW GENERATE THE SENSITIVITY INDEXES
        # efast_sd IS WITHIN efast_sd.R
        print("Generating Sensitivity Indexes")
        result_list <- efast_sd(RESULTSARRAY, omi, MI, NUMOUTMEASURES,
                               NUMPARAMS, NUMCURVES)

        # GET THE COEFFICIENTS OF VARIANCE
        cv_si_coeff_results <- NULL
        cv_sti_coeff_results <- NULL
        errors_si <- NULL
        errors_sti <- NULL

        for (OUTPUTMEASURE in 1:NUMOUTMEASURES) {
          # efast_cvmethod is within CVmethod.R
          output_measure_cvs  <-  efast_cvmethod(result_list$si,
                                               result_list$range_si,
                                               result_list$sti,
                                               result_list$range_sti,
                                               OUTPUTMEASURE, NUMPARAMS,
                                               NUMCURVES, NUMOUTMEASURES)

          cv_si_coeff_results  <-  rbind(cv_si_coeff_results,
                                       output_measure_cvs$cv_si)
          cv_sti_coeff_results  <-  rbind(cv_sti_coeff_results,
                                        output_measure_cvs$cv_sti)
          errors_si  <-  cbind(errors_si, output_measure_cvs$error_si)
          errors_sti  <-  cbind(errors_sti, output_measure_cvs$error_sti)
        }

        # TRANSPOSE SO THAT THE OUTPUT ORDERING IS IN THE SAME FORMAT AS THE
        # REST OF THE RESULTS (MEASURES ARE COLUMNS,  PARAMETERS ARE ROWS)
        cv_si_coeff_results <- t(cv_si_coeff_results)
        cv_sti_coeff_results <- t(cv_sti_coeff_results)

        # NOW DO THE T-TEST TO GET THE P-VALUES AGAINST THE DUMMY PARAMETER
        # efast_ttest IS WITHIN efast_ttest.R

        print("Generating measures of statistical significance")
        t_tests  <-  efast_ttest(result_list$si, result_list$range_si,
                                 result_list$sti, result_list$range_sti,
                                 OUTPUTMEASURES_TO_TTEST, NUMPARAMS,
                                 NUMCURVES, TTEST_CONF_INT)

        # NOW GET THE OUTPUT IN A FORMAT WHICH CAN BE OUTPUT TO CSV FILE

        formatted_results <- NULL
        for (MEASURE in seq(OUTPUTMEASURES_TO_TTEST)) {
          # OUTPUT FORMAT
          # COLUMNS ORDERED BY MEASURE
          # 5 COLUMNS PER MEASURE: Si, Si P Val, STi, STi P Val, SCi

          measure_results <- cbind(result_list$si[, , MEASURE],
                                  t_tests$p_si[, , MEASURE],
                                  result_list$sti[, , MEASURE],
                                  t_tests$p_sti[, , MEASURE],
                                  (1 - result_list$sti[, , MEASURE]),
                                  cv_si_coeff_results[, MEASURE],
                                  cv_sti_coeff_results[, MEASURE],
                                  errors_si[, MEASURE], errors_sti[, MEASURE])

          colnames(measure_results) <- c(
            join_strings(c(MEASURES[MEASURE], "_Si"), ""),
            join_strings(c(MEASURES[MEASURE], "_Si_PVal"), ""),
            join_strings(c(MEASURES[MEASURE], "_STi"), ""),
            join_strings(c(MEASURES[MEASURE], "_STi_PVal"), ""),
            join_strings(c(MEASURES[MEASURE], "_SCi"), ""),
            join_strings(c(MEASURES[MEASURE], "_Si_CoEff_of_Var"), ""),
            join_strings(c(MEASURES[MEASURE], "_STi_CoEff_of_Var"), ""),
            join_strings(c(MEASURES[MEASURE], "_Si_ErrorBar"), ""),
            join_strings(c(MEASURES[MEASURE], "_STi_ErrorBar"), ""))

          formatted_results <- cbind(formatted_results, measure_results)
        }

        rownames(formatted_results) <- c(PARAMETERS)

        # OUTPUT THE SUMMARY RESULTS FILES
        # A - FILE WITH THE AMOUNT OF VARIANCE ACCOUNTED FOR BY EACH PARAMETER
        RESULTSFILE <- paste(FILEPATH, "/", EFASTRESULTFILENAME, sep = "")
        write.csv(formatted_results, RESULTSFILE, quote = FALSE)

        print(paste("eFAST Results file generated. Output to ",
                    RESULTSFILE, sep = ""))

        # GRAPH THE RESULTS IF REQUIRED
        if (GRAPH_FLAG) {
          print("Graphing Results")
          efast_graph_Results(FILEPATH, PARAMETERS, result_list$si,
                              result_list$sti, errors_si, errors_sti,
                              MEASURES, GRAPHTIME, TIMEPOINTSCALE)
        }
      } else {
        print("No summary file for Curve 1. Are you sure you have run the
              method to generate it?")
      }
    } else {
      print("The directory specified in FILEPATH does not exist.
            No eFAST Graphs Created")
    }
  } else {
    # PROCESS EACH TIMEPOINT,  AMENDING FILENAMES AND RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {
      current_time <- TIMEPOINTS[n]
      print(paste("PROCESSING TIMEPOINT: ", current_time, sep = ""))

      efast_resultfileformat <- check_file_extension(EFASTRESULTFILENAME)
      EFASTRESULTFILENAME_FULL <- paste(substr(EFASTRESULTFILENAME, 0,
                                               nchar(EFASTRESULTFILENAME) - 4),
                                        "_", current_time, ".",
                                        efast_resultfileformat, sep = "")

      efast_run_Analysis(FILEPATH, MEASURES, PARAMETERS, NUMCURVES, NUMSAMPLES,
                         OUTPUTMEASURES_TO_TTEST, TTEST_CONF_INT, GRAPH_FLAG,
                         EFASTRESULTFILENAME_FULL, TIMEPOINTS = NULL,
                         TIMEPOINTSCALE, GRAPHTIME = current_time)
    }
  }
}
