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
                                    ALTERNATIVEFILENAME, OUTPUTCOLSTART,
                                    OUTPUTCOLEND, CSV_FILE_NAME,
                                    BASELINE, PMIN = NULL, PMAX = NULL,
                                    PINC = NULL, PARAMVALS = NULL,
                                    TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL) {

  # CREATE THE MEDIAN DISTRIBUTION OVER THE SET OF RUNS FOR EACH PARAMETER SET,
  # FOR EACH PARAMETER (AS THIS IS ONE AT A TIME)
  # SPARTAN VERSION 2: THESE MEDIANS ARE NOW STORED IN ONE SINGLE FILE, NOT
  # PER PARAMETER VALUE AS IN SPARTAN 1.0-1.3.
  # LATER PROCESSING NOW DEALS WITH THESE FILES ONLY.
  # NOTE FROM SPARTAN 2, THIS FILE CAN ONLY BE A CSV FILE - XML FILES OF THIS
  # SIZE TAKE A LARGE AMOUNT OF TIME TO PROCESS

  if (is.null(TIMEPOINTS)) {
    if (file.exists(FILEPATH)) {
      print("Generating Median Response File (oat_processParamSubsets)")

      # NOW ALL THE MEDIANS ARE HELD TOGETHER, ACCOMPANIED BY THEIR
      # SIMULATION PARAMETERS BEING ANALYSED
      ALL_MEDIAN_RESULTS <- NULL

      # THE WAY WE DO OUR SIMULATIONS, WITH THE OLD FOLDER STRUCTURE, THE
      # BASELINE WILL BE SIMULATED FOR EACH PARAMETER IN ROBUSTNESS ANALYSIS.
      # THERE IS NO NEED TO PERFORM THE ANALYSIS AGAIN AND AGAIN (AS WOULD
      # HAPPEN WHEN WE INCREMENT THROUGH THE VALUES. THUS, WE DO THE
      # BASELINE WHEN FIRST DETECTED, AND IGNORE IT LATER. THIS FLAG ENSURES
      # THIS HAPPENS
      BASELINEFLAG <- 0

      for (PARAM in 1:length(PARAMETERS)) {
        EXP_PARAMS <- as.character(BASELINE)

        # SET THE VALUE TO ITS LOWEST LIMIT
        PARAMVAL <- PMIN[PARAM]

        if (file.exists(paste(FILEPATH, "/", PARAMETERS[PARAM], sep = ""))) {
          # NOW WE CAN WORK WITH INCREMENTS BETWEEN MAX AND MIN, AND SPECIFIED
          # VALUES, WE NEED TO GET THE VALUES OF THE PARAMETERS WE'RE ANALYSING
          # NOTE CONVERSION TO NUMBERS - GETS RID OF TRAILING ZEROS MADE BY SEQ
          PARAM_VAL_LIST <-
            as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                    PARAMVALS, PARAM))

          # NOW WE ITERATE THROUGH THE VALUES IN THIS LIST
          for (PARAMVAL in 1:length(PARAM_VAL_LIST)) {
            # SET THE VALUE OF THE PARAMETERS BEING EXAMINED TO INCLUDE THE
            # CURRENT VALUE OF THE PARAMETER
            EXP_PARAMS[PARAM] <- as.character(PARAM_VAL_LIST[PARAMVAL])

            if (PARAM_VAL_LIST[PARAMVAL] != BASELINE[PARAM] ||
                BASELINEFLAG == 0) {

              if (file.exists(paste(FILEPATH, "/", PARAMETERS[PARAM], "/",
                                    toString(PARAM_VAL_LIST[PARAMVAL]),
                                    sep = ""))) {

                print(paste("Generating Median Results for Parameter: ",
                            PARAMETERS[PARAM], ", Value: ",
                            PARAM_VAL_LIST[PARAMVAL], sep = ""))

                # CREATE THE START OF THE FILE ADDRESS WHERE THE RESULTS
                #FOR THIS PARAMETER ARE
                SAMPLEFILEPATH <- paste(FILEPATH, "/", PARAMETERS[PARAM], "/",
                                        toString(PARAM_VAL_LIST[PARAMVAL]),
                                        "/", sep = "")

                # NOW CALL THE MEDIAN FUNCTIONS METHOD TO GET THE
                # DISTRIBUTION FOR THIS SET OF RUNS
                MEDIAN_RESULTS <- getMediansSubset(SAMPLEFILEPATH,
                                                   NUMRUNSPERSAMPLE, MEASURES,
                                                   RESULTFILENAME,
                                                   ALTERNATIVEFILENAME,
                                                   OUTPUTCOLSTART,
                                                   OUTPUTCOLEND)

                # NOW WE NEED TO REPLICATE THE PARAMETER CONDITIONS THESE WERE
                # OBTAINED UNDER FOR THE NUMBER OF MEDIAN RESULTS
                PARAMS <- NULL
                for (p in 1:length(EXP_PARAMS)) {
                  PARAMS <- cbind(PARAMS, array(as.numeric(EXP_PARAMS[p]),
                                                dim = c(nrow(MEDIAN_RESULTS))))

                }
                # NOW BIND THESE TO THE RESULTS
                PARAMRESULT <- cbind(PARAMS, MEDIAN_RESULTS)
                # NOW ADD THIS TO THE LIST OF ALL MEDIANS BEING PROCESSED
                # IN THIS ANALYSIS
                ALL_MEDIAN_RESULTS <- rbind(ALL_MEDIAN_RESULTS, PARAMRESULT)
              } else {
                print(paste("No results can be found for parameter: ",
                            PARAMETERS[PARAM], " Value: ", PARAMVAL, sep = ""))
              }

              if (PARAM_VAL_LIST[PARAMVAL] == BASELINE[PARAM]) {
                BASELINEFLAG <- 1
              }
            }
          }
        } else {
          print(paste("No results can be found for the parameter specified: ",
                      PARAMETERS[PARAM], sep = ""))
        }
      }

      # NOW OUTPUT ALL THE MEDIAN RESULTS TO THE SPECIFIED FILEPATH
      colnames(ALL_MEDIAN_RESULTS) <- c(PARAMETERS, MEASURES)

      # OUTPUT IF THE RESULTS ARE NOT BLANK
      if (!is.null(ALL_MEDIAN_RESULTS)) {
        RESULTSFILE <- paste(FILEPATH, "/", CSV_FILE_NAME, sep = "")
        print(paste("Writing Median Results to CSV File: ", RESULTSFILE,
                    sep = ""))
        write.csv(ALL_MEDIAN_RESULTS, RESULTSFILE, quote = FALSE,
                  row.names = FALSE)
      }
    } else {
      print("The directory specified in FILEPATH does not exist.
            No analysis completed")
    }
    } else {
      # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
      for (n in 1:length(TIMEPOINTS)) {

        current_time <- TIMEPOINTS[n]
        print(paste("PROCESSING TIMEPOINT: ", current_time, sep = ""))

        RESULTFILEFORMAT <- check_file_extension(RESULTFILENAME)
        SIMRESULTFILENAME <- paste(substr(RESULTFILENAME, 0,
                                          nchar(RESULTFILENAME) - 4),
                                   "_", current_time, ".",
                                   RESULTFILEFORMAT, sep = "")

        if (!is.null(ALTERNATIVEFILENAME))
          ALTERNATIVEFILENAMEFULL <- paste(substr(ALTERNATIVEFILENAME, 0,
                                                  nchar(ALTERNATIVEFILENAME) - 4),
                                           "_", current_time, ".",
                                           RESULTFILEFORMAT, sep = "")
        else
          ALTERNATIVEFILENAMEFULL <- ALTERNATIVEFILENAME


        CSV_FILE_NAMEFORMAT <- substr(CSV_FILE_NAME,
                                      (nchar(CSV_FILE_NAME) + 1) - 3,
                                      nchar(CSV_FILE_NAME))

        CSV_FILE_NAMEFULL <- paste(substr(CSV_FILE_NAME, 0,
                                          nchar(CSV_FILE_NAME) - 4),
                                   "_", current_time, ".", CSV_FILE_NAMEFORMAT,
                                   sep = "")

        # NOW CALL THIS FUNCTION AGAIN TO DO THE TIMEPOINTS - WE SET THE
        # TIMEPOINTS AND TIMEPOINTSCALE TO NULL NOW SO WE DONT END UP BACK
        # IN THIS ELSE

        oat_processParamSubsets(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE, MEASURES,
                                SIMRESULTFILENAME, ALTERNATIVEFILENAMEFULL,
                                OUTPUTCOLSTART, OUTPUTCOLEND, CSV_FILE_NAMEFULL,
                                BASELINE, PMIN, PMAX, PINC, PARAMVALS, NULL, NULL)
      }
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


