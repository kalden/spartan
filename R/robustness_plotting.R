#' Takes each parameter in turn and creates a plot showing A-Test score against parameter value.
#'
#' This makes it easy to determine how the effect that changing the parameter
#' has had on simulation results. Graph for each parameter is output as a PDF
#'
#' @inheritParams oat_processParamSubsets
#' @inheritParams oat_csv_result_file_analysis
#' @param ATESTSIGLEVEL The A-Test determines if there is a large difference between two sets if the result is greater than 0.21 either side of the 0.5 line. Should this not be suitable, this can be changed here
#'
#' @export
#'
#' @importFrom graphics abline par plot lines axis legend plot text title
oat_graphATestsForSampleSize <- function(FILEPATH, PARAMETERS, MEASURES,
                                         ATESTSIGLEVEL, ATESTRESULTFILENAME,
                                         BASELINE, PMIN = NULL, PMAX = NULL,
                                         PINC = NULL, PARAMVALS = NULL,
                                         TIMEPOINTS = NULL,
                                         TIMEPOINTSCALE = NULL) {

  if (is.null(TIMEPOINTS) || length(TIMEPOINTS) == 1) {
    # NOTE THAT OUTPUT_FOLDER AND BASELINE PARAMETERS ADDED IN SPARTAN 2.0
    # IN VERSION 2.0, WE PROCESS ONE FILE, NOT AN ATEST FILE FOR EACH PARAMETER
    # THE GRAPHS GO IN SAME DIRECTORY AS THIS FOLDER, NOT IN THE TOP LEVEL

    if (file.exists(FILEPATH)) {
      print("Creating graphs of A-Test results (oat_graphATestsForSampleSize)")

      # FIRSTLY READ IN THE ATESTS FILE (NOW ALL IN ONE FILE)
      RESULT <- read.csv(make_path(c(FILEPATH, ATESTRESULTFILENAME)),
                         sep = ",", header = TRUE, check.names = FALSE)

      for (PARAM in 1:length(PARAMETERS)) {
        print(paste("Creating graph for Parameter ", PARAMETERS[PARAM],
                    sep = ""))

        # NEED TO RECOVER THE A-TESTS FOR THIS PARAMETER FROM THE RESULTS
        EXP_PARAMS <- as.character(BASELINE)

        # GET THE LIST OF PARAMETER VALUES BEING EXPLORED FOR THIS PARAMETER
        # NOTE CONVERSION TO NUMBERS: GETS RID OF TRAILING ZEROS MADE BY SEQ
        val_list <- as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                                  PARAMVALS,
                                                                  PARAM))

        PARAM_ATESTS <- NULL

        # NOW WE ITERATE THROUGH THE VALUES IN THIS LIST
        for (PARAMVAL in 1:length(val_list)) {
          # NOW TO RECOVER THE EXPERIMENTS RUN UNDER THIS PARAMETER SET
          ATESTS <- RESULT

          # SET THE VALUE OF THIS PARAMETER TO BE THAT WE ARE PROCESSING
          EXP_PARAMS[PARAM] <- as.character(val_list[PARAMVAL])

          for (PARAMOFINT in 1:length(PARAMETERS))
            ATESTS <- subset(ATESTS, ATESTS[[PARAMETERS[PARAMOFINT]]]
                             == as.numeric(EXP_PARAMS[PARAMOFINT]))

          # KEEP ALL THE ATESTS RESULTS TOGETHER FOR THIS PARAMETER
          PARAM_ATESTS <- rbind(PARAM_ATESTS, ATESTS)

        }

        # Where the resulting graph should go
        if (is.null(TIMEPOINTS)) {
          GRAPHFILE <- make_path(c(FILEPATH,
                                   make_extension(PARAMETERS[PARAM], "pdf")))
          GRAPHTITLE <- paste("A-Test Scores when adjusting parameter \n",
                              PARAMETERS[PARAM], sep = "")
        } else {
          GRAPHFILE <- make_path(c(FILEPATH,
                                   make_extension(
                                     make_filename(
                                       c(PARAMETERS[PARAM],
                                         TIMEPOINTS)), "pdf")))

          GRAPHTITLE <- paste("A-Test Scores when adjusting parameter \n",
                              PARAMETERS[PARAM], " at Timepoint: ",
                              TIMEPOINTS, " ", TIMEPOINTSCALE, sep = "")
        }

        pdf(GRAPHFILE, width = 12, height = 7)
        par(xpd = NA, mar = c(4, 4, 4, 17))

        # NOW PLOT THE MEASURES, START WITH THE FIRST
        MEASURELABEL <- paste("ATest", MEASURES[1], sep = "")
        plot(PARAM_ATESTS[[PARAMETERS[PARAM]]], PARAM_ATESTS[, MEASURELABEL],
             type = "o", main = GRAPHTITLE,
             lty = 1, ylim = c(0, 1), pch = 1, xlab = "Parameter Value",
             ylab = "A Test Score", xaxt = "n")

        if (length(MEASURES) > 1) {
          # NOW ADD THE REST OF THE MEASURES
          for (l in 2:length(MEASURES)) {
            MEASURELABEL <- paste("ATest", MEASURES[l], sep = "")
            lines(PARAM_ATESTS[[PARAMETERS[PARAM]]],
                  PARAM_ATESTS[, MEASURELABEL],
                  type = "o", lty = 5, pch = l)
          }
        }

        axis(1, val_list)
        legend(par("usr")[2], par("usr")[4], title = "Measures", MEASURES,
               pch = 1:length(MEASURES), cex = 0.7, ncol = 1)
        par(xpd = FALSE)

        abline(a = 0.5, b = 0, lty = 4)
        text_pos <- (max(val_list) + min(val_list)) / 2
        text(text_pos, 0.52, "no difference",
             col = "blue")
        a_abline <- 0.5 + ATESTSIGLEVEL
        abline(a = a_abline, b = 0, lty = 4)
        text(text_pos, (0.5 + ATESTSIGLEVEL
                                                    + 0.02),
             "large difference", col = "blue")
        a_abline <- 0.5 - ATESTSIGLEVEL
        abline(a = a_abline, b = 0, lty = 4)
        text(text_pos, (0.5 - ATESTSIGLEVEL
                                                    - 0.02),
             "large difference", col = "blue")

        dev.off()
      }
    } else {
      print("The directory specified in FILEPATH does not exist.
            No graph created")
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {
      current_time <- TIMEPOINTS[n]
      print(paste("PROCESSING TIMEPOINT: ", current_time, sep = ""))

      atest_result_filename_format <- check_file_extension(ATESTRESULTFILENAME)
      ATESTRESULTFILENAME_FULL <- paste(substr(ATESTRESULTFILENAME, 0,
                                               nchar(ATESTRESULTFILENAME) - 4),
                                        "_", current_time, ".",
                                        atest_result_filename_format, sep = "")

      oat_graphATestsForSampleSize(FILEPATH, PARAMETERS, MEASURES,
                                   ATESTSIGLEVEL, ATESTRESULTFILENAME_FULL,
                                   BASELINE, PMIN, PMAX, PINC, PARAMVALS,
                                   TIMEPOINTS = current_time, TIMEPOINTSCALE
                                   = TIMEPOINTSCALE)
    }
  }
}

#' For stochastic simulations plots the distribution of results for each parameter value
#'
#' Only applicable for stochastic simulations where the results are provided in
#' the folder structure: this takes each parameter in turn, and creates a boxplot
#' for each output measure, showing the result distribution for each value of that
#' parameter.
#'
#' @inheritParams oat_processParamSubsets
#' @inheritParams oat_csv_result_file_analysis
#' @param MEASURE_SCALE An array containing the measure used for each of the output measures (i.e. microns, microns/min).  Used to label graphs
#'
#' @export
#'
#' @importFrom graphics boxplot
oat_plotResultDistribution <- function(FILEPATH, PARAMETERS, MEASURES,
                                       MEASURE_SCALE, CSV_FILE_NAME, BASELINE,
                                       PMIN = NULL, PMAX = NULL, PINC = NULL,
                                       PARAMVALS = NULL, TIMEPOINTS = NULL,
                                       TIMEPOINTSCALE = NULL) {

  if (is.null(TIMEPOINTS) || length(TIMEPOINTS) == 1) {
    if (file.exists(FILEPATH)) {
      print("Plotting result distribution for each parameter (oat_plotResultDistribution)")

      # NEW TO SPARTAN VERSION 2
      # READS SIMULATION RESPONSES FROM A CSV FILE, IN THE FORMAT: PARAMETER
      # VALUES (COLUMNS), SIMULATION OUTPUT MEASURES IN A CHANGE TO SPARTAN 1,
      # THE FIRST FUNCTION THAT PROCESSES SIMULATION RESPONSES CREATES THIS
      # FILE, NOT MEDIANS FOR EACH PARAMETER AS IT USED TO. THIS WAY WE ARE
      # NOT DEALING WITH TWO METHODS OF SIMULATION RESULT SPECIFICATION
      # READ IN THE OAT RESULT FILE
      RESULT <- read.csv(make_path(c(FILEPATH, CSV_FILE_NAME)), sep = ",",
                                   header = TRUE, check.names = FALSE)

      for (PARAM in 1:length(PARAMETERS)) {
        print(paste("Creating Output Responses Box Plot Graph for Parameter ",
                    PARAMETERS[PARAM], sep = ""))

        # THE RESULTS OF THE OAT ANALYSIS IS IN ONE PLACE. THUS WE NEED TO
        # REFER TO THE CORRECT BASELINE RESULT FOR PARAMETERS THAT ARE
        # NOT BEING CHANGED SO WE USE THE VARIABLE EXP_PARAMS WHEN WE START
        #A NEW VARIABLE - WE SET THE PARAMS TO THE BASELINE AND THEN ONLY
        # ALTER THE ONE BEING CHANGED
        EXP_PARAMS <- as.character(BASELINE)

        # NOW GET THE LIST OF PARAMETER VALUES BEING EXPLORED FOR THIS
        # PARAMETER. NOTE CONVERSION TO NUMBERS: GETS RID OF TRAILING
        # ZEROS MADE BY SEQ
        val_list <- as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                            PARAMVALS, PARAM))

        ALLRESULTS <- NULL

        for (PARAMVAL in 1:length(val_list)) {
          EXP_PARAMS[PARAM] <- as.character(val_list[PARAMVAL])
          PARAM_RESULT <- subset_results_by_param_value_set(PARAMETERS,
                                                            RESULT, EXP_PARAMS)

          VALUE_RESULT <- cbind(PARAM_RESULT[PARAMETERS[PARAM]])

          # NOW ADD ALL MEASURES
          for (MEASURE in 1:length(MEASURES)) {
            VALUE_RESULT <- cbind(VALUE_RESULT,
                                  PARAM_RESULT[MEASURES[MEASURE]])
          }

          ALLRESULTS <- rbind(ALLRESULTS, VALUE_RESULT)
        }

        for (MEASURE in 1:length(MEASURES)) {
          # NOW DO THE BOXPLOTS FOR EACH MEASURE
          # BOXPLOT THE MEASURE
          if (is.null(TIMEPOINTS)) {

            GRAPHFILE <- make_path(c(FILEPATH,
                                     make_filename(c(PARAMETERS[PARAM],
                                                     MEASURES[MEASURE],
                                                     "BP.pdf"))))

            GRAPHTITLE <- paste("Distribution of ", MEASURES[MEASURE],
                                " Responses \n when altering parameter ",
                              PARAMETERS[PARAM], sep = "")
          } else {
            GRAPHFILE <- make_extension(make_path(c(FILEPATH,
                                     make_filename(c(PARAMETERS[PARAM],
                                                     MEASURES[MEASURE],
                                                     "BP",
                                                     TIMEPOINTS)))),
                                                     "pdf")

            GRAPHTITLE <- paste("Distribution of ", MEASURES[MEASURE],
                              " Responses \n when altering parameter ",
                              PARAMETERS[PARAM], " at Timepoint ",
                              TIMEPOINTS, " ", TIMEPOINTSCALE,
                              sep = "")
          }

          pdf(GRAPHFILE)

          # GENERATE YLABEL BASED ON PARAMETER MEASURE
          YLABEL <- paste("Median ", MEASURES[MEASURE], " (",
                          MEASURE_SCALE[PARAM], ")", sep = "")

          boxplot(ALLRESULTS[, MEASURE + 1] ~ ALLRESULTS[, 1],
                  ylab = YLABEL, xlab = "Parameter Value",
                  main = GRAPHTITLE)

          dev.off()

          print(paste("Box Plot Generated and output as ", GRAPHFILE,
                      sep = ""))

        }
      }
    } else {
      print("The directory specified in FILEPATH does not exist.
            No graph created")
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {
      current_time <- TIMEPOINTS[n]
      print(paste("PROCESSING TIMEPOINT: ", current_time, sep = ""))

      csv_file_name_format <- check_file_extension(CSV_FILE_NAME)
      CSV_FILE_NAME_FULL <- paste(substr(CSV_FILE_NAME, 0,
                                         nchar(CSV_FILE_NAME) - 4),
                                  "_", current_time, ".",
                                  csv_file_name_format, sep = "")

      oat_plotResultDistribution(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE,
                                 CSV_FILE_NAME_FULL, BASELINE, PMIN, PMAX,
                                 PINC, PARAMVALS, TIMEPOINTS = current_time,
                                 TIMEPOINTSCALE = TIMEPOINTSCALE)
    }
  }
}
