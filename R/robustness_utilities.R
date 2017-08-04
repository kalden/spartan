#' Counts the number of simulation responses where a output response equals a desired result, for a specified parameter.
#'
#' For example, how many simulations produce a value of 0 as the output. Outputs this information as a CSV file.
#'
#' @inheritParams oat_processParamSubsets
#' @param PARAMETER Current parameter being analysed in this loop
#' @param MEASURE Current simulation output measure being analysed in this loop
#' @param DESIREDRESULT The specific requirement to match when counting simulation responses
#' @param OUTPUTFILENAME CSV file name to contain the counts where simulation responses meet a specific requirement
#'
#' @export
oat_countResponsesOfDesiredValue <- function(FILEPATH, PARAMETERS,
                                             RESULTFILENAME, OUTPUTCOLSTART,
                                             OUTPUTCOLEND, PARAMETER,
                                             NUMRUNSPERSAMPLE, MEASURE,
                                             DESIREDRESULT, OUTPUTFILENAME,
                                             BASELINE, PMIN=NULL, PMAX=NULL,
                                             PINC=NULL, PARAMVALS=NULL,
                                             TIMEPOINTS=NULL,
                                             TIMEPOINTSCALE=NULL) {
  if (is.null(TIMEPOINTS)) {
    if (file.exists(FILEPATH)) {
      print(paste("Summing Responses for Parameter ", PARAMETER, " where ",
                  MEASURE, " = ", DESIREDRESULT, sep = ""))

      if (file.exists(paste(FILEPATH, "/", PARAMETER, sep = ""))) {

        EXP_PARAMS <- as.character(BASELINE)

        # NOW WE CAN WORK WITH INCREMENTS BETWEEN MAX AND MIN, AND SPECIFIED
        # VALUES, WE NEED TO GET THE VALUES OF THE PARAMETERS WE ARE ANALYSING
        # NOTE THE CONVERSION BACK TO NUMBERS - GETS RID OF TRAILING ZEROS
        # MADE BY SEQ

        val_list <- as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                            PARAMVALS,
                                                            match(PARAMETER,
                                                                  PARAMETERS)))

        ALLRESULTS <- NULL

        # NOW WE ITERATE THROUGH THE VALUES IN THIS LIST
        for (P in 1:length(val_list)) {
          # SET THE VALUE OF THE PARAMETERS BEING EXAMINED TO INCLUDE THE
          # CURRENT VALUE OF THE PARAMETER
          EXP_PARAMS[match(PARAMETER, PARAMETERS)] <- as.character(val_list[P])

          TRUECOUNT <- 0
          FALSECOUNT <- 0

          if (file.exists(make_path(c(FILEPATH, PARAMETER, toString(P))))) {
            for (i in 1:NUMRUNSPERSAMPLE) {
              FILEADDRESS <- make_path(c(FILEPATH, PARAMETER, toString(P), i,
                                         RESULTFILENAME))

              if (result_format == "csv") {

                if (file.exists(make_extension(FILEADDRESS, result_format))) {
                  if (OUTPUTCOLSTART > 1) {
                    col_diff <- OUTPUTCOLEND - OUTPUTCOLSTART
                    import <- read.csv(make_extension(FILEADDRESS, "csv"),
                                       colClasses = c(rep("NULL",
                                                          OUTPUTCOLSTART - 1),
                                                      rep(NA, col_diff + 1)),
                                       header = TRUE, check.names = FALSE)
                  } else {
                    import <- read.csv(make_extension(FILEADDRESS, "csv"),
                                       colClasses = c(rep(NA, OUTPUTCOLEND)),
                                       header = TRUE, check.names = FALSE)
                  }

                  MODELRESULT <- data.frame(import, check.names = FALSE)

                } else if (result_format == "xml") {
                  if (requireNamespace("XML", quietly = TRUE)) {
                    MODELRESULT <- XML::xmlToDataFrame(
                      make_extension(FILEADDRESS, "xml"))
                  } else {
                    print("oat_countResponsesOfDesiredValue needs XML package")
                  }
                }

                if (MODELRESULT[[MEASURE]] == DESIREDRESULT)
                  TRUECOUNT <- TRUECOUNT + 1
                else
                  FALSECOUNT <- FALSECOUNT + 1
              }
            }

            ROWRESULT <- cbind(P, TRUECOUNT, FALSECOUNT)
            ALLRESULTS <- rbind(ALLRESULTS, ROWRESULT)
          } else {
            print(paste("No results can be found for parameter: ",
                        PARAMETER, " Value: ", P, sep = ""))
          }
        }

        # Output the true false results
        RESULTSFILE <- make_path(c(FILEPATH, PARAMETER,
                                   paste(MEASURE, DESIREDRESULT, sep = "_")))
        write.csv(ALLRESULTS, make_extension(RESULTSFILE, "csv"),
                  quote = FALSE, row.names = FALSE)
      } else {
        print(paste("No results can be found for the parameter specified: ",
                    PARAMETER, sep = ""))
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

      result_format <- check_file_extension(RESULTFILENAME)
      SIMRESULTFILENAME <- paste(substr(RESULTFILENAME, 0,
                                      nchar(RESULTFILENAME) - 4),
                               "_", current_time, ".", result_format,
                               sep = "")

      output_file_format <- check_file_extension(OUTPUTFILENAME)
      OUTPUTFILENAME_FULL <- paste(substr(OUTPUTFILENAME, 0,
                                          nchar(OUTPUTFILENAME) - 4),
                                   "_", current_time, ".", output_file_format,
                                   sep = "")

      # NOW CALL THIS FUNCTION AGAIN TO DO THE TIMEPOINTS - SET THE TIMEPOINTS
      # AND TIMEPOINTSCALE TO NULL NOW SO WE DONT END UP BACK IN THIS ELSE
      oat_countResponsesOfDesiredValue(FILEPATH, SIMRESULTFILENAME,
                                       OUTPUTCOLSTART, OUTPUTCOLEND, PARAMETER,
                                       NUMRUNSPERSAMPLE, MEASURE,
                                       DESIREDRESULT, OUTPUTFILENAME_FULL,
                                       BASELINE, PMIN, PMAX, PINC, PARAMVALS,
                                       TIMEPOINTS = NULL,
                                       TIMEPOINTSCALE = NULL)
    }
  }

}
