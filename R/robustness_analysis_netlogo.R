#' Takes a Netlogo behaviour space file and performs a robustness analysis from that simulation data
#'
#' From a Netlogo behaviour space file, extracts the required timepoint
#' information from it, storing this in a Spartan compatible CSV file.
#' This CSV file is then processed using the methods described in
#' \code{oat_csv_result_file_analysis}, with A-Test scores determined
#' for each value assigned to each parameter. Once this method has been
#' called, the researcher should use the \code{oat_graphATestsForSampleSize}
#' and \code{oat_plotResultDistribution} methods to graph the results.
#'
#' @param FILEPATH Location where the behaviour space results can be found
#' @param NETLOGO_BEHAVIOURSPACEFILE The name of the file produced by Netlogo for Parameter Robustness (Technique 2). This is the result file that is analysed.
#' @param PARAMETERS Array containing the names of the parameters for which parameter samples were be generated
#' @param BASELINE Array containing the baseline, or calibrated value, of each parameter.
#' @param PMIN Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space.
#' @param PMAX Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space.
#' @param PINC Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10.
#' @param MEASURES Array containing the names of the Netlogo output measures which are used to analyse the simulation.
#' @param RESULTFILENAME Name of the results summary file that should be produced when analysing the Netlogo results
#' @param ATESTRESULTSFILENAME File name of the ATests result summary file created by \code{oat_analyseAllParams}
#' @param TIMESTEP The timestep of the Netlogo simulation being analysed.
#'
#' @export
oat_process_netlogo_result <-
  function(FILEPATH, NETLOGO_BEHAVIOURSPACEFILE, PARAMETERS, BASELINE, PMIN,
           PMAX, PINC, MEASURES, RESULTFILENAME, ATESTRESULTSFILENAME,
           TIMESTEP) {
  # NOTE THAT ALTHOUGH NETLOGO FILE CONTAINS EACH PARAMETER FOR THE TURTLE,
    # THE INPUT HERE ONLY NEEDS TO BE THE PARAMETERS THAT HAVE BEEN PERTURBED

  ## FIRSTLY READ IN THE NETLOGO RESULT FILE
  NL_RESULT <- read.csv(paste(FILEPATH, "/", NETLOGO_BEHAVIOURSPACEFILE,
                              sep = ""), sep = ",", skip = 6,
                        check.names = FALSE)

  # ORDER IT BY RUN FOR EFFICIENCY LATER
  NL_RESULT_ORDERED <- NL_RESULT[order(NL_RESULT[, 1]), ]

  TIMESTEP_RESULTS <- subset(NL_RESULT_ORDERED,
                             NL_RESULT_ORDERED["[step]"] == TIMESTEP)

  write.csv(TIMESTEP_RESULTS, paste(FILEPATH, "/", RESULTFILENAME, sep = ""),
            quote = FALSE, row.names = FALSE)

  message(join_strings_space(c("Analysing Netlogo Robustness Analysis File",
        "and Generating A-Test Scores")))
  oat_csv_result_file_analysis(FILEPATH, RESULTFILENAME, PARAMETERS,
                               BASELINE, MEASURES, ATESTRESULTSFILENAME,
                               PMIN = PMIN, PMAX = PMAX,
                               PINC = PINC, PARAMVALS = NULL)
}
