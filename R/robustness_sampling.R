#' Create parameter samples for robustness (local) analysis
#'
#' The robustness of a simulation to parameter alteration can be determined
#' through the use of this approach. Following the method described by Read
#' et al, the value of each parameter is adjusted independently, with the
#' remaining parameters staying unchanged from their calibrated value. This
#' method within the toolkit creates a set of simulation parameter sets to
#' enable such an analysis to be performed. One CSV file is created for each
#' parameter being examined (with the filename being
#' [Parameter Name]_Values.csv). Each CSV file will contain the parameters for
#' runs that need to be performed. For each set of parameters, the simulation
#' should be run for the number of times determined by Aleatory Analysis.
#' Once this has been completed, the results can be analysed
#' using the robustness analysis  methods included within this package
#'
#' @param FILEPATH Directory where the parameter samples should be output to
#' @param PARAMETERS Array containing the names of the parameters of which parameter samples will be generated
#' @param BASELINE Array containing the values assigned to each of these parameters in the calibrated baseline
#' @param PMIN Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space
#' @param PINC Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10
#' @param PARAMVALS Array containing a list of strings for each parameter, each string containing comma separated values that should be assigned to that parameter. Thus sampling can be performed for specific values for each parameter, rather than a uniform incremented value. This replaces the PMIN, PMAX, and PINC where this method is used.
#'
#' @export
oat_parameter_sampling <- function(FILEPATH, PARAMETERS, BASELINE, PMIN = NULL,
                                   PMAX = NULL, PINC = NULL, PARAMVALS = NULL) {
  # SPARTAN 2.0 - CHANGES SUCH THAT SAMPLING CAN BE PERFORMED USING SPECIFIED
  # PARAMETER VALUES AS WELL AS INCREMENTS

  if (file.exists(FILEPATH)) {
    # CONSIDER EACH PARAMETER IN TURN
    for (PARAMOFINT in 1:length(PARAMETERS)) {
      # NOW GET THE LIST OF PARAMETER VALUES BEING EXPLORED FOR THIS PARAMETER
      # NOTE CONVERSION BACK TO NUMBERS: GETS RID OF TRAILING ZEROS MADE BY SEQ
      val_list <- as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                          PARAMVALS,
                                                          PARAMOFINT))
      PARAMETERTABLE <- NULL

      for (PARAM in 1:length(PARAMETERS)) {
        if (PARAMOFINT == PARAM)
          PARAMETERTABLE <- cbind(PARAMETERTABLE, val_list)
        else
          PARAMETERTABLE <- cbind(PARAMETERTABLE,
                                  array(BASELINE[PARAM],
                                        dim = c(length(val_list))))
      }


      # FORMAT THEN OUTPUT THE PARAMETER TABLE FOR THIS PARAMETER OF INTEREST
      colnames(PARAMETERTABLE) <- PARAMETERS

      # WRITE THE A-TEST RESULTS TO FILE
      results_file <- make_path(c(FILEPATH,
                                  make_filename(c(PARAMETERS[PARAMOFINT],
                                                  "OAT_Values.csv"))))

      write.csv(PARAMETERTABLE, results_file, quote = FALSE, row.names = FALSE)

      print(paste("Sample File Generated for parameter ",
                  PARAMETERS[PARAMOFINT], " and output to ",
                  results_file, sep = ""))
    }
  } else {
    print("The directory specified in FILEPATH does not exist.
          No parameter samples generated")
  }

}
