#' Generates sets of simulation parameters using latin-hypercube sampling
#'
#' Though robustness analysis does elucidate the effects of perturbations of
#' one parameter, it cannot show any non-linear effects which occur when two
#' or more are adjusted simultaneously. A Global Sensitivity Analysis
#' technique is needed to identify such effects, and to give an indication
#' of the parameters which have the greatest influence on the simulation
#' output. This technique uses the method described by Read et al in their
#' paper reference in the tutorial, which uses a latin-hypercube design to
#' sample the parameter space. Ranges are set for each parameter, and all
#' parameter values perturbed concurrently. This method creates the parameter
#' value sets with which simulations should be run. This is output as a CSV
#' file. For each set of parameters, the simulation should be run for the
#' number of times identified in Aleatory Analysis. Once this has been
#' completed, the set of remaining methods within spartan can be used to
#' analyse the results. Note: To run this, you will require the lhs library.
#'
#' @param FILEPATH Directory where the parameter samples should be output to
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated
#' @param NUMSAMPLES The number of parameter subsets to generate
#' @param PMIN Array containing the minimum value that should be used for
#' each parameter. Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for
#' each parameter. Sets an upper bound on sampling space
#' @param ALGORITHM Choice of algorithm to use to generate the hypercube.
#' Can be set to either 'normal' or 'optimum'. Beware optimum can take a
#' long time to generate an optimised parameter set (more than 24 hours
#' in some circumstances)
#'
#' @export
lhc_generate_lhc_sample <- function(FILEPATH, PARAMETERS, NUMSAMPLES, PMIN,
                                    PMAX, ALGORITHM) {
  if (requireNamespace("lhs", quietly = TRUE)) {
    if (file.exists(FILEPATH)) {
      NUMPARAMS <- length(PARAMETERS)

      ALGORITHM <- tolower(ALGORITHM)

      # PERFORM THE SAMPLING - JUDGING ON USERS CHOICE OF ALGORITHM
      if (ALGORITHM == "optimum") {
        # MAY TAKE A WHILE FOR A LARGE NUMBER OF SAMPLES
        # (THIS BEING TWO DAYS WHERE NUMSAMPLES=500)
        design <- lhs::optimumLHS(NUMSAMPLES, NUMPARAMS, maxSweeps = 2,
                                  eps = .1)
      } else {
        design <- lhs::randomLHS(NUMSAMPLES, NUMPARAMS)
      }

      # NOW LOOK AT THE VALUE CHOSEN FOR EACH SAMPLE, AS THESE WILL
      # CURRENTLY BE BETWEEN 0 AND 1
      for (k in 1:NUMSAMPLES) {
        # NOW LOOK AT EACH PARAMETER IN TURN
        # THE LHC WILL HAVE GIVEN EACH A VALUE BETWEEN 0 AND 1
        # NOW USE THE MAX AND MIN VALUES FOR EACH PARAMETER TO
        # GIVE IT A PROPER VALUE
        for (l in 1:NUMPARAMS) {
          # GET THE MAX AND MIN VALUES FOR THIS PARAMETER FROM THE ARRAY
          lhc_max <- PMAX[l]
          lhc_min <- PMIN[l]

          # NOW CALCULATE THE VALUE TO USE FOR THIS PARAMETER
          value <- (design[k, l] * (lhc_max - lhc_min)) + lhc_min

          # NOW REPLACE THE VALUE IN THE TABLE (BETWEEN 0 AND 1) WITH
          # THE PARAMETER VALUE
          design[k, l] <- value
        }
      }

      # LABEL THE RESULTS
      colnames(design) <- c(PARAMETERS)

      # OUTPUT THE LHC DESIGN AS A CSV FILE
      write.csv(design, paste(FILEPATH, "/LHC_Parameters_for_Runs.csv",
                              sep = ""), row.names = FALSE, quote = FALSE)

      print(paste("Parameter Set Generated and Output to ", FILEPATH,
                  "/LHC_Parameters_for_Runs.csv", sep = ""))
    } else {
      print("The directory specified in FILEPATH does not exist.
            No parameter samples generated")
    }
  } else {
    print("The lhc_generate_lhc_sample function requires package lhs")
  }
}
