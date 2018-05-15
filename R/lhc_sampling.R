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
#' Version 3.1 adds returning the sample as a object, should this be easier
#' to process than reading back in a file
#'
#' @param FILEPATH Directory where the parameter samples should be output to
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated
#' @param NUMSAMPLES The number of parameter subsets to generate
#' @param PMIN Array containing the minimum value that should be used for
#' each parameter. Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for
#' each parameter. Sets an upper bound on sampling space
#' @param PINC Array containing the increment value that should be applied
#' for each parameter. For example, a parameter could have a minimum value
#' of 10, and maximum value of 100, and be incremented by 10. Added after
#' user request on Github
#' @param ALGORITHM Choice of algorithm to use to generate the hypercube.
#' Can be set to either 'normal' or 'optimum'. Beware optimum can take a
#' long time to generate an optimised parameter set (more than 24 hours
#' in some circumstances)
#' @return LHC generated parameter sets
#'
#' @export
lhc_generate_lhc_sample <- function(FILEPATH, PARAMETERS, NUMSAMPLES, PMIN,
                                    PMAX, ALGORITHM, PINC = NULL) {
  # Version 3.1 adds pre-execution check functions as part of refactoring:
  # Get the provided function arguments
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:

  if(check_input_args(input_check$names, input_check$arguments)) {

      # PERFORM THE SAMPLING - JUDGING ON USERS CHOICE OF ALGORITHM
      design <- sample_parameter_space(ALGORITHM, NUMSAMPLES, PARAMETERS)

      # Now scale this design, as currently all values are between 0 and 1
      #design <- scale_lhc_sample(PARAMETERS, PMIN, PMAX, NUMSAMPLES, design)
      design <- scale_lhc_sample(PARAMETERS, PMIN, PMAX, PINC, NUMSAMPLES, design)

      # Output the scaled design to csv file
      if(!is.null(FILEPATH))
      {
        write_data_to_csv(design, make_path(c(FILEPATH,"LHC_Parameters_for_Runs.csv")))
        message(paste("Parameter Set Generated and Output to ", FILEPATH,
                    "/LHC_Parameters_for_Runs.csv", sep = ""))
      }
      else
        message("No FILEPATH specified. Returning sample as R Object")

      return(design)
  }
}

#' Generate the LHC design for the chosen algorithm
#' @param ALGORITHM Choice of algorithm to use to generate the hypercube.
#' Can be set to either 'normal' or 'optimum'. Beware optimum can take a
#' long time to generate an optimised parameter set (more than 24 hours
#' in some circumstances)
#' @param NUMSAMPLES The number of parameter subsets to generate
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated
#' @return Latin-Hypercube sample
sample_parameter_space <- function(ALGORITHM, NUMSAMPLES, PARAMETERS)
{
  if (tolower(ALGORITHM) == "optimum") {
    # MAY TAKE A WHILE FOR A LARGE NUMBER OF SAMPLES
    # (THIS BEING TWO DAYS WHERE NUMSAMPLES=500)
    design <- lhs::optimumLHS(NUMSAMPLES, length(PARAMETERS), maxSweeps = 2,
                            eps = .1)
  } else {
    design <- lhs::randomLHS(NUMSAMPLES, length(PARAMETERS))
  }
}

#' Scale the LHC design to be the range explored for each parameter
#'
#' As the lhc design is scaled between 0 and 1, this method rescales the
#' sample, putting the sampled value within the range specified for that
#' parameter
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated
#' @param NUMSAMPLES The number of parameter subsets to generate
#' @param PMIN Array containing the minimum value that should be used for
#' each parameter. Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for
#' each parameter. Sets an upper bound on sampling space
#' @param PINC Array containing the increment value that should be applied
#' for each parameter. For example, a parameter could have a minimum value
#' of 10, and maximum value of 100, and be incremented by 10.
#' @param design The generated lhc design, all values between 0 and 1
#' @return Rescaled design in the required ranges
#'
scale_lhc_sample <- function(PARAMETERS, PMIN, PMAX, PINC, NUMSAMPLES, design)
{
  # NOW LOOK AT THE VALUE CHOSEN FOR EACH SAMPLE, AS THESE WILL
  # CURRENTLY BE BETWEEN 0 AND 1
  for (k in 1:NUMSAMPLES) {
    # NOW LOOK AT EACH PARAMETER IN TURN
    # THE LHC WILL HAVE GIVEN EACH A VALUE BETWEEN 0 AND 1
    # NOW USE THE MAX AND MIN VALUES FOR EACH PARAMETER TO
    # GIVE IT A PROPER VALUE
    for (l in 1:length(PARAMETERS)) {
      # GET THE MAX AND MIN VALUES FOR THIS PARAMETER FROM THE ARRAY
      lhc_max <- PMAX[l]
      lhc_min <- PMIN[l]
      lhc_inc <- PINC[l]

      # NOW CALCULATE THE VALUE TO USE FOR THIS PARAMETER
      #value <- (design[k, l] * (lhc_max - lhc_min)) + lhc_min

      if (is.null(PINC)) {
        value <- (design[k, l] * (lhc_max - lhc_min)) + lhc_min
      } else {
        value <- (design[k, l] %/% (1 / (1 + (lhc_max - lhc_min) %/% lhc_inc))) * lhc_inc + lhc_min;
        value <- min (value, lhc_max)
      }

      # NOW REPLACE THE VALUE IN THE TABLE (BETWEEN 0 AND 1) WITH
      # THE PARAMETER VALUE
      design[k, l] <- value
    }
  }

  # LABEL THE RESULTS
  colnames(design) <- c(PARAMETERS)

  return(design)
}
