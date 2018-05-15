#' Generates parameter sets for variance-based eFAST Sensitivity Analysis
#'
#' This technique analyses simulation results generated through sampling
#' using the eFAST approach (extended Fourier Amplitude Sampling Test).
#' This perturbs the value of all parameters at the same time, with the
#' aim of partitioning the variance in simulation output between input
#' parameters. Values for each parameter are chosen using fourier frequency
#' curves through a parameters potential range of values. A selected
#' number of values are selected from points along the curve. Though all
#' parameters are perturbed simultaneously, the method does focus on
#' one parameter of interest in turn, by giving this a very different
#' sampling frequency to that assigned to the other parameters.
#' Thus for each parameter of interest in turn, a sampling frequency is
#' assigned to each parameter and values chosen at points along the curve. So
#' a set of simulation parameters then exists for each parameter of interest.
#' As this is the case, this method can be computationally expensive,
#' especially if a large number of samples is taken on the parameter search
#' curve, or there are a large number of parameters. On top of this, to ensure
#' adequate sampling each curve is also resampled with a small adjustment to the
#' frequency, creating more parameter sets on which the simulation should be run.
#' This attempts to limit any correlations and limit the effect of repeated
#' parameter value sets being chosen. Samples are output to CSV file, one per
#' parameter/curve pairing
#'
#' @param FILEPATH Directory where the parameter samples should be output to
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param NUMSAMPLES The number of parameter subsets to generate - should be at
#'  least 65 for eFAST
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated.  For eFAST, remember to add a
#' parameter named 'Dummy'
#' @param PMIN Array containing the minimum value that should be used for each
#' parameter and the dummy. Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for each
#' parameter and the dummy. Sets an upper bound on sampling space
#'
#' @export
efast_generate_sample <- function(FILEPATH, NUMCURVES, NUMSAMPLES,
                                  PARAMETERS, PMIN, PMAX) {

  # Version 3.1 adds pre-execution check functions as part of refactoring:
  # Get the provided function arguments
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:

  if(check_input_args(input_check$names, input_check$arguments)) {

    parameter_vals <- generate_efast_parameter_sets(FILEPATH, NUMCURVES,
                                                    NUMSAMPLES, PARAMETERS,
                                                    PMIN, PMAX)

    # NOW OUTPUT THE RESULTS - SPLIT BY CURVE FILE
    # SO, WILL HAVE ONE FILE FOR EACH PARAMETER OF INTEREST, FOR EACH CURVE
    output_param_sets_per_curve(FILEPATH, NUMCURVES, PARAMETERS, parameter_vals)
  }
}


#' Use the eFAST approach to generate parameter sets
#'
#' @param FILEPATH Directory where the parameter samples should be output to
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param NUMSAMPLES The number of parameter subsets to generate - should be at
#'  least 65 for eFAST
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated.  For eFAST, remember to add a
#' parameter named 'Dummy'
#' @param PMIN Array containing the minimum value that should be used for each
#' parameter and the dummy. Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for each
#' parameter and the dummy. Sets an upper bound on sampling space
generate_efast_parameter_sets <- function(FILEPATH, NUMCURVES, NUMSAMPLES,
                                          PARAMETERS, PMIN, PMAX)
{
  wanted_n <- NUMSAMPLES * length(PARAMETERS) * NUMCURVES # wanted no. of sample points

  # OUTPUT
  # SI[] : first order sensitivity indices
  # STI[] : total effect sensitivity indices
  # Other used variables/constants:
  # OM[] : vector of k frequencies
  # omi : frequency for the group of interest
  # omci[] : set of freq. used for the compl. group
  # X[] : parameter combination rank matrix
  # AC[],BC[]: fourier coefficients
  # FI[] : random phase shift
  # V : total output variance (for each curve)
  # VI : partial var. of par. i (for each curve)
  # VCI : part. var. of the compl. set of par...
  # AV : total variance in the time domain
  # AVI : partial variance of par. i
  # AVCI : part. var. of the compl. set of par.
  # Y[] : model output

  MI <- 4  # maximum number of fourier coefficients
  # that may be retained in calculating the partial
  # variances without interferences between the
  # assigned frequencies

  # Computation of the frequency for the group
  # of interest omi and the # of sample points NUMSAMPLES (here N=NUMSAMPLES)
  omi <- floor(((wanted_n / NUMCURVES) - 1) / (2 * MI) / length(PARAMETERS))
  NUMSAMPLES <- 2 * MI * omi + 1
  if (NUMSAMPLES * NUMCURVES < 65)
    message("Error: sample size must be >= 65 per factor")

  PARAMETERVALS <- array(0, dim = c(NUMSAMPLES, length(PARAMETERS),
                                    length(PARAMETERS), NUMCURVES))

  for (PARAMNUM in 1:length(PARAMETERS)) {

    # Algorithm for selecting the set of frequencies.
    # omci(i), i=1:k-1, contains the set of frequencies
    # to be used by the complementary group.

    omci <- efast_setfreq(length(PARAMETERS), omi / 2 / MI, PARAMNUM)
    OM <- array(0, dim = c(1, length(PARAMETERS), 1))

    # Loop over the NUMCURVES search curves.
    for (CURVENUM in 1:NUMCURVES) {
      # Setting the vector of frequencies OM
      # for the k parameters
      cj <- 1
      for (j in 1:length(PARAMETERS)) {
        if (j == PARAMNUM) {
          # For the parameter (factor) of interest
          # RECODE WHEN WORKED OUT OM ARRAY
          OM[PARAMNUM] <- omi
        } else {
          # For the complementary group.
          # RECODE WHEN WORKED OUT ARRAY
          OM[j] <- omci[cj]
          cj <- cj + 1
        }
      }

      # Setting the relation between the scalar
      # variable S and the coordinates
      # {X(1),X(2),...X(k)} of each sample point.
      FI <- array(runif(length(PARAMETERS), min = 0, max = 1),
                  dim = c(length(PARAMETERS), 1, 1))
      FI <- FI * 2 * pi

      S_VEC <- pi * (2 * (1:NUMSAMPLES) - NUMSAMPLES - 1) / NUMSAMPLES
      OM_VEC <- OM[1:length(PARAMETERS)]

      FI_MAT <- array(0, dim = c(length(PARAMETERS), NUMSAMPLES, 1))

      for (i in 1:NUMSAMPLES) {
        FI_MAT[, i, 1] <- FI
      }

      # FORMULA IN ORIGINAL MATLAB CODE:
      #ANGLE = OM_VEC'*S_VEC+FI_MAT;
      # CONVERSION TO R:
      om_vec_svec <- array(OM_VEC %*% t(S_VEC),
                           dim = c(length(PARAMETERS), NUMSAMPLES, 1))
      ANGLE <- om_vec_svec + FI_MAT

      # TRANSPOSE ARRAY
      ANGLET <- array(0, dim = c(NUMSAMPLES, length(PARAMETERS), 1))
      for (i in 1:NUMSAMPLES) {
        ANGLET[i, , 1] <- ANGLE[, i, 1]
      }

      # NOW CALCULATE THE PARAMETER VALUES - THESE ARE STORED IN A
      # MULTIDIMENSIONAL ARRAY, AS EACH CURVE HAS SEVEN SETS OF PARAMETER
      # VALUES
      PARAMETERVALS[, , PARAMNUM, CURVENUM] <- 0.5 + asin(sin(ANGLET)) / pi

      # AS THESE VALUES WILL CURRENTLY BE BETWEEN 0 AND 1, TRANSFORM THE
      # DISTRIBUTION TO GIVE TRUE PARAMETER VALUES
      PARAMETERVALS[, , PARAMNUM, CURVENUM] <- efast_parameterdist(
        PARAMETERVALS[, , PARAMNUM, CURVENUM], PMAX, PMIN, NUMSAMPLES,
        length(PARAMETERS))
    }
  }

  return(PARAMETERVALS)
}

#' Output the generated parameter sets for each curve
#'
#' @param FILEPATH Directory where the parameter samples should be output to
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated.  For eFAST, remember to add a
#' parameter named 'Dummy'
#' @param PARAMETERVALS The parameter sets generated by the eFAST sampling method
output_param_sets_per_curve <- function(FILEPATH, NUMCURVES, PARAMETERS, PARAMETERVALS)
{
  for (CURVENUM in 1:NUMCURVES) {
    for (PARAMNUM in 1:length(PARAMETERS)) {
      parameter_file <- paste(FILEPATH, "/Curve", CURVENUM, "_",
                              PARAMETERS[PARAMNUM], ".csv", sep = "")
      output_params <- PARAMETERVALS[, , PARAMNUM, CURVENUM]
      colnames(output_params) <- c(PARAMETERS)

      write.csv(output_params, parameter_file, quote = FALSE,
                row.names = FALSE)

      message(paste("Parameter Set for ", CURVENUM,
                  " Generated and Output to ", FILEPATH, "/Curve", CURVENUM,
                  "_", PARAMETERS[PARAMNUM], ".csv", sep = ""))
    }
  }
}
