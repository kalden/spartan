#' Internal function used in calculating the partitions of variance in eFAST
#' @param si Amount of variance partitioned to each parameter
#' @param range_si Range of the Si values across all curves
#' @param sti Amount of variance accounted for by non-linear effects with other
#' parameters, for each parameter
#' @param range_sti Range of the STi values across all curves
#' @param OUTMEASURE Simulation output measure being analysed
#' @param NUMPARAMS Number of parameters in the simulation
#' @param NUMCURVES Number of resample curves being examined
#' @param NUMOUTMEASURES Number of simulation output measures in total
#'
#' @keywords internal
efast_cvmethod <- function(si,  range_si,  sti,  range_sti,  OUTMEASURE,
                           NUMPARAMS, NUMCURVES, NUMOUTMEASURES) {

  mean_si <- array(0, dim = c(NUMPARAMS, NUMOUTMEASURES, 1))
  mean_sti <- array(0, dim = c(NUMPARAMS, NUMOUTMEASURES, 1))
  std_si <- array(0, dim = c(NUMPARAMS, NUMOUTMEASURES, 1))
  std_sti <- array(0, dim = c(NUMPARAMS, NUMOUTMEASURES, 1))
  error_si <- array(0, dim = c(NUMPARAMS, 1))
  error_sti <- array(0, dim = c(NUMPARAMS, 1))

  if (NUMOUTMEASURES == 1) {
    for (PARAM in 1:NUMPARAMS) {
      mean_si[PARAM, OUTMEASURE, 1] <- mean(range_si[PARAM, , OUTMEASURE])
      mean_sti[PARAM, OUTMEASURE, 1] <- mean(range_sti[PARAM, , OUTMEASURE])

      std_si[PARAM, OUTMEASURE, 1] <- (sd(range_si[PARAM, , OUTMEASURE]))
      std_sti[PARAM, OUTMEASURE, 1] <- (sd(range_sti[PARAM, , OUTMEASURE]))

      error_si[PARAM, 1] <- sd(range_si[PARAM, , OUTMEASURE]) /
        sqrt(length(range_si[PARAM, , OUTMEASURE]))
      error_sti[PARAM, 1] <- sd(range_sti[PARAM, , OUTMEASURE]) /
        sqrt(length(range_sti[PARAM, , OUTMEASURE]))
    }
    a <- si[, OUTMEASURE, 1] / mean_si[, OUTMEASURE, 1]
    b <- sti[, OUTMEASURE, 1] / mean_sti[, OUTMEASURE, 1]
  } else {
    for (PARAM in 1:NUMPARAMS) {
      mean_si[PARAM, OUTMEASURE, 1] <- drop(mean(range_si[PARAM, , OUTMEASURE]))
      mean_sti[PARAM, OUTMEASURE, 1] <- drop(mean(
        range_sti[PARAM, , OUTMEASURE]))

      std_si[PARAM, OUTMEASURE, 1] <- drop(sd(range_si[PARAM, , OUTMEASURE]))
      std_sti[PARAM, OUTMEASURE, 1] <- drop(sd(range_sti[PARAM, , OUTMEASURE]))

      error_si[PARAM, 1] <- sd(range_si[PARAM, , OUTMEASURE]) /
        sqrt(length(range_si[PARAM, , OUTMEASURE]))
      error_sti[PARAM, 1] <- sd(range_sti[PARAM, , OUTMEASURE]) /
        sqrt(length(range_sti[PARAM, , OUTMEASURE]))
    }

    a <- std_si[, OUTMEASURE, 1] / mean_si[, OUTMEASURE, 1]
    b <- std_sti[, OUTMEASURE, 1] / mean_sti[, OUTMEASURE, 1]
  }

  cv_si <- 100 * t(a)
  cv_sti <- 100 * t(b)

  return(list("error_si" = error_si, "error_sti" = error_sti, "cv_si" = cv_si,
              "cv_sti" = cv_sti))
}

#' Internal function used in calculating the partitions of variance in eFAST
#' @param PARAMVALS Values under which parameters have been examined
#' @param PMAX Max sample values of all parameters
#' @param PMIN Min sample values of all parameters
#' @param NUMSAMPLES Number of samples to take from the sinusoidal curves
#' @param NUMPARAMS Number of parameters being analysed
#'
#' @keywords internal
efast_parameterdist <- function(PARAMVALS, PMAX, PMIN, NUMSAMPLES, NUMPARAMS) {
  # NOTE NO CHANGE IN TYPE HAS BEEN IMPLEMENTED HERE - ASSUMES UNIFORM
  # DISTRIBUTION IS REQUIRED. SALTELLI ET AL's MATLAB CODE DOES HOWEVER
  # ALLOW FOR NORMAL OR LOGNORMAL DISTRIBUTIONS LOOP THROUGH THE PARAMETERS
  for (PARAM in 1:NUMPARAMS) {

    ran <- array(runif (NUMSAMPLES, min = 0, max = 1),
                 dim = c(NUMSAMPLES, 1, 1))

    # NOW MULTIPLY THE VALUE THAT IS BETWEEN 0 AND 1 BY SUM OF PARAM (MAX-MIN)
    # + MIN TO GET TRUE PARAM VALUE
    PARAMVALS[, PARAM] <- PARAMVALS[, PARAM] * (PMAX[PARAM] - PMIN[PARAM]) +
      PMIN[PARAM]
  }

  return(PARAMVALS)
}

#' Internal function used in calculating the partitions of variance in eFAST
#'
#' @keywords internal
efast_sd <- function(RESULTSARRAY, omi, MI, OUTMEASURES, NUMPARAMS, NUMCURVES) {
  # RESULTSMAT FORMAT - FIRST: PARAM SET NUMBER,
  # 2ND: TIMEPOINT (NOT USED BUT IS IN EXAMPLE),
  # 3RD: RESULT VALUE ARRAY, 4TH:PARAMETER, 5TH: CURVE

  si <- array(0, dim = c(NUMPARAMS, 1, OUTMEASURES))
  sti <- array(0, dim = c(NUMPARAMS, 1, OUTMEASURES))
  range_si <- array(0, dim = c(NUMPARAMS, NUMCURVES, OUTMEASURES))
  range_sti <- array(0, dim = c(NUMPARAMS, NUMCURVES, OUTMEASURES))
  vci <- array(0, dim = c(1, NUMCURVES, 1))
  vi <- array(0, dim = c(1, NUMCURVES, 1))
  V <- array(0, dim = c(1, NUMCURVES, 1))

  for (MEASURE in 1:OUTMEASURES) {
    for (PARAMNUM in 1:NUMPARAMS) {
      # Initialize av, avi, avci to zero.
      # THOUGH THESE SEEM TO BE HIGHLIGHTED OUT OF THE ORIGINAL
      #av <- 0
      #avi <- 0
      #avci <- 0

      for (CURVENUM in 1:NUMCURVES) {
        # GET THE RESULTS FOR THIS CURVE,  FOR THIS PARAMETER,
        # FOR THIS MEASURE. THEN SUBTRACT THE MEAN OF THE COLUMN
        # FROM THE OUTPUT VALUE
        MEASURE_RESULTS_FOR_PARAM <-
          na.omit(RESULTSARRAY[, (((PARAMNUM * OUTMEASURES) - OUTMEASURES) +
                                    MEASURE), CURVENUM])
        MEASURE_RESULTS_FOR_PARAM <- MEASURE_RESULTS_FOR_PARAM -
          as.vector(mean(MEASURE_RESULTS_FOR_PARAM))

        # Fourier coeff. at [1:omi / 2].
        # GET THE NUMBER OF SAMPLES FOR THIS OUTPUT
        N <- length(MEASURE_RESULTS_FOR_PARAM)

        # NQ GOES JUST BELOW THE MIDPOINT
        NQ <- (N - 1) / 2
        # NO GOES JUST ABOVE THE MIDPOINT
        N0 <- NQ + 1

        Y_VECP <- MEASURE_RESULTS_FOR_PARAM[N0 + (1:NQ)]  +
          MEASURE_RESULTS_FOR_PARAM[N0 - (1:NQ)]

        Y_VECM <- MEASURE_RESULTS_FOR_PARAM[N0 + (1:NQ)] -
          MEASURE_RESULTS_FOR_PARAM[N0 - (1:NQ)]

        AC <- array(0, dim = c(1, 4, 1))
        BC <- array(0, dim = c(1, 4, 1))

        COMPL <- 0

        range_j <- omi / 2
        for (j in 1:range_j) {
          ANGLE <- (j * 2 * (1:NQ) * pi / N)
          C_VEC <- cos(ANGLE)
          S_VEC <- sin(ANGLE)
          AC[j] <- (MEASURE_RESULTS_FOR_PARAM[N0]  +
                      t(Y_VECP) %*% C_VEC) / N
          BC[j] <- t(Y_VECM) %*% S_VEC / N
          COMPL <- COMPL + AC[j] ^ 2 + BC[j] ^ 2
        }

        # Computation of V_{(ci)}.
        vci[CURVENUM] <- 2 * COMPL

        # Fourier coeff. at [P * omi,  for P=1:MI].
        COMPL <- 0

        Y_VECP <- MEASURE_RESULTS_FOR_PARAM[N0 + (1:NQ)]  +
          MEASURE_RESULTS_FOR_PARAM[N0 - (1:NQ)]

        Y_VECM <- MEASURE_RESULTS_FOR_PARAM[N0 + (1:NQ)] -
          MEASURE_RESULTS_FOR_PARAM[N0 - (1:NQ)]

        for (i in seq(omi, omi * MI, omi)) {
          ANGLE <- i * 2 * (1:NQ) * pi / N
          C_VEC <- cos(ANGLE)
          S_VEC <- sin(ANGLE)
          AC[j] <- (MEASURE_RESULTS_FOR_PARAM[N0]  +  t(Y_VECP) %*%
                      C_VEC) / N
          BC[j] <- t(Y_VECM) %*% S_VEC / N
          COMPL <- COMPL + AC[j] ^ 2 + BC[j] ^ 2
        }

        # Computation of V_i.
        vi[CURVENUM] <- 2 * COMPL
        # Computation of the total variance in the time domain.
        V[CURVENUM] <- t(MEASURE_RESULTS_FOR_PARAM) %*%
          MEASURE_RESULTS_FOR_PARAM / N
      }	# END CURVE NUMBER LOOP

      # CALCULATE SENSITIVITY INDEXES
      si[PARAMNUM, 1, MEASURE] <- mean(vi) / mean(V)
      sti[PARAMNUM, 1, MEASURE] <- 1 - mean(vci) / mean(V)
      range_si[PARAMNUM, , MEASURE] <- vi / V
      range_sti[PARAMNUM, , MEASURE] <- 1 - ( vci / V)

      if (is.nan(si[PARAMNUM, 1, MEASURE]))
        si[PARAMNUM, 1, MEASURE] <- 0

      if (is.nan(sti[PARAMNUM, 1, MEASURE]))
        sti[PARAMNUM, 1, MEASURE] <- 0

      for (i in seq(1:length(range_si[PARAMNUM, , MEASURE]))) {
        if (is.nan(range_si[PARAMNUM, i, MEASURE]))
          range_si[PARAMNUM, i, MEASURE] <- 0
      }
      for (i in seq(1:length(range_sti[PARAMNUM, , MEASURE]))) {
        if (is.nan(range_sti[PARAMNUM, i, MEASURE]))
          range_sti[PARAMNUM, i, MEASURE] <- 0
      }
    } # END PARAMNUM
  }

  # THE si,  sti,  RANGESi,  AND RANGESTi ARRAYS ARE RETURNED AS A LIST
  return(list("si" = si, "sti" = sti, "range_si" = range_si,
              "range_sti" = range_sti))
}

#' Internal function used in calculating the partitions of variance in eFAST
#'
#' @keywords internal
efast_setfreq <- function(NUMPARAMS ,omci_max, PARAMNUM) {

  omci <- array(0, dim = c(1, NUMPARAMS, 1))

  if (NUMPARAMS == 1) {
    omci <- 1
  } else if (omci_max == 1) {
    omci <- sample(1:1, NUMPARAMS, replace = TRUE)
  } else {
    if (omci_max < NUMPARAMS) {
      INFD <- omci_max
    } else {
      INFD <- NUMPARAMS
    }

    ISTEP <- round( (omci_max - 1) / (INFD - 1))

    if (omci_max == 1)
      ISTEP <- 0

    OTMP <- 1:ISTEP:INFD * ISTEP

    fl_INFD <- floor(INFD)

    for (i in 1:NUMPARAMS) {
      j <- (i - 1 %% fl_INFD) + 1
      omci[i] <- OTMP[j]
    }
  }
  return(omci)
}

#' Internal function used in calculating the partitions of variance in eFAST
#'
#' @keywords internal
efast_ttest <- function(si, range_si, sti, range_sti, OUTPUTMEASURES_TO_TTEST,
                        NUMPARAMS, NUMCURVES, TTEST_CONF_INT) {

  # ARRAY FOR P-VALUES FOR THE si COMPONENT
  p_si <- array(0, dim = c(NUMPARAMS, 1, length(OUTPUTMEASURES_TO_TTEST)))
  # ARRAY FOR P-VALUES FOR THE sti COMPONENT
  p_sti <- array(0, dim = c(NUMPARAMS, 1, length(OUTPUTMEASURES_TO_TTEST)))

  # DO ALL THE OUTPUT MEASURES REQUESTED IN THE OUTPUTMEASURES_TO_TTEST RANGE
  # SHOULD BE SOMETHING LIKE 1:3 (i.e. TO EXAMINE THE OUTPUT MEASURES 1 2 AND 3
  for (OUTMEASURE in seq(OUTPUTMEASURES_TO_TTEST)) {
    # Compare si or sti of parameter j with the dummy

    for (PARAM in 1:(NUMPARAMS - 1)) {
      # MINUS ONE, COMPARISONS ARE BEING DONE AGAINST THE DUMMY
      # si done first
      ttest_res <- t.test(range_si[PARAM, , OUTMEASURE],
                         range_si[NUMPARAMS, , OUTMEASURE],
                         alternative = "greater",
                         var.equal = FALSE, conf.level = TTEST_CONF_INT)

      # put in array of p values
      p_si[PARAM, , OUTMEASURE] <- ttest_res$p.value

      # NOW DO STI
      ttest_res <- t.test(range_sti[PARAM, , OUTMEASURE],
                         range_sti[NUMPARAMS, , OUTMEASURE],
                         alternative = "greater",
                         var.equal = FALSE, conf.level = TTEST_CONF_INT)

      # put in array of values
      p_sti[PARAM, , OUTMEASURE] <- ttest_res$p.value

    }
    # SO THAT THE p_si and p_sti arrays can be output with the rest,
    # these need to be the same size.
    # At the moment these do not include the dummy,
    # making this difficult - so add a Null to make the lengths match
    p_si[NUMPARAMS, , OUTMEASURE] <- 0
    p_sti[NUMPARAMS, , OUTMEASURE] <- 0
  }

  return(list("p_si" = p_si, "p_sti" = p_sti))
}
