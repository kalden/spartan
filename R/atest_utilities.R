#' Calculates the A-test score for two distributions
#'
#' @param x,y  distributions of simulation results
#' @return A-Test score
#'
atest <- function(x, y) {
  # Calculate the RankSum required to perform the A-Test.
  ranksum <- sum(rank(c(x, y))[1:length(x)])

  # Now work out the A-test Value
  return((ranksum / length(x) - (length(x) + 1) / 2) / length(y))
}

#' Normalises the A-Test such that it is above 0.5
#'
#' @param result A-Test score to be normalised
#' @return Normalised A-Test score
normaliseATest <- function(result) {
  if (result < 0.5)
    result <- 1 - result
  return(result)
}

#' Diagnostic function used to determine number of decimal places
#'
#' @param x Numeric value to examine
#' @return Number of decimal places
num.decimals <- function(x) {
  stopifnot(class(x) == "numeric")
  x <- sub("0+$", "", x)
  x <- sub("^.+[.]", "", x)
  nchar(x)
}

#' Plots the A-Tests for all timepoints being examined
#'
#' When plotting a time-course analysis, it may be useful to compare results
#' gained at multiple timepoints, and determine the differences in
#' performance over time. This function provides a means of plotting those
#' results
#'
#' @inheritParams aa_summariseReplicateRuns
#' @inheritParams aa_getATestResults
#' @param PARAMETERS Array containing the names of the parameters of which parameter samples will be generated
#' @param ATESTRESULTFILENAME Name of the CSV file containing the A-Test results to be plotted
#' @param ATESTSIGLEVEL Value to plot for a large difference between distributions on this plot
#' @param PMIN Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space
#' @param PINC Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10
#'
#' @export
plotATestsFromTimepointFiles <- function(FILEPATH, PARAMETERS,
                                         ATESTRESULTFILENAME, ATESTSIGLEVEL,
                                         MEASURES, PMIN, PMAX, PINC,
                                         TIMEPOINTS) {
  for (PARAM in 1:length(PARAMETERS)) {

    FULLPARAMRESULTS <- data.frame()
    CNAME <- NULL

    for (i in 1:length(TIMEPOINTS)) {
      hour <- TIMEPOINTS[i]

      result_file <- make_extension(make_path
                                    (c(FILEPATH, PARAMETERS[PARAM],
                                       make_filename(
                                         c(ATESTRESULTFILENAME, hour)))),
                                    "csv")

      OATResults <- read.csv(result_file, header = T)

      # ADD THE PARAMETER VALUES
      if (ncol(FULLPARAMRESULTS) == 0) {
        FULLPARAMRESULTS <- OATResults[1]
        CNAME <- c("parametervalue")
      }

      # Add the result for all measures
      for (m in 1:length(MEASURES)) {
        FULLPARAMRESULTS <- cbind(FULLPARAMRESULTS,
                                  OATResults[paste("ATest",
                                                   MEASURES[m],
                                                   sep = "")])
        CNAME <- cbind(CNAME, paste(MEASURES[m], "_", hour,
                                    sep = ""))
      }
    }

    colnames(FULLPARAMRESULTS) <- CNAME

    # Now produce the A-Tests timepoints plot for all measures

    for (j in 1:length(MEASURES)) {
      # PLOT EACH TIMEPOINT.
      GRAPHFILE <- paste(FILEPATH, "/", PARAMETERS[PARAM], "_", MEASURES[j],
                         ".pdf", sep = "")
      pdf(GRAPHFILE, width = 7, height = 7.2)

      MEASURELABEL <- paste(MEASURES[j], "_", TIMEPOINTS[1], sep = "")

      GRAPHTITLE <- paste("One-A-Time Parameter Analysis Over Simulation
                          Time\nParameter: ", PARAMETERS[PARAM], ", Measure: ",
                          MEASURES[j], sep = "")

      plot(FULLPARAMRESULTS$parametervalue, FULLPARAMRESULTS[, MEASURELABEL],
           type = "o", main = GRAPHTITLE, lty = 1, ylim = c(0, 1), pch = 1,
           xlab = "Parameter Value", ylab = "A Test Score", xaxt = "n")

      # NOW ADD THE REST
      for (l in 2:length(TIMEPOINTS)) {
        MEASURELABEL <- paste(MEASURES[j], "_", TIMEPOINTS[l], sep = "")
        lines(FULLPARAMRESULTS$parametervalue,
              FULLPARAMRESULTS[, MEASURELABEL],
              type = "o", lty = 5,
              pch = l)
      }

      # Add the x axis
      axis(1, at = seq(PMIN[PARAM], PMAX[PARAM], by = PINC[PARAM]))

      # legend
      legend("topleft", inset = .0, title = "Timepoints",
             TIMEPOINTS, pch = 1:length(TIMEPOINTS), cex = 0.75)

      par(xpd = FALSE)

      # Lines for A-Test scores
      abline(a = 0.5, b = 0, lty = 4)
      text( (PMAX[PARAM] + PMIN[PARAM]) / 2, 0.52,
           "no difference", col = "blue")
      abline(a = (0.5 + ATESTSIGLEVEL), b = 0, lty = 4)
      text( (PMAX[PARAM] + PMIN[PARAM]) / 2, (0.5 + ATESTSIGLEVEL + 0.02),
           "large difference", col = "blue")
      abline(a = (0.5 - ATESTSIGLEVEL), b = 0, lty = 4)
      text( (PMAX[PARAM] + PMIN[PARAM]) / 2, (0.5 - ATESTSIGLEVEL - 0.02),
           "large difference", col = "blue")
      dev.off()
    }
  }
}

#' Performs A-Test to compare all simulation measures
#'
# This takes a set of parameters on which a simulation was run, the results
#' for this set, the simulation baseline behaviour, and performs the A-Test
#' to get a comparison for all simulation measures
#'
#' @param PARAMETER_SET Set of simulation parameters for which a set of runs was performed
#' @param BASELINE_RESULT Simulation behaviour under baseline conditions
#' @param PARAMETER_SET_RESULT Simulation behaviour under conditions in this parameter set
#' @param MEASURES An array containing the names of the simulation output measures to be analysed.
#'
#'  @export
perform_aTest_for_all_sim_measures <- function(PARAMETER_SET, BASELINE_RESULT,
                                               PARAMETER_SET_RESULT, MEASURES) {


  ATESTRESULTROW <- t(PARAMETER_SET)

  for (MEASURE in 1:length(MEASURES)) {
    ATESTMEASURERESULT <- atest(as.numeric
                                (as.matrix(
                                  BASELINE_RESULT[MEASURES[MEASURE]])),
                              as.numeric
                              (as.matrix(
                                PARAMETER_SET_RESULT[MEASURES[MEASURE]][, 1])))

    ATESTNORM <- normaliseATest(ATESTMEASURERESULT)

    # ADD TO THE SET OF RESULTS
    ATESTRESULTROW <- cbind(ATESTRESULTROW, ATESTMEASURERESULT, ATESTNORM)
  }

  return(ATESTRESULTROW)
}
