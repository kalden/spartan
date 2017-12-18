#' Plot the parition of variance in a simulation response for each measure
#'
#' @param RESULTS_FILE_PATH Where the eFAST results were saved to
#' @param PARAMETERS Simulation parameters being explored
#' @param si Vector of Si values calculated in eFAST for all parameters
#' @param sti Vector of STi values calculated in eFAST for all parameters
#' @param errors_si Vector of confidence intervals for Si values for all
#' parameters
#' @param errors_sti Vector of confidence intervals for STi values for all
#' parameters
#' @param MEASURES Simulation output measures
#' @param TIMEPOINT Timepoint being analysed
#' @param TIMEPOINTSCALE Scale in which the timepoints are measures
#'
#' @export
efast_graph_Results <- function(RESULTS_FILE_PATH, PARAMETERS, si, sti,
                                errors_si, errors_sti, MEASURES, TIMEPOINT,
                                TIMEPOINTSCALE) {
  if (requireNamespace("gplots", quietly = TRUE)) {

    colors <- c("black", "grey50")

    for (MEASURE in seq(length(MEASURES))) {

      if (is.null(TIMEPOINT)) {
        GRAPHFILE <- paste(RESULTS_FILE_PATH, "/", MEASURES[MEASURE], ".pdf",
                           sep = "")
        GRAPHTITLE <- paste("Partitioning of Variance in Simulation Results
                            Measure: ", MEASURES[MEASURE],
                            sep = "")
      } else {
        GRAPHFILE <- paste(RESULTS_FILE_PATH, "/", MEASURES[MEASURE], "_",
                           TIMEPOINT, ".pdf", sep = "")
        GRAPHTITLE <- paste("Partitioning of Variance in Simulation Results
                            Measure: ", MEASURES[MEASURE],
                            ". Timepoint: ", TIMEPOINT, " ",
                            TIMEPOINTSCALE, sep = "")
      }

      pdf(GRAPHFILE)
      labelspacing <- seq(2, (length(PARAMETERS) * 3), 3)

      # DATA TO GRAPH RETRIEVES THE PARAMETERS,
      # si AND sti TO BE GRAPHED FROM THE MAIN RESULT SET
      data_to_graph <- data.frame(cbind(si[, , MEASURE], sti[, , MEASURE]),
                                check.names = FALSE)

      # CONSTRUCT THE ERROR BAR
      high_si <- data_to_graph[, 1] + errors_si[, MEASURE]
      high_sti <- data_to_graph[, 2] + errors_sti[, MEASURE]
      # COMBINE
      errors_high <- cbind(high_si, high_sti)

      colnames(data_to_graph) <- c("Si", "STi")
      par(mar = c(9, 4, 4, 2) + 0.1)
      gplots::barplot2(t(data_to_graph), names.arg = PARAMETERS, beside = TRUE,
                       main = GRAPHTITLE,
                       ylim = c(0, 1.0),
                       ylab = "eFAST Sensitivity", col = colors, xaxt = "n",
                       plot.ci = TRUE, ci.u = t(errors_high),
                       ci.l = t(data_to_graph))

      # TEXT SIZE CONTROLLED BY CEX.AXIS
      axis(1, at = labelspacing, labels = PARAMETERS, las = 2, cex.axis = 0.6)
      legend("topleft", title = NULL, c("Si", "STi"), fill = colors)

      dev.off()
    }
    message(paste("Graphs Output to ", RESULTS_FILE_PATH, sep = ""))
  }
}

#' Plot the Si value for all parameters for multiple simulation timepoints
#'
#' Permits easy comparison of when a parameter may become more influential
#' than others throughout a simulation timecourse
#'
#' @param FILEPATH Where the eFAST results have been stored
#' @param PARAMETERS Names of simulation parameters being explored
#' @param MEASURES Names of simulation output responses
#' @param EFASTRESULTFILENAME Name of the CSV file output by eFAST Analysis,
#' containing all the Si and STi values
#' @param TIMEPOINTS Timepoints to include in this analysis
#' @param TIMEPOINTSCALE Scale in which the timepoints are measured
#'
#' @export
ploteFASTSiFromTimepointFiles <- function(FILEPATH, PARAMETERS, MEASURES,
                                          EFASTRESULTFILENAME, TIMEPOINTS,
                                          TIMEPOINTSCALE) {
  for (m in 1:length(MEASURES)) {
    MEASURE <- MEASURES[m]
    # Add si onto the measure to get this from the result set
    MEASURELABEL <- paste(MEASURE, "_Si", sep = "")

    si_measureresults <- data.frame()


    for (i in 1:length(TIMEPOINTS)) {
      hour <- TIMEPOINTS[i]

      # Add the timepoint onto the end of the filename
      efastresultfilename_format <- check_file_extension(EFASTRESULTFILENAME)
      EFASTRESULTFILENAME_FULL <- paste(substr(EFASTRESULTFILENAME, 0,
                                             nchar(EFASTRESULTFILENAME) - 4),
                                      "_", hour, ".",
                                      efastresultfilename_format, sep = "")


      # READ IN THE TIMEPOINT DATA
      efast_results <- read.csv(paste(FILEPATH, "/", EFASTRESULTFILENAME_FULL,
                                     sep = ""), header = T)

      TIMERESULT <- data.frame(hour, t(efast_results[, MEASURELABEL]))
      si_measureresults <- rbind(si_measureresults, TIMERESULT)
    }

    colnames(si_measureresults) <- c(TIMEPOINTSCALE, PARAMETERS)

    # PLOT THE GRAPH
    GRAPHFILE <- paste(FILEPATH, "/", MEASURE, "_OT.pdf", sep = "")
    pdf(GRAPHFILE, width = 7, height = 7.8)

    GRAPHTITLE <- paste("eFAST First Order Sensitivity Indexes Over Simulation
                        Time\nCell Response Measure: ", MEASURE, sep = "")

    plot(TIMEPOINTS, si_measureresults[, 2], main = GRAPHTITLE, type = "o",
         lty = 1, ylim = c(0, 1), pch = 1, xaxt = "n", xlab = TIMEPOINTSCALE,
         ylab = "eFAST First-Order Sensitivity Index (Si)")

    # -1 TO EXCLUDE DUMMY
    for (l in 2:length(PARAMETERS) - 1) {
      lines(TIMEPOINTS, si_measureresults[, l + 1], type = "o", lty = 5,
            pch = l)
    }

    axis(1, at = seq(as.numeric(min(TIMEPOINTS)), as.numeric(max(TIMEPOINTS)),
                     by = as.numeric(max(TIMEPOINTS)) / length(TIMEPOINTS)))
    legend("topleft", inset = .0, title = "Parameter",
           PARAMETERS[1:length(PARAMETERS) - 1],
           pch = 1:length(PARAMETERS) - 1, cex = 0.75)

    dev.off()
  }
}
