#' Generates parameter/measure plot for each pairing in the analysis
#'
#' Produces a graph for each parameter, and each output measure, showing
#' the simulation output achieved when that parameter was assigned that value.
#' Eases identification of any non-linear effects.
#'
#' @inheritParams lhc_generateLHCSummary
#' @param MEASURE_SCALE Scale in which each of the output responses is
#' measured. Used to label plots
#' @param CORCOEFFSOUTPUTFILE File produced by spartan containing the Partial
#' Rank Correlation Coefficients for each parameter/measure pairing
#' @param OUTPUT_TYPE Type of graph to plot. Can be PDF, PNG, TIFF, BMP, etc,
#'  all formats supported by ggplot2
#' @param GRAPHTIME The timepoint being processed, if any. NULL if not.
#'
#' @export
#'
lhc_graphMeasuresForParameterChange <-
  function(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CORCOEFFSOUTPUTFILE,
           LHCSUMMARYFILENAME, OUTPUT_TYPE = c("PDF"), TIMEPOINTS = NULL,
           TIMEPOINTSCALE = NULL, GRAPHTIME = NULL) {

  if (is.null(TIMEPOINTS)) {
    if (file.exists(FILEPATH)) {
      # LHCSUMMARYFILENAME IS LHCSummary.csv FOR 1 TIMEPOINT
      # CORCOEFFSOUTPUTFILE IS corCoefs.csv FOR 1 TIMEPOINT
      if (file.exists(paste(FILEPATH, "/", CORCOEFFSOUTPUTFILE, sep = ""))) {

        CORCOEFFS <- read.csv(paste(FILEPATH, "/", CORCOEFFSOUTPUTFILE,
                                    sep = ""),
                              header = TRUE, check.names = FALSE)

        if (file.exists(paste(FILEPATH, "/", LHCSUMMARYFILENAME, sep = ""))) {
          LHCRESULTFILE <- read.csv(paste(FILEPATH, "/", LHCSUMMARYFILENAME,
                                          sep = ""),
                                    header = TRUE, check.names = FALSE)

          print("Generating output graphs for LHC Parameter Analysis")
          print("(lhc_graphMeasuresForParameterChange)")

          # CREATE A GRAPH FOR EACH PARAMETER, FOR EACH MEASURE
          for (p in 1:length(PARAMETERS)) {
            for (m in 1:length(MEASURES)) {
              # CREATE LABELS
              y_label <- paste("Median Value Across Runs (", MEASURE_SCALE[m],
                              ")", sep = "")
              x_label <- "Parameter Value"
              # CREATE CORRELATION LABEL FOR ABOVE GRAPH
              correlation_lab <- paste(MEASURES[m], "_Estimate", sep = "")
              # GET THE CORRELATION FIGURE
              corr_result <- CORCOEFFS[p, correlation_lab]

              GRAPHTITLE <- paste("LHC Analysis for Parameter: ",
                                  PARAMETERS[p], sep = "")
              if (is.null(GRAPHTIME)) {
                GRAPHFILE <- paste(FILEPATH, "/", PARAMETERS[p], "_",
                                   MEASURES[m], sep = "")
                SUBTITLE <- paste("Measure: ", MEASURES[m],
                                  "\nCorrelation Coefficient: ",
                                  toString(signif(corr_result, 3)),
                                  sep = "")
              } else {
                GRAPHFILE <- paste(FILEPATH, "/", PARAMETERS[p], "_",
                                   MEASURES[m], "_", GRAPHTIME, sep = "")
                SUBTITLE <- paste("Measure: ", MEASURES[m], ". Timepoint: ",
                                  GRAPHTIME, " ", TIMEPOINTSCALE,
                                  "\n Correlation Coefficient: ",
                                  toString(signif(corr_result, 3)), sep = "")
              }

              # GGPLOT replacement in spartan 3.0
              data_to_plot <- data.frame(LHCRESULTFILE[, PARAMETERS[p]],
                                       LHCRESULTFILE[, MEASURES[m]])

              output_graph <- ggplot(data_to_plot,
                                    aes(x = data_to_plot[, 1],
                                        y = data_to_plot[, 2])) +
                geom_point(size = 0.5) +
                scale_y_continuous(limits = c(
                  0, ceiling(max(data_to_plot[, 2])))) +
                labs(x = x_label, y = y_label,
                     title = GRAPHTITLE, subtitle = SUBTITLE) +
                theme(axis.title = element_text(size = 7),
                      axis.text = element_text(size = 7),
                      plot.title = element_text(size = 9, hjust = 0.5),
                      plot.subtitle = element_text(size = 8, hjust = 0.5))

              for (file_type in 1:length(OUTPUT_TYPE)) {
                # Save the graphs in the requested format
                if (OUTPUT_TYPE[file_type] == "PDF") {
                  ggsave(paste(GRAPHFILE, ".pdf", sep = ""),
                         plot = output_graph, width = 4, height = 4)
                } else if (OUTPUT_TYPE[file_type] == "PNG") {
                  ggsave(paste(GRAPHFILE, ".png", sep = ""),
                         plot = output_graph, width = 4, height = 4)
                } else if (OUTPUT_TYPE[file_type] == "TIFF") {
                  ggsave(paste(GRAPHFILE, ".tiff", sep = ""),
                         plot = output_graph, width = 4, height = 4)
                } else if (OUTPUT_TYPE[file_type] == "BMP") {
                  ggsave(paste(GRAPHFILE, ".bmp", sep = ""),
                         plot = output_graph, width = 4, height = 4)
                }
              }
            }
          }
          print("LHC Graphs Complete")
        } else {
          print("Cannot find LHC Summary File.
                Are you sure you have run the method to generate it?")
        }
      } else {
        print("Cannot find Partial Rank Correlation Coefficients File.
              Are you sure you have run the method to generate it?")
      }
    } else {
      print("The directory specified in FILEPATH does not exist.
            No Output Graphs Generated")
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {
      current_time <- TIMEPOINTS[n]
      print(paste("PROCESSING TIMEPOINT: ", current_time, sep = ""))

      CORCOEFFSOUTPUTFILE_FORMAT <- check_file_extension(CORCOEFFSOUTPUTFILE)
      CORCOEFFSOUTPUTFILE_FULL <- paste(substr(CORCOEFFSOUTPUTFILE, 0,
                                               nchar(CORCOEFFSOUTPUTFILE) - 4),
                                        "_", current_time, ".",
                                        CORCOEFFSOUTPUTFILE_FORMAT, sep = "")

      LHCSUMMARYFILENAME_FORMAT <- check_file_extension(LHCSUMMARYFILENAME)
      LHCSUMMARYFILENAME_FULL <- paste(substr(LHCSUMMARYFILENAME, 0,
                                              nchar(LHCSUMMARYFILENAME) - 4),
                                       "_", current_time, ".",
                                       LHCSUMMARYFILENAME_FORMAT, sep = "")

      lhc_graphMeasuresForParameterChange(FILEPATH, PARAMETERS, MEASURES,
                                          MEASURE_SCALE,
                                          CORCOEFFSOUTPUTFILE_FULL,
                                          LHCSUMMARYFILENAME_FULL,
                                          TIMEPOINTS = NULL,
                                          TIMEPOINTSCALE = TIMEPOINTSCALE,
                                          GRAPHTIME = current_time)

    }
  }
}

#' Count number of significant (p<0.01) parameters over a timecourse
#'
#' @param FILEPATH Where pre-processed P-Value dataset is found
#' @param MEASURES Simulation output responses vector
#' @param TIMEPOINTS Timepoints to analyse
#'
#' @importFrom grDevices png
lhc_countSignificantParametersOverTime <- function(FILEPATH, MEASURES,
                                                   TIMEPOINTS) {
  #Count number of signficant parameters over time (where P Value < 0.01)

  for (m in 1:length(MEASURES)) {
    SIGPARAMS_OVER_TIME <- NULL

    MEASURE <- MEASURES[m]

    PVALS <- read.csv(paste(FILEPATH, "/All_Timepoint_PVALS_", MEASURE,
                            ".csv", sep = ""), header = TRUE)

    for (t in 1:length(TIMEPOINTS)) {
      current_time <- TIMEPOINTS[t]
      col_head <- paste(MEASURE, "_PValue_", current_time, sep = "")
      # Read in the column for this timepoint, and see what is below 0.01
      T_PVALS <- PVALS[col_head]

      SIGPARAMS_OVER_TIME <- rbind(SIGPARAMS_OVER_TIME,
                                   (cbind(
                                     current_time, nrow(
                                       subset(
                                         T_PVALS,
                                         T_PVALS[, col_head] < 0.01)))))

    }

    # Plot the number of parameters that are significant for this measure
    png(filename = paste(FILEPATH, "/", MEASURE, "_Significant_Parameters.png",
                         sep = ""))
    plot(TIMEPOINTS, SIGPARAMS_OVER_TIME[, 2], type = "l",
         ylim = c(0, max(SIGPARAMS_OVER_TIME[, 2])),
         col = "red", xlab = "Hours", ylab = "Significant Parameters",
         main = MEASURE)
    abline(v = 500, lty = 2, col = "gray")
    abline(v = 1000, lty = 2, col = "gray")
    abline(v = 1500, lty = 2, col = "gray")
    abline(h = 5, lty = 2, col = "gray")
    abline(h = 10, lty = 2, col = "gray")
    abline(h = 15, lty = 2, col = "gray")
    abline(h = 20, lty = 2, col = "gray")
    legend("bottomright", legend = c("Parameters with p<0.01"), lty = c(1),
           lwd = c(2.5), col = c("red"))
    dev.off()
  }
}

#' Produce a plot of PRCC values obtained at multiple timepoints
#'
#' Calculates the PRCC for each parameter at each timepoint in the TIMEPOINTS
#' vector. Unlike the other methods in Spartan, this stores PRCC and P-Value
#' in 2 different files to make the plot function easier.
#'
#' @param FILEPATH Directory where the pre-processed PRCC values can be found
#' @param MEASURES Simulation response measures
#' @param TIMEPOINTS Timepoints being analysed
#'
lhc_graphPRCCForMultipleTimepoints <- function(FILEPATH, MEASURES,
                                               TIMEPOINTS) {
  # For all measures
  for (m in 1:length(MEASURES)) {
    MEASURE <- MEASURES[m]

    # Read in the PRCC summary
    ALL_PRCCS <- read.csv(paste(FILEPATH, "/All_Timepoint_PRCCS_", MEASURE,
                                ".csv", sep = ""), header = T)

    # NOW TO GRAPH EACH PARAMETER(ROW) - MINUS 1 TO NOT DO THIS FOR THE DUMMY
    for (PARAM in 1:(nrow(ALL_PRCCS) - 1)) {
      # GOING TO GRAPH THIS ROW, AND THE DUMMY (WHICH SHOULD BE THE FINAL ROW)
      PARAMDATA <- ALL_PRCCS[PARAM, 2:ncol(ALL_PRCCS) ]
      DUMMYDATA <- ALL_PRCCS[nrow(ALL_PRCCS), 2:ncol(ALL_PRCCS) ]
      NAMES <- c(toString(ALL_PRCCS[PARAM, 1]), "Dummy")

      png(filename = paste(FILEPATH, "/", MEASURE, "_", NAMES[1], ".png",
                           sep = ""))
      plot(TIMEPOINTS, PARAMDATA, type = "l", ylim = c(-1, 1),
           col = "red", xlab = "Hours",
           ylab = "Partial Rank Corrrelation Coefficient",
           main = paste("PRCC Over Time for Parameter ", ALL_PRCCS[PARAM, 1],
                        "\n Measure: ", MEASURE, sep = ""), yaxt = "n")
      axis(side = 2, at = seq(-1, 1, by = 0.25))
      lines(TIMEPOINTS, DUMMYDATA, type = "l", col = "blue")

      abline(v = 500, lty = 2, col = "gray")
      abline(v = 1000, lty = 2, col = "gray")
      abline(v = 1500, lty = 2, col = "gray")
      abline(h = 0.25, lty = 2, col = "gray")
      abline(h = 0.5, lty = 2, col = "gray")
      abline(h = 0.75, lty = 2, col = "gray")
      abline(h = 0.00, lty = 2, col = "gray")
      abline(h = -0.25, lty = 2, col = "gray")
      abline(h = -0.5, lty = 2, col = "gray")
      abline(h = -0.75, lty = 2, col = "gray")


      legend("topleft", legend = NAMES, lty = c(1, 1),
             lwd = c(2.5, 2.5), col = c("red", "blue"))
      dev.off()
    }
  }

}

#' Deprecated. Use \code{lhc_graphMeasuresForParameterChange} instead
#'
#' @inheritParams lhc_graphMeasuresForParameterChange
lhc_netlogo_graphMeasuresForParameterChange <- function(FILEPATH, PARAMETERS,
                                                        MEASURES,
                                                        MEASURE_SCALE,
                                                        CORCOEFFSOUTPUTFILE,
                                                        LHCSUMMARYFILENAME,
                                                        TIMEPOINTS,
                                                        TIMEPOINTSCALE) {

  # Call the spartan function
  lhc_graphMeasuresForParameterChange(FILEPATH, PARAMETERS,
                                      MEASURES, MEASURE_SCALE,
                                      CORCOEFFSOUTPUTFILE,
                                      LHCSUMMARYFILENAME,
                                      TIMEPOINTS,
                                      TIMEPOINTSCALE)

}


#' Plots the PRCC coefficients against each other for ease of comparison
#'
#' Plots the Partial Rank Correlation Coefficients for either all measures
#' or for one individual measure, for all simulation parameters.
#'
#' @param FILEPATH Location of the LHC result set
#' @param CORCOEFFSOUTPUTFILE Name of the CSV file in FILEPATH containing
#' the Partial Rank Correlation Coefficients
#' @param MEASURES Names of the simulation responses
#' @param PRINTOPT Used in plotting Partial Rank Correlation Coefficients,
#' should be either "ALL" or "INDIVIDUAL"
#' @param TIMEPOINTS Implemented so this method can be used when analysing
#' multiple simulation timepoints. If only analysing one timepoint, this
#' should be set to NULL. If not, this should be an array of timepoints,
#' e.g. c(12,36,48,60)
#' @param TIMEPOINTSCALE Implemented so this method can be used when
#' analysing multiple simulation timepoints. Sets the scale of the timepoints
#' being analysed, e.g. "Hours"
#'
#' @export
#'
#' @importFrom graphics barplot
lhc_plotCoEfficients <- function(FILEPATH, CORCOEFFSOUTPUTFILE, MEASURES,
                                 PRINTOPT, TIMEPOINTS = NULL,
                                 TIMEPOINTSCALE = NULL) {

  if (is.null(TIMEPOINTS) || length(TIMEPOINTS) == 1) {
    if (file.exists(FILEPATH)) {

      # READ IN THE COEFFICIENTS FILE
      COEFFSRESULTSFILENAME <- paste(FILEPATH, "/", CORCOEFFSOUTPUTFILE,
                                     sep = "")
      COEFFS <- read.csv(COEFFSRESULTSFILENAME, header = TRUE,
                         check.names = FALSE)

      # COLUMN 1 HAS PARAMETER NAMES, THEN FOLLOWS FOR EACH MEASURE -
      # THE PRCC AND THE P VALUE
      # WE'RE GOING TO GRAPH ALL THE PRCC'S ON ONE GRAPH

      if (PRINTOPT == "INDIVIDUAL") {
        # INDIVIDUAL PLOTS FOR EACH MEASURE
        print(paste("Producing Partial Rank Correlation Coefficient Plots",
                    " for each measure",sep=""))

        for (i in 1:length(MEASURES)) {
          if (is.null(TIMEPOINTS)) {
            GRAPHFILE <- paste(FILEPATH, "/PRCC_Measure_", MEASURES[i], ".pdf",
                               sep = "")
            GRAPHTITLE <- paste("PRCC Values for Measure: ", MEASURES[i],
                                sep = "")
          } else {
            GRAPHFILE <- paste(FILEPATH, "/PRCC_Measure_", MEASURES[i], "_",
                               TIMEPOINTS, ".pdf", sep = "")
            GRAPHTITLE <- paste("PRCC Values for Measure: ", MEASURES[i],
                                "\nTimepoint: ", TIMEPOINTS, sep = "")
          }

          pdf(GRAPHFILE, width = 9, height = 5)
          par(xpd = NA, mar = c(2, 4, 2, 17))

          # Generate the heading of the CSV file - the measure plus _Estimate
          M <- paste(MEASURES[i], "_Estimate", sep = "")
          # We can now use this to get the column out the dataset

          barplot(COEFFS[, M], ylim = c(-1, 1), col = "black",
                  main = GRAPHTITLE,
                  ylab = "Partial Rank Correlation Coefficient",
                  names.arg = seq(1, nrow(COEFFS), by = 1))

          thelabels <- paste(1:nrow(COEFFS), " ", COEFFS[, 1], sep = "")
          par(xpd = TRUE)
          legend_size <- nrow(COEFFS) + 1.5
          legend(legend_size, 1.0, legend = thelabels, pch = "",
                 cex = 0.6, ncol = 1)
          par(xpd = FALSE)
          dev.off()
        }

      } else if (PRINTOPT == "ALL") {
        print("Producing Partial Rank Correlation Coefficient Summary Plot
              of All Measures")

        # ALL PRCCS FOR ALL MEASURES, ON ONE PLOT
        # Make the data frame for the plot
        # FIRST OF ALL WE NEED TO REMOVE THE P VALUES SO WE CAN AUTOMATE THIS

        if (is.null(TIMEPOINTS)) {
          GRAPHFILE <- paste(FILEPATH, "/PRCC_AllMeasures.pdf", sep = "")
          GRAPHTITLE <- "PRCC Values for All Measures"
        } else {
          GRAPHFILE <- paste(FILEPATH, "/PRCC_AllMeasures_", TIMEPOINTS,
                             ".pdf", sep = "")
          GRAPHTITLE <- paste("PRCC Values for All Measures\nTimepoint: ",
                              TIMEPOINTS, sep = "")
        }

        pdf(GRAPHFILE, width = 9, height = 5)

        par(xpd = NA, mar = c(2, 4, 2, 17))

        PRCCS <- NULL
        for (p in seq(2, ncol(COEFFS), by = 2)) {
          PRCCS <- cbind(PRCCS, COEFFS[, p])
        }

        # NOW MAKE THE DATA FRAME
        d <- data.frame(row.names = levels(COEFFS[, 1]), PRCCS,
                        check.names = FALSE)
        colnames(d) <- MEASURES
        d <- do.call(rbind, d)
        barplot(d, beside = TRUE, ylim = c(-1, 1.4),
                legend.text = rownames(d),
                args.legend = list(x = "topright", bty = "n"),
                names.arg = seq(1, nrow(COEFFS), by = 1),
                main = GRAPHTITLE,
                ylab = "Partial Rank Correlation Coefficient")

        thelabels <- paste(1:nrow(COEFFS), " ", COEFFS[, 1], sep = "")
        par(xpd = TRUE)
        legend_size <- nrow(COEFFS) + 1
        legend(legend_size, 1.0, legend = thelabels, pch = "",
               cex = 0.7, ncol = 1)
        par(xpd = FALSE)
        dev.off()
      }
    } else {
      print("The directory specified in FILEPATH does not exist.")
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING THE FILENAMES, RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {

      current_time <- TIMEPOINTS[n]
      print(paste("PROCESSING TIMEPOINT: ", current_time, sep = ""))

      CORCOEFFSOUTPUTFILE_FORMAT <- check_file_extension(CORCOEFFSOUTPUTFILE)
      CORCOEFFSOUTPUTFILE_FULL <- paste(substr(CORCOEFFSOUTPUTFILE, 0,
                                               nchar(CORCOEFFSOUTPUTFILE) - 4),
                                        "_", current_time, ".",
                                        CORCOEFFSOUTPUTFILE_FORMAT, sep = "")

      lhc_plotCoEfficients(FILEPATH, CORCOEFFSOUTPUTFILE_FULL, MEASURES,
                           PRINTOPT, current_time, NULL)
    }
  }
}

#' Creates a polar plot for each response, showing PRCC for each parameter
#'
#' Added in Spartan 3.0. Provides a means of plotting the partial rank
#' correlation coefficients as a polar plot, to ease comparison of these values.
#'
#' @inheritParams lhc_graphMeasuresForParameterChange
#'
#' @export
#'
#' @import plotrix
lhc_polarplot <- function(FILEPATH, PARAMETERS, MEASURES, CORCOEFFSOUTPUTFILE,
                          TIMEPOINTS = NULL, TIMEPOINTSCALE = NULL) {
  # Produce a polar plot that shows all the PRCCs for the PARAMETERS
  # in a latin-hypercube analysis
  if (is.null(TIMEPOINTS) || length(TIMEPOINTS) == 1) {
    # Check the FILEPATH exists
    if (file.exists(FILEPATH)) {
      # Check the coefficient file exists
      if (file.exists(paste(FILEPATH, "/", CORCOEFFSOUTPUTFILE, sep = ""))) {

        # Read in the file
        CORCOEFFS <- read.csv(paste(FILEPATH, "/", CORCOEFFSOUTPUTFILE,
                                    sep = ""),
                              header = TRUE, check.names = FALSE,
                              row.names = 1)

        # Plot set up:
        # convert 360 degrees to radians
        circle_in_radians <- 6.28319
        degree <- circle_in_radians / length(PARAMETERS)
        # outputs:
        output_forms <- c("png", "pdf")

        # Now create a plot for all simulation MEASURES
        for (m in 1:length(MEASURES)) {
          # Create the angles at which the PARAMETERS will be shown on the
          # plot, as well as the colours (blue negative, red positive)
          angle <- c()
          colours <- c()
          # Make the header for this measure
          col_head <- paste(MEASURES[m], "_Estimate",
                           sep = "")


          for (i in 1:length(PARAMETERS)) {
            angle <- c(angle, degree * i)
            # Now see if the correlation is positive or negative
            if (CORCOEFFS[PARAMETERS[i], col_head] < 0)
              colours <- c(colours, "blue")
            else
              colours <- c(colours, "red")
          }

          # Now plot the graph:
          for (o in 1:length(output_forms))  {
            if (output_forms[o] == "pdf")
              pdf(paste(FILEPATH, "/polarPlot_", MEASURES[m], ".pdf",
                        sep = ""), width = 12)
            if (output_forms[o] == "png")
              png(filename = paste(FILEPATH, "/polarPlot_", MEASURES[m],
                                   ".png", sep = ""), width = 800)

            # Sets the size of the labels on the outside of the polar plot
            par(cex.axis = 1.5)

            # readjust the parameter list to align with the correct angles
            PARAM_NAMES <- c(PARAMETERS[length(PARAMETERS)],
                             PARAMETERS[1:length(PARAMETERS) - 1])


            # Note we use absolute values as plot goes from 0 to 1, it is the
            # colour which shows if it is positive or negative
            radial.plot(abs(CORCOEFFS[, col_head]),
                        angle, rp.type = "r",
                        lwd = 4, line.col = colours,
                        labels = seq(1, length(PARAMETERS), by = 1),
                        radial.lim = c(0, 1), #range of grid circle
                        main = paste("Partial Rank Correlation Coefficient
                                     Values for ",
                                     MEASURES[m], sep = ""),
                        show.grid.labels = 2,
                        #put the concentric circle labels going down
                        show.radial.grid = TRUE,
                        cex.lab = 0.7
            )

            legend("topleft", 1, c("Positive", "Negative"), lty = 1, lwd = 1:2,
                   col = c("red", "blue"), cex = 0.9, pt.cex = 1)
            par(xpd = TRUE)
            legend(1, 1, pch = as.character(c(1:length(PARAMETERS))),
                   PARAM_NAMES, cex = 0.9, pt.cex = 1)
            par(xpd = FALSE)
            dev.off()
          }
        }
      } else {
        print("Cannot find Partial Rank Correlation Coefficients File.
              Are you sure you have run the method to generate it?")
      }

    } else {
      print("The directory specified in FILEPATH does not exist.
            No Output Graphs Generated")
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {
      current_time <- TIMEPOINTS[n]
      print(paste("Processing Timepoint: ", current_time, sep = ""))

      CORCOEFFSOUTPUTFILE_FORMAT <- check_file_extension(CORCOEFFSOUTPUTFILE)
      CORCOEFFSOUTPUTFILE_FULL <- paste(substr(CORCOEFFSOUTPUTFILE, 0,
                                               nchar(CORCOEFFSOUTPUTFILE) - 4),
                                        "_", current_time, ".",
                                        CORCOEFFSOUTPUTFILE_FORMAT, sep = "")

      lhc_polarplot(FILEPATH, PARAMETERS, MEASURES, CORCOEFFSOUTPUTFILE_FULL,
                    TIMEPOINTS = current_time,
                    TIMEPOINTSCALE = TIMEPOINTSCALE)
    }
  }
}


#' Plots Graphs for Partial Rank Correlation Coefficients Over Time
#'
#' Produces plots to show how the impact of a parameter changes over time,
#' measured by the change in PRCC
#'
#' @inheritParams lhc_graphMeasuresForParameterChange
#' @param CORCOEFFSFILENAME Name of the CSV file containining the correlation
#' coefficients
#' @param DISPLAYPVALS Boolean stating whether PRCC p-values should be printed
#' on the graph
#'
#' @export
plotPRCCSFromTimepointFiles <- function(FILEPATH, PARAMETERS, MEASURES,
                                      CORCOEFFSFILENAME, TIMEPOINTS,
                                      TIMEPOINTSCALE, DISPLAYPVALS = FALSE) {
  print("Plotting Graphs for Partial Rank Correlation Coefficients Over Time")

  if (requireNamespace("plotrix", quietly = TRUE)) {

    # One plot for each parameter
    for (PARAM in 1:length(PARAMETERS)) {
      # PRCCS for this parameter
      FULLPARAMRESULTS <- NULL
      # P-Values for this parameter
      PARAMPVALS <- data.frame()

      # Now to gather the data for each hour from the relevant result files
      for (i in 1:length(TIMEPOINTS)) {

        hour <- TIMEPOINTS[i]

        # Add the timepoint to the correlation coefficient results file
        CORCOEFFSOUTPUTFILE_FORMAT <- check_file_extension(CORCOEFFSFILENAME)
        CORCOEFFSOUTPUTFILE_FULL <- paste(substr(CORCOEFFSFILENAME, 0,
                                                 nchar(CORCOEFFSFILENAME) - 4),
                                          "_", hour, ".",
                                          CORCOEFFSOUTPUTFILE_FORMAT, sep = "")


        # Read in the coefficients
        LHCResults <- read.csv(paste(FILEPATH, "/", CORCOEFFSOUTPUTFILE_FULL,
                                   sep = ""), header = T)
        # Get the PRCCS
        results <- c(hour, LHCResults[PARAM, 2], LHCResults[PARAM, 4])
        # Get the P-Values
        pvals.d <- data.frame(LHCResults[PARAM, 3], LHCResults[PARAM, 5])

        # Append the PRCCS for this timepoint to those of all timepoints
        FULLPARAMRESULTS <- rbind(FULLPARAMRESULTS, results)
        # Append the P-Values for this timepoint to those of all timepoints
        PARAMPVALS <- rbind(PARAMPVALS, pvals.d)
      }

      # Set the row and column names of the P-Values data frame
      rownames(PARAMPVALS) <- TIMEPOINTS
      colnames(PARAMPVALS) <- MEASURES

      # Now to make the plot
      GRAPHFILE <- paste(FILEPATH, "/", PARAMETERS[PARAM], "_OverTime.pdf",
                         sep = "")
      pdf(GRAPHFILE, width = 7, height = 7)

      # Title, with parameter name
      GRAPHTITLE <- paste("Partial Rank Correlation Coefficients Over Simulation
                        Time\nParameter: ", PARAMETERS[PARAM], sep = "")

      # Plot the first measure
      plot(FULLPARAMRESULTS[, 1], FULLPARAMRESULTS[, 2], type = "o",
           main = GRAPHTITLE, lty = 1, xlab = "", ylim = c(-1, 1),
           ylab = "Partial Rank Correlation Coefficient",
           xaxt = "n", yaxt = "n", bty = "n")

      # Now add the rest
      lines(FULLPARAMRESULTS[, 1], FULLPARAMRESULTS[, 3], type = "o",
            lty = 5, pch = 2)

      axis(2, at = seq(-1, 1, by = 0.25))
      axis(1, pos = 0, at = seq(as.numeric(min(TIMEPOINTS)),
                                as.numeric(max(TIMEPOINTS)),
                                by = (as.numeric(max(TIMEPOINTS)) /
                                        length(TIMEPOINTS))),
           tck = 0.015, labels = FALSE)
      # Add the axis at 0
      abline(h = 0)
      # Add the labels to the axis
      for (h in 1:length(TIMEPOINTS)) {
        text(as.numeric(TIMEPOINTS[h]), 0.08, TIMEPOINTS[h])
      }

      max_time <- max(TIMEPOINTS)
      num_max_time <- as.numeric(max_time)
      min_time <- min(TIMEPOINTS)
      num_min_time <- as.numeric(min_time)
      total_time <- num_max_time + num_min_time

      # Add the X axis label
      text(total_time / 2, 0.18, TIMEPOINTSCALE)

      # P-Values Table, if the user wants this displayed
      if (DISPLAYPVALS == TRUE) {
        xaxis_loc <- (((as.numeric(max(TIMEPOINTS)) -
                        as.numeric(min(TIMEPOINTS))) / 100) * 71) +
          as.numeric(min(TIMEPOINTS))
        plotrix::addtable2plot(xaxis_loc, 0.7, signif(PARAMPVALS, digits = 3),
                               cex = 0.7,
                               display.rownames = TRUE,
                               title = "p-Values",
                               display.colnames = TRUE, bty = "o",
                               hlines = TRUE)
      }

      # Graph legend
      legend("topleft", inset = .025, title = "Measures", MEASURES,
             pch = 1:length(MEASURES))

      # Output graph
      dev.off()

    }
    print(paste("Complete. Check for output in the directory ", FILEPATH,
                sep = ""))
  } else  {
    print("The plotPRCCSFromTimepointFiles function requires the
          plotrix package to be installed")
  }
}
