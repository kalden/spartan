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
#' @param check_done For multiple timepoints, whether input has been checked
#' @param corcoeffs_output_object Correlation coefficients can be input as an
#' R object as well as CSV file. In this case, CORCOEFFSOUTPUTFILE will be NULL
#' @param lhc_summary_object If not specified in a CSV file, results can be specified in an
#' R object. In this case LHCSUMMARYFILENAME will be NULL
#'
#' @export
#'
lhc_graphMeasuresForParameterChange <-
  function(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CORCOEFFSOUTPUTFILE,
           LHCSUMMARYFILENAME, OUTPUT_TYPE = c("PDF"), TIMEPOINTS = NULL,
           TIMEPOINTSCALE = NULL, GRAPHTIME = NULL, check_done=FALSE,
           corcoeffs_output_object=NULL, lhc_summary_object=NULL) {


  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:
  if(check_input_args(input_check$names, input_check$arguments)) {

  if (is.null(TIMEPOINTS)) {

    if(!is.null(CORCOEFFSOUTPUTFILE))
    {
      corcoeffs <- read_from_csv(file.path(FILEPATH, CORCOEFFSOUTPUTFILE))
      lhcresult <- read_from_csv(file.path(FILEPATH, LHCSUMMARYFILENAME))
    }
    else if(!is.null(corcoeffs_output_object))
    {
      corcoeffs <- corcoeffs_output_object
      lhcresult <- lhc_summary_object
    }


        message ("Generating output graphs for LHC Parameter Analysis")

        # CREATE A GRAPH FOR EACH PARAMETER, FOR EACH MEASURE
        for (p in 1:length(PARAMETERS)) {
          for (m in 1:length(MEASURES)) {

            # Get the PRCC value for this pairing
            corr_result <- corcoeffs[
              p, paste(MEASURES[m], "_Estimate", sep = "")]

            # In some instances, the correlation coefficient has been reported as NA
            # especially in cases where the output result is the same for all parameter
            # values. This needs detecting and no graph plotted if that occurs

            if(!is.na(corr_result))
            {
              # Make filename, titles, and labels
              titles <- make_graph_title(FILEPATH, PARAMETERS[p], GRAPHTIME, MEASURES[m],
                             MEASURE_SCALE[m],corr_result, TIMEPOINTSCALE)

              # Filter the data to plot
              data_to_plot <- data.frame(lhcresult[, PARAMETERS[p]],
                                       lhcresult[, MEASURES[m]])

              # Create graphs
              output_ggplot_graph(titles$file, OUTPUT_TYPE,
                                make_lhc_plot(data_to_plot, titles))
            } else {
              message(paste0("For Parameter ",PARAMETERS[p], " Measure ",MEASURES[m], " Pairing, Correlation Coefficient was reported as NA. Excluded from plotting."))
            }
          }
        }
        message("LHC Graphs Complete")
    } else {
        # Process each timepoint
        lhc_graphMeasuresForParameterChange_overTime(
          FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CORCOEFFSOUTPUTFILE,
                   LHCSUMMARYFILENAME, OUTPUT_TYPE, TIMEPOINTS,
                   TIMEPOINTSCALE, GRAPHTIME)
    }
  }
  }

#' Generates parameter/measure plot for each pairing in the analysis, from results stored in a database
#'
#' Produces a graph for each parameter, and each output measure, showing
#' the simulation output achieved when that parameter was assigned that value.
#' Eases identification of any non-linear effects. This method uses simulation
#' results stored in a database by spartanDB
#'
#' @param db_results Results for a specified experiment mined from the database
#' @param corcoeffs Correlation coefficients calculated for those results,
#' held in the databae
#' @param parameters Parameters included in this analysis
#' @param measures Simulation output measures
#' @param MEASURE_SCALE Scale in which each of the output responses is
#' measured. Used to label plots
#' @param output_directory Folder where the graphs should be stored
#' @param OUTPUT_TYPE Type of graph to plot. Can be PDF, PNG, TIFF, BMP, etc,
#'  all formats supported by ggplot2
#'
#' @export
#'
lhc_graphMeasuresForParameterChange_from_db <-
  function(db_results, corcoeffs, parameters, measures, MEASURE_SCALE, output_directory,
           OUTPUT_TYPE = c("PDF")) {

  message ("Generating output graphs for LHC Parameter Analysis")

  # CREATE A GRAPH FOR EACH PARAMETER, FOR EACH MEASURE
  for (p in 1:length(parameters)) {
    for (m in 1:length(measures)) {

      # Get the PRCC value for this pairing
      #corr_result <- subset(corcoeffs, corcoeffs$parameter==parameters[p] & corcoeffs$measure==measures[m],select=c(statistic_1))
      corr_result <- corcoeffs[corcoeffs$parameter == parameters[p] & corcoeffs$measure == measures[m],]["statistic_1"]

      # Make filename, titles, and labels
      titles <- make_graph_title(output_directory, parameters[p], NULL, measures[m],
                                 MEASURE_SCALE[m],as.numeric(corr_result), NULL)

      # Filter the data to plot
      data_to_plot <- data.frame(as.numeric(db_results[, parameters[p]]),
                                 as.numeric(db_results[, measures[m]]))

      # Create graphs
      output_ggplot_graph(titles$file, OUTPUT_TYPE,
                          make_lhc_plot(data_to_plot, titles))
    }
  }
  message("LHC Graphs Complete")
}

#' Wrapper for graphing LHC results for multiple timepoints
#' @inheritParams lhc_graphMeasuresForParameterChange
lhc_graphMeasuresForParameterChange_overTime <-
    function(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CORCOEFFSOUTPUTFILE,
             LHCSUMMARYFILENAME, OUTPUT_TYPE = c("PDF"), TIMEPOINTS = NULL,
             TIMEPOINTSCALE = NULL, GRAPHTIME = NULL) {

      for (n in 1:length(TIMEPOINTS)) {
        current_time <- TIMEPOINTS[n]
        message(paste("Processing Timepoint: ", current_time, sep = ""))

        corcoeffs_output_full <- append_time_to_argument(
          CORCOEFFSOUTPUTFILE, current_time,
          check_file_extension(CORCOEFFSOUTPUTFILE))

        lhcsummary_full <- append_time_to_argument(
          LHCSUMMARYFILENAME, current_time,
          check_file_extension(LHCSUMMARYFILENAME))

        lhc_graphMeasuresForParameterChange(
          FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, corcoeffs_output_full,
          lhcsummary_full, TIMEPOINTS = NULL, TIMEPOINTSCALE = TIMEPOINTSCALE,
          GRAPHTIME = current_time, check_done = TRUE)

      }
    }

#' Make graph title, sub title, and file name
#' @param filepath Directory to output graph to
#' @param parameter Current parameter being processed
#' @param graph_time Timepoint, if multiple timepoints
#' @param measure Current measure being processed
#' @param measure_scale Scale of the measure being processed
#' @param corr_stat The PRCC for this parameter-measure pair
#' @param timepointscale Scale of timepoints, if multiple
#' @return List containing file, title, and subtitle, and axes labels
make_graph_title <- function(filepath, parameter, graph_time, measure,
                             measure_scale, corr_stat, timepointscale) {
  graph_title <- paste("LHC Analysis for Parameter: ",parameter, sep = "")
  y_label <- paste("Median Value Across Runs (", measure_scale,
                   ")", sep = "")
  x_label <- "Parameter Value"

  if (is.null(graph_time)) {
    graph_file <- file.path(filepath,paste(parameter,measure,sep="_"))
    sub_title <- paste("Measure: ",measure,"\nCorrelation Coefficient: ",
                      toString(signif(corr_stat, 3)), sep = "")
  } else {
    graph_file <- file.path(filepath,paste(parameter, measure, graph_time,
                                           sep="_"))
    sub_title <- paste(
      "Measure: ",measure, ". Timepoint: ", graph_time, " ", timepointscale,
      "\nCorrelation Coefficient: ", toString(signif(corr_stat, 3)), sep = "")
  }
  return(list("title"=graph_title,"file"=graph_file,"sub_title"=sub_title,
         "xlabel"=x_label,"ylabel"=y_label))
}

#' Make the LHC output plot
#' @param data_to_plot Parameter and measure pair data
#' @param titles Object containing graph title and subtitle
#' @return Created graph object
make_lhc_plot <- function(data_to_plot, titles) {
  output_graph <- ggplot(data_to_plot,
                         aes(x = data_to_plot[, 1],
                             y = data_to_plot[, 2])) +
    geom_point(size = 0.5) +
    scale_y_continuous(limits = c(
      floor(min(as.numeric(data_to_plot[,2]))), ceiling(max(as.numeric(data_to_plot[, 2]))))) +
    labs(x = titles$xlabel, y = titles$ylabel,
         title = titles$title, subtitle = titles$sub_title) +
    theme(axis.title = element_text(size = 7),
          axis.text = element_text(size = 7),
          plot.title = element_text(size = 9, hjust = 0.5),
          plot.subtitle = element_text(size = 8, hjust = 0.5))

  return(output_graph)
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

  message("Deprecated. Use the lhc_graphMeasuresForParameterChange method instead")
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
      COEFFS <- read_from_csv(file.path(FILEPATH,CORCOEFFSOUTPUTFILE))

      # COLUMN 1 HAS PARAMETER NAMES, THEN FOLLOWS FOR EACH MEASURE -
      # THE PRCC AND THE P VALUE
      # WE'RE GOING TO GRAPH ALL THE PRCC'S ON ONE GRAPH

      if (PRINTOPT == "INDIVIDUAL") {
        # INDIVIDUAL PLOTS FOR EACH MEASURE
        message("Producing Partial Rank Correlation Coefficient Plots for each measure")

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
        message("Producing Partial Rank Correlation Coefficient Summary Plot of All Measures")

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

        pdf(GRAPHFILE, width = 10, height = 5)

        par(xpd = NA, mar = c(2, 4, 2, 9))

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
        legend_size <- nrow(COEFFS)*3
        #legend(legend_size, 1.0, legend = thelabels, pch = "",
        #       cex = 0.7, ncol = 1)
        legend("topright", inset=c(-0.2,0), 1.0, legend = thelabels, pch = "",
               cex = 0.7, ncol = 1)

        par(xpd = FALSE)
        dev.off()
      }
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING THE FILENAMES, RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {

      current_time <- TIMEPOINTS[n]
      message(paste("Processing Timepoint ", current_time, sep = ""))

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

        # outputs:
        output_forms <- c("png", "pdf")

        # Now create a plot for all simulation MEASURES
        for (m in 1:length(MEASURES)) {

          # Need to exclude any parameters that are NA prior to plotting
          na_corrs <- which(is.na(CORCOEFFS[,paste0(MEASURES[m],"_Estimate")]))

          plot_parameters<-PARAMETERS
          if(length(na_corrs)>0)
          {
            plot_parameters<-PARAMETERS[!(PARAMETERS %in% PARAMETERS[na_corrs])]
            message(paste0("For Measure ",MEASURES[m],", Parameter(s) ",toString(PARAMETERS[na_corrs])," reported correlation coefficients of NA. Excluded from Plot. Check calculation"))
          }

          # Check there are still parameters left to plot after those removed!
          if(length(plot_parameters)>0)
          {

            degree <- circle_in_radians / length(plot_parameters)

            # Create the angles at which the PARAMETERS will be shown on the
            # plot, as well as the colours (blue negative, red positive)
            angle <- c()
            colours <- c()
            # Make the header for this measure
            col_head <- paste(MEASURES[m], "_Estimate",
                             sep = "")


            #for (i in 1:length(PARAMETERS)) {
            for (i in 1:length(plot_parameters))
            {
              angle <- c(angle, degree * i)
              # Now see if the correlation is positive or negative
              #if (CORCOEFFS[PARAMETERS[i], col_head] < 0)
              if (CORCOEFFS[plot_parameters[i], col_head] < 0)
              {
                  colours <- c(colours, "blue")
              } else {
                  colours <- c(colours, "red")
              }
            }

            graph_name <- paste(FILEPATH, "/polarPlot_", MEASURES[m],sep="")
            if(!is.null(TIMEPOINTS))
              graph_name<-paste(graph_name,"_",TIMEPOINTS,sep="")

            # Now plot the graph:
            for (o in 1:length(output_forms))  {
              if (output_forms[o] == "pdf")
                pdf(paste(graph_name, ".pdf", sep = ""), width = 12)
              if (output_forms[o] == "png")
                png(filename = paste(graph_name,".png",sep = ""), width = 800)

              # Sets the size of the labels on the outside of the polar plot
              par(cex.axis = 1.5)

              # readjust the parameter list to align with the correct angles
              #PARAM_NAMES <- c(PARAMETERS[length(PARAMETERS)],
              #                 PARAMETERS[1:length(PARAMETERS) - 1])
              PARAM_NAMES <- c(plot_parameters[length(plot_parameters)],
                               plot_parameters[1:length(plot_parameters) - 1])


              # Note we use absolute values as plot goes from 0 to 1, it is the
              # colour which shows if it is positive or negative
              radial.plot(abs(CORCOEFFS[plot_parameters, col_head]),
                          angle, rp.type = "r",
                          lwd = 4, line.col = colours,
                          labels = seq(1, length(plot_parameters), by = 1),
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
              legend(1, 1, pch = as.character(c(1:length(plot_parameters))),
                     PARAM_NAMES, cex = 0.9, pt.cex = 1)
              par(xpd = FALSE)
              dev.off()
            }
          }
        }
      }
    }
  } else {
    # PROCESS EACH TIMEPOINT, AMENDING FILENAMES AND RECALLING THIS FUNCTION
    for (n in 1:length(TIMEPOINTS)) {
      current_time <- TIMEPOINTS[n]
      message(paste("Processing Timepoint: ", current_time, sep = ""))

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
#' @importFrom grDevices png
plotPRCCSFromTimepointFiles <- function(FILEPATH, PARAMETERS, MEASURES,
                                      CORCOEFFSFILENAME, TIMEPOINTS,
                                      TIMEPOINTSCALE, DISPLAYPVALS = FALSE) {
  message("Plotting Graphs for Partial Rank Correlation Coefficients Over Time")

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
        LHCResults <- read_from_csv(file.path(FILEPATH,CORCOEFFSOUTPUTFILE_FULL))

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
      GRAPHFILE <- file.path(FILEPATH, paste(PARAMETERS[PARAM], "_OverTime.pdf",sep=""))
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
    message(paste("Complete. Check for output in the directory ", FILEPATH,
                sep = ""))
  }
}
