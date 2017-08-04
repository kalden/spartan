#' Internal function used to create accuracy plots of the emulation against observed data
#'
#' Outputs plot to PDF in the current working directory
#' @param technique The machine learning technique used to develop the emulator
#' @param measure The simulation output response being plotted
#' @param model_predictions Predicted dataset
#' @param observed_data Observed dataset (testing or validation)
#' @param timepoint If using multiple timepoints, the timepoint for which the
#' emulator has been created
produce_accuracy_plots_single_measure <- function(technique, measure,
                                                  model_predictions,
                                                  observed_data,
                                                  timepoint = NULL) {
  if (is.null(timepoint))
    graph_file_name <- paste(technique, "_", measure, ".pdf", sep = "")
  else
    graph_file_name <- paste(technique, "_", measure, "_", timepoint, ".pdf",
                           sep = "")

  plot_compare_sim_observed_to_model_prediction(
    observed_data, model_predictions, technique, measure,
    meanSquaredError(model_predictions, observed_data), graph_file_name,
    timepoint)
}


#' Internal function used to create accuracy plots of the emulation against
#' observed data, for all measures
#'
#' Outputs plot to PDF in the current working directory
#'
#' @param technique The machine learning technique used to develop the emulator
#' @param measures All simulation output responses to plot
#' @param model_predictions Predicted dataset
#' @param observed_data Observed dataset (testing or validation)
#' @param timepoint If using multiple timepoints, the timepoint for which the
#' emulator has been created
produce_accuracy_plots_all_measures <- function(technique, measures,
                                                model_predictions,
                                                observed_data,
                                                timepoint=NULL) {
  for (m in 1:length(measures)) {
    # Note model_predictions index is just m, not measures[m], the
    # column will be labelled technique_measure, not just measure
    produce_accuracy_plots_single_measure(technique, measures[m],
                                          model_predictions[, m],
                                          observed_data[, measures[m]],
                                          timepoint)
  }
}

#' Internal function used to create accuracy plots of the emulation against observed data
#'
#' Outputs plot to PDF in the current working directory
#'
#' @param predicted Predicted dataset
#' @param observed Observed dataset (testing or validation)
#' @param technique The machine learning technique used to develop the emulator
#' @param measure The simulation output response being plotted
#' @param mse Mean Squared Error between predicted and observed
#' @param graph_file_name Name to give the produced PDF plot
#' @param timepoint If using multiple timepoints, the timepoint for which the
#' emulator has been created
plot_compare_sim_observed_to_model_prediction <- function(observed, predicted,
                                                          technique, measure,
                                                          mse, graph_file_name,
                                                          timepoint = NULL) {
  plot_data <- data.frame(predicted, observed)

  if (is.null(timepoint))
    graph_title <- paste(technique, ",", measure, "\n MSE: ", round(mse, 3),
                        sep = "")
  else
    graph_title <- paste(technique, ",", measure, "\n MSE: ", round(mse, 3),
                        "\n Timepoint: ", timepoint, sep = "")

  # Important that x and y are on the same scale - take the max of the
  # observed and predicted and use that plus a bit of leeway as a limit
  axis_limit <- round(max(max(predicted), max(observed)))

  graph <- ggplot2::ggplot(plot_data, aes(x = plot_data[, 1],
                                          y = plot_data[, 2]))
  # Dots for each point
  graph + geom_point() + labs(x = "Predicted", y = "Observed") +
    ggtitle(graph_title) + theme(plot.title = element_text(hjust = 0.5,
                                                          size = 14)) +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_continuous(limits = c(0, axis_limit)) +
    scale_y_continuous(limits = c(0, axis_limit))

  ggplot2::ggsave(graph_file_name, device = "pdf")
}
