#' Internal function used to combine test set predictions from emulators to
#' form the ensemble training set
#'
#' @param emulator An emulator object from which the test set data is being
#' predicted
#' @param parameters Vector containing the names of the simulation parameters
#' in the dataset on which the emulator is being trained
#' @param measures Vector containing the simulation outputs that the emulators
#' should be able to predict
#' @param observed_data Data obtained from experimentation on the simulator
#' itself, and now used to train the ensemble
#' @param all_model_predictions The set of predictions from numerous emulators
#' to which this set of predictions is being added
#' @return updated all_model_predictions containing predictions for this
#' emulator. This updated list becomes the training set for the ensemble.
generate_ensemble_training_set <- function(emulator, parameters, measures,
                                           observed_data,
                                           all_model_predictions) {

  model_predictions <- generate_predictions_from_emulator(emulator, parameters,
                                                          measures,
                                                          observed_data)

  all_model_predictions <- cbind(all_model_predictions,
                                 extract_predictions_from_result_list(
                                   model_predictions, emulator$type, measures))

  return(all_model_predictions)
}

#' Internal function to create the ensemble
#' @param ensemble_emulations All emulations to build into the ensemble
#' @param all_emulator_predictions Test set predictions from all emulators, on
#' which the ensemble will be trained / emulators weighted
#' @param emulator_test_data Data on which the ensemble performance will be
#' assessed
#' @param measures Simulation responses the model should predict
#' @param emulator_types Machine learning techniques being employed
#' @param pre_normed_mins The minimum values of each parameter prior to data
#' normalisation. Used to rescale the results
#' @param pre_normed_maxes The maximum values of each parameter prior to data
#' normalisation. Used to rescale the results
#' @param algorithm_settings Object output from the function
#' emulation_algorithm_settings, containing the settings of the machine
#' learning algorithms to use in emulation creation. Used here to obtain
#' settings relevant to ensemble creation - namely number of generations and
#' whether the ensemble should be saved to file, as well as whether plots
#' should be produced showing ensemble performance.
#' @param normalise Whether the predictions generated when testing the
#' ensemble should be normalised for presenting test results
#' @param timepoint Simulation timepoint for which an ensemble is being created
#' @param output_formats File formats in which result graphs should be produced
#' @return Generated ensemble object
create_ensemble <- function(ensemble_emulations, all_emulator_predictions,
                            emulator_test_data, measures, emulator_types,
                            pre_normed_mins, pre_normed_maxes,
                            algorithm_settings = NULL, normalise = FALSE,
                            timepoint = NULL, output_formats=c("pdf")) {


  # If called in a process where the emulations are being made,
  # algorithm_settings will already exist. If we're making an ensemble from
  # already existing emulations, this will not. Here it is assumed that the
  # user will modify the algorithm settings if they do not require a
  #different number of generations
  if (is.null(algorithm_settings))
    algorithm_settings <- emulation_algorithm_settings()

  # Calculate the weightings
  weights <- calculate_weights_for_ensemble_model(
    all_emulator_predictions, emulator_test_data, measures, emulator_types,
      algorithm_settings$num_of_generations)

  # Generate prediction from the ensemble
  ensemble_predictions <- weight_emulator_predictions_by_ensemble(
    weights, all_emulator_predictions, measures,
      algorithm_settings$num_of_generations)



  # See how accurate this ensemble is
  if (algorithm_settings$plot_test_accuracy == TRUE) {

    if (normalise) {

      # Scale the predictions such that these are plotted correctly
      unscaled_predictions <- denormalise_dataset(
        ensemble_predictions, rbind(pre_normed_mins[measures]),
        rbind(pre_normed_maxes[measures]))
      unscaled_simulations <- denormalise_dataset(
        emulator_test_data[, measures, drop = FALSE],
        rbind(pre_normed_mins[measures]),
        rbind(pre_normed_maxes[measures]))

      # Performance statistics
      performance_stats<-c("Ensemble")
      performance_names<-c("Technique")

      for (m in 1:length(measures)) {
        produce_accuracy_plots_single_measure(
          "Ensemble_Testing", measures[m], unscaled_predictions[, measures[m]],
          unscaled_simulations[, measures[m]], output_formats, timepoint = timepoint)

        performance_stats<-cbind(performance_stats,meanSquaredError(unscaled_predictions[, measures[m]],
                                                                    unscaled_simulations[, measures[m]]),
                                 rSquared(unscaled_predictions[, measures[m]],
                                          unscaled_simulations[, measures[m]]))

        performance_names<-c(performance_names,paste0(measures[m],"_MSE"),paste0(measures[m],"_R2"))
      }
    } else {

      # Performance statistics
      performance_stats<-c("Ensemble")
      performance_names<-c("Technique")

      for (m in 1:length(measures)) {
        produce_accuracy_plots_single_measure(
          "Ensemble_Testing", measures[m], ensemble_predictions[, measures[m]],
          emulator_test_data[, measures[m]], output_formats, timepoint = timepoint)

        performance_stats<-cbind(performance_stats,meanSquaredError(ensemble_predictions[, measures[m]],
                                                                    emulator_test_data[, measures[m]]))

        performance_names<-c(performance_names,paste0(measures[m],"_MSE"),paste0(measures[m],"_R2"))
      }
    }
    colnames(performance_stats)<-performance_names
  }



  generated_ensemble <- list("emulators" = ensemble_emulations,
                             "weights" = weights,
                             "performance_stats"=performance_stats)

  ## Return so this can be used by user in new predictions
  return(generated_ensemble)
}

#' Internal function to calculate the weights for all emulators in the ensemble
#' @param all_model_predictions Set of test set predictions obtained for all
#' emulators in the ensemble
#' @param emulator_test_data Data on which the ensemble performance will be
#' assessed
#' @param measures Simulation responses the model should predict
#' @param emulator_types Machine learning techniques being employed
#' @param num_of_generations Number of generations for which the neural
#' network that is generating the weights should attempt to converge within
#' @return weights to use for each emulator in the ensemble
calculate_weights_for_ensemble_model <- function(all_model_predictions,
                                                 emulator_test_data,
                                                 measures, emulator_types,
                                                 num_of_generations = 800000) {

  weights <- NULL

  ## NOW WE HAVE THE MODELS, WE NEED AN ENSEMBLE FOR EACH MEASURE
  for (m in 1:length(measures)) {
    # As all the measure data is in the same dataset, we need to refer to the
    # correct columns (always the same measure). Can ensure we do this using
    # a sequence
    columns <- seq(m, ncol(all_model_predictions), by = length(measures))

    # Extract this data for training the net
    # KA: rbind added in October 2018: May need further checking
    weightings_net_training_data <- rbind(all_model_predictions[, columns])

    model_formula <- generate_model_formula(
      colnames(weightings_net_training_data), measures[m])

    # Need to add the actual result to the training data, so weights can be
    # appropriately set
    weightings_net_training_data <- cbind(weightings_net_training_data,
                                          emulator_test_data[, measures[m]])

    # Set the column header for the measure - needed for the formula
    # to work correctly
    colnames(weightings_net_training_data)[ncol(
      weightings_net_training_data)] <- measures[m]

    # Generate the neural network
    ensemble_nn <- neuralnet::neuralnet(model_formula, data =
                                          weightings_net_training_data,
                                        hidden = c(0),
                                        linear.output = T,
                                        stepmax = num_of_generations)

    # Get the weights to apply to all the models, on from 2 as 1 is the bias
    model_weights <- ensemble_nn$weights[[1]][[1]][2:ncol(
      weightings_net_training_data + 1)]

    weights <- rbind(weights, model_weights)
  }

  # Set the column names of the weights
  colnames(weights) <- emulator_types
  rownames(weights) <- measures

  return(weights)
}

#' Internal function to weight emulator predictions by that calculated for the
#' ensemble
#' @param model_weights Weights for all emulators in the ensemble, based on
#' test set performance
#' @param all_model_predictions Set of test set predictions obtained for all
#' emulators in the ensemble
#' @param measures Simulation responses the model should predict
#' @param num_generations Number of generations for which the neural
#' network that is generating the weights should attempt to converge within
#' @return predictions generated by the ensemble
weight_emulator_predictions_by_ensemble <- function(model_weights,
                                                    all_model_predictions,
                                                    measures,
                                                    num_generations = 800000) {

  all_ensemble_predictions <- NULL

  for (m in 1:length(measures)) {
    #As all the measure data is in the same dataset, we need to refer to the
    # correct columns (always the same measure). Ensure this using a sequence
    columns <- seq(m, ncol(all_model_predictions), by = length(measures))

    # Extract this data for training the net
    predicted_response <- all_model_predictions[, columns]

    # Extract the weights
    measure_weights <- model_weights[m, ]

    # Iterate through the model predictions, multiplying the results by the
    # neural network weight. Check: If only generating 1 prediction,
    # nrow may be NULL, so convert to df
    if (is.null(nrow(predicted_response)))
      predicted_response <- t(
        as.data.frame(predicted_response))

    running_total <- matrix(0, 1, nrow = nrow(predicted_response))

    for (i in 1:(ncol(predicted_response))) {
      running_total <- running_total + (predicted_response[, i]
                                      * measure_weights[i])
    }

    all_ensemble_predictions <- cbind(all_ensemble_predictions,
                                      (running_total / sum(measure_weights)))
  }
  colnames(all_ensemble_predictions) <- measures

  return(all_ensemble_predictions)
}

#' Predict simulation responses for a parameter set using an ensemble
#'
#' This takes a set of unseen parameter values and uses an ensemble to make
#' predictions of the responses that the simulator would generate
#'
#' @param generated_ensemble Ensemble to use to make predictions
#' @param data_to_predict Parameter sets to make predictions from
#' @param parameters Simulation parameter names
#' @param measures Simulation output response names
#' @param normalise_values Whether the unseen parameter sets should be
#' normalised to be between 0 and 1
#' @param normalise_result Whether the generated predictions should be
#' normalised to be between 0 and 1
#' @return List of predictions made for specified responses for all
#' parameter sets
#' @export
use_ensemble_to_generate_predictions <- function(generated_ensemble,
                                                 data_to_predict, parameters,
                                                 measures, normalise_values = FALSE,
                                                 normalise_result = FALSE) {

  # Normalise unseen data if required
  if (normalise_values) {
    data_to_predict <- normalise_dataset(
      data_to_predict, generated_ensemble$pre_normed_mins[,parameters],
      generated_ensemble$pre_normed_maxes[,parameters], parameters)$scaled
  }

  all_model_predictions <- NULL

  for (model in 1:length(generated_ensemble$ensemble$emulators)) {
    emulator <- generated_ensemble$ensemble$emulators[[model]]
    emulator_predictions <- generate_predictions_from_emulator(emulator,
                                                               parameters,
                                                               measures,
                                                               data_to_predict)

    all_model_predictions <- cbind(all_model_predictions,
                                   extract_predictions_from_result_list(
                                     emulator_predictions, emulator$type,
                                     measures))
  }

  novel_predictions <- weight_emulator_predictions_by_ensemble(
    generated_ensemble$ensemble$weights, all_model_predictions, measures,
    num_generations = 800000)

  # Rescale the predictions if data was normalised
  if (normalise_result) {
    # rbind used to get the mins and maxes into the correct format
    # (compatible with all other calls to this function)
    novel_predictions <- denormalise_dataset(
      novel_predictions, rbind(generated_ensemble$pre_normed_mins[, measures]),
      rbind(generated_ensemble$pre_normed_maxes[, measures]))
  }
  return(novel_predictions)
}
