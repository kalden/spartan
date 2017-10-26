#' Initialise machine-learning algorithms settings for emulation creation
#'
#' Some of the machine learning algorithms incorporated have their own specific
#' settings (neural network and random forest, for example). To keep the
#' methods generic to multiple algorithms and to save passing multiple objects
#' around, these are declared in this function and returned as an
#' algorithmSettings object. Where the user wishes to use the default values,
#' there is no need to run this function: this will be created during emulator
#' generation. However, should the user wish to change any of the settings,
#' such as the number of generations in the neural network or the number of
#' trees in random forest, they should run this method with the new values.
#' In addition, this object will also contain two other settings: whether
#' graphs should be plotted of the accuracy of the emulator against the
#' training and test sets, and whether the emulator object that is created
#' should be stored in the working directory. Parameters this object stores
#' are detailed in the arguments section. However, for neural network
#' emulation, the user is required to initialise this object with a list
#' of neural network hidden layer structures to evaluate. Should this not
#' be done, an error message will be produced and the call will terminate.
#' @param num_trees Number of trees used to generate a random forest model. If
#' a random forest is not selected as the emulation technique, this argument
#' does not need to be provided. Defaults to 500
#' @param num_of_generations Used in neural network generation, as the maximum
#' number of steps used in training the network. If this is reached, then
#' training is stopped. If a neural network is not selected as the emulation
#' technique, this argument does not need to be provided. Defaults to 800,000
#' @param num_of_folds Number of folds (subsets of training data) used in
#' k-fold cross validation when developing a neural network emulator. If a
#' neural network is not selected as the emulation technique, this argument
#' does not need to be provided. Defaults to 10.
#' @param network_structures List of neural network structures to examine
#' using k-fold cross validation when developing a neural network emulator.
#' Should be a list in the form of number of nodes in each hidden layer. For
#' example, c(c(4),c(4,3)) would consider two structures, one for a single
#' hidden layer of 4 nodes, and another with two hidden layers of 4 and 3 nodes.
#' If a neural network is not selected as the emulation technique, this
#' argument does not need to be provided.
#' @param save_emulators Boolean indicating whether the generated emulator
#' for each simulation output should be saved to file (as an Rda object).
#' This is stored in the current working directory. Defaults to TRUE
#' @param save_ensemble Used in Technique 7, to state whether an ensemble
#' generated from multiple emulators should be saved to file. Defaults to
#' TRUE
#' @param plot_test_accuracy Boolean indicating whether a plot showing a
#' comparison against the emulator predicted values to those observed in
#' the test set should be created. This ggplot is stored as a PDF in the
#' current working directory. Defaults to FALSE
#' @return List of all the elements in the parameters above
#'
#' @export
#'
emulation_algorithm_settings <-
  function(num_trees = 500, num_of_generations = 800000, num_of_folds = 10,
           network_structures = NULL, save_emulators = TRUE,
           save_ensemble = TRUE, plot_test_accuracy = TRUE) {

  # This function compiles the settings for the various algorithms into a list.
  #If the user wishes to change these defaults,  they should call this function
  # first then pass the returned object into the generate_requested_emulations
  # method. If they are happy with the defaults,  this is called by the
  # generate_algorithm if  a settings object has not been passed.

  # The only difference to the above is the neural network structures -
  # if using a neural network the user MUST call this first and provide a
  # list of structures
  return(list("num_trees" = num_trees,
              "num_of_generations" = num_of_generations,
              "num_of_folds" = num_of_folds,
              "network_structures" = network_structures,
              "save_emulators" = save_emulators,
              "save_ensemble" = save_ensemble,
              "plot_test_accuracy" = plot_test_accuracy))

}

#' Internal function to check whether acceptable models have been specified
#' @param model_type Model type to be generated
#' @return True or False, whether the model is acceptable
#' @keywords internal
check_acceptable_model_type <- function(model_type) {
  acceptablemodel_types <- c("NNET", "RF", "GLM", "GP", "SVM")

  if (model_type %in% acceptablemodel_types)
    return(TRUE)
  else
    return(FALSE)
}

#' Internal function to generate the formula for training the models
#' @param parameters Parameters on which the model will take as input
#' @param measures Simulation responses the model should predict
#' @return formula object for use in model creation
#' @keywords internal
generate_model_formula <- function(parameters, measures) {
  # For the emulation methods that require a formula (nn, lm, svm),
  # this method generates that formula
  return(stats::as.formula(paste(paste(measures, collapse = "+"), "~",
                                 paste(parameters, collapse = "+")),env = parent.frame()))
}

#' Internal function used to generate the requested emulator, and graph
#' training performance
#' @param technique Machine learning technique being employed
#' @param parameters Parameters on which the model will take as input
#' @param measures Simulation responses the model should predict
#' @param dataset The training dataset used to create the model
#' @param algorithm_settings Algorithm settings object, of parameters used by
#' the emulation creation algorithm
#' @param timepoint Simulation timepoint for which an emulator is being created
#' @param normalised Whether the data being input as training data has been
#' normalised. Affects whether predictions generated by the emulator are
#' rescaled or not
#' @return Emulator objects of the specified technique for all measures, and
#' performance statistics
#'
#' @import e1071 randomForest mlegp stats neuralnet
#'
#' @keywords internal
generate_emulator_model <- function(technique, parameters, measures, dataset,
                                    algorithm_settings, timepoint = NULL,
                                    normalised = FALSE) {
  start.time <- proc.time()
  models_for_all_measures <- vector("list", length(measures))

  # We're going to do this by measure - creating an emulation for each response
  for (m in 1:length(measures)) {
    # Make the formula
    model_formula <- generate_model_formula(parameters, measures[m])

    if (technique == "SVM") {
      model_fit <- e1071::svm(
        model_formula, data = dataset$training[, c(parameters, measures[m])])
      # Stats for the training fit:
      model_training_fit <- predict(model_fit,
                                  newdata = dataset$training[, parameters])
    }

    if (technique == "GLM") {
      # Generate the linear model
      model_fit <- stats::lm(
        model_formula, data = dataset$training[, c(parameters, measures[m])])
      # Stats for the training fit:
      model_training_fit <- stats::predict.lm(
        model_fit,  newdata = dataset$training[, parameters])
    }

    if (technique == "RF")  {
      # Generate the RF:
      model_fit <- randomForest::randomForest(
        model_formula, data = dataset$training[, c(parameters, measures[m])],
        ntree = algorithm_settings$num_trees)
      # Stats for the training fit:
      model_training_fit <- predict(model_fit,
                                  newdata = dataset$training[, parameters])
    }

    if (technique == "GP")  {
      ## Generate the gaussian process model:
      model_fit <- mlegp::mlegp(
        dataset$training[, parameters], dataset$training[, measures[m]])
      # Stats for the training fit
      model_training_fit <- mlegp::predict.gp(
        model_fit, as.matrix(dataset$training[, parameters]))
    }

    if (technique == "NNET") {
      # Determine optimal structure
      optimal_structure <- determine_optimal_neural_network_structure(
        dataset$training,  parameters,  measures[m],  algorithm_settings)
      # Create the network
      # NULL is the fold number,  only used in k fold validatiokn - second
      # dataset is sent in so mins and maxes can be accessed by the neural net
      # That is a legacy from our previous neural network code,
      # and could possibly be removed
      model_fit <- create_neural_network(model_formula, dataset$training,
                                     NULL, dataset, optimal_structure,
                                     algorithm_settings$num_of_generations)

      # Stats for the training fit
      model_training_fit <- neuralnet::compute(model_fit,
                                               dataset$training[, parameters])
      model_training_fit <- model_training_fit$net.result
      #  Set to be solely the predictions (not neurons) for consistency
      # with all other approaches
    }

    # Plot the training accuracy for this measure,  if the user wanted it
    if (algorithm_settings$plot_test_accuracy == TRUE) {
      if (normalised) {
        # Want to denormalise the predictions such that these are plotted
        # on the correct scale
        unscaled_predictions <- denormalise_dataset(
          cbind(model_training_fit),
          rbind(dataset$pre_normed_mins[measures[m]]),
          rbind(dataset$pre_normed_maxes[measures[m]]))
        unscaled_simulations <- denormalise_dataset(
          dataset$training, rbind(dataset$pre_normed_mins),
          rbind(dataset$pre_normed_maxes))
        produce_accuracy_plots_single_measure(paste(
          technique, "_TrainingSet", sep = ""), measures[m],
          unscaled_predictions,
          unscaled_simulations[, measures[m]], timepoint = timepoint)
      } else {
        produce_accuracy_plots_single_measure(
          paste(technique, "_TrainingSet", sep = ""), measures[m],
          model_training_fit, dataset$training[, measures[m]],
          timepoint = timepoint)
      }
    }

    # Add this emulation for this measure to list of models for all measures
    models_for_all_measures[[m]] <- model_fit

  }

  # Calculate time taken,  only want the elapsed value (3rd one)
  time.taken <- as.numeric(proc.time() - start.time)[3]

  return(list("models" = models_for_all_measures, "benchmark" = time.taken,
              "type" = technique))
}

#' Used to generate predictions from an emulator, normalising data if required
#'
#' With an emulator object produced, this can be used to generate predictions
#' on unseen data. This method is called with the emulation object,
#' parameters, meaasures, and unseen data. A flag should also be set as to
#' whether the unseen data, and thus the generated prediction, need to be
#' normalised and rescaled accordingly. Unseen data being input into the
#' emulator must be scaled between 0 and 1, with predictions rescaled after
#' generation.
#' @param emulation The emulation object to use to make the predictions
#' @param parameters Parameters on which the model will take as input
#' @param measures Simulation responses the model should predict
#' @param data_to_predict Unseen values for the parameters for which the
#' measures should be predicted
#' @param normalise Whether the data_to_predict should be normalised
#' @param normalise_result Whether the resultant predictions should be
#' normalised
#' @return Predictions generated for this unseen data
#'
#' @export
emulator_predictions <- function(emulation, parameters, measures,
                                 data_to_predict, normalise=FALSE,
                                 normalise_result = FALSE) {
  if (normalise) {
    # We need to normalise the parameters the user has sent in:
    data_to_predict <- normalise_dataset(data_to_predict,
                                       emulation$pre_normed_mins[parameters],
                                       emulation$pre_normed_maxes[parameters],
                                       parameters)

    # Normalise returns an object with scaled data, mins and maxes. To keep
    # consistency get rid of the mins and maxes
    data_to_predict <- data_to_predict$scaled
  }


  predictions <- NULL
  # Run this for all emulators in the object
  for (e in 1:length(emulation$emulators)) {
    model_predictions <- generate_predictions_from_emulator(
      emulation$emulators[[e]], parameters, measures, data_to_predict)
    predictions <- cbind(
      predictions, extract_predictions_from_result_list(
        model_predictions, emulation$emulators[[e]]$type, measures))
  }

  # Now we need to scale the predictions back,  if these were normalised
  if (normalise_result)  {
    # rbind used to get the mins and maxes into the correct format
    # (compatible with all other calls to this function)
    predictions <- denormalise_dataset(
      predictions, rbind(emulation$pre_normed_mins[measures]),
      rbind(emulation$pre_normed_maxes[measures] ))
  }

  return(predictions)
}

#' Internal function to generate predictions from an emulator
#'
#' @param emulation The emulation object to use to make the predictions
#' @param parameters Parameters on which the model will take as input
#' @param measures Simulation responses the model should predict
#' @param data_to_predict Unseen values for the parameters for which the
#' measures should be predicted. Has been pre-processed such that it is
#' in the correct scale for the emulation.
#' @return Predictions generated for this unseen data, in the same scale
#' as the normalised data used to train the emulation.
#' @keywords internal
#'
generate_predictions_from_emulator <- function(emulator, parameters, measures,
                                               data_to_predict) {
  # Note this receives a list of emulators,  one for each simulation response
  # Thus this will return a list of predicted responses,  necessary for making
  # the ensemble
  predictions_all_measures <- vector("list", length(measures))

  for (m in 1:length(measures)) {
    measure_emulation <- emulator$models[[m]]
    technique <- emulator$type

    ## Neural net uses compute rather than predict,  so check for that here
    if (technique == "NNET") {
      predictions <- neuralnet::compute(measure_emulation,
                                        data_to_predict[, parameters])
      # To make common with everything else,  set predictions to be just
      # the net results
      predictions <- predictions$net.result
    }
    else if (technique == "GP") {
      ## mlegp insists that the data to predict is within a matrix
      predictions <- mlegp::predict.gp(measure_emulation,
                                       as.matrix(data_to_predict[, parameters]))
    }
    else if (technique == "SVM") {
      predictions <- predict(measure_emulation,
                             as.matrix(data_to_predict[, parameters]))
    }
    else if (technique == "GLM") {
      predictions <- stats::predict.lm(measure_emulation,
                                       newdata = data_to_predict[, parameters])
    }
    else if (technique == "RF") {
      predictions <- predict(measure_emulation,
                             newdata = data_to_predict[, parameters])
    }


    # Add the predictions for this measure to the return list
    predictions_all_measures[[m]] <- predictions
  }

  return(predictions_all_measures)
}


#' Internal function used to extract the predictions made in emulation generation from a list
#' @param model_predictions Predictions that have been generated by an emulator
#' @param technique The machine learning technique used to develop the emulator
#' @param measures Simulation responses the model should predict
#' @return Predicted results extracted from the model_predictions list
#' @keywords internal
extract_predictions_from_result_list <- function(model_predictions, technique,
                                                 measures) {
  results <- NULL
  result_names <- NULL
  for (m in 1:length(measures)) {
    results <- cbind(results, as.numeric(model_predictions[[m]]))
    result_names <- c(result_names, paste(technique, "_", measures[m],
                                          sep = ""))
  }
  colnames(results) <- result_names
  return(results)
}

#' Calculate the mean squared error between predictions and observed data
#' @param model_predictions Predicted dataset#
#' @param observed_data Observed dataset (testing or validation)
#' @return Mean Squared Error between the two sets
#' @keywords internal
meanSquaredError <- function(model_predictions, observed_data) {
  # Mean squared error between prediction and observed
  return(sqrt( (sum( (observed_data - model_predictions) ^ 2)) /
                length(observed_data)))
}

#' Calculate the R squared statistic for predictions and observed data
#' @param model_predictions Predicted dataset
#' @param observed_data Observed dataset (testing or validation)
#' @return R squared statistic for the two sets
#' @keywords internal
rSquared <- function(model_predictions,  observed_data) {
  linear.model <- lm(model_predictions ~ observed_data)
  return(summary(linear.model)$r.squared)
}

#' Internal function to build performance statistics (MSE, R2, generation
#' time) for an emulation
#'
#' @param technique The machine learning technique used to develop the emulator
#' @param model_predictions Predicted dataset
#' @param observed_data Observed dataset (testing or validation)
#' @param measures Simulation responses the model should predict
#' @param benchmark Dataset of benchmarking data to which these results are
#' being added
#' @return Updated benchmark object, probably of numerous machine learning
#' techniques, showing the MSE, R2, and generation time for this technique
#' @keywords internal
build_performance_statistics <- function(technique, model_predictions,
                                         observed_data, measures, benchmark) {
  # Used in all modelling techniques to generate performance statistics
  # for comparison
  model_stats <- cbind(technique)
  colnames(model_stats) <- c("Technique")

  for (m in 1:length(measures)) {
    # Note use of [, m] here,  not measures[m] - the model predictions are
    #headed technique_measure,  so we use the order instead of a direct
    #column reference by name
    measure_stats <- cbind(meanSquaredError(model_predictions[, m],
                                            observed_data[, measures[m]]),
                           "rsquared" = rSquared(model_predictions[, m],
                                                 observed_data[, measures[m]]))
    colnames(measure_stats) <- c(paste(measures[m], "_MSE", sep = ""),
                                 paste(measures[m], "_R2", sep = ""))

    # Bind to current set of results for other measures
    model_stats <- cbind(model_stats, measure_stats)
  }

  # Add the generation time for the technique
  model_stats <- cbind(model_stats, benchmark)
  colnames(model_stats)[length(model_stats)] <- "Generation_Time"

  return(model_stats)
}
