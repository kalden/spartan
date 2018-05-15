#' Create neural network emulator, using neuralnet package
#' @param formula Parameters and measures formula to use in creating the
#' network
#' @param training_set Training data on which to train the network
#' @param fold_number Number of the fold being created in k-fold cross
#' validation
#' @param normalised_data Normalised version of the training data
#' @param network_structure Hidden layers structure to use to create the
#' network
#' @param number_of_generations Number of generations to train the
#' network within in the hope of convergence, else training terminates
#' @return Neural network created
create_neural_network <- function(formula, training_set, fold_number,
                                  normalised_data, network_structure,
                                  number_of_generations) {
  # Now we can train the Neural Network
  # Need a catch here incase the network does not converge:
  success <- tryCatch({
      nn <- neuralnet::neuralnet(formula, data = training_set,
                                 hidden = network_structure,
                                 linear.output = T,
                                 stepmax = number_of_generations)
    },
    error = function(cond) {
      if (is.null(fold_number)){
        message(paste("Problem generating Neural Network ", cond, sep = ""))
      } else {
        message(paste("Fold ", fold_number,
                      ": Problem generating Neural Network ",
                      cond, sep = ""))
      }
    },
    warning = function(cond) {
      if (is.null(fold_number)) {
        message(paste("Problem generating Neural Network ", cond, sep = ""))
      } else {
        message(paste("Fold ", fold_number,
                      ": Problem generating Neural Network ", cond, sep = ""))
      }
    })

  if (typeof(success) == "list") {
    # The mins/maxes must be outside of the trycatch for the error check to work
    nn$mins <- normalised_data$mins
    nn$maxes <- normalised_data$maxs
    return(nn)
  } else  {
    return(NULL)
  }
}

#' Selects the most suitable neural network structure from the potentials made
#'
#' The user will provide a list of neural network objects to assess. These
#' will be assessed on their performance by mean squared error. This method
#' simply selects the most suitable structure (where MSE is lowest)
#' @param network_errors MSE obtained for each measure for all neural network
#' layer structures examined
#' @return Lowest error network structure
selectSuitableStructure <- function(network_errors) {

  # Get the row numbers of the minimum mean squared error for each measure.
  # These are all strings due to the way the data is labelled, so the
  #structure needs to be converted to numeric once the label is removed
  unlabelled_results <- sapply(
    data.frame(network_errors[, 2:ncol(network_errors)],
               stringsAsFactors = FALSE), as.numeric)

  min_mses <- apply(unlabelled_results, 2, which.min)

  # Now the simplest case, where the minimum is the same network structure
  #for all simulation measures
  if (isTRUE(all.equal(max(min_mses), min(min_mses)))) {
    # The row number should equal the structure in the network structure
    # list, so this can be returned
    return(min_mses[1])
  } else {

    # However it is more difficult to deal with the case where these are
    # different.
    # Eventually it may be best to come up with a weighting for each measure,
    # however for the moment we'll take the average
    # of each row, for each measure, and return the structure that performs
    #best on average
    # this needs to be a dataframe or you can get an error
    rowMeans <- data.frame(apply(unlabelled_results, 1, mean))
    # Now return the minimum mean
    #think that this doesnt like the fact that its not a dataframe

    return(which.min( (apply(rowMeans, 1, mean))))
  }
}

#' Determine the optimal hidden layer structure from those provided
#' @param dataset Data on which the neural network is being trained
#' @param parameters Names of the parameters that form the input nodes of
#' the neural network
#' @param measures Names of the simulation responses that form the output
#' node of the neural network
#' @param algorithm_settings Object output from the function
#' emulation_algorithm_settings, containing the settings of the machine
#' learning algorithms to use in emulation creation. In this case, the
#' settings parameter we are interested in is number of generations
#' @return Optimal network structure from those provided for assessment
determine_optimal_neural_network_structure <- function(dataset, parameters,
                                                       measures,
                                                       algorithm_settings) {
  # k-fold cross validation of neural network structures
  network_errors <- kfoldCrossValidation(dataset, parameters, measures,
                                        algorithm_settings)

  #choose the best structure and generate the network, sending it to filepath
  if (!is.null(network_errors)) {

    # Now select the best performing network
    if(length(algorithm_settings$network_structures))
    {
      return(algorithm_settings$network_structures[[1]])
    }
    else
    {
      network_index <- selectSuitableStructure(network_errors)

      return(algorithm_settings$network_structures[[network_index]])
    }
  } else {
    message("Network Structure could not be determined")
    return(NULL)
  }
}

#' Calculate the mean squared error for this fold in k-fold cross validation
#' @param nn_predictions Predictions made by neural net
#' @param test_fold The test data on which performance is being assessed
#' @param nn The generated neural network
#' @param measures The simulation output responses that are being predicted
#' @return Mean Squared error for this fold.
calculate_fold_MSE <- function(nn_predictions, test_fold, nn, measures) {

  fold_ms_errors <- NULL
  for (measure in 1:length(measures)) {

    fold_ms_errors <- cbind(fold_ms_errors,
                          meanSquaredError(nn_predictions$net.result,
                                           test_fold[measures]))
  }
  return(fold_ms_errors)
}

#' Create training data fold for k-fold cross validation
#' @param fold Number of the fold being examined
#' @param number_of_folds Total number of folds
#' @param training_fold_start Position at which the training fold starts in the
#' dataset
#' @param training_fold_end Position at which the training fold ends in the
#' dataset
#' @param dataset Training data
#' @return Training data for this fold of k-fold cross validation
createTrainingFold <- function(fold, number_of_folds, training_fold_start,
                               training_fold_end, dataset) {

  # Now join the training folds
  # First is easy, just the remainder of the set
  if (fold == 1) {
    return(dataset[(training_fold_end + 1):nrow(dataset), ])
  } else if (fold == number_of_folds) {
    # Last one need to make sure we stop before test set
    return(dataset[1:(training_fold_start - 1), ])
  } else
    # Training data will be construction of folds either side of this
    return(rbind(dataset[1:(training_fold_start - 1), ],
                 dataset[(training_fold_end + 1):nrow(dataset), ]))
}

#' Create test data fold for k-fold cross validation
#' @param fold Number of the fold being examined
#' @param number_of_folds Total number of folds
#' @param test_fold_start Position at which the test fold starts in the dataset
#' @param test_fold_end Position at which the test fold ends in the dataset
#' @param dataset Testing data
#' @return Testing data for this fold of k-fold cross validation
createtest_fold <- function(fold, number_of_folds, test_fold_start,
                            test_fold_end, dataset) {

  # Last test set must contain the remainder
  if (fold == number_of_folds)
    test_fold_end <- nrow(dataset)

  # Create the test fold data
  return(dataset[test_fold_start:test_fold_end, ])
}

#' Perform k-fold cross validation for assessing neural network structure
#' performance
#' @param dataset Data on which the neural network is being trained and
#' assessed
#' @param parameters Names of the parameters that form the input nodes of
#' the neural network
#' @param measures Names of the simulation responses that form the output
#' node of the neural network
#' @param algorithm_settings Object output from the function
#' emulation_algorithm_settings, containing the settings of the machine
#' learning algorithms to use in emulation creation. In this case, the
#' settings parameter we are interested in is number of generations
#' @return Mean Squared errors for all potential structures
kfoldCrossValidation <- function(dataset, parameters, measures,
                                 algorithm_settings) {
  # Make the formula
  model_formula <- generate_model_formula(parameters, measures)

  # Get the number of records in all the data
  data_size <- nrow(dataset)
  # Calculate fold size
  fold_size <- data_size %/% algorithm_settings$num_of_folds

  average_errors <- analysenetwork_structures(fold_size, dataset, model_formula,
                                             parameters, measures,
                                             algorithm_settings)

  return(average_errors)
}

#' Analyse each network structure provided as a potential NN structure
#' @param fold_size Number of rows in which the dataset is divided into folds
#' @param dataset Dataset used to assess each strutcure
#' @param model_formula Parameters and measures formula used to specify the
#' input and output layers of the hidden network
#' @param parameters Names of the parameters that form the input nodes of
#' the neural network
#' @param measures Names of the simulation responses that form the output
#' node of the neural network
#' @param algorithm_settings Object output from the function
#' emulation_algorithm_settings, containing the settings of the machine
#' learning algorithms to use in emulation creation. In this case, the
#' settings parameter we are interested in is number of generations
#' @return mean squared errors for the assessed structures
analysenetwork_structures <- function(fold_size, dataset, model_formula,
                                      parameters, measures,
                                      algorithm_settings){

  # Now for the k-fold cross validation of a number of neural network setups
  # Average errors will show the mean error for all setups, for each measure,
  # easing the choice of structure
  average_errors <- NULL

  for (n in 1:length(algorithm_settings$network_structures)) {
    message(paste("Network Structure: ",
                algorithm_settings$network_structures[[n]], sep = ""))

    # Set the pointers for the first test fold
    test_fold_start <- 1
    test_fold_end <- fold_size

    network_ms_errors <- createAndEvaluateFolds(
      fold_size, test_fold_start, test_fold_end, dataset,
      algorithm_settings$network_structures[[n]], model_formula, parameters,
      measures, algorithm_settings)
    average_errors <- updateErrorForStructure(
      network_ms_errors, algorithm_settings$network_structures[[n]],
      average_errors, measures)
  }
  return(average_errors)

}

#' Add the MSE for a newly examined structure to the list of those already seen
#' @param network_ms_errors Mean Squared errors for a newly evaluated structure
#' @param network_struc The network structure evaluated
#' @param average_errors The current list of MSEs for all structures being
#' evaluated
#' @param measures Names of the simulation responses that form the output
#' node of the neural network
updateErrorForStructure <- function(network_ms_errors, network_struc,
                                    average_errors, measures) {

  # Now need to check that network_ms_errors is not NULL
  # (i.e. none of the folds converged)
  if (!is.null(network_ms_errors))
    average_errors <- rbind(average_errors, c(apply(cbind(network_struc), 2,
                                                  paste, collapse = " "),
                                            apply(network_ms_errors, 2, mean)))
  else
    # We'll put an inf in for this network set up to show no convergence
    average_errors <- rbind(average_errors, c(apply(cbind(network_struc), 2,
                                                  paste, collapse = " "),
                                            rep(Inf, length(measures))))
  return(average_errors)

}

#' Create and evaluate folds within k-fold cross validation
#' @param fold_size Number of rows of the dataset that form each fold
#' @param test_fold_start Starting position of the training or test fold in the
#'  dataset
#' @param test_fold_end End position of the training or test fold in the
#' dataset
#' @param dataset Dataset for which the training and test folds are being
#' developed
#' @param network_struc Network structure for which the folds are being assessed
#' @param formula Parameters and measures formula to use in creating the
#' network
#' @param parameters Names of the parameters that form the input nodes of
#' the neural network
#' @param measures Names of the simulation responses that form the output
#' node of the neural network
#' @param algorithm_settings Object output from the function
#' emulation_algorithm_settings, containing the settings of the machine
#' learning algorithms to use in emulation creation. In this case, the
#' settings parameter we are interested in is number of generations
#' @return MSE errors for all network structures
createAndEvaluateFolds <- function(fold_size, test_fold_start, test_fold_end,
                                   dataset, network_struc, formula,
                                   parameters, measures, algorithm_settings) {
  # networkMSerror will hold the errors for all folds in this structure.
  #Result will then be the mean of each fold
  network_ms_errors <- NULL

  for (fold in 1:algorithm_settings$num_of_folds) {
    test_fold <- createtest_fold(fold, algorithm_settings$num_of_folds,
                               test_fold_start, test_fold_end, dataset)

    # Status of run
    if (!fold == algorithm_settings$num_of_folds)
      message(paste("Fold ", fold, " Test Start: ", test_fold_start, " End: ",
                  test_fold_end, sep = ""))
    else
      message(paste("Fold ", fold, " Test Start: ", test_fold_start, " End: ",
                  nrow(dataset), sep = ""))

    # Create the training fold
    train_fold <- createTrainingFold(fold, algorithm_settings$num_of_folds,
                                    test_fold_start, test_fold_end, dataset)

    # Create the network
    nn <- create_neural_network(formula, train_fold, fold, dataset,
                                network_struc,
                                algorithm_settings$num_of_generations)

    if (!is.null(nn)) {
      # Generate predictions on the test data
      nn_predictions <- neuralnet::compute(nn, test_fold[parameters])

      fold_ms_errors <- calculate_fold_MSE(nn_predictions, test_fold, nn,
                                           measures)

      # Add the MSE for this fold to those for this network structure
      network_ms_errors <- rbind(network_ms_errors, fold_ms_errors)
    }

    # Increment the pointers whether this fold worked or not
    test_fold_start <- test_fold_start + fold_size
    test_fold_end <- test_fold_end + fold_size
  }

  return(network_ms_errors)
}
