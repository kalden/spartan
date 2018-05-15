#' Evolve parameter sets that meet a desired ensemble outcome
#'
#' This method takes a user specified fitness function and runs the nsga2
#' algorithm on an ensemble using the nsga2 implementation provided in the
#' mco package, in an attempt to locate parameters that achieve a desired
#' response (determined by the fitness function). The method outputs a list
#' describing the values for each simulation output measure, (or objective,
#' res), an  evolved set of parameter inputs (par), and a boolean stating
#' whether the candidate is pareto optimal (pareto.optimal)
#' @param function_to_evaluate A user-defined function that NSGA2 seeks to
#' minimise
#' @param nsga2_user_set_parameters An object containing the emulator input
#' and output names, the input parameters for function to evaluate, minimum
#' and maximum values for emulator inputs. These should be set using the
#' function that creates that object prior to running this method
#' @param nsga2_settings An object containing the population size,
#' number of generations, crossover probability and mutation probability
#' to be assessed. Again see the function nsga2_settings to set these
#' values before running this function
#' @return List containing evolved parameter sets, the output for the
#' ensemble using those sets, and whether these sets are pareto optimal
#'
#' @export
emulator_parameter_evolution <- function(function_to_evaluate,
                                         nsga2_user_set_parameters,
                                         nsga2_settings) {
  res <- mco::nsga2(function_to_evaluate,
                    length(nsga2_user_set_parameters$parameters),
                    length(nsga2_user_set_parameters$measures),
                    nsga2_user_set_parameters,
                    lower.bounds = nsga2_user_set_parameters$lower.bounds,
                    upper.bounds = nsga2_user_set_parameters$upper.bounds,
                    popsize = nsga2_settings$popsize,
                    generations = nsga2_settings$generations,
                    cprob = nsga2_settings$cprob,
                    mprob = nsga2_settings$mprob)
  return(res)
}


#' Screens NSGA-2 related parameters, guiding which to select for evolving
#' parameter sets
#'
#' This method performs a sensitvity analysis of key settings for the nsga2
#' algorithm. Different values of generation number, crossover and mutation
#' rate are assessed and the values for each objective, along with the
#' variance of the parameter inputs are written out to file in .csv format
#' so the user can assess which settings are best suited to the chosen
#' application. Values for the crossover and mutation distribution indices,
#' used in simulated binary crossover, are left at their default settings,
#' but can be overwritten when running the emulator_parameter_evolution method.
#' @param function_to_evaluate A user-defined function that NSGA2 seeks to
#' minimise
#' @param nsga2_user_set_parameters An object containing the emulator input
#' and output names, the input parameters for function to evaluate, minimum
#' and maximum values for emulator inputs. These should be set using the
#' function that creates that object prior to running this method,
#' nsga2_set_user_params
#' @param nsga_sensitivity_parameters an object containing the minimum and
#' maximum values of generation number, crossover probability and mutation
#' probability to be assessed
#' @param nsga2_settings An object containing the population size, number
#' of generations, crossover probability and mutation probability to be
#' assessed
#' @return Output showing the results of this sensitivity analysis for the
#' NSGA-2 parameters
#'
#' @export
screen_nsga2_parameters <- function(function_to_evaluate,
                                    nsga2_user_set_parameters,
                                    nsga_sensitivity_parameters,
                                    nsga2_settings) {
  #set the seed
  if (!is.null(nsga_sensitivity_parameters$seed))
    set.seed(nsga_sensitivity_parameters$seed)

  output <- c()

  #iterate through all of the generation, crossover and mutation values
  #recording the outputs in a dataframe
  for (i in seq(nsga_sensitivity_parameters$Generation_min,
                nsga_sensitivity_parameters$Generation_max, by = 100)) {
    for (j in seq(nsga_sensitivity_parameters$Crossover_min,
                 nsga_sensitivity_parameters$Crossover_max, by = 0.1)) {
      for (k in seq(nsga_sensitivity_parameters$Mutation_min,
                   nsga_sensitivity_parameters$Mutation_max, by = 0.1)) {

        message(paste("Generation Number: ", i, sep = ""))
        message(paste("Crossover: ", j, sep = ""))
        message(paste("Mutation: ", k, sep = ""))

        #run nsga2
        nsga2_settings$cprob <- j
        nsga2_settings$mprob <- k
        nsga2_settings$generations <- i

        res <- emulator_parameter_evolution(function_to_evaluate,
                                            nsga2_user_set_parameters,
                                            nsga2_settings)

        colnames(res$par) <- nsga2_user_set_parameters$parameters

        normalised_params <- normalise_dataset(
          res$par, nsga2_user_set_parameters$lower.bounds,
          nsga2_user_set_parameters$upper.bounds,
          nsga2_user_set_parameters$parameters)

        #calculate the median variance of the pareto front parameters
        var <- median(apply(normalised_params$scaled, 2, var))

        #add the results to a dataframe
        data <- c(i, j, k, median(res$value[, 1]), median(res$value[, 2]),
                  median(res$value[, 3]), var)
        output <- rbind(output, data)

      }
    }
  }

  #write the results out to file
  colnames(output) <- c("generation", "crossoverrate", "mutationrate",
                        "objective1", "objective2", "objective3",
                        "parameter_variance")
  write.csv(output, file = "NSGA2OptimisationResults.csv",
            row.names = F, quote = F)
  return(output)
}

#' Initialise analysis specific parameters for NSGA-2
#'
#' Creates an object of the analysis parameters that will be used to evolve
#' parameter sets or screen parameters for NSGA-2. The user should ensure this
#' is calle first, establishing this object such that it can be passed in to
#' the relevant method
#' @param built_ensemble Ensemble object that will be used in the NSGA-2
#' algorithm to generate predictions
#' @param parameters Names of simulation parameters for which values are input
#' to the  ensemble
#' @param measures Names of the simulation measures for which the ensemble
#' predicts
#' @param desiredResponses Vector of desired responses for the simulation
#' measures, used by the fitness function to determine goodness of fit for
#' evolved parameter sets
#' @param sampleMins Minimum value of the range of each parameter to be used
#' in evolving parameter sets
#' @param sampleMaxes Maximum value of the range of each parameter to be used
#' in evolving parameter sets
#' @return List of the above objects for passing in as settings object to
#' NSGA-2 related methods
#'
#' @export
nsga2_set_user_params <- function(built_ensemble, parameters, measures,
                                  desiredResponses, sampleMins, sampleMaxes) {

  return(list("emulator"=built_ensemble,"parameters"=parameters,
       "measures"=measures, "desiredResponses"=desiredResponses,
       "lower.bounds"=sampleMins, "upper.bounds"=sampleMaxes))
}

#' Set parameters for NSGA-2 sensitivity analysis
#'
#' Establish the parameters for the NSGA-2 sensitivity analysis, creating an
#' object that is used within the method that screens NSGA-2 parameters.
#'
#' @param generation_min Minimum value for number of generations
#' @param crossover_min Minimum value for crossover
#' @param mutation_min Minimum value for mutation rate
#' @param generation_max Maximum value for number of generations
#' @param crossover_max Maximum value for crossover
#' @param mutation_max Maximum value for mutation rate
#' @param seed Random seed value to use in the algorithm
#' @return List of the above, for passing in as a settings object to NSGA-2
#' related methods
#'
#' @export
set.nsga_sensitivity_params <- function(generation_min, crossover_min,
                                        mutation_min, generation_max,
                                        crossover_max, mutation_max, seed) {

  return(list("Generation_min" = generation_min,
              "Crossover_min" = crossover_min,
              "Mutation_min" = mutation_min,
              "Generation_max" = generation_max,
              "Crossover_max" = crossover_max,
              "Mutation_max" = mutation_max,
              seed = seed))
}
