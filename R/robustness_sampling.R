#' Create parameter samples for robustness (local) analysis
#'
#' The robustness of a simulation to parameter alteration can be determined
#' through the use of this approach. Following the method described by Read
#' et al, the value of each parameter is adjusted independently, with the
#' remaining parameters staying unchanged from their calibrated value. This
#' method within the toolkit creates a set of simulation parameter sets to
#' enable such an analysis to be performed. One CSV file is created for each
#' parameter being examined (with the filename being
#' [Parameter Name]_Values.csv). Each CSV file will contain the parameters for
#' runs that need to be performed. For each set of parameters, the simulation
#' should be run for the number of times determined by Aleatory Analysis.
#' Once this has been completed, the results can be analysed
#' using the robustness analysis  methods included within this package
#'
#' @param FILEPATH Directory where the parameter samples should be output to. For spartan-db this can be NULL
#' @param PARAMETERS Array containing the names of the parameters of which parameter samples will be generated
#' @param BASELINE Array containing the values assigned to each of these parameters in the calibrated baseline
#' @param PMIN Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space
#' @param PMAX Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space
#' @param PINC Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10
#' @param PARAMVALS Array containing a list of strings for each parameter, each string containing comma separated values that should be assigned to that parameter. Thus sampling can be performed for specific values for each parameter, rather than a uniform incremented value. This replaces the PMIN, PMAX, and PINC where this method is used.
#' @param write_csv Whether the sample should be output to CSV file. Only used when using spartan with spartan-db package
#' @param return_sample Used by spartan database link package, to return parameter value samples generated. These can then be added to the database
#'
#' @export
oat_parameter_sampling <- function(FILEPATH, PARAMETERS, BASELINE, PMIN = NULL,
                                   PMAX = NULL, PINC = NULL, PARAMVALS = NULL,
                                   write_csv=TRUE,
                                   return_sample = FALSE) {

  # Version 3.1 adds pre-execution check functions as part of refactoring:
  # Get the provided function arguments
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])

  # For link with spartan database package, all samples should be returned. We can do this
  # by adding each sample to a returned object
  all_samples <- vector("list", length(PARAMETERS))

  # Run if all checks pass:
  if(check_input_args(input_check$names, input_check$arguments)) {

    # CONSIDER EACH PARAMETER IN TURN
    for (PARAMOFINT in 1:length(PARAMETERS)) {

      # NOW GET THE LIST OF PARAMETER VALUES BEING EXPLORED FOR THIS PARAMETER
      # NOTE CONVERSION BACK TO NUMBERS: GETS RID OF TRAILING ZEROS MADE BY SEQ
      val_list <- as.numeric(prepare_parameter_value_list(PMIN, PMAX, PINC,
                                                          PARAMVALS,
                                                          PARAMOFINT))

      PARAMETERTABLE <- generate_parameter_table(PARAMETERS, BASELINE, PARAMOFINT, val_list)

      all_samples[[PARAMOFINT]] <- PARAMETERTABLE

      # WRITE THE A-TEST RESULTS TO FILE
      results_file <- make_path(c(FILEPATH,
                                  make_filename(c(PARAMETERS[PARAMOFINT],
                                                  "OAT_Values.csv"))))
      if(write_csv==TRUE)
      {
        write_data_to_csv(PARAMETERTABLE, results_file)

        message(paste("Sample File Generated for parameter ",
                  PARAMETERS[PARAMOFINT], " and output to ",
                  results_file, sep = ""))
      }
    }
  }

  # Utility here to return the parameter tables
  # Used by spartan-db to generate robustness parameter samples
  if(return_sample==TRUE)
  {
    message("Returning all samples")
    return(all_samples)
  }


}

#' Takes the value list and generates the sample that is output to csv file
#'
#' @param PARAMETERS Array containing the names of the parameters of which parameter samples will be generated
#' @param BASELINE Array containing the values assigned to each of these parameters in the calibrated baseline
#' @param PARAMOFINT Number of the parameter for which samples are currently being built
#' @param val_list List of parameter values to output for each parameter, to be combined with the baseline value of the complementary set
#' @return Parameter table ready for output to CSV file
generate_parameter_table <- function(PARAMETERS, BASELINE, PARAMOFINT, val_list) {

  PARAMETERTABLE <- NULL

  for (PARAM in 1:length(PARAMETERS)) {
    if (PARAMOFINT == PARAM)
      PARAMETERTABLE <- cbind(PARAMETERTABLE, val_list)
    else
      PARAMETERTABLE <- cbind(PARAMETERTABLE,
                              array(BASELINE[PARAM],
                                    dim = c(length(val_list))))
  }

  # FORMAT THEN OUTPUT THE PARAMETER TABLE FOR THIS PARAMETER OF INTEREST
  colnames(PARAMETERTABLE) <- PARAMETERS

  return(PARAMETERTABLE)
}

#' Creates a Netlogo compatible behaviour space experiment for robustness analysis
#'
#' This generates a Netlogo XML Experiment file, to be used with the
#' BehaviourSpace feature or Headless Netlogo, which perturbs each parameter
#' over a set value space, altering one parameter at a time in this case
#'
#' @param FILEPATH Directory where the parameter samples are to be stored
#' @param NETLOGO_SETUPFILE_NAME Name to give, or given to, the Netlogo XML experiment file(s) created in sampling. For more than one, a sample number is appended
#' @param PARAMETERS Array containing the names of the parameters of which parameter samples will be generated
#' @param PARAMVALS Array containing either the parameter value (if not of interest in the analysis), or range under which this is being explored (stated as as string e.g. "[5,50,5]" for a range of 5-50, increment of 5). See tutorial for more detail
#' @param NETLOGO_SETUP_FUNCTION The name of the function in Netlogo that sets up the simulation. Commonly is named setup.
#' @param NETLOGO_RUN_FUNCTION The name of the function in Netlogo that starts the simulation. Commonly named go.
#' @param MEASURES Array containing the names of the Netlogo output measures which are used to analyse the simulation.
#' @param EXPERIMENT_REPETITIONS The number of times Netlogo should repeat the experiment for each set of parameter values.
#' @param RUNMETRICS_EVERYSTEP Boolean stating whether Netlogo should produce output for each timestep.
#'
#' @export
oat_generate_netlogo_behaviour_space_XML <-
  function(FILEPATH, NETLOGO_SETUPFILE_NAME, PARAMETERS, PARAMVALS,
           NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION, MEASURES,
           EXPERIMENT_REPETITIONS, RUNMETRICS_EVERYSTEP) {

    if (requireNamespace("XML", quietly = TRUE)) {

      # START A NEW XML FILE, WITH EXPERIMENTS AS THE TOP TAG (AS REQUIRED BY
      # NETLOGO)
      xml <- XML::xmlOutputDOM(tag = "experiments")

      # NEXT TAG IN IS EXPERIMENT
      xml$addTag("experiment", attrs = c(
        name = "OAT_Sample", repetitions = EXPERIMENT_REPETITIONS,
        runMetricsEveryStep = RUNMETRICS_EVERYSTEP), close = FALSE)

      # NOW THE PROCEDURES TO CALL SETUP, GO, AND OUTPUT MEASURES TO ANALYSE
      xml$addTag("setup", NETLOGO_SETUP_FUNCTION)
      xml$addTag("go", NETLOGO_RUN_FUNCTION)

      ## NOW TO DO THE MEASURES
      for (MEASURE in 1:length(MEASURES)) {
        xml$addTag("metric", MEASURES[MEASURE])
      }

      for (PARAM in 1:length(PARAMETERS)) {
        # NOW SOME PARAMETERS ARE BEING VARIED, SOME NOT
        # THE ONES THAT ARE BEING VARIED HAVE A MIN, MAX, AND INCREMENT IN
        # SQUARE BRACKETS, SEPARATED BY A COMMA
        # THUS WE CAN DISTINGUISH THESE WITH A STRING TOKENIZER
        PARAMVALSPLIT <- (strsplit(PARAMVALS[PARAM], ","))[[1]]

        if (length(PARAMVALSPLIT) == 1) {

          # THIS PARAMETER IS NOT BEING VARIED, AND THUS WE CAN JUST SPECIFY
          # THE PARAMETER VALUE FOR EACH PARAMETER, ADD THE ENUMERATEDVALUESET
          # TAG, SIMULATION VARIABLE NAME
          xml$addTag("enumeratedValueSet",
                     attrs = c(variable = (PARAMETERS[PARAM])), close = FALSE)

          # NOW ADD THE VALUE
          xml$addTag("value", attrs = c(value = (PARAMVALS[PARAM])))

          # CLOSE THE ENUMERATED VALUE SET TAG
          xml$closeTag()
        } else {
          # THIS IS A PARAMETER BEING ANALYSED, AND THUS WE NEED TO TELL NETLOGO
          # TO ALTER THE VALUES
          # GET THE MIN, MAX, AND INCREMENT
          # NOTE FOR MIN AND INCREMENT, WE NEED TO REMOVE THE OPENING AND CLOSING
          # SQUARE BRACKET
          MIN <- substring(PARAMVALSPLIT[[1]], 2)
          MAX <- PARAMVALSPLIT[[2]]
          INC <- substring(PARAMVALSPLIT[[3]], 1, nchar(PARAMVALSPLIT[[3]]) - 1)

          # NOW TO BUILD THE TAGS
          xml$addTag("steppedValueSet", attrs = c(variable = (PARAMETERS[PARAM]),
                                                  first = MIN, step = INC,
                                                  last = MAX))
        }
      }


      # CLOSE THE EXPERIMENT TAG
      xml$closeTag()

      # CLOSE THE EXPERIMENTS TAG
      xml$closeTag()

      xml <- XML::saveXML(xml, file = file.path(FILEPATH, paste(NETLOGO_SETUPFILE_NAME,
                                     ".xml", sep = "")), indent = TRUE,
                   prefix = '<?xml version="1.0" encoding="us-ascii"?>\n',
                   doctype = '<!DOCTYPE experiments SYSTEM "behaviorspace.dtd">')

      message(paste("Netlogo Robustness Setup file generated in ",FILEPATH, "/",NETLOGO_SETUPFILE_NAME,
                                                                                  ".xml", sep = ""))
    }
  }
