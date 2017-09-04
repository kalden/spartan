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

    print("Netlogo Robustness Setup file generated:")

    XML::saveXML(xml, file = paste(FILEPATH, "/", NETLOGO_SETUPFILE_NAME,
                                   ".xml", sep = ""), indent = TRUE,
                 prefix = '<?xml version="1.0" encoding="us-ascii"?>\n',
                 doctype = '<!DOCTYPE experiments SYSTEM "behaviorspace.dtd">')
  } else {
    print("oat_generate_netlogo_behaviour_space_XML method needs XML package")
  }
}

#' Takes a Netlogo behaviour space file and performs a robustness analysis from that simulation data
#'
#' From a Netlogo behaviour space file, extracts the required timepoint
#' information from it, storing this in a Spartan compatible CSV file.
#' This CSV file is then processed using the methods described in
#' \code{oat_csv_result_file_analysis}, with A-Test scores determined
#' for each value assigned to each parameter. Once this method has been
#' called, the researcher should use the \code{oat_graphATestsForSampleSize}
#' and \code{oat_plotResultDistribution} methods to graph the results.
#'
#' @param FILEPATH Location where the behaviour space results can be found
#' @param NETLOGO_BEHAVIOURSPACEFILE The name of the file produced by Netlogo for Parameter Robustness (Technique 2). This is the result file that is analysed.
#' @param PARAMETERS Array containing the names of the parameters for which parameter samples were be generated
#' @param BASELINE Array containing the baseline, or calibrated value, of each parameter.
#' @param PMIN Array containing the minimum value that should be used for each parameter.  Sets a lower bound on sampling space.
#' @param PMAX Array containing the maximum value that should be used for each parameter.  Sets an upper bound on sampling space.
#' @param PINC Array containing the increment value that should be applied for each parameter. For example, a parameter could have a minimum value of 10, and maximum value of 100, and be incremented by 10.
#' @param MEASURES Array containing the names of the Netlogo output measures which are used to analyse the simulation.
#' @param RESULTFILENAME Name of the results summary file that should be produced when analysing the Netlogo results
#' @param ATESTRESULTSFILENAME File name of the ATests result summary file created by \code{oat_analyseAllParams}
#' @param TIMESTEP The timestep of the Netlogo simulation being analysed.
#'
#' @export
oat_process_netlogo_result <-
  function(FILEPATH, NETLOGO_BEHAVIOURSPACEFILE, PARAMETERS, BASELINE, PMIN,
           PMAX, PINC, MEASURES, RESULTFILENAME, ATESTRESULTSFILENAME,
           TIMESTEP) {
  # NOTE THAT ALTHOUGH NETLOGO FILE CONTAINS EACH PARAMETER FOR THE TURTLE,
    # THE INPUT HERE ONLY NEEDS TO BE THE PARAMETERS THAT HAVE BEEN PERTURBED

  ## FIRSTLY READ IN THE NETLOGO RESULT FILE
  NL_RESULT <- read.csv(paste(FILEPATH, "/", NETLOGO_BEHAVIOURSPACEFILE,
                              sep = ""), sep = ",", skip = 6,
                        check.names = FALSE)

  # ORDER IT BY RUN FOR EFFICIENCY LATER
  NL_RESULT_ORDERED <- NL_RESULT[order(NL_RESULT[, 1]), ]

  TIMESTEP_RESULTS <- subset(NL_RESULT_ORDERED,
                             NL_RESULT_ORDERED["[step]"] == TIMESTEP)

  write.csv(TIMESTEP_RESULTS, paste(FILEPATH, "/", RESULTFILENAME, sep = ""),
            quote = FALSE, row.names = FALSE)

  print(join_strings_space(c("Analysing Netlogo Robustness Analysis File",
        "and Generating A-Test Scores")))
  oat_csv_result_file_analysis(FILEPATH, RESULTFILENAME, PARAMETERS,
                               BASELINE, MEASURES, ATESTRESULTSFILENAME,
                               PMIN = PMIN, PMAX = PMAX,
                               PINC = PINC, PARAMVALS = NULL)
}
