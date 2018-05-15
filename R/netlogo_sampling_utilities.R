#' Processes netlogo parameter information to generate names of those of interest to this analysis
#'
#' Not all parameters may be being perturbed, although specified in PARAMETERS.
#' Some have a specific value, rather than a range. This detects the names of
#' the parameters that are being perturbed, and their mins and maxes
#'
#' @param PARAMETERS Parameter names specified in the R script
#' @param PARAMVALS Values of each parameter, either a specific value or range
#' @return List of: names of parameters of interest, minimum value of each
#' parameter, and maximum value of each parameter
process_netlogo_parameter_range_info <- function(PARAMETERS, PARAMVALS) {

  STUDIED_PARAMETERS <- NULL
  PMIN <- NULL
  PMAX <- NULL
  PINC <- NULL

  for (PARAM in 1:length(PARAMETERS)) {

    PARAMVALSPLIT <- strsplit(PARAMVALS[PARAM], ",")[[1]]

    if (length(PARAMVALSPLIT) == 2) {
      # ADD THE PARAMETER TO THE CSV FILE HEADER
      STUDIED_PARAMETERS <- c(STUDIED_PARAMETERS, PARAMETERS[PARAM])
      # GET THE MIN AND MAX
      PMIN <- c(PMIN,as.numeric(substring(PARAMVALSPLIT[[1]], 2)))
      PMAX <- c(PMAX, as.numeric(substring(PARAMVALSPLIT[[2]], 1,
                                           nchar(PARAMVALSPLIT[[2]]) - 1)))

    }
    else if (length(PARAMVALSPLIT) == 3) {
      # ADD THE PARAMETER TO THE CSV FILE HEADER
      STUDIED_PARAMETERS <- c(STUDIED_PARAMETERS, PARAMETERS[PARAM])
      # GET THE MIN, MAX and INC
      PMIN <- c(PMIN, as.numeric(substring(PARAMVALSPLIT[[1]], 2)))
      PMAX <- c(PMAX, as.numeric(substring(PARAMVALSPLIT[[2]], 1)))
      PINC <- c(PINC, as.numeric(substring(PARAMVALSPLIT[[3]], 1,
                      nchar(PARAMVALSPLIT[[3]]) - 1)))

    }
  }
  #return(list("STUDIED_PARAMETERS"=STUDIED_PARAMETERS,"PMIN"=PMIN,"PMAX"=PMAX))
  return(list("STUDIED_PARAMETERS"=STUDIED_PARAMETERS,"PMIN"=PMIN,"PMAX"=PMAX, "PINC"=PINC))
}

#' Initialises the Netlogo setup file for this experiment
#' @param EXPERIMENT Test for the experiment tag of the file (e.g. LHC, eFAST, etc)
#' @param SAMPLE Number of the sample being processed
#' @param EXPERIMENT_REPETITIONS The number of times Netlogo should repeat the experiment for each set of parameter values.
#' @param RUN_METRICS_EVERYSTEP Boolean stating whether Netlogo should produce output for each timestep.
#' @param NETLOGO_SETUP_FUNCTION The name of the function in Netlogo that sets up the simulation. Commonly is named setup.
#' @param NETLOGO_RUN_FUNCTION The name of the function in Netlogo that starts the simulation. Commonly named go.
#' @param MEASURES Array containing the names of the Netlogo output measures which are used to analyse the simulation.
#' @return Initialised XML file object
initialise_netlogo_xml_file <- function(
  EXPERIMENT, SAMPLE, EXPERIMENT_REPETITIONS, RUN_METRICS_EVERYSTEP,
  NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION, MEASURES)
{
  xml <- XML::xmlOutputDOM(tag = "experiments")

  # NEXT TAG IN IS EXPERIMENT
  xml$addTag("experiment", attrs = c(
    name = paste(EXPERIMENT, SAMPLE, sep = ""),
    repetitions = EXPERIMENT_REPETITIONS,
    runMetricsEveryStep = RUN_METRICS_EVERYSTEP),
    close = FALSE)

  # PROCEDURES TO CALL SETUP, GO, AND OUTPUT MEASURES TO ANALYSE
  xml$addTag("setup", NETLOGO_SETUP_FUNCTION)
  xml$addTag("go", NETLOGO_RUN_FUNCTION)

  for (MEASURE in 1:length(MEASURES)) {
    xml$addTag("metric", MEASURES[MEASURE])
  }

  return(xml)
}

#' Iterates through the parameters, adding their sampled value to the netlogo experiment file
#'
#' @param xml Object of the XML file being constructed
#' @param PARAMETERS Parameters specified in the R set-up file, of interest in this experiment
#' @param ParameterInfo Parameters that are being perturbed, and their ranges
#' @param LHC_DESIGN The LHC sample generated for these parameters
#' @param SAMPLE Number of the sample being processed
#' @param PARAMVALS Input of parameter ranges or set values specified by the user
#' @return Updated XML object ready for output to file
add_parameter_value_to_file <- function(xml, PARAMETERS, ParameterInfo, LHC_DESIGN, SAMPLE, PARAMVALS) {

  # NOW TO SET THE VALUE OF EACH PARAMETER IN THIS RUN.
  # SOME ARE STATIC, SOME WILL BE TAKEN FROM THE LHC
  for (PARAM in 1:length(PARAMETERS))
  {
    if(PARAMETERS[PARAM] %in% ParameterInfo$STUDIED_PARAMETERS)
    {
      # Get the value from spartan
      VALUE <- LHC_DESIGN[SAMPLE, PARAMETERS[PARAM]]
    } else {
      # Is a constant value
      VALUE <- PARAMVALS[PARAM]
    }

    # NOW CREATE THE XML FOR THIS PARAMETER
    xml$addTag("enumeratedValueSet", attrs = c(
      variable = PARAMETERS[PARAM]), close = FALSE)
    # NOW ADD THE VALUE
    xml$addTag("value", attrs = c(value = VALUE))

    # CLOSE THE ENUMERATED VALUE SET TAG
    xml$closeTag()
  }
  return(xml)
}

#' Close the current netlogo sample file and write out
#'
#' @param xml Object of the XML file being constructed
#' @param FILENAME Name of the file to write out
close_and_write_netlogo_file <- function(xml, FILENAME) {
  # CLOSE THE EXPERIMENT TAG
  xml$closeTag()

  # CLOSE THE EXPERIMENTS TAG
  xml$closeTag()

  # SAVE THE XML FILE IN THE FOLDER FOR THIS EXPERIMENT
  XML::saveXML(xml, file = FILENAME, indent = TRUE, prefix =
      '<?xml version="1.0" encoding="us-ascii"?>\n', doctype =
      '<!DOCTYPE experiments SYSTEM "behaviorspace.dtd">')
}
