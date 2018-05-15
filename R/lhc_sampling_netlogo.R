#' Prepares Netlogo experiment files for a sampling-based sensitivity analysis, using latin-hypercube sampling
#'
#' This generates a specified number of simulation parameters sets using
#' latin-hypercube sampling. These are then processed into Netlogo XML
#' experiment files, one for each set of parameters.
#'
#' @param FILEPATH Directory where the parameter samples are to be stored
#' @param PARAMETERS Array containing the names of the parameters of which parameter samples will be generated
#' @param PARAMVALS Array containing either the parameter value (if not of
#' interest in the analysis), or range under which this is being explored
#' (stated as as string e.g. "[5,50]" for a range of 5-50.
#' @param NUMSAMPLES The number of parameter subsets to be generated in the LHC design
#' @param ALGORITHM Choice of algorithm to use to generate the hypercube. Can be set to either 'normal' or 'optimum'. Beware optimum can take a long time to generate an optimised parameter set (more than 24 hours in some circumstances).
#' @param EXPERIMENT_REPETITIONS The number of times Netlogo should repeat the experiment for each set of parameter values.
#' @param RUN_METRICS_EVERYSTEP Boolean stating whether Netlogo should produce output for each timestep.
#' @param NETLOGO_SETUP_FUNCTION The name of the function in Netlogo that sets up the simulation. Commonly is named setup.
#' @param NETLOGO_RUN_FUNCTION The name of the function in Netlogo that starts the simulation. Commonly named go.
#' @param MEASURES Array containing the names of the Netlogo output measures which are used to analyse the simulation.
#'
#' @export
lhc_generate_lhc_sample_netlogo <- function(FILEPATH, PARAMETERS, PARAMVALS,
                                            NUMSAMPLES, ALGORITHM,
                                            EXPERIMENT_REPETITIONS,
                                            RUN_METRICS_EVERYSTEP,
                                            NETLOGO_SETUP_FUNCTION,
                                            NETLOGO_RUN_FUNCTION, MEASURES) {

  # Version 3.1 of spartan introduces input error checking. This has also been
  # refactored to make better use of the original lhc sampling function
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  #print(paste("FILEPATH IN: ",eval(input_arguments$FILEPATH),sep=""))
  #print(paste("Does this Exist: ",file.exists(eval(input_arguments$FILEPATH)),sep=""))
  # Run if all checks pass:

  if(check_input_args(input_check$names, input_check$arguments)) {

      # Get the information and ranges of the parameters being perturbed
      ParameterInfo <- process_netlogo_parameter_range_info(PARAMETERS, PARAMVALS)

      # Perform the sampling
      design <- sample_parameter_space(ALGORITHM, NUMSAMPLES,
                                       ParameterInfo$STUDIED_PARAMETERS)

      # Now scale this design, as currently all values are between 0 and 1
      design <- scale_lhc_sample(ParameterInfo$STUDIED_PARAMETERS,
                                 ParameterInfo$PMIN,
                                 ParameterInfo$PMAX,
                                 ParameterInfo$PINC, NUMSAMPLES, design)

      write_data_to_csv(design, make_path(c(FILEPATH,
                                            "LHC_Parameters_for_Runs.csv")))

      #design <- lhc_generate_lhc_sample(
      #  FILEPATH, PARAMETERS=ParameterInfo$STUDIED_PARAMETERS, NUMSAMPLES,
      #  PMIN=ParameterInfo$PMIN, PMAX=PMAX, ALGORITHM)

     # Now construct the netlogo experiment file for each of these samples
      for (SAMPLE in 1:NUMSAMPLES) {
          # Create a folder for this sample
          dir.create(file.path(FILEPATH, SAMPLE), showWarnings = FALSE)

          # Initialise the XML file
          xml <- initialise_netlogo_xml_file(
            "LHC_Sample", SAMPLE, EXPERIMENT_REPETITIONS, RUN_METRICS_EVERYSTEP,
            NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION, MEASURES)

          # Add the parameter information
          xml<-add_parameter_value_to_file(xml, PARAMETERS, ParameterInfo,
                                      design, SAMPLE, PARAMVALS)

          # Close and write the file
          Output_File <- make_extension(
            make_path(c(FILEPATH,SAMPLE,
                        join_strings_nospace(c("lhc_analysis_set",SAMPLE)))),
            "xml")

          close_and_write_netlogo_file(xml, Output_File)
      }

      message(paste(NUMSAMPLES, " Netlogo Experiment Files Output to ",
                  FILEPATH, sep = ""))
  }
}




