#' Prepares Netlogo experiment files for a variance-based sensitivity analysis,
#' using eFAST
#'
#' Creates a set of parameter values, over the specified value space, using the
#' sampling method described in the eFAST technique. Then processes each of
#' these into a Netlogo experiment XML file, from which a simulation can be run.
#'
#' @param FILEPATH Directory where the parameter samples and XML models are to
#' be stored
#' @param NUMCURVES The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3.
#' @param NUMSAMPLES The number of parameter subsets to be generated for each
#' curve in the eFAST design.
#' @param MEASURES Array containing the names of the Netlogo output measures
#' which are used to analyse the simulation.
#' @param PARAMETERS Array containing the names of the parameters of which
#' parameter samples will be generated
#' @param PARAMVALS Array containing either the parameter value (if not of
#' interest in the analysis), or range under which this is being explored
#'  (stated as as string e.g. "[5,50,5]" for a range of 5-50, increment of 5).
#'  The reader should read the relevant tutorial in detail.
#' @param EXPERIMENT_REPETITIONS The number of times Netlogo should repeat the
#' experiment for each set of parameter values.
#' @param RUNMETRICS_EVERYSTEP Boolean stating whether Netlogo should produce
#' output for each timestep.
#' @param NETLOGO_SETUP_FUNCTION The name of the function in Netlogo that sets
#' up the simulation. Commonly is named setup.
#' @param NETLOGO_RUN_FUNCTION The name of the function in Netlogo that starts
#' the simulation. Commonly named go.
#'
#' @export
efast_generate_sample_netlogo <-
  function(FILEPATH, NUMCURVES, NUMSAMPLES, MEASURES, PARAMETERS, PARAMVALS,
           EXPERIMENT_REPETITIONS, RUNMETRICS_EVERYSTEP,
           NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION) {

  # Version 3.1 of spartan introduces input error checking. This has also been
  # refactored to make better use of the original lhc sampling function
  input_check <- list("arguments"=as.list(match.call()),"names"=names(match.call())[-1])
  # Run if all checks pass:

  if(check_input_args(input_check$names, input_check$arguments)) {

    # Get the information and ranges of the parameters being perturbed
    ParameterInfo <- process_netlogo_parameter_range_info(PARAMETERS, PARAMVALS)

    # Call to the eFAST method in spartan
    parameter_vals <- generate_efast_parameter_sets(
      FILEPATH, NUMCURVES, NUMSAMPLES, ParameterInfo$STUDIED_PARAMETERS,
      ParameterInfo$PMIN, ParameterInfo$PMAX)

    # Output the samples for use in analysis later
    output_param_sets_per_curve(FILEPATH, NUMCURVES,
                                ParameterInfo$STUDIED_PARAMETERS, parameter_vals)

    # Now we can write the netlogo files
    for (CURVENUM in 1:NUMCURVES) {
      # CREATE A FOLDER TO STORE THE SETUP FILE FOR THIS CURVE
      dir.create(file.path(FILEPATH, CURVENUM), showWarnings = FALSE)

      for (PARAMNUM in 1:length(ParameterInfo$STUDIED_PARAMETERS)) {
        # Create a folder to store the setup files for this parameter
        param_output <-  paste(FILEPATH, "/", CURVENUM, "/", PARAMNUM,
                               sep = "")
        dir.create(file.path(param_output), showWarnings = FALSE)

        # Need to open the generated CSV file for this parameter:
        efast_sample <- read.csv(paste(
          FILEPATH,"/Curve",CURVENUM,"_",
          ParameterInfo$STUDIED_PARAMETERS[PARAMNUM],".csv",sep=""),
          check.names=FALSE)

        for (SAMPLE in 1:NUMSAMPLES) {
          # Create a folder for this experiment
          dir.create(file.path(param_output, SAMPLE),
                     showWarnings = FALSE)

          # Initialise the XML file
          xml <- initialise_netlogo_xml_file(
            "EFAST_Sample", SAMPLE, EXPERIMENT_REPETITIONS, RUNMETRICS_EVERYSTEP,
            NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION, MEASURES)

          # Add the parameter information
          xml<-add_parameter_value_to_file(xml, PARAMETERS, ParameterInfo,
                                      efast_sample, SAMPLE, PARAMVALS)

          # Close and write the file
          Output_File <- paste(param_output,"/",SAMPLE,"/efast_analysis_set",
                               SAMPLE, ".xml", sep="")
          close_and_write_netlogo_file(xml, Output_File)
        }
      }

      message(paste("Curve ", CURVENUM, " Complete", sep = ""))
    }
  }
}
