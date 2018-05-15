# Method checking works as follows:
# 1. Method being checked passes a list of the EXPECTED arguments and the
# arguments the user has passed
# 2. generate+list_of_checks generates the list of functions that should
# be called based on the expected arguments
# 3. These functions are called sequentially by execute_checks, until
# either one fails and false is returned, or all pass & true is returned
# 4. As the functions are called from a list, it is vital these all have
# the same input argument structure. If need be, we'll need to tailor the
# function internally

#' Wrapper function called by all spartan methods to check input pre-execution
#'
#' @param argNames Expected argument names for the calling function
#' @param arguments User input to the called function
#' @return Boolean stating whether the input checks pass or fail
check_input_args <- function(argNames, arguments)
{
  # If processing multiple timepoints, the check will already have been done
  # and no point doing it again, so return TRUE
  #print(arguments)
  if(!is.null(eval(arguments$check_done))) {
    if(eval(arguments$check_done)) {
      return(TRUE)
    }
  }
  else {
    # Generate list of checks
    check_methods_to_call <- generate_list_of_checks(argNames)
    # Execute
    check_result <- execute_checks(check_methods_to_call,
                                 arguments, argNames)

    return(check_result)
  }
}

#' Defines which functions to call to check an input argument.
#'
#' This creates a list of check functions to call for the input arguments
#' for a spartan function. This is using the expected argument names,
#' provided by the calling function
#' @param argNames Expected argument names for the calling function
#' @return List of check functions that should be called to check input
generate_list_of_checks <-function(argNames)
{
  argList <- NULL
  # We initialise the length of the list, but in reality this could be
  # longer, if some methods require more than one check. As such we keep
  # a list index count
  check_methods_to_call <- vector("list", length(argNames))
  list_index <- 1

  for(arg in 1:length(argNames))
  {
    if(argNames[arg] == "FILEPATH")
      check_methods_to_call[[list_index]] = check_filepath_exists  # TAKES ALL ARGS FIXED
    else if(argNames[arg] == "SAMPLESIZES")
      check_methods_to_call[[list_index]] = check_list_all_integers   # TAKES ALL ARGS FIXED
    else if(argNames[arg] %in% c("PARAMETERS","MEASURES","MEASURE_SCALE"))
      check_methods_to_call[[list_index]] = check_text_list            # TAKES ALL ARGS FIXED
    else if(argNames[arg] %in% c("NUMSUBSETSPERSAMPLESIZE","NUMRUNSPERSAMPLE","NUMSAMPLES","NUMCURVES","EXPERIMENT_REPETITIONS"))
      check_methods_to_call[[list_index]] = check_argument_positive_int      # TAKES ALL ARGS FIXED
    else if(argNames[arg] %in% c("ATESTRESULTSFILENAME", "RESULTFILENAME", "SUMMARYFILENAME", "CSV_FILE_NAME", "NETLOGO_SETUP_FUNCTION","NETLOGO_RUN_FUNCTION", "ATESTRESULTFILENAME",
                                 "SPARTAN_PARAMETER_FILE","LHC_ALL_SIM_RESULTS_FILE", "LHCSUMMARYFILENAME", "CORCOEFFSOUTPUTFILE","EFASTRESULTFILENAME"))
      check_methods_to_call[[list_index]] = check_text     # TAKES ALL ARGS FIXED
    else if(argNames[arg] %in% c("RUN_METRICS_EVERYSTEP","GRAPH_FLAG"))
      check_methods_to_call[[list_index]] = check_boolean
    else if(argNames[arg] == "LARGEDIFFINDICATOR")
      check_methods_to_call[[list_index]] = check_double_value_in_range
    else if(argNames[arg] == "TTEST_CONF_INT")
      check_methods_to_call[[list_index]] = check_confidence_interval
    else if(argNames[arg] == "AA_SIM_RESULTS_FILE")
      check_methods_to_call[[list_index]] = check_consistency_result_type
    else if(argNames[arg] == "OUTPUTFILECOLSTART")
      check_methods_to_call[[list_index]] = check_column_ranges
    else if(argNames[arg] == "PMIN")
      check_methods_to_call[[list_index]] = check_global_param_sampling_args
    else if(argNames[arg] == "PARAMVALS")   # This will also need some more thought once other approaches added
      check_methods_to_call[[list_index]] = check_function_dependent_paramvals
    else if(argNames[arg] == "ALGORITHM")
      check_methods_to_call[[list_index]] = check_lhs_algorithm
    else if(argNames[arg] == "OUTPUT_TYPE")
      check_methods_to_call[[list_index]] = check_graph_output_type
    # To deal with AA_SIM_RESULTS_OBJECT, and OUTPUTFILECOLEND, which are checked by FILE and START respectively,
    # and PMAX, BASELINE, PINC, that are checked in PMIN/PARAMVALS checks,we put in an ignore, and detect this later
    # This needs to be done to keep the function list referenced to the argument names
    else if(argNames[arg] %in% c("AA_SIM_RESULTS_OBJECT", "OUTPUTFILECOLEND", "PMAX","PINC","BASELINE","ALTFILENAME","OUTPUTMEASURES_TO_TTEST"))
      check_methods_to_call[[list_index]] = NULL

    list_index = list_index + 1

    # KA: No testing of nested filepaths in here - this may be worth re-adding: check_nested_filepaths(arguments$FILEPATH, arguments$PARAMETERS, preCheckSuccess)
    # Also no testing of timepoints
    # Need to test PARAMVALS for Netlogo LHC here, as well as test for installation of XML package
  }

  return(check_methods_to_call)
}

#' Executes the list of check functions compiled for the calling function
#'
#' This returns TRUE if all arguments pass, or FALSE when the first test fails
#'
#' @param check_methods_to_call List of check functions that should be called
#' @param input_arguments The user's input to a function
#' @param function_args The expected names of the arguments, provided by
#' calling function
#' @return Boolean stating whether the input checks pass or fail
execute_checks <- function(check_methods_to_call, input_arguments, function_args)
{
  out<-NULL

  for(check_method in 1:length(check_methods_to_call))
  {
    #out<-rbind(out,function_args[check_method])
    #write_data_to_csv(out, "outlog.csv")
    #print(function_args[check_method])
    # Need to tailor some methods that cannot be called with the same two arguments, so check these first
    if(identical(check_methods_to_call[[check_method]],check_double_value_in_range))
    {
      if(!check_double_value_in_range(input_arguments$LARGEDIFFINDICATOR, "LARGEDIFFINDICATOR",0,0.5))   # This will need changing if we use this method for any other arguments
        return(FALSE)
    }
    else if(identical(check_methods_to_call[[check_method]],check_confidence_interval))
    {
      if(!check_confidence_interval(input_arguments$TTEST_CONF_INT, "TTEST_CONF_INT",0,1))
        return(FALSE)
    }
    else if(identical(check_methods_to_call[[check_method]],check_consistency_result_type))
    {
      # As check_consistency_result type differs for each function, need to get the names of the R and file name arguments before calling
      check_args <- get_file_and_object_argument_names(input_arguments)
      # Now can call the function
      if(!check_consistency_result_type(input_arguments, check_args$file, check_args$object))
        return(FALSE)
    }
    else if(identical(check_methods_to_call[[check_method]],check_column_ranges))
    {
      # As this varies across functions, we need to get the correct file name to check
      filepath <- get_correct_file_path_for_function(input_arguments)
      # Now we can run the function
      if(!check_column_ranges(input_arguments, filepath, input_arguments$RESULTFILENAME))
        return(FALSE)
      if(!is.null(eval(input_arguments$ALTFILENAME)))
        if(!check_column_ranges(input_arguments, filepath, input_arguments$ALTFILENAME))
          return(FALSE)
    }
    # Every other method needs no specific tailoring, just make sure we ignore any that were checked elsewhere
    else if(!is.null(check_methods_to_call[[check_method]]))
    {
      if(!check_methods_to_call[[check_method]](input_arguments, function_args[check_method]))
      {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#' Gets the correct file and R object argument names for the input checker
#'
#' check_consistency_result_type takes the argument name of a file or R
#' object, and this changes across functions. This takes the function name
#' and returns the correct argument names for passing to the function
#' @param input_arguments Input arguments object passed from the checked
#' function
#' @return List containg argument names for file and R object
get_file_and_object_argument_names <- function(input_arguments)
{
  # First entry of input_arguments is the calling function, so we can adjust here as need be:
  if(input_arguments[[1]]=="aa_summariseReplicateRuns")
    return(list("file"="AA_SIM_RESULTS_FILE","object"="AA_SIM_RESULTS_OBJECT"))
  else if(input_arguments[[1]]=="aa_SampleSizeSummary")
    return(list("file"="ATESTRESULTS_FILE","object"="ATESTRESULTS_OBJECT"))
  else if(input_arguments[[1]]=="aa_getATestResults")
    return(list("file"="AA_SIM_RESULTS_FILE","object"="AA_SIM_RESULTS_OBJECT"))
}

#' Gets the correct filepath for the column range input checker
#'
#' check_column_ranges takes a filepath containing a sample results file,
#' and this changes across functions. This takes the function name
#' and returns the correct argument names for passing to the function
#' @param arguments Input arguments object passed from the checked
#' function
#' @return Filepath to the results file to check
get_correct_file_path_for_function <- function(arguments)
{
  if(arguments[[1]] == "aa_summariseReplicateRuns")
    return(file.path(eval(arguments$FILEPATH),eval(arguments$SAMPLESIZES)[1],"1","1"))
  else if(arguments[[1]] == "oat_processParamSubsets")
    return(paste(eval(arguments$FILEPATH),"/",eval(arguments$PARAMETERS)[1],"/",eval(arguments$PMIN)[1],"/1/",sep=""))
}

#' Call the correct paramvals check for the calling function, as netlogo & robustness differ
#' @param input_arguments List of the arguments provided to the called function
#' @param argument_name Null in this case, but keeps auto-called functions consistent
#' @return Boolean stating the status of the pre-execution checks
check_function_dependent_paramvals <- function(input_arguments, argument_name)
{
  ## Check for param vals also is dependent on the function called
  if(toString(input_arguments[[1]]) %in% c("lhc_generate_lhc_sample_netlogo","efast_generate_sample_netlogo"))
    return(check_netlogo_parameters_and_values(input_arguments, argument_name))
  else if(toString(input_arguments[[1]]) %in% c("oat_parameter_sampling","oat_processParamSubsets","oat_csv_result_file_analysis"))
    return(check_robustness_sampling_args(input_arguments, argument_name))
}

#' Checks the input values for global parameter sampling techniques
#' @param input_arguments List of the arguments provided to the called function
#' @param argument_name Null in this case, but keeps auto-called functions consistent
#' @return Boolean stating the status of the pre-execution checks
check_global_param_sampling_args <- function(input_arguments, argument_name)
{
  # As PMIN also in robustness (yet this would fail if PMIN is null, we check the calling function)
  # If this is not lhc or efast sampling, we simply return true. Robustness is taken care of in PARAMVALS check
  if(toString(input_arguments[[1]]) %in% c("lhc_generate_lhc_sample","efast_generate_sample"))
    return(check_parameters_and_ranges(input_arguments, argument_name))
  else
    return(TRUE)

}

#' Check the requested graph types are correct (PDF, PNG, TIFF, BMP)
#' @param input_arguments List of the arguments provided to the called function
#' @param argument_name Null in this case, but keeps auto-called functions consistent
#' @return Boolean stating the status of the pre-execution checks
check_graph_output_type <- function(input_arguments, argument_name)
{
  outputs_requested <- eval(input_arguments$OUTPUT_TYPE)
  for(out in 1:length(outputs_requested))
  {
    if(!outputs_requested[out] %in% c("PDF","PNG","TIFF","BMP"))
    {
      message("Graph types can only be PDF, PNG, TIFF, BMP")
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Pre-execution checks to perform before the spartan robustness samplng
#' technique is executed. Checks all parameter input
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Null in this case, but keeps auto-called functions consistent
#' @return Boolean stating the status of the pre-execution checks
check_robustness_sampling_args <- function(arguments, argument_name)
{
  check = FALSE
  while(!check)
  {
    if(!check_robustness_range_or_values(arguments))
      break;

    # From the above test we know that the user has either specified PARAMVALS
    # or PMIN,PMAX,INC choice - now we test dependent on that
    if(is.null(eval(arguments$PARAMVALS)))
    {
      if(!check_robustness_parameter_and_ranges_lengths(arguments))
        break
      if(!check_numeric_list_values(arguments, "PMIN", "PMAX"))
        break
      if(!check_numeric_list_values(arguments, "PINC", "PMAX"))
        break
      if(!check_robustness_range_contains_baseline(arguments))
        break
    }
    else
    {
      if(!check_paramvals_length_equals_parameter_length(arguments))
        break
      if(!check_robustness_paramvals_contains_baseline(arguments))
        break
    }

    check = TRUE
  }
  return(check)
}

#' For robustness, check whether using PMIN/PMAX/PINC entry or PARAMVALS
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Name of the argument being checked.
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_range_or_values <- function(arguments, argument_name)
{
  if(is.null(eval(arguments$PARAMVALS)))
  {
    if(is.null(eval(arguments$PMIN)) | is.null(eval(arguments$PMAX)) | is.null(eval(arguments$PINC)))
    {
      message("You need to specify either PMIN,PMAX,and PINC, or the values to sample in PARAMVALS")
      return(FALSE)
    }
    else
      return(TRUE)
  }
  else
  {
    if(!is.null(eval(arguments$PMIN)) | !is.null(eval(arguments$PMAX)) | !is.null(eval(arguments$PINC)))
    {
      message("You need to specify either PMIN,PMAX,and PINC, or the values to sample in PARAMVALS")
      return(FALSE)
    }
    else
      return(TRUE)
  }
}

#' Checks the netlogo parameters and values are formatted correctly
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Here for consistency in auto-function calling, but not used
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_netlogo_parameters_and_values <- function(arguments, argument_name)
{
  ## KA: FOR THE MOMENT, THIS JUST CHECKS XML LIBRARY INSTALLED AND CALLS
  ## check_paramvals_length_equals_parameter_length: IDEALLY THIS THEN NEEDS
  ## TO CHECK THE VALUES
  if(!check_package_installed("XML"))
    return(FALSE)
  else
    return(check_paramvals_length_equals_parameter_length(arguments))
}

#' Where used in robustness analysis, check that the length of PARAMVALS equals
# the number of PARAMETERS
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_paramvals_length_equals_parameter_length <- function(arguments)
{
  tryCatch(
  {
    if(length(eval(arguments$PARAMETERS)) == length(eval(arguments$PARAMVALS)))
      return(TRUE)
    else
    {
      message("Number of entries in PARAMVALS should match the number of parameters")
      message("Spartan Terminated")
      return(FALSE)
    }
  },
  error=function(cond) {
    message("PARAMVALS or PARAMETERS has been declared incorrectly")
    message("Spartan Function Terminated")
    return(FALSE)
  })

}

#' Checks that the parameter values specified in PARAMVALS contain the BASELINE
#'
#' The paramvals need to contain the BASELINE value else no behaviours can be
#' compared using the robustness analysis approach in Technique 2
#'
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_paramvals_contains_baseline <- function(arguments)
{
  tryCatch(
  {
    paramvalsCheck <- eval(arguments$PARAMVALS)
    baselineCheck <- eval(arguments$BASELINE)

    for(PARAM in 1:length(paramvalsCheck))
    {
      # PARAMVALS is a string separated list, so need to convert to numeric
      numeric_paramVals <- lapply(strsplit(paramvalsCheck[PARAM], ','), as.numeric)[[1]]

      if(!baselineCheck[PARAM] %in% numeric_paramVals)
      {
        message("PARAMVALS should contain the BASELINE value, else behaviours cannot be compared")
        message("Spartan Terminated")
        return(FALSE)
      }
    }
    return(TRUE)
  },
  error=function(cond) {
    message("Error in declaring BASELINE or PARAMVALS")
    message("Spartan Function Terminated")
    return(FALSE)
  })
}

#' Checks that the range specified by PMIN and PMAX contains the BASELINE
#'
#' The paramvals need to contain the BASELINE value else no behaviours can be
#' compared using the robustness analysis approach in Technique 2
#'
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_range_contains_baseline <- function(arguments)
{
  tryCatch(
  {
    minCheck <- eval(arguments$PMIN)
    baselineCheck <- eval(arguments$BASELINE)
    incCheck <- eval(arguments$PINC)
    maxCheck <- eval(arguments$PMAX)

    for(PARAM in 1:length(baselineCheck))
    {
      # Generate the sequence
      sample <- seq(minCheck[PARAM],maxCheck[PARAM],by=incCheck[PARAM])
      # Trouble here as due to precision of doubles, we may not locate the baseline
      # i.e. if 0.3 is there, and baseline is 0.3, the %in% check may still fail
      # So convert to string using paste

      # Does this contain the baseline:
      if(!toString(baselineCheck[PARAM]) %in% paste(sample))
      {
        message("Range specified by PMIN and PMAX should contain the BASELINE value, else behaviours cannot be compared")
        message("Spartan Terminated")
        return(FALSE)
      }
    }
    return(TRUE)
  },
  error=function(cond) {
    message("Error in declaring BASELINE in range specified by PMIN, PMAX, and PINC")
    message("Spartan Function Terminated")
    return(FALSE)
  })
}


#' Where used, checks that PARAMETERS, PMIN, PMAX, PINC, and BASELINE are all
#' the same length
#'
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_parameter_and_ranges_lengths <- function(arguments)
{
  tryCatch(
  {
    inputLengths <- c(length(eval(arguments$PARAMETERS)),
                      length(eval(arguments$BASELINE)),
                      length(eval(arguments$PMIN)),length(eval(arguments$PMAX)),
                      length(eval(arguments$PINC)))

    # These should be all the same
    if(all(inputLengths[1] == inputLengths))
      return(TRUE)
    else
    {
      message("Number of entries in PARAMETERS, BASELINE, PMIN, PMAX, and PINC should be equal")
      message("Spartan Terminated")
      return(FALSE)
    }
  },
  error=function(cond) {
    message("Error in declaring PARAMETERS, BASELINE, PMIN, PMAX, or PINC. Check all lengths are equal and all are numeric")
    message("Spartan Function Terminated")
    return(FALSE)
  })

}

#' Pre-Check of the parameters and ranges specified for sampling parameter
#' space
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Here for consistency in auto-function calling, but not used
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_parameters_and_ranges <- function(arguments, argument_name)
{
  preCheckSuccess = check_lengths_parameters_ranges(arguments)
  preCheckSuccess = check_numeric_list_values(arguments, "PMIN", "PMAX")

  return(preCheckSuccess)
}

#' Check that the filepath of required data or output exists
#' @param arguments Input arguments
#' @param argument_name Name of the argument, as provided by function
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_filepath_exists <- function(arguments, argument_name)
{
  tryCatch(
    {
      if(file.exists(eval(arguments[argument_name][[1]])))
        return(TRUE)
      else
      {

        message(paste("FILEPATH does not seem to exist:", eval(arguments[argument_name][[1]])))
        message("Spartan Function Terminated")
        return(FALSE)
      }
    },
    error=function(cond) {
      #print(cond)
      message(paste("FILEPATH does not seem to exist"))
      message("Spartan Function Terminated")
      return(FALSE)
    })
}

#' Check that the chosen lhc sampling algorithm is either normal or optimal.
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Argument name being checked. May be null, but here for consistency
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_lhs_algorithm <- function(arguments, argument_name)
{
  tryCatch(
  {
    if(!check_package_installed("lhs"))
      return(FALSE)
    else
    {
      if(tolower(eval(arguments$ALGORITHM)) == "normal" |  tolower(eval(arguments$ALGORITHM)) == "optimal")
        return(TRUE)
      else {
        message("LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
        return(FALSE)
      }
    }
  },
  error=function(cond) {
    message("LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
    return(FALSE)
  })


}

#' Check that a required package has been installed
#' @param packageName Name of the package to check for
#' @return Boolean stating whether the package is installed or not
check_package_installed <- function(packageName)
{
  tryCatch(
  {
    if(requireNamespace(packageName,quietly=TRUE))
      return(TRUE)
    else
      message(paste("Looking for package ",packageName,", which is not installed or does not exist",sep=""))
    return(FALSE)
  },
  error=function(cond) {
    message(paste("Looking for package ",packageName,", which is not installed or does not exist",sep=""))
    return(FALSE)
  })
}

#' Check that the lengths of the parameters, minimum values, and maximum values, are equal
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_lengths_parameters_ranges <- function(arguments)
{
  tryCatch(
    {
      minCheck <- as.numeric(get_argument_correct_case(arguments, "PMIN"))
      maxCheck <- as.numeric(get_argument_correct_case(arguments, "PMAX"))
      parameters <- get_argument_correct_case(arguments, "PARAMETERS")

      if(length(parameters) == length(minCheck) & length(parameters) == length(maxCheck))
        return(TRUE)
      else {
        message("Number of parameters must match the numbers of entries in PMIN and PMAX")
        return(FALSE)
      }
    },
    error=function(cond) {
      message("Value error in PMIN or PMAX. Check these are numeric, and declared in capitals (not 'pmin' and 'pmax')")
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    },
    # Warning may become apparent if the user enters a string as a number
    warning=function(cond) {
      message("Value error in PMIN or PMAX. Check these are numeric")
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    }
  )
}

#' Check that two lists are numeric, and the values of one are less than the other
#' @param arguments List of the arguments provided to the called function
#' @param nameSmall Parameter name of the smaller list, for error reporting
#' @param nameLarge Parameter name of the larger list, for error reporting
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_numeric_list_values <- function(arguments, nameSmall, nameLarge)
{
  #print(arguments)
  tryCatch(
    {
      smallCheck <- get_argument_correct_case(arguments, nameSmall)
      #print(smallCheck)
      largeCheck <- get_argument_correct_case(arguments, nameLarge)
      #print(largeCheck)

      if(all(smallCheck < largeCheck) & is.numeric(smallCheck) & is.numeric(largeCheck))
        return(TRUE)
      else {
        message(paste(nameSmall, " must be less than ", nameLarge, " for all parameters, both must be numeric, and declared in capitals: e.g. PMIN, PMAX, PINC",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste("Value error in ",nameSmall, " or ", nameLarge, ". Check these are numeric, and declared in capitals: e.g. PMIN, PMAX, PINC",sep=""))
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    })
}

#' Tries upper and lower case names for input arguments
#'
#' The method checking relies on the user calling the variables the right name.
#' If they don't, an error is produced. This helper function checks they haven't
#' just used lower case arguments, in which case the lower case is checked
#'
#' @param arguments List of the arguments provided to the called function
#' @param argName Name of the argument to return
#' @return The evaluated input value of this argument
#'
get_argument_correct_case <- function(arguments, argName)
{
  value = eval(arguments[argName][[1]])

  if(is.null(value) & argName!="PMIN" & argName !="PMAX")
     # Try raising argname to upper case, as it should have been specified
     value = eval(arguments[tolower(argName)][[1]])


  return(value)
}

#' Check that an argument that should be a positive integer has been specified correctly
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_argument_positive_int <- function(arguments,argument_name)
{
  tryCatch(
    {
      # KA: Taken this out for the moment, given new check method
      # Get the argument. We're going to force to upper case incase the user has specified lower
      #arg <- get_argument_correct_case(arguments, argName)
      arg <- eval(arguments[argument_name][[1]])

      if(all.equal(arg, as.integer(arg)) & arg > 0)
        return(TRUE)
      else {
        message(paste(argument_name, " must be a positive integer. Terminated",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste(argument_name, " must be a positive integer. Terminated",sep=""))
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    },
    warning=function(cond) {
      message(paste(argument_name, " must be a positive integer. Terminated",sep=""))
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    })
}

#' Check that an argument that should be a boolean has been specified correctly
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_boolean <- function(arguments, argument_name)
{
  tryCatch(
    {
      evaled_arg<-eval(arguments[argument_name][[1]])

      if(tolower(evaled_arg)=="true" | tolower(evaled_arg)=="false")
        return(TRUE)
      else {
        message(paste(argument_name," must be either true or false. Terminated", sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste(argument_name, " must be either true or false. Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that an argument that should be a text label has been specified correctly
#'
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_text <-function(arguments, argument_name)
{
  tryCatch(
    {
      argEvaled <- eval(arguments[argument_name][[1]])

      if((typeof(argEvaled)=="character" | typeof(argEvaled) == "double") & length(argEvaled)>0)
        return(TRUE)
      else {
        message(paste(argument_name, " must be either a text string or numeric. Error in declaration. Terminated",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste(argument_name, " must be either a text string or numeric. Error in declaration. Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that an arguments of a list that should be a text label has been specified correctly
#'
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_text_list <-function(arguments, argument_name)
{
  tryCatch(
    {
      # Get the list
      arg_list <- eval(arguments[argument_name][[1]])
      for(i in 1:length(arg_list))
      {
        check = check_text(arg_list[i], argument_name)
        #print(paste("Check: ",check,sep=""))
        if(!check)
        {
          message(paste("Error in declaration of ",argument_name,". Terminated",sep=""))
          return(FALSE)
        }
      }
      return(TRUE)
    },
    error=function(cond) {
      message(paste("Error in declaration of ",argument_name,". Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that a double argument is within a specified range
#'
#' @param argument Value of the argument to check
#' @param argument_name Name of the argument, for inclusion in the error message
#' @param range_min Minimum of the range
#' @param range_max Maximum of the range
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_double_value_in_range <- function(argument, argument_name, range_min, range_max)
{
  tryCatch(
    {
      # Get the list
      value <- eval(argument)

      if(is.numeric(value) & (value >= range_min & value <= range_max))
        return(TRUE)
      else
      {
        message(paste(argument_name, " must be between ",range_min," and ", range_max,". Spartan terminated",sep=""))
        return(FALSE)
      }

    },
    error=function(cond) {
      message(paste("Error: ", argument_name, " must be between ",range_min," and ", range_max,". Spartan terminated",sep=""))
      return(FALSE)
    })
}

#' Check that a confidence interval is within a specified range
#'
#' @param argument Value of the argument to check
#' @param argument_name Name of the argument, for inclusion in the error message
#' @param range_min Minimum of the range
#' @param range_max Maximum of the range
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_confidence_interval <- function(argument, argument_name, range_min, range_max)
{
  tryCatch(
    {
      # Get the list
      value <- eval(argument)
      #h<-c(value, range_min,range_max)


      if(is.numeric(value) & (value >= range_min & value <= range_max))
      {
        #h<-c(h,"TRUE")
        return(TRUE)
      }
      else
      {
        #h<-c(h,"FALSE")
        message(paste(argument_name, " must be between ",range_min," and ", range_max,". Spartan terminated",sep=""))
        return(FALSE)
      }
      #write_data_to_csv(h,"Values.csv")

    },
    error=function(cond) {
      message(paste("Error: ", argument_name, " must be between ",range_min," and ", range_max,". Spartan terminated",sep=""))
      return(FALSE)
    })
}

#' Check that all objects of a list are integers
#'
#' @param arguments List of the arguments provided to the called function
#' @param argument_name Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_list_all_integers <- function(arguments, argument_name)
{
  tryCatch(
    {
      # Get the list
      value_list <- eval(arguments[argument_name][[1]])
      #print(value_list)

      if(all(value_list == floor(value_list)) & all(value_list > 0))
        return(TRUE)
      else
      {
        message(paste(argument_name," must be a list of positive integers. Terminated",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      #print(cond)
      message(paste("Error in declaration of ",argument_name,". Spartan Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that result filepaths under the root directory exist
#'
#' @param file_root Root directory of the data to analyse
#' @param sub_dirs List of the subdirectories that should be under the root
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_nested_filepaths <- function(file_root, sub_dirs)
{
  tryCatch(
    {
      root <- eval(file_root)
      sub_dir_list <- eval(sub_dirs)
      not_exist <- c()

      for(i in 1:length(sub_dir_list))
      {
        if(!file.exists(file.path(root,sub_dir_list[i])))
        {
          not_exist <- c(not_exist, sub_dir_list[i])
          #message(paste("Sub-directory ",file.path(root,sub_dir_list[i])," does not exist. Spartan Terminated",sep=""))
          #return(FALSE)
        }
      }
      if(length(not_exist)>0)
      {
        message(paste("Sub-directories ",toString(not_exist), "do not exist. Spartan Terminated",sep=""))
        return(FALSE)
      }
      else
      {
        return(TRUE)
      }
    },
    error=function(cond) {
      message(paste("Error in declaration of file paths to data to analyse. Spartan Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that the user has declared either a file name or an R object
#' @param arguments List of the arguments provided to the called function
#' @param fileArg Name of the argument for which a file should be specified
#' @param rObjArg Name of the argument for which an R object should be specified
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_consistency_result_type <- function(arguments, fileArg, rObjArg)
{
  tryCatch(
  {
    r_object <- eval(arguments[rObjArg][[1]])
    file_name <- eval(arguments[fileArg][[1]])
    filepath <- eval(arguments$FILEPATH)

    # Must have specified either an R object or a file
    if(is.null(r_object) & is.null(file_name))
    {
      message(paste("Error in declaring either ",fileArg," or ",rObjArg,". You must specify one. Spartan Terminated",sep=""))
      return(FALSE)
    }
    else {
      if(is.null(r_object))
      {
        # The user is specifying a results file name
        # Can check this here
        file_check <- check_text(arguments[fileArg][[1]], fileArg)

        if(file_check)
        {
          if(is.null(eval(arguments$TIMEPOINTS)))
          {
            # Check the file exists
            if(file.exists(file.path(filepath,file_name)))
              return(TRUE)
            else
            {
              message(paste("File ",file_name, " in argument ",fileArg, " does not exist in ",filepath,sep=""))
              return(FALSE)
            }
          }
          else
          {
            for(t in 1:length(eval(arguments$TIMEPOINTS)))
            {
              filename_full <- append_time_to_argument(
                file_name, eval(arguments$TIMEPOINTS)[t],
                check_file_extension(file_name))

              if(!file.exists(file.path(filepath, filename_full)))
                return(FALSE)
            }
            return(TRUE)
          }
        }
        else
        {
          message(paste("Problem with declaration of argument ",fileArg,", ",file_name, ". Spartan Terminated",sep=""))
          return(FALSE)
        }
      }
      else
      {
        # Existence of R object will have been checked when evaluated earlier
        return(TRUE)
      }
    }
  },
  error=function(cond) {
    #print(cond)
    message(paste("Error in declaring either ",fileArg," or ",rObjArg,". You must specify one. Spartan Terminated",sep=""))
    return(FALSE)
  })
}

#' Checks for the existence of a file
#' @param filepath Directory where the file should be
#' @param filename Name of the file
#' @return Boolean showing whether or not the file exists
check_file_exist <- function(filepath, filename)
{
  tryCatch(
  {
    if(file.exists(file.path(filepath, filename)))
      return(TRUE)
    else
      return(FALSE)
  },
  error=function(cond) {
    # error printed in calling function
    return(FALSE)
  })
}

#' For aleatory analysis, checks the analysis start and end columns are sensible
#' @param arguments List of the arguments provided to the called function
#' @param filepath Evaluated filepath argument
#' @param resultfile Name of the result file of columns to check
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_column_ranges <- function(arguments, filepath, resultfile)
{
  # This opens the first result file and checks the number of columns.
  # The assumption is made all others will be of the same structure
  # So we need to get the first sample size

  tryCatch(
  {
    if(is.null(eval(arguments$TIMEPOINTS)))
    {
      if(check_file_exist(filepath, eval(resultfile)))
      {

        # Load it and check number of columns
        result<-read.csv(file.path(filepath, eval(resultfile)),header=T)

        if(eval(arguments$OUTPUTFILECOLSTART) > 0 &
           eval(arguments$OUTPUTFILECOLSTART) <= ncol(result) &
           eval(arguments$OUTPUTFILECOLEND) > 0 &
           eval(arguments$OUTPUTFILECOLEND) <= ncol(result) &
           eval(arguments$OUTPUTFILECOLEND) >=
           eval(arguments$OUTPUTFILECOLSTART)) {
          return(TRUE)
        } else {
          message("Error in declaring either OUTPUTFILECOLSTART or OUTPUTFILECOLEND. Spartan Terminated")
          return(FALSE)
        }
      }
      else {
        message(paste("Attempted to check OUTPUTFILECOLSTART and OUTPUTFILECOLEND in first result file, but file ",
                      paste(filepath,"/",resultfile,sep=""), " does not exist",sep=""))
        return(FALSE)
      }
    }
    else
    {
      # Timepoints not checked yet - must be added later
      return(TRUE)
    }
  },
  error=function(cond) {
    #print(cond)
    message("Error in declaring either OUTPUTFILECOLSTART or OUTPUTFILECOLEND. Spartan Terminated")
    return(FALSE)
  })
}



