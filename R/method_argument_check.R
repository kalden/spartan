#' Pre-execution checks to perform before the spartan get aTest results method
#' within the consistency analysistechnique is executed. Checks all input
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_getATestResult_Input <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"NUMSUBSETSPERSAMPLESIZE")
  preCheckSuccess = check_double_value_in_range(arguments$LARGEDIFFINDICATOR, preCheckSuccess, "LARGEDIFFINDICATOR",0,0.5)
  preCheckSuccess = check_text(arguments$ATESTRESULTSFILENAME, preCheckSuccess, "ATESTRESULTSFILENAME")
  preCheckSuccess = check_text_list(arguments$MEASURES, preCheckSuccess, "MEASURES")
  preCheckSuccess = check_list_all_integers(arguments$SAMPLESIZES, preCheckSuccess, "SAMPLESIZES")
  preCheckSuccess = check_consistency_result_type(arguments, preCheckSuccess)

  return(preCheckSuccess)
}

check_aa_summariseReplicateRuns <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_list_all_integers(arguments$SAMPLESIZES, preCheckSuccess, "SAMPLESIZES")
  preCheckSuccess = check_nested_filepaths(arguments$FILEPATH, arguments$SAMPLESIZES, preCheckSuccess)
  preCheckSuccess = check_text_list(arguments$MEASURES, preCheckSuccess, "MEASURES")
  preCheckSuccess = check_text(arguments$RESULTFILENAME, preCheckSuccess, "RESULTFILENAME")
  preCheckSuccess = check_text(arguments$ALTFILENAME, preCheckSuccess, "ALTFILENAME")
  preCheckSuccess = check_column_ranges(arguments, arguments$FILEPATH, arguments$RESULTFILENAME, preCheckSuccess)
  if(!is.null(eval(arguments$ALTFILENAME)))
    preCheckSuccess = check_column_ranges(arguments, arguments$FILEPATH, arguments$ALTFILENAME, preCheckSuccess)
  preCheckSuccess = check_text(arguments$SUMMARYFILENAME, preCheckSuccess, "SUMMARYFILENAME")

  # Timepoints needs adding

  return(preCheckSuccess)

}


#' Pre-execution checks to perform before the spartan lhc samplng technique
#' is executed. Checks all parameter input
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_lhc_sampling_args <- function(arguments)
{
  preCheckSuccess = TRUE
  # From Version 3.1, FILEPATH could be not specified, if user does not want a
  # CSV file, but an R object returned
  #preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_package_installed("lhs",preCheckSuccess)
  preCheckSuccess = check_lhs_algorithm(arguments,preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_parameters_and_ranges(arguments, preCheckSuccess, "lhc")

  return(preCheckSuccess)

}

#' Pre-execution checks to perform before the spartan lhc samplng technique
#' is executed for a netlogo simulation
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_lhc_sampling_netlogo_args <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_package_installed("lhs",preCheckSuccess)
  preCheckSuccess = check_package_installed("XML",preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_lhs_algorithm(arguments,preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"EXPERIMENT_REPETITIONS")
  preCheckSuccess = check_text(arguments$NETLOGO_SETUP_FUNCTION, preCheckSuccess, "NETLOGO_SETUP_FUNCTION")
  preCheckSuccess = check_text(arguments$NETLOGO_RUN_FUNCTION, preCheckSuccess, "NETLOGO_RUN_FUNCTION")
  preCheckSuccess = check_boolean(arguments$RUN_METRICS_EVERYSTEP, preCheckSuccess, "RUN_METRICS_EVERYSTEP")
  preCheckSuccess = check_paramvals_length_equals_parameter_length(arguments, preCheckSuccess)
  preCheckSuccess = check_text_list(arguments$PARAMETERS, preCheckSuccess, "PARAMETERS")
  preCheckSuccess = check_text_list(arguments$MEASURES, preCheckSuccess, "MEASURES")

  return(preCheckSuccess)
  # To check: PARAMVALS, RUNMETRICS_EVERYSTEP
}

#' Pre-execution checks to perform before the spartan efast samplng technique
#' is executed for a netlogo simulation
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_efast_sampling_netlogo_args <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_package_installed("XML",preCheckSuccess)
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"NUMCURVES")
  preCheckSuccess = check_argument_positive_int(arguments, preCheckSuccess,"EXPERIMENT_REPETITIONS")
  preCheckSuccess = check_boolean(arguments$RUNMETRICS_EVERYSTEP, preCheckSuccess, "RUNMETRICS_EVERYSTEP")
  preCheckSuccess = check_text(arguments$NETLOGO_SETUP_FUNCTION, preCheckSuccess, "NETLOGO_SETUP_FUNCTION")
  preCheckSuccess = check_text(arguments$NETLOGO_RUN_FUNCTION, preCheckSuccess, "NETLOGO_RUN_FUNCTION")
  preCheckSuccess = check_paramvals_length_equals_parameter_length(arguments, preCheckSuccess)
  preCheckSuccess = check_text_list(arguments$PARAMETERS, preCheckSuccess, "PARAMETERS")
  preCheckSuccess = check_text_list(arguments$MEASURES, preCheckSuccess, "MEASURES")

  return(preCheckSuccess)
}

#' Pre-execution checks to perform before the spartan efast samplng technique
#' is executed. Checks all parameter input
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_efast_sampling_args <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_argument_positive_int(arguments,preCheckSuccess,"NUMCURVES")
  preCheckSuccess = check_parameters_and_ranges(arguments, preCheckSuccess, "efast")

  return(preCheckSuccess)
}

#' Pre-execution checks to perform before the spartan robustness samplng
#' technique is executed. Checks all parameter input
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_robustness_sampling_args <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_robustness_range_or_values(arguments,preCheckSuccess)

  # From the above test we know that the user has either specified PARAMVALS
  # or PMIN,PMAX,INC choice - now we test dependent on that
  if(is.null(eval(arguments$PARAMVALS)))
  {
    preCheckSuccess = check_robustness_parameter_and_ranges_lengths(arguments, preCheckSuccess)
    preCheckSuccess = check_numeric_list_values(arguments, "PMIN", "PMAX", preCheckSuccess)
    preCheckSuccess = check_numeric_list_values(arguments, "PINC", "PMAX", preCheckSuccess)
    preCheckSuccess = check_robustness_range_contains_baseline(arguments, preCheckSuccess)
  }
  else
  {
    preCheckSuccess = check_paramvals_length_equals_parameter_length(arguments, preCheckSuccess)
    preCheckSuccess = check_robustness_paramvals_contains_baseline(arguments, preCheckSuccess)
  }

  return(preCheckSuccess)
}

#' For robustness, check whether using PMIN/PMAX/PINC entry or PARAMVALS
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_range_or_values <- function(arguments,preCheckSuccess)
{
  if(is.null(eval(arguments$PARAMVALS)))
  {
    if(is.null(eval(arguments$PMIN)) | is.null(eval(arguments$PMAX)) | is.null(eval(arguments$PINC)))
    {
      message("You need to specify either PMIN,PMAX,and PINC, or the values to sample in PARAMVALS")
      return(FALSE)
    }
    else
      return(preCheckSuccess)
  }
  else
  {
    if(!is.null(eval(arguments$PMIN)) | !is.null(eval(arguments$PMAX)) | !is.null(eval(arguments$PINC)))
    {
      message("You need to specify either PMIN,PMAX,and PINC, or the values to sample in PARAMVALS")
      return(FALSE)
    }
    else
      return(preCheckSuccess)
  }
}

#' Where used in robustness analysis, check that the length of PARAMVALS equals
# the number of PARAMETERS
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_paramvals_length_equals_parameter_length <- function(arguments, preCheckSuccess)
{
  tryCatch(
  {
    if(length(eval(arguments$PARAMETERS)) == length(eval(arguments$PARAMVALS)))
      return(preCheckSuccess)
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
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_paramvals_contains_baseline <- function(arguments, preCheckSuccess)
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
    return(preCheckSuccess)
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
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_range_contains_baseline <- function(arguments, preCheckSuccess)
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
    return(preCheckSuccess)
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
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_robustness_parameter_and_ranges_lengths <- function(arguments, preCheckSuccess)
{
  tryCatch(
  {
    inputLengths <- c(length(eval(arguments$PARAMETERS)),
                      length(eval(arguments$BASELINE)),
                      length(eval(arguments$PMIN)),length(eval(arguments$PMAX)),
                      length(eval(arguments$PINC)))

    # These should be all the same
    if(all(inputLengths[1] == inputLengths))
      return(preCheckSuccess)
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

# NOT UT AS YET
#' Pre-Check of the parameters and ranges specified for sampling parameter
#' space
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @param method Spartan method being checked
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_parameters_and_ranges <- function(arguments, preCheckSuccess, method)
{
  preCheckSuccess = check_lengths_parameters_ranges(arguments,preCheckSuccess)
  preCheckSuccess = check_numeric_list_values(arguments, "PMIN", "PMAX", preCheckSuccess)

  return(preCheckSuccess)
}

#' Check that the filepath of required data or output exists
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_filepath_exists <- function(arguments,preCheckSuccess)
{
  tryCatch(
    {
      if(file.exists(eval(arguments$FILEPATH)))
        return(preCheckSuccess)
      else
      {
        message(paste("FILEPATH does not seem to exist:", eval(arguments$FILEPATH)))
        message("Spartan Function Terminated")
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste("FILEPATH does not seem to exist"))
      message("Spartan Function Terminated")
      return(FALSE)
    })
}

#' Check that the chosen lhc sampling algorithm is either normal or optimal
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_lhs_algorithm <- function(arguments,preCheckSuccess)
{
  tryCatch(
  {
    if(tolower(eval(arguments$ALGORITHM)) == "normal" |  tolower(eval(arguments$ALGORITHM)) == "optimal")
      return(preCheckSuccess)
    else {
      message("LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
      return(FALSE)
    }
  },
  error=function(cond) {
    message("LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
    return(FALSE)
  })


}

#' Check that a required package has been installed
#' @param packageName Name of the package to check for
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating whether the package is installed or not
check_package_installed <- function(packageName,preCheckSuccess)
{
  tryCatch(
  {
    if(requireNamespace(packageName,quietly=TRUE))
      return(preCheckSuccess)
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
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_lengths_parameters_ranges <- function(arguments,preCheckSuccess)
{
  tryCatch(
    {
      minCheck <- as.numeric(get_argument_correct_case(arguments, "PMIN"))
      maxCheck <- as.numeric(get_argument_correct_case(arguments, "PMAX"))
      parameters <- get_argument_correct_case(arguments, "PARAMETERS")

      if(length(parameters) == length(minCheck) & length(parameters) == length(maxCheck))
        return(preCheckSuccess)
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
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_numeric_list_values <- function(arguments, nameSmall, nameLarge, preCheckSuccess)
{
  #print(arguments)
  tryCatch(
    {
      smallCheck <- get_argument_correct_case(arguments, nameSmall)
      largeCheck <- get_argument_correct_case(arguments, nameLarge)

      if(all(smallCheck < largeCheck) & is.numeric(smallCheck) & is.numeric(largeCheck))
        return(preCheckSuccess)
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
#' @param preCheckSuccess Current status of pre-execution checks
#' @param argName Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_argument_positive_int <- function(arguments,preCheckSuccess,argName)
{
  tryCatch(
    {
      # Get the argument. We're going to force to upper case incase the user has specified lower
      arg <- get_argument_correct_case(arguments, argName)

      if(all.equal(arg, as.integer(arg)) & arg > 0)
        return(preCheckSuccess)
      else {
        message(paste(argName, " must be a positive integer. Terminated",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste(argName, " must be a positive integer. Terminated",sep=""))
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    },
    warning=function(cond) {
      message(paste(argName, " must be a positive integer. Terminated",sep=""))
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    })
}

#' Check that an argument that should be a boolean has been specified correctly
#' @param argument Value of the argument to check
#' @param preCheckSuccess Current status of pre-execution checks
#' @param argName Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_boolean <- function(argument, preCheckSuccess, argName)
{
  tryCatch(
    {
      evaled_arg<-eval(argument)

      if(tolower(evaled_arg)=="true" | tolower(evaled_arg)=="false")
        return(preCheckSuccess)
      else {
        message(paste(argName," must be either true or false. Terminated", sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste(argName, " must be either true or false. Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that an argument that should be a text label has been specified correctly
#'
#' @param argument Value of the argument to check
#' @param preCheckSuccess Current status of pre-execution checks
#' @param argName Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_text <-function(argument, preCheckSuccess, argName)
{
  tryCatch(
    {
      argEvaled <- eval(argument)

      if((typeof(argEvaled)=="character" | typeof(argEvaled) == "double") & length(argEvaled)>0)
        return(preCheckSuccess)
      else {
        message(paste(argName, " must be either a text string or numeric. Error in declaration. Terminated",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste(argName, " must be either a text string or numeric. Error in declaration. Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that an arguments of a list that should be a text label has been specified correctly
#'
#' @param argument Value of the argument to check
#' @param preCheckSuccess Current status of pre-execution checks
#' @param argName Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_text_list <-function(argument, preCheckSuccess, argName)
{
  tryCatch(
    {
      # Get the list
      arg_list <- eval(argument)
      for(i in 1:length(arg_list))
      {
        check = check_text(arg_list[i], preCheckSuccess, argName)
        #print(paste("Check: ",check,sep=""))
        if(!check)
        {
          message(paste("Error in declaration of ",argName,". Terminated",sep=""))
          return(FALSE)
        }
      }
      return(preCheckSuccess)
    },
    error=function(cond) {
      message(paste("Error in declaration of ",argName,". Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that a double argument is within a specified range
#'
#' @param argument Value of the argument to check
#' @param preCheckSuccess Current status of pre-execution checks
#' @param argName Name of the argument, for inclusion in the error message
#' @param range_min Minimum of the range
#' @param range_max Maximum of the range
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_double_value_in_range <- function(argument, preCheckSuccess, argName, range_min, range_max)
{
  tryCatch(
    {
      # Get the list
      value <- eval(argument)

      if(is.numeric(value) & (value >= range_min & value <= range_max))
        return(preCheckSuccess)
      else
      {
        message(paste(argName, " must be between ",range_min," and ", range_max,". Spartan terminated",sep=""))
        return(FALSE)
      }

    },
    error=function(cond) {
      message(paste("Error: ", argName, " must be between ",range_min," and ", range_max,". Spartan terminated",sep=""))
      return(FALSE)
    })
}

#' Check that all objects of a list are integers
#'
#' @param argument Value of the argument to check
#' @param preCheckSuccess Current status of pre-execution checks
#' @param argName Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_list_all_integers <- function(argument, preCheckSuccess, argName)
{
  tryCatch(
    {
      # Get the list
      value_list <- eval(argument)

      if(all(value_list == floor(value_list)) & all(value_list > 0))
        return(preCheckSuccess)
      else
      {
        message(paste(argName," must be a list of positive integers. Terminated",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste("Error in declaration of ",argName,". Spartan Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that result filepaths under the root directory exist
#'
#' @param file_root Root directory of the data to analyse
#' @param sub_dirs List of the subdirectories that should be under the root
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_nested_filepaths <- function(file_root, sub_dirs, preCheckSuccess)
{
  tryCatch(
    {
      root <- eval(file_root)
      sub_dir_list <- eval(sub_dirs)

      for(i in 1:length(sub_dir_list))
      {
        if(!file.exists(paste(root,"/",sub_dir_list[i],sep="")))
        {
          message(paste("Sub-directory ",root,"/",sub_dir_list[i])," does not exist. Spartan Terminated",sep="")
          return(FALSE)
        }
      }
      return(preCheckSuccess)
    },
    error=function(cond) {
      message(paste("Error in declaration of file paths to data to analyse. Spartan Terminated",sep=""))
      return(FALSE)
    })
}

#' Check that the user has declared a-test results either in an R object or in a file name
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_consistency_result_type <- function(arguments, preCheckSuccess)
{
  tryCatch(
  {
    r_object <- eval(arguments$AA_SIM_RESULTS_OBJECT)
    #print(paste("R OBJECT: ",r_object,sep=""))
    file_name <- eval(arguments$AA_SIM_RESULTS_FILE )
    filepath <- eval(arguments$FILEPATH)

    # Must have specified either AA_SIM_RESULTS_OBJECT (an R object) or a results file
    if(is.null(r_object) & is.null(file_name))
    {
      message("Error in declaring either AA_SIM_RESULTS_OBJECT or MEDIANS_SUMMARY_FILE_NAME. You must specify one. Spartan Terminated")
      return(FALSE)
    }
    else {
      if(is.null(r_object))
      {
        # The user is specifying a results file name
        # Can check this here
        file_check <- check_text(file_name, preCheckSuccess, "AA_SIM_RESULTS_FILE")
        if(file_check)
          # Check the file exists
          if(file.exists(paste(filepath,"/",file_name,sep="")))
            return(preCheckSuccess)
          else
          {
            message(paste("Simulation results summary file ",file_name, " does not exist in ",filepath,sep=""))
            return(FALSE)
          }
        else
        {
          message(paste("Declaring A-Test results in file ",file_name," yet problem with declaration of this file name string. Spartan Terminated",sep=""))
          return(FALSE)
        }
      }
      else
      {
        # Any issue with the R object not existing will have already been caught
        # when this argument was evaluated above
        return(preCheckSuccess)
      }
    }
  },
  error=function(cond) {
    message("Error in declaring either AA_SIM_RESULTS_OBJECT or MEDIANS_SUMMARY_FILE_NAME. Spartan Terminated")
    return(FALSE)
  })
}

#' Checks for the existence of a file
#' @param filepath Directory where the file should be
#' @param file Name of the file
#' @return Boolean showing whether or not the file exists
check_file_exist <- function(filepath, file)
{
  tryCatch(
  {
    if(file.exists(file.path(filepath, file)))
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
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_column_ranges <- function(arguments, filepath, resultfile, preCheckSuccess)
{
  # This opens the first result file and checks the number of columns.
  # The assumption is made all others will be of the same structure
  if(check_file_exist(filepath, resultfile)==TRUE)
  {
    tryCatch(
    {
      # Load it and check number of columns
      result<-read.csv(file.path(filepath,resultfile),header=T)
      if(eval(arguments$OUTPUTFILECOLSTART) > 0 &
         eval(arguments$OUTPUTFILECOLSTART) <= ncol(result) &
         eval(arguments$OUTPUTFILECOLEND) > 0 &
         eval(arguments$OUTPUTFILECOLEND) <= ncol(result) &
         eval(arguments$OUTPUTFILECOLEND) >=
         eval(arguments$OUTPUTFILECOLSTART)) {
        return(preCheckSuccess)
      } else {
        message("Error in declaring either OUTPUTFILECOLSTART or OUTPUTFILECOLEND. Spartan Terminated")
        return(FALSE)
      }
    },
    error=function(cond) {
      message("Error in declaring either OUTPUTFILECOLSTART or OUTPUTFILECOLEND. Spartan Terminated")
      return(FALSE)
    })
  } else {
    message(paste("Attempted to check OUTPUTFILECOLSTART and OUTPUTFILECOLEND in first result file, but file ",
                  file.path(filepath,resultfile), " does not exist",sep=""))
    return(FALSE)
  }
}
