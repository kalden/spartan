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
  preCheckSuccess = check_argument_positive_int(arguments$NUMSAMPLES,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_parameters_and_ranges(arguments, preCheckSuccess, "lhc")

  return(preCheckSuccess)

}

check_lhc_sampling_netlogo_args <- function(arguments)
{
  preCheckSuccess = TRUE
  #print(paste("preCheck: ",preCheckSuccess,sep=""))
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  #print(paste("preCheck: ",preCheckSuccess,sep=""))
  preCheckSuccess = check_package_installed("lhs",preCheckSuccess)
  #print(paste("preCheck: ",preCheckSuccess,sep=""))
  preCheckSuccess = check_package_installed("XML",preCheckSuccess)
  #print(paste("preCheck: ",preCheckSuccess,sep=""))
  preCheckSuccess = check_argument_positive_int(arguments$NUMSAMPLES,preCheckSuccess,"NUMSAMPLES")
  #print(paste("preCheck: ",preCheckSuccess,sep=""))
  preCheckSuccess = check_lhs_algorithm(arguments,preCheckSuccess)
  #print(paste("preCheck: ",preCheckSuccess,sep=""))
  preCheckSuccess = check_argument_positive_int(arguments$EXPERIMENT_REPETITIONS,preCheckSuccess,"EXPERIMENT_REPETITIONS")
  #print(paste("preCheck: ",preCheckSuccess,sep=""))

  return(preCheckSuccess)
  # To check: PARAMETERS, PARAMVALS, RUNMETRICS_EVERYSTEP, NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION, MEASURES


  #lhc_generate_lhc_sample_netlogo <- function(FILEPATH, PARAMETERS, PARAMVALS,
  #                                            NUMSAMPLES, ALGORITHM,
  #                                            EXPERIMENT_REPETITIONS,
  #                                            RUNMETRICS_EVERYSTEP,
  #                                            NETLOGO_SETUP_FUNCTION,
  #                                            NETLOGO_RUN_FUNCTION, MEASURES)
}

#' Pre-execution checks to perform before the spartan efast samplng technique
#' is executed. Checks all parameter input
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_efast_sampling_args <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments$NUMSAMPLES,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_argument_positive_int(arguments$NUMCURVES,preCheckSuccess,"NUMCURVES")
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
    preCheckSuccess = check_numeric_list_values(arguments$PMIN, arguments$PMAX, "PMIN", "PMAX", preCheckSuccess)
    preCheckSuccess = check_numeric_list_values(arguments$PINC, arguments$PMAX, "PINC", "PMAX", preCheckSuccess)
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

# Where used in robustness analysis, check that the length of PARAMVALS equals
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
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_parameters_and_ranges <- function(arguments, preCheckSuccess, method)
{
  preCheckSuccess = check_lengths_parameters_ranges(arguments,preCheckSuccess)
  preCheckSuccess = check_numeric_list_values(arguments$PMIN, arguments$PMAX, "PMIN", "PMAX", preCheckSuccess)

  return(preCheckSuccess)
}

#' Check that the filepath of required data or output exists
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_filepath_exists <- function(arguments,preCheckSuccess)
{
  out <- tryCatch(
    {
      #print(paste("preCheck in FP: ",preCheckSuccess,sep=""))
      #print(paste("in: ", eval(arguments$FILEPATH),sep=""))
      #print(paste("Does File Exist: ",file.exists(eval(arguments$FILEPATH)),sep=""))
      if(file.exists(eval(arguments$FILEPATH)))
        return(preCheckSuccess)
      else
      {
        message(paste("FILEPATH does not seem to exist:", arguments$FILEPATH))
        message("Spartan Function Terminated")
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste("FILEPATH does not seem to exist:", arguments$FILEPATH))
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
      minCheck <- as.numeric(eval(arguments$PMIN))
      maxCheck <- as.numeric(eval(arguments$PMAX))

      if(length(eval(arguments$PARAMETERS)) == length(minCheck) & length(eval(arguments$PARAMETERS)) == length(maxCheck))
        return(preCheckSuccess)
      else {
        message("Number of parameters must match the numbers of entries in PMIN and PMAX")
        return(FALSE)
      }
    },
    error=function(cond) {
      message("Value error in PMIN or PMAX. Check these are numeric")
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
#' @param smallList List of values that should be smaller
#' @param largeList List of values that should be larger
#' @param nameSmall Parameter name of the smaller list, for error reporting
#' @param nameLarge Parameter name of the larger list, for error reporting
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_numeric_list_values <- function(smallList, largerList, nameSmall, nameLarge, preCheckSuccess)
{
  tryCatch(
    {
      smallCheck <- eval(smallList)
      largeCheck <- eval(largerList)

      if(all(smallCheck < largeCheck) & is.numeric(smallCheck) & is.numeric(largeCheck))
        return(preCheckSuccess)
      else {
        message(paste(nameSmall, " must be less than ",nameLarge, " for all parameters, and must be numeric",sep=""))
        return(FALSE)
      }
    },
    error=function(cond) {
      message(paste("Value error in ",nameSmall, " or ", nameLarge, ". Check these are numeric",sep=""))
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    },
    warning=function(cond) {
      message(paste("Value error in ",nameSmall, " or ", nameLarge, ". Check these are numeric",sep=""))
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    })
}

#' Check that an argument that should be a positive integer has been specified correctly
#' @param argument Value of the argument to check
#' @param preCheckSuccess Current status of pre-execution checks
#' @param argName Name of the argument, for inclusion in the error message
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_argument_positive_int <- function(argument,preCheckSuccess,argName)
{
  tryCatch(
    {
      if(all(eval(argument) == as.integer(eval(argument))) & eval(argument) > 0)
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
