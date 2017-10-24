#' Pre-execution checks to perform before the spartan lhc samplng technique
#' is executed. Checks all parameter input
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
check_lhc_sampling_args <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_package_installed("lhs",preCheckSuccess)
  preCheckSuccess = check_lhs_algorithm(arguments,preCheckSuccess)
  preCheckSuccess = check_argument_positive_int(arguments$NUMSAMPLES,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_parameters_and_ranges(arguments, preCheckSuccess, "lhc")

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
  preCheckSuccess = check_argument_positive_int(arguments$NUMSAMPLES,preCheckSuccess,"NUMSAMPLES")
  preCheckSuccess = check_argument_positive_int(arguments$NUMCURVES,preCheckSuccess,"NUMCURVES")
  preCheckSuccess = check_parameters_and_ranges(arguments, preCheckSuccess, "efast")

  return(preCheckSuccess)
}

#' Pre-Check of the parameters and ranges specified for sampling parameter
#' space
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks,
#' or FALSE if this check fails
check_parameters_and_ranges <- function(arguments, preCheckSuccess, method)
{
  preCheckSuccess = check_lengths_parameters_ranges(arguments,preCheckSuccess)
  preCheckSuccess = check_min_and_max_values(arguments,preCheckSuccess)

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
      # Choose a return value in case of error
      return(FALSE)
    })
}

#' Check that the chosen lhc sampling algorithm is either normal or optimal
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_lhs_algorithm <- function(arguments,preCheckSuccess)
{
  if(tolower(eval(arguments$ALGORITHM)) == "normal" |  tolower(eval(arguments$ALGORITHM)) == "optimal")
    return(preCheckSuccess)
  else {
    message("LHS Algorithm must be either 'normal' or 'optimal'. Terminated")
    return(FALSE)
  }

}

#' Check that a required package has been installed
#' @param packageName Name of the package to check for
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating whether the package is installed or not
check_package_installed <- function(packageName,preCheckSuccess)
{
  return(requireNamespace(packageName))
}

#' Check that the lengths of the parameters, minimum values, and maximum values, are equal
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_lengths_parameters_ranges <- function(arguments,preCheckSuccess)
{
  tryCatch(
    {
      minCheck <- eval(arguments$PMIN)
      maxCheck <- eval(arguments$PMAX)

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
    }
  )
}

#' Check that the minimum and maximum values are numeric, and minimums are less than the maximum
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_min_and_max_values <- function(arguments,preCheckSuccess)
{

  tryCatch(
    {
      minCheck <- eval(arguments$PMIN)
      maxCheck <- eval(arguments$PMAX)

      if(all(minCheck < maxCheck) & is.numeric(minCheck) & is.numeric(maxCheck))
        return(preCheckSuccess)
      else {
        message("PMIN and PMAX must be numeric, and minimum values must be less than maximum values for all parameters")
        return(FALSE)
      }
    },
    error=function(cond) {
      message("Value error in PMIN or PMAX. Check these are numeric")
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    },
  warning=function(cond) {
    message("Value error in PMIN or PMAX. Check these are numeric, and have the same number of entries")
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
