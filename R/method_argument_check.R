#' Pre-execution checks to perform before a spartan technique is executed. Checks all parameter input
#' @param arguments List of the arguments provided to the called function
#' @return Boolean stating the status of the pre-execution checks
method_argument_check <- function(arguments)
{
  preCheckSuccess = TRUE
  preCheckSuccess = check_package_installed("lhs",preCheckSuccess)
  preCheckSuccess = check_filepath_exists(arguments,preCheckSuccess)
  preCheckSuccess = check_lhs_algorithm(arguments,preCheckSuccess)
  preCheckSuccess = check_lengths_parameters_ranges(arguments,preCheckSuccess)
  preCheckSuccess = check_numSamples(arguments,preCheckSuccess)
  preCheckSuccess = check_min_less_than_max(arguments,preCheckSuccess)
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
      file.exists(eval(arguments$FILEPATH))
      return(preCheckSuccess)
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
  if(length(eval(arguments$PARAMETERS)) == length(eval(arguments$PMIN)) & length(eval(arguments$PARAMETERS)) == length(eval(arguments$PMAX)))
    return(preCheckSuccess)
  else {
    message("Number of parameters must match the numbers of entries in PMIN and PMAX")
    return(FALSE)
  }
}

#' Check that the minimum and maximum values are numeric, and minimums are less than the maximum
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_min_and_max_values <- function(arguments,preCheckSuccess)
{
  tryCatch(
    {
      if(all(eval(arguments$PMIN) < eval(arguments$PMAX)) & is.numeric(eval(arguments$PMIN)) & is.numeric(eval(arguments$PMAX)))
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

#' Check that the number of samples to generate is numeric and greater than 0
#' @param arguments List of the arguments provided to the called function
#' @param preCheckSuccess Current status of pre-execution checks
#' @return Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
check_numSamples <- function(arguments,preCheckSuccess)
{
  tryCatch(
    {
      if(all(eval(arguments$NUMSAMPLES) == as.integer(eval(arguments$NUMSAMPLES))) & eval(arguments$NUMSAMPLES) > 0)
        return(preCheckSuccess)
      else {
        message("Number of samples must be a positive integer. Terminated")
        return(FALSE)
      }
    },
    error=function(cond) {
      message("Number of samples must be a positive integer")
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    },
    warning=function(cond) {
      message("Number of samples must be a positive integer")
      message("Spartan Function Terminated")
      # Choose a return value in case of error
      return(FALSE)
    })
}









  #tryCatch(file.exists(arguments$FILEPATH)),
  #         warning = function(w) {"Filepath does not exist"));
  #           log(-x)},
  #         +            error = function(e) {print(paste("non-numeric argument", x));
  #           NaN}


  #if(file.exists(arguments$FILEPATH)) {
  #  return(TRUE)
  #} else {
  #  return(FALSE)
  #}

#}

lhc_generate_lhc_sample2 <- function(FILEPATH, PARAMETERS, NUMSAMPLES, PMIN=PMIN,
                                    PMAX=PMAX, ALGORITHM)
{
  allArgs <- as.list(match.call())
  print(method_argument_check(allArgs))
}
