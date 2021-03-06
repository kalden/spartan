% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_argument_check.R
\name{check_lhs_algorithm}
\alias{check_lhs_algorithm}
\title{Check that the chosen lhc sampling algorithm is either normal or optimal.}
\usage{
check_lhs_algorithm(arguments, argument_name)
}
\arguments{
\item{arguments}{List of the arguments provided to the called function}

\item{argument_name}{Argument name being checked. May be null, but here for consistency}
}
\value{
Boolean stating the current status of the pre-execution checks, or FALSE if this check fails
}
\description{
Check that the chosen lhc sampling algorithm is either normal or optimal.
}
