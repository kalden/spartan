#' Latin-hypercube value set use to demonstrate emulated sensitivity analysis
#'
#' A dataset containing values for the six parameters that control the
#' simulation detailed in the case study section of the vignette. These
#' parameters are defined the accompanying publications referred to in
#' the vignette
#'
#' \itemize{
#'   \item stableBindProbability. Parameter values between 0 and 100
#'   \item chemokineExpressionThreshold. Parameter values between 0 and 1
#'   \item initialChemokineExpressionValue. Parameter values between 0.1 and 0.5
#'   \item maxChemokineExpressionValue. Parameter values between 0.015 and 0.08
#'   \item maxProbabilityOfAdhesion. Parameter values between 0 and 1
#'   \item adhesionFactorExpressionSlope. Parameter values between 0.25 and 5
#' }
#'
#' @docType data
#' @keywords datasets
#' @name emulated_lhc_values
#' @usage data(emulated_lhc_values)
#' @format A list with 500 rows (one per parameter set) and six columns
NULL

#' Set of parameter and response pairs for training an emulator of a simulation
#'
#' This dataset contains 500 sets of parameter values and the responses that
#' were observed under those conditions when run through the simulator. This
#' is used as a dataset to show how one could use a set of simulation results
#' to train emulators, that in turn could be combined to form an ensemble.
#'
#' \itemize{
#'   \item stableBindProbability. Parameter values between 0 and 100
#'   \item chemokineExpressionThreshold. Parameter values between 0 and 1
#'   \item initialChemokineExpressionValue. Parameter values between 0.1 and 0.5
#'   \item maxChemokineExpressionValue. Parameter values between 0.015 and 0.08
#'   \item maxProbabilityOfAdhesion. Parameter values between 0 and 1
#'   \item adhesionFactorExpressionSlope. Parameter values between 0.25 and 5
#'   \item Velocity. Simulation response measure for cell speed
#'   \item Displacement. Simulation response measure for cell displacement
#'   \item PatchArea. Simulation response measure for size of formed clusters
#' }
#'
#' @docType data
#' @keywords datasets
#' @name sim_data_for_emulation
#' @usage data(sim_data_for_emulation)
#' @format A list with 500 rows (one per parameter set) and nine columns
NULL

#' Example dataset showing the structure for consistency analysis data
#'
#' This dataset contains exemplar results from a consistency analysis, that
#' shown in the tutorial. This data can be read in and processed using the
#' functions detailed in the vignette. The important thing to obtain from this
#' object is the structure the data is expected to be in
#'
#' \itemize{
#'   \item SampleSize. Number of simulation executions in each subset
#'   \item Set. The number of the subset this result belongs to
#'   \item Velocity. The median velocity observed in simulation executions in
#'   this set
#'   \item Displacement. The median displacement observed in simulation
#'   executions in this set
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tutorial_consistency_set
#' @usage data(tutorial_consistency_set)
#' @format A list with 9060 rows and four columns
NULL
