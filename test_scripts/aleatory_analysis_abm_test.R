# Firstly, import the package
library(spartan)
# Directory where the example simulation results for this technique were extracted
# (i.e. the results downloaded from the website)
FILEPATH <- "/home/kja505/Documents/Spartan3_Test_Data/AA_Spartan2/Folder_Structured/"
# Sample sizes (number of simulation replicates in each distribution) to be analysed
SAMPLESIZES <- c(1, 5, 50, 100, 300)
# The simulation output measures to be analysed
MEASURES <- c("Velocity", "Displacement")
# Number of distributions being compared. Default: 20, as performed by Read et al
NUMSUBSETSPERSAMPLESIZE <- 20
# Output file name containing the simulation responses.
RESULTFILENAME <- "trackedCells_Close.csv"
# Not used in this case. Useful where two result files exist (e.g.\ if tracking cells
# close and those further away, two output files could be used). Here, results in a
# second file are processed if the first is blank or does not exist.
ALTFILENAME <- NULL
# Notes the column in the CSV results file where the results start.
# Useful as it restricts what is read in to R, getting round potential errors where
# the first column contains a label
OUTPUTFILECOLSTART <- 10
# Last column of the output measure results
OUTPUTFILECOLEND <- 11
# Use this if simulation results are in CSV format.
# Last column of the output measure results
OUTPUTFILECOLEND <- 11
# File either A: created by method 1 of this technique, containing the median of each
# output measure of each simulation run in that subset, or B: The name of the provided
# single CSV file containing the simulation responses. So if you are using the CSV
# structured tutorial data, this fill be the name of that CSV file.
MEDIANS_SUMMARY_FILE_NAME <- "AA_SimResponses.csv"
# The results of the A-Test comparisons of the twenty subsets for each sample size
# are stored within an output file. This parameter sets the name of this file.
# Note no file extension. Current versions of spartan output to CSV files
ATESTRESULTSFILENAME <- "AA_ATest_Scores.csv"
# A summary file is created containing the maximum and median
# A-Test values for each sample size. This parameter sets the name of this file.
SUMMARYFILENAME <- "AA_ATestMaxAndMedians.csv"
# The A-Test value either side of 0.5 which should be considered a 'large difference'
# between two sets of results. Use of 0.23 was taken from the Vargha-Delaney
# publication but can be adjusted here as necessary.
LARGEDIFFINDICATOR <- 0.23
# A-Test values above 0.5 (no difference) which should be considered as small,
# medium, and large differences between two result sets. Used in the graph
# summarising all sample sizes.
SMALL <- 0.56
MEDIUM <- 0.66
LARGE <- 0.73
# Name of the graph which summarises the analysis results for all sample sizes.
# Current versions of spartan output to pdf.
GRAPHOUTPUTFILE <- "AA_ATestMaxes.pdf"
# Timepoints being analysed. Must be NULL if no timepoints being analysed, or else
# be an array of timepoints. Scale sets the measure of these timepoints
TIMEPOINTS <- NULL; TIMEPOINTSCALE <- NULL
# Example Timepoints, if being used:
#TIMEPOINTS <- c(12, 36, 48, 60); TIMEPOINTSCALE <- "Hours"

aa_summariseReplicateRuns(FILEPATH, SAMPLESIZES, MEASURES, RESULTFILENAME, ALTFILENAME, OUTPUTFILECOLSTART, OUTPUTFILECOLEND, MEDIANS_SUMMARY_FILE_NAME, TIMEPOINTS, TIMEPOINTSCALE)

a_test_results <- aa_getATestResults(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE, MEASURES, ATESTRESULTSFILENAME, LARGEDIFFINDICATOR, AA_SIM_RESULTS_FILE = MEDIANS_SUMMARY_FILE_NAME,
                                     TIMEPOINTS, TIMEPOINTSCALE)

# By object:
sample_summary <- aa_sampleSizeSummary(FILEPATH, SAMPLESIZES, MEASURES, SUMMARYFILENAME, ATESTRESULTS_OBJECT = a_test_results, TIMEPOINTS, TIMEPOINTSCALE)
# By File:
sample_summary <- aa_sampleSizeSummary(FILEPATH, SAMPLESIZES, MEASURES, SUMMARYFILENAME, ATESTRESULTS_FILE = ATESTRESULTSFILENAME, TIMEPOINTS, TIMEPOINTSCALE)

# Graphs:
# By Object:
aa_graphSampleSizeSummary(FILEPATH, MEASURES, 300, SMALL, MEDIUM, LARGE, GRAPHOUTPUTFILE, SAMPLESUMMARY_OBJECT = sample_summary, TIMEPOINTS, TIMEPOINTSCALE)

# By the CSV file that method produces:
aa_graphSampleSizeSummary(FILEPATH, MEASURES, 300, SMALL, MEDIUM, LARGE, GRAPHOUTPUTFILE, SAMPLESUMMARY_FILE = SUMMARYFILENAME, TIMEPOINTS, TIMEPOINTSCALE)
