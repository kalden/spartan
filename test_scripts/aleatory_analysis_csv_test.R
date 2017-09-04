# The directory where you have extracted the example simulation results.
FILEPATH <- "/home/kja505/Documents/Spartan3_Test_Data/AA_Spartan2/CSV_Structure/"
# The sample sizes that are to be analysed, contained within an array
SAMPLESIZES <- c(1,5,50,100,300)
# The simulation output measures to be analysed, again contained within an array
MEASURES<-c("Velocity","Displacement")
# The number of subsets used. By default use 20, as performed by Read et al in
# their published technique
NUMSUBSETSPERSAMPLESIZE<-20
# The output file containing the simulation results from that simulation run. Note
# there should be no file extension
RESULTFILENAME<-"trackedCells_Close.csv"
# Not used in this case, but this is useful in cases where two result files may
# exist (for example if tracking cells close to an area, and those further away
# two output files could be used). Here, results in a second file are processed
# if the first is blank or does not exist.
ALTFILENAME<-NULL
# Use this if simulation results are in CSV format.
# The column within the csv results file where the results start. This is useful
# as it restricts what is read in to R, getting round potential errors where the
# first column contains an agent label (as R does not read in CSV files where the
# first column contains duplicates)
OUTPUTFILECOLSTART<-10
# Use this if simulation results are in CSV format.
# Last column of the output measure results
OUTPUTFILECOLEND<-11
# File either A: created by method 1 to contain the median of each output measure
# of each simulation run in that subset, or B: The name of the provided single
# CSV file containing the simulation responses
MEDIANS_SUMMARY_FILE_NAME<-"AA_SimResponses.csv"
# The results of the A-Test comparisons of the twenty subsets for each sample size
# are stored within an output file. This parameter sets the name of this file.
# Note no file extension. Current versions of spartan output to CSV files
ATESTRESULTSFILENAME<-"AA_ATest_Scores.csv"
# A summary file is created containing the maximum and median
# A-Test values for each sample size. This parameter sets the name of this file.
SUMMARYFILENAME<-"AA_ATestMaxAndMedians.csv"
# The A-Test value either side of 0.5 which should be considered a 'large difference'
# between two sets of results. Use of 0.23 was taken from the Vargha-Delaney
# publication but can be adjusted here as necessary.
LARGEDIFFINDICATOR<-0.23
# A-Test values above 0.5 (no difference) which should be considered as small,
# medium, and large differences between two result sets. Used in the graph
# summarising all sample sizes.
SMALL<-0.56
MEDIUM<-0.66
LARGE<-0.73
# Name of the graph which summarises the analysis results for all sample sizes.
# Current versions of spartan output to pdf. Note no file extension
GRAPHOUTPUTFILE<-"AA_ATestMaxes.pdf"
# Timepoints being analysed. Must be NULL if no timepoints being analysed, or else
# be an array of timepoints. Scale sets the measure of these timepoints
TIMEPOINTS<-NULL; TIMEPOINTSCALE<-NULL
# Example Timepoints:
#TIMEPOINTS<-c(12,36,48,60); TIMEPOINTSCALE<-"Hours"

a_test_results <- aa_getATestResults(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE, MEASURES, ATESTRESULTSFILENAME, LARGEDIFFINDICATOR, AA_SIM_RESULTS_FILE = MEDIANS_SUMMARY_FILE_NAME)

# By object:
sample_summary <- aa_sampleSizeSummary(FILEPATH, SAMPLESIZES, MEASURES, SUMMARYFILENAME, ATESTRESULTS_OBJECT = a_test_results)
# By File:
sample_summary <- aa_sampleSizeSummary(FILEPATH, SAMPLESIZES, MEASURES, SUMMARYFILENAME, ATESTRESULTS_FILE = ATESTRESULTSFILENAME)

# Graphs:
# By Object:
aa_graphSampleSizeSummary(FILEPATH, MEASURES, 300, SMALL, MEDIUM, LARGE, GRAPHOUTPUTFILE, SAMPLESUMMARY_OBJECT = sample_summary)

# By the CSV file that method produces:
aa_graphSampleSizeSummary(FILEPATH, MEASURES, 300, SMALL, MEDIUM, LARGE, GRAPHOUTPUTFILE, SAMPLESUMMARY_FILE = SUMMARYFILENAME)

# Testing by loading in example data:
data("tutorial_consistency_set")
a_test_results <- aa_getATestResults(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE, MEASURES, ATESTRESULTSFILENAME, LARGEDIFFINDICATOR, AA_SIM_RESULTS_OBJECT = tutorial_consistency_set)

# Test Timepoints:
a_test_results <- aa_getATestResults(FILEPATH, SAMPLESIZES, NUMSUBSETSPERSAMPLESIZE, MEASURES, ATESTRESULTSFILENAME, LARGEDIFFINDICATOR, AA_SIM_RESULTS_FILE = MEDIANS_SUMMARY_FILE_NAME,
                                     TIMEPOINTS=c(12,36,48,60), TIMEPOINTSCALE="Hours")

sample_summary <- aa_sampleSizeSummary(FILEPATH, SAMPLESIZES, MEASURES, SUMMARYFILENAME, ATESTRESULTS_FILE = ATESTRESULTSFILENAME,
                                       TIMEPOINTS=c(12,36,48,60),TIMEPOINTSCALE="Hours")

aa_graphSampleSizeSummary(FILEPATH, MEASURES, 300, SMALL, MEDIUM, LARGE, GRAPHOUTPUTFILE, SAMPLESUMMARY_FILE = SUMMARYFILENAME,
                          TIMEPOINTS=c(12,36,48,60),TIMEPOINTSCALE="Hours")
