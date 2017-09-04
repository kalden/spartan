library(spartan)
# Folder containing the example simulation results. Make sure the folder is unzipped
FILEPATH <- "/home/kja505/Documents/Spartan3_Test_Data/OAT_Spartan2/Folder_Structured/"
# Array of the parameters to be analysed.
# Note only two of the six here for download size reasons
PARAMETERS <- c("chemoLowerLinearAdjust", "chemoUpperLinearAdjust")
# Similar to the sampling function discussed above, there are two ways to specify
# parameter value information in the analysis. Ensure you are using the appropriate
# method, setting these to NULL if using the alternative (see comments in sampling
# function description).
# Method 1:
PMIN <- c(0.015, 0.10)
PMAX <- c(0.08, 0.50)
PINC <- c(0.005, 0.05)
PARAMVALS<-NULL
# Method 2:
#PARAMVALS <- c("0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.045, 0.05, 0.055, 0.06,
# 0.065, 0.07,0.075, 0.08", "0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5")
#PMIN <- NULL; PMAX <- NULL; PINC <- NULL
BASELINE <- c(0.04, 0.2)
MEASURES <- c("Velocity", "Displacement")
# What each measure represents. Used in graphing results
MEASURE_SCALE <- c("microns/min", "microns")
RESULTFILENAME <- "trackedCells_Close.csv"
OUTPUTCOLSTART <- 10
OUTPUTCOLEND <- 11
ALTERNATIVEFILENAME <- NULL
# Either 1: The name of the CSV file containing all simulation output (see description
# that follows in this section) or name to give the summary file that spartan generates
CSV_FILE_NAME <- "OAT_Medians.csv"
# Number of replicate runs performed for each parameter value set
NUMRUNSPERSAMPLE <- 300
# The results of the A-Test comparisons of each parameter value against that of the
# parameters baseline value are output as a file. This sets the name of this file.
# Current versions of spartan output this to a CSV file
ATESTRESULTSFILENAME <- "EgSet_ATests.csv"
# A-Test result value either side of 0.5 at which the difference between two sets of
# results is significant
ATESTSIGLEVEL <- 0.23
# Timepoints being analysed. Must be NULL if no timepoints being analysed, or else
# be an array of timepoints. Scale sets the measure of these timepoints
TIMEPOINTS <- NULL; TIMEPOINTSCALE <- NULL
# Example Timepoints, if being used:
#TIMEPOINTS <- c(12, 36, 48, 60); TIMEPOINTSCALE <- "Hours"

oat_processParamSubsets(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE, MEASURES,RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART, OUTPUTCOLEND,CSV_FILE_NAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS,TIMEPOINTS, TIMEPOINTSCALE)

oat_csv_result_file_analysis(FILEPATH, CSV_FILE_NAME, PARAMETERS, BASELINE, MEASURES, ATESTRESULTSFILENAME, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS, TIMEPOINTSCALE)

oat_graphATestsForSampleSize(FILEPATH, PARAMETERS, MEASURES, ATESTSIGLEVEL, ATESTRESULTSFILENAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS, TIMEPOINTSCALE)

oat_plotResultDistribution(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CSV_FILE_NAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS, TIMEPOINTSCALE)

# Param Values test:
# Method 2:
PARAMVALS <- c("0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.045, 0.05, 0.055, 0.06,
 0.065, 0.07,0.075, 0.08", "0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5")
PMIN <- NULL; PMAX <- NULL; PINC <- NULL

oat_processParamSubsets(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE, MEASURES,RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART, OUTPUTCOLEND,CSV_FILE_NAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS,TIMEPOINTS, TIMEPOINTSCALE)

oat_csv_result_file_analysis(FILEPATH, CSV_FILE_NAME, PARAMETERS, BASELINE, MEASURES, ATESTRESULTSFILENAME, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS, TIMEPOINTSCALE)

oat_graphATestsForSampleSize(FILEPATH, PARAMETERS, MEASURES, ATESTSIGLEVEL, ATESTRESULTSFILENAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS, TIMEPOINTSCALE)

oat_plotResultDistribution(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CSV_FILE_NAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS, TIMEPOINTSCALE)

# Test timepoints:
oat_processParamSubsets(FILEPATH, PARAMETERS, NUMRUNSPERSAMPLE, MEASURES,RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART, OUTPUTCOLEND,CSV_FILE_NAME,
                        BASELINE, PMIN, PMAX, PINC, PARAMVALS, TIMEPOINTS=c(12,36,48,60), TIMEPOINTSCALE="Hours")
oat_csv_result_file_analysis(FILEPATH, CSV_FILE_NAME, PARAMETERS, BASELINE, MEASURES, ATESTRESULTSFILENAME, PMIN, PMAX, PINC, PARAMVALS,
                             TIMEPOINTS=c(12,36,48,60), TIMEPOINTSCALE="Hours")
oat_graphATestsForSampleSize(FILEPATH, PARAMETERS, MEASURES, ATESTSIGLEVEL, ATESTRESULTSFILENAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS,
                             TIMEPOINTS=c(12,36,48,60), TIMEPOINTSCALE="Hours")
oat_plotResultDistribution(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CSV_FILE_NAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS,
                           TIMEPOINTS=c(12,36,48,60), TIMEPOINTSCALE="Hours")
