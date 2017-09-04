library(spartan)
# Folder containing the eFAST simulation results. Make sure example data is unzipped
FILEPATH <- "/home/kja505/Documents/Spartan3_Test_Data/eFAST/Analysis/"
PARAMETERS <- c("BindProbability", "ChemoThreshold", "ChemoUpperLinearAdjust",
                "ChemoLowerLinearAdjust", "VCAMProbabilityThreshold", "VCAMSlope", "Dummy")
MEASURES <- c("Velocity", "Displacement")
RESULTFILENAME <- "trackedCells_Close.csv"
ALTERNATIVEFILENAME <- NULL
OUTPUTCOLSTART <- 10
OUTPUTCOLEND <- 11
# Number of resample curves employed when the parameter space was sampled
NUMCURVES <- 3
# The number of parameter sample sets taken from each curve
NUMSAMPLES <- 65
# Number of simulation runs performed for each parameter value set
NUMRUNSPERSAMPLE <- 300
# Which of the output measures to T-Test for significance
OUTPUTMEASURES_TO_TTEST <- 1:2
# T-Test confidence level
TTEST_CONF_INT <- 0.95
# Name of the final result file for this analysis, showing the partitioning of
# the variance between input parameters
EFASTRESULTFILENAME <- "EgSet_eFAST_Analysis.csv"
# Boolean to note whether summary graphs should be produced
GRAPH_FLAG <- TRUE
TIMEPOINTS <- NULL; TIMEPOINTSCALE <- NULL

# Would run if we had all sim results
#efast_generate_medians_for_all_parameter_subsets(FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, NUMRUNSPERSAMPLE, MEASURES, RESULTFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART, OUTPUTCOLEND, TIMEPOINTS,
#TIMEPOINTSCALE)

efast_get_overall_medians(FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, MEASURES, TIMEPOINTS, TIMEPOINTSCALE)

efast_run_Analysis(FILEPATH, MEASURES, PARAMETERS, NUMCURVES, NUMSAMPLES, OUTPUTMEASURES_TO_TTEST, TTEST_CONF_INT, GRAPH_FLAG, EFASTRESULTFILENAME, TIMEPOINTS, TIMEPOINTSCALE)

# Test Timepoints:
TIMEPOINTS <- c(12,36,48,60)
TIMEPOINTSCALE<-"Hours"

efast_get_overall_medians(FILEPATH, NUMCURVES, PARAMETERS, NUMSAMPLES, MEASURES, TIMEPOINTS, TIMEPOINTSCALE)

efast_run_Analysis(FILEPATH, MEASURES, PARAMETERS, NUMCURVES, NUMSAMPLES, OUTPUTMEASURES_TO_TTEST, TTEST_CONF_INT, GRAPH_FLAG, EFASTRESULTFILENAME, TIMEPOINTS, TIMEPOINTSCALE)
