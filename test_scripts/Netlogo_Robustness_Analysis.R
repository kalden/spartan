# Import the package
library(spartan)
# Folder containing the Netlogo Behaviour Space table, AND where the
# processed results will be written to
FILEPATH<-"/home/kja505/Documents/Spartan3_Test_Data/Netlogo/OAT/Analysis/"
# Name of the Netlogo Behaviour Space Table file
NETLOGO_BEHAVIOURSPACEFILE<-"virus_oat.csv"
# Array of the parameters to be analysed, but ONLY those perturbed
PARAMETERS<-c("infectiousness","chance-recover","duration")
# Value assigned to each parameter at calibration (the baseline value)
BASELINE<-c(60,50,20)
# The maximum value for each parameter
PMAX<-c(90,90,40)
# The minimum value explored for each parameter
PMIN<-c(10,10,5)
# Amount the parameter value was incremened during sampling
PINC<-c(10,10,5)
# Timestep of interest. The behaviour space table is likely to contain
# all timesteps - this narrows the analysis
TIMESTEP<-5200
# The simulation output measures being examined. Should be specified
# as they are in the Netlogo file
MEASURES<-c("death-thru-sickness","death-but-immune","death-old-age",
            "death-old-and-sick")
# For each parameter value being analysed, a file is created
# containing the median of each output measure, of each simulation run
# for that value. This sets the name of this file.
RESULTFILENAME<-"ParamValResponses.csv"
# The results of the A-Test comparisons of each parameter value
# against that of the parameters baseline value are output as a file.
# This sets the name of this file.
ATESTRESULTSFILENAME<-"VirusOAT_ATests.csv"
# A-Test result value either side of 0.5 at which the difference
# between two sets of results is significant
ATESTSIGLEVEL<-0.21
# What each measure represents. Used in graphing results
MEASURE_SCALE<-c("Number of People","Number of People",
                 "Number of People","Number of People")
# Not used in this case, but when a simulation is analysed at multiple
# timepoints (see tutorials 1-4)
TIMEPOINTS<-NULL; TIMEPOINTSCALE<-NULL

oat_process_netlogo_result(FILEPATH,NETLOGO_BEHAVIOURSPACEFILE, PARAMETERS,
                           BASELINE, PMIN, PMAX, PINC, MEASURES, RESULTFILENAME,
                           ATESTRESULTSFILENAME, TIMESTEP)

# Note that PARAMVALS is set to NULL - we don't use that for Netlogo
oat_graphATestsForSampleSize(FILEPATH, PARAMETERS, MEASURES, ATESTSIGLEVEL,
                             ATESTRESULTSFILENAME, BASELINE, PMIN, PMAX, PINC, NULL,
                             TIMEPOINTS, TIMEPOINTSCALE)
