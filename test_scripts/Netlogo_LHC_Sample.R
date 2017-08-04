library(spartan)
library(lhs)
library(XML)
# Directory where the samples should be stored
FILEPATH<-"C:/Users/kja505/Documents/NetlogoTest"
# Parameters in simulation. List all the parameters that Netlogo is
# required to know,
PARAMETERS<-c("people","infectiousness","chance-recover","duration")
# Now values for each of the parameters above
# For parameters not being analysed, simply list the value
# For parameters being analysed, put the min and max values of the
# parameter in square brackets, in double quotes, e.g. "[0.1,0.5]"
# Encapsulate strings, i.e.: "\"/home/user/Experiment/\""
PARAMVALS<-c(150,"[10,90]","[10,90]","[5,40]")
# Number of parameter samples to take from hypercube
NUMSAMPLES<-500
# Name of function that sets up Netlogo simulation. Usually setup
NETLOGO_SETUP_FUNCTION<-"setup"
# Name of function that starts Netlogo simulation. Usually go
NETLOGO_RUN_FUNCTION<-"go"
# Simulation output measures
MEASURES<-c("death-thru-sickness","death-but-immune","death-old-age",
            "death-old-and-sick")
# Number of times the Netlogo experiment is repeated for each parameter
# set
EXPERIMENT_REPETITIONS<-1
# Whether Netlogo metrics should be collected at each timestep
RUNMETRICS_EVERYSTEP<-"true"
# Algorithm to use to generate the hypercube (normal or optimal)
ALGORITHM<-"normal"

lhc_generate_lhc_sample_netlogo(FILEPATH,PARAMETERS,PARAMVALS,NUMSAMPLES,ALGORITHM,
                                EXPERIMENT_REPETITIONS,RUNMETRICS_EVERYSTEP,NETLOGO_SETUP_FUNCTION,
                                NETLOGO_RUN_FUNCTION,MEASURES)
