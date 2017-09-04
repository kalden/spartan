library(spartan)
# Directory where the sample file should be stored
FILEPATH<-"/home/kja505/Documents/Spartan3_Test_Data/Netlogo/OAT/Sampling"
# Name to give the setup file. No need for file extension
NETLOGO_SETUPFILE_NAME<-"nl_robustness_setup"
# Parameters in simulation. List all the parameters that Netlogo is
# required to know,
PARAMETERS<-c("people","infectiousness","chance-recover","duration")
# Now values for each of the parameters above
# For parameters not being analysed, simply list the value
# For parameters being analysed, put the min, max and increment values
# of the parameter in square brackets, in double quotes, e.g.
# "[0.1,0.5,0.1]"
PARAMVALS<-c(150,"[10,90,10]","[10,90,10]","[5,40,5]")
# Name of the setup simulation function in Netlogo simulation
# Usually setup
NETLOGO_SETUP_FUNCTION<-"setup"
# Name of the function that starts the simulation. Usually go
NETLOGO_RUN_FUNCTION<-"go"
# Simulation output measures
MEASURES<-c("death-thru-sickness","death-but-immune",
            "death-old-age","death-old-and-sick")
# Number of times Netlogo should repeat the experiment for each
# parameter set
EXPERIMENT_REPETITIONS<-1
# Whether Netlogo should collect metrics at each timestep
RUNMETRICS_EVERYSTEP<-"true"

oat_generate_netlogo_behaviour_space_XML(FILEPATH, NETLOGO_SETUPFILE_NAME, PARAMETERS,
                                         PARAMVALS, NETLOGO_SETUP_FUNCTION, NETLOGO_RUN_FUNCTION,
                                         MEASURES, EXPERIMENT_REPETITIONS, RUNMETRICS_EVERYSTEP)
