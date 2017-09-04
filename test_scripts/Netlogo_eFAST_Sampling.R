library(spartan)
# The directory where the samples should be stored
FILEPATH<-"/home/kja505/Documents/Spartan3_Test_Data/Netlogo/eFAST/Sampling/"
# Parameters for this simulation. List ALL the parameters that Netlogo needs to know, even those not being perturbed. Make # sure you include the dummy
PARAMETERS<-c("people","infectiousness","chance-recover","duration", "dummy")
# Now values for each of the parameters above. For parameters not being analysed, simply list the value
# For parameters being analysed, put the min and max values of the parameter in square brackets, in double quotes, e.g.
# "[0.1,0.5]" Encapsulate strings, i.e.: "\"/home/user/Experiment/\""
PARAMVALS<-c(150,"[10,90]","[10,90]","[5,40]","[1,10]")
# Number of resampling curves to use
NUMCURVES<-3
# Number of value samples to take from each parameter curve
NUMSAMPLES<-65
# The name of the function in Netlogo that sets up the simulation (Usually setup)
NETLOGO_SETUP_FUNCTION<-"setup"
# Name of the function in Netlogo that starts the simulation (Usually go)
NETLOGO_RUN_FUNCTION<-"go"
MEASURES<-c("death-thru-sickness","death-but-immune","death-old-age","death-old-and-sick")
# Number of times the Netlogo run should be repeated for each parameter set
EXPERIMENT_REPETITIONS<-1
# Whether Netlogo should collect metrics at all timesteps
RUNMETRICS_EVERYSTEP<-"true"

efast_generate_sample_netlogo(FILEPATH,NUMCURVES,NUMSAMPLES,MEASURES,PARAMETERS,PARAMVALS,EXPERIMENT_REPETITIONS,RUNMETRICS_EVERYSTEP,NETLOGO_SETUP_FUNCTION,NETLOGO_RUN_FUNCTION)
