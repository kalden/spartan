# Import the packages
# The folder where the parameter samples should be output to
FILEPATH <- "/home/kja505/Documents/Spartan3_Test_Data/LHC/Sampling"
# Names of the parameters to generate values for.
PARAMETERS <- c("thresholdBindProbability", "chemoThreshold", "chemoUpperLinearAdjust",
                "chemoLowerLinearAdjust", "maxVCAMeffectProbabilityCutoff", "vcamSlope")
# The number of parameter sample sets to create using the hypercube
NUMSAMPLES <- 500
# The minimum value in the range for each parameter
PMIN <- c(0, 0.10, 0.10, 0.015, 0.1, 0.25)
# The maximum value in the range for each parameter
PMAX <- c(100, 0.9, 0.50, 0.08, 1.0, 5.0)
# Algorithm to use to generate the hypercube. Can be normal (quick) or optimal,
# which can take a long time (especially for high number of parameters)
ALGORITHM <- "normal"
lhc_generate_lhc_sample(FILEPATH, PARAMETERS, NUMSAMPLES, PMIN, PMAX, ALGORITHM)
