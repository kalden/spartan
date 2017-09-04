# Import the package
library(spartan)
# The folder where the parameter samples should be output to
FILEPATH <- "/home/kja505/Documents/Spartan3_Test_Data/eFAST/Sampling"
# Number of resample curves (phase shifts) to use in the sampling procedure
NUMCURVES <- 3
# Names of the parameters to generate parameter value samples for.
PARAMETERS <- c("BindProbability", "ChemoThreshold", "ChemoUpperLinearAdjust",
                "ChemoLowerLinearAdjust", "VCAMProbabilityThreshold", "VCAMSlope", "Dummy")
# The number of parameter sample sets to create for each curve
NUMSAMPLES <- 65
PMIN <- c(0, 0.10, 0.10, 0.015, 0.1, 0.25, 1)
# The maximum value in the range for each parameter
PMAX <- c(100, 0.9, 0.50, 0.08, 1.0, 5.0, 10)
efast_generate_sample(FILEPATH, NUMCURVES, NUMSAMPLES, PARAMETERS, PMIN, PMAX)
