load_lhc_training_data <- function()
{
  parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue",
                "maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
  sampleMaxes <- cbind(100,0.9,0.5,0.08,1,5)
  sampleMins <-cbind(0,0.1,0.1,0.015,0.1,0.25)
  data("sim_data_for_emulation")

  return(list("dataset"=sim_data_for_emulation,"parameters"=parameters,"sampleMaxes"=sampleMaxes,"sampleMins"=sampleMins))
}
