make_test_sim_result_file <- function(filePath, variation)
{
  if(variation==1)
  {
    # Make a test file
    result <- cbind(6.8,65.0885278894)
    result <- rbind(result,cbind(2.0744955518,51.5400494906))
    result <- rbind(result,cbind(1.6685778396,36.655424325))
    result <- rbind(result,cbind(2.129119649,24.0564354369))
  }
  else
  {
    result <- cbind(2.8976989449,91.4103843638)
    result <- rbind(result,cbind(2.4649829961,27.1394617466))
    result <- rbind(result,cbind(1.6962630223,30.048049107))
    result <- rbind(result,cbind(1.8487578332,56.3204259164))
  }
  colnames(result) <- c("Velocity","Displacement")
  write.csv(result,file=filePath,quote=F,row.names=F)
}

setup_multiple_parameter_result_analysis <- function()
{
  # Some setup here
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.05"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.05","1"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.1"))
  dir.create(file.path(getwd(),"chemoLowerLinearAdjust","0.1","1"))
  make_test_sim_result_file(paste(getwd(),"/chemoLowerLinearAdjust/0.05/1/Test_Robustness_Result.csv",sep=""),1)
  make_test_sim_result_file(paste(getwd(),"/chemoLowerLinearAdjust/0.1/1/Test_Robustness_Result.csv",sep=""),2)
  dir.create(file.path(getwd(),"chemoUpperLinearAdjust"))
  dir.create(file.path(getwd(),"chemoUpperLinearAdjust","1"))
  dir.create(file.path(getwd(),"chemoUpperLinearAdjust","1","1"))
  dir.create(file.path(getwd(),"chemoUpperLinearAdjust","2"))
  dir.create(file.path(getwd(),"chemoUpperLinearAdjust","2","1"))
  make_test_sim_result_file(paste(getwd(),"/chemoUpperLinearAdjust/1/1/Test_Robustness_Result.csv",sep=""),2)
  make_test_sim_result_file(paste(getwd(),"/chemoUpperLinearAdjust/2/1/Test_Robustness_Result.csv",sep=""),1)
}


cleanup <- function()
{
  unlink(paste(getwd(),"/chemoLowerLinearAdjust/",sep=""), recursive = TRUE)
  unlink(paste(getwd(),"/chemoUpperLinearAdjust/",sep=""), recursive = TRUE)
}
