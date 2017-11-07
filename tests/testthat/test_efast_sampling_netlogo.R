library(spartan)
context("Test of Spartan Netlogo eFAST Sampling")

test_that("efast_generate_sample_netlogo", {

  # Generate the samples, then we'll check the content
  # eFAST sampling has been tested independently, within that method

  efast_generate_sample_netlogo(
    FILEPATH=getwd(), 1, 65,
    c("death-thru-sickness","death-but-immune","death-old-age","death-old-and-sick"),
    c("people","infectiousness","chance-recover","duration", "dummy"),
    c(150,"[10,90]","[10,90]","[5,40]","[1,10]"),
    1,"true","setup","go")

   # Test the sample files have been generated
   expect_true(file.exists(paste(getwd(),"/Curve1_infectiousness.csv",sep="")))
   expect_true(file.exists(paste(getwd(),"/Curve1_duration.csv",sep="")))
   expect_true(file.exists(paste(getwd(),"/Curve1_chance-recover.csv",sep="")))
   expect_true(file.exists(paste(getwd(),"/Curve1_dummy.csv",sep="")))

   # Now we can check the content of a random file
   xml_doc <- read_created_efast_xml_file()

   # Now compare the results
   expect_equal(xml_doc$xml_param_vals,xml_doc$sampled_vals)
   expect_equal(xml_doc$measures,c("death-thru-sickness","death-but-immune","death-old-age", "death-old-and-sick"))
   expect_equal(xml_doc$setup,"setup")
   expect_equal(xml_doc$go,"go")
   expect_equal(xml_doc$people,150)


   # Remove the created files
   file.remove(paste(getwd(),"/Curve1_infectiousness.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve2_infectiousness.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve3_infectiousness.csv",sep=""))
   file.remove(paste(getwd(),"/Curve1_duration.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve2_duration.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve3_duration.csv",sep=""))
   file.remove(paste(getwd(),"/Curve1_dummy.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve2_dummy.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve3_dummy.csv",sep=""))
   file.remove(paste(getwd(),"/Curve1_chance-recover.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve2_chance-recover.csv",sep=""))
   #file.remove(paste(getwd(),"/Curve3_chance-recover.csv",sep=""))
   unlink(paste(getwd(),"/1/",sep=""), recursive = TRUE)
   #unlink(paste(getwd(),"/2/",sep=""), recursive = TRUE)
   #unlink(paste(getwd(),"/3/",sep=""), recursive = TRUE)

})
