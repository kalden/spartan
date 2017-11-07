read_created_xml_file <- function()
{

  # Now using xml2
  #doc <- read_xml(paste(getwd(),"/1/lhc_analysis_set1.xml", sep=""))
  #all_metrics <- xml_text(xml_find_all(doc, ".//metric"))

  library(XML)

  doc = xmlTreeParse(paste(getwd(),"/1/lhc_analysis_set1.xml", sep=""), useInternalNodes = TRUE)
  els = getNodeSet(doc, "/experiments//metric")
  all_metrics <- sapply(els, xmlValue)

  xml_param_vals <- NULL
  els = getNodeSet(doc, "/experiments//enumeratedValueSet[@variable='infectiousness']//value[@value.infectiousness]")
  sapply(els, function(el) xmlGetAttr(el, "value.infectiousnesss"))
  xml_param_vals <- c(xml_param_vals, xmlAttrs(els[[1]])[[1]])
  els = getNodeSet(doc, "/experiments//enumeratedValueSet[@variable='chance-recover']//value[@value.chance-recover]")
  xml_param_vals <- c(xml_param_vals, xmlAttrs(els[[1]])[[1]])
  els = getNodeSet(doc, "/experiments//enumeratedValueSet[@variable='duration']//value[@value.duration]")
  xml_param_vals <- c(xml_param_vals, xmlAttrs(els[[1]])[[1]])

  # People is a static value, recover this
  els = getNodeSet(doc, "/experiments//enumeratedValueSet[@variable='people']//value[@value]")
  people <- as.numeric(xmlAttrs(els[[1]]))

  # Read in the CSV sample file so we can check the values are correct
  sample <- read.csv(paste(getwd(),"/LHC_Parameters_for_Runs.csv",sep=""),header=T)

  # Get the set up and go values
  els <- getNodeSet(doc, "/experiments//setup")
  setupVal <- xmlValue(els[[1]])
  els <- getNodeSet(doc, "/experiments//go")
  goVal <- xmlValue(els[[1]])

  return(list("measures"=all_metrics,"xml_param_vals"=as.numeric(xml_param_vals),"sampled_vals"=as.numeric(sample[1,]),"setup"=setupVal,"go"=goVal,"people"=people))
}
