## Test environments
* local Ubuntu 18.04, R 3.5.1
* local Ubuntu 18.10 R development version (2018-11-12)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs

There was one NOTE:
Possibly mis-spelled words in DESCRIPTION:
  ApplicatioN (3:48)
  Netlogo (11:1267, 11:1325)
  PLoS (11:188)
  eFAST (11:1053)
  epistemic (11:889)
  hypercube (11:743)
  stochasticity (11:481)
  
We are happy with the spelling of each word

In addition, on the last upload we were asked by Swetlana Herbrandt to add more examples, but we explained that this is a huge problem given the data spartan processes. Given we have three detailed vignettes, an R journal paper, and GB's of data available for use in those vignettes and tutorials (a subset of which took us over the maximum 5MB tarball size so had to be removed), Uwe Ligges suggested that CRAN could live with fewer examples.
