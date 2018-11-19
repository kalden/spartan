## Test environments
* local Ubuntu 18.04, R 3.5.1
* local Ubuntu 18.10 R development version (2018-11-12)
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (release)
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs

# R-Hub gives the warning:
Possibly mis-spelled words in DESCRIPTION:
  ApplicatioN (3:48)
  eFAST (11:1055)
  epistemic (11:891)
  hypercube (11:745)
  Netlogo (11:1269, 11:1327)
  PLoS (11:190)
  stochasticity (11:483)
  
We are however happy with the spelling of each word

After checks and an email from CRAN, we have examined the additional issues for the last version released on CRAN, and confirm these are addressed in this version.

For version 3.0.1, we were asked by Swetlana Herbrandt to add more examples, but we explained that this is a huge problem given the data spartan processes. Given we have three detailed vignettes, an R journal paper, and GB's of data available for use in those vignettes and tutorials (a subset of which took us over the maximum 5MB tarball size so had to be removed), Uwe Ligges suggested that CRAN could live with fewer examples.


