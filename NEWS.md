---
title: '# spartan 3.0.2'
author: "Kieran Alden"
date: "12 November 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# spartan 3.0.2
Changes introduced since spartan 3.0.1
* Plots translated to use ggplot, removing requirement for gplots package
* Additional functions added to ease integration with forthcoming additional packages and roboSpartan platforms
* Additional tests and fixes to descriptions in the three vignettes

# spartan 3.0.1

Changes introduced since spartan 2:
* Use of machine learning techniques to emulate a previously developed simulation (Technique 6)
* Creation of an ensemble that captures multiple emulators (Technique 7)
* Use of an ensemble/emulator to perform sensitivity analyses using spartan (Technique 8)
* Use of an ensemble/emulator in performing approximate bayesian computation (ABC, Technique 9)
* Use of an emulator and evolutionary algorithm to find parameter sets that generate a specified set of behaviours
* More detailed vignettes for all techniques
* Version 3.0.1 added improved test coverage, integration with Travis-CI, and links with Github
