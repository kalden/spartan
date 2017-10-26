library(spartan)
context("Testing Spartan Utilities Class")

test_that("generate_model_formula", {

  expect_equal(generate_model_formula(c("ChemokineThreshold","AdhesionResponse"),c("Velocity","Dislacement")),as.formula("Velocity + Displacement ~ ChemokineThreshold + AdhesionResponse",env=parent.frame()))

})
