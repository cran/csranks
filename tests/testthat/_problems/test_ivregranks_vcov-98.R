# Extracted from test_ivregranks_vcov.R:98

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "csranks", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data(mtcars)
model <- model <- ivregranks(r(mpg) ~ r(hp) + 1 | r(disp) + 1, data = mtcars)
projection_matrix <- calculate_projection_residual_matrix_ivregranks(model)
