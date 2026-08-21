# Extracted from test_ivregranks_vcov.R:3

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "csranks", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
model <- ivregranks(r(mpg) ~ r(hp) + cyl | r(disp) + cyl, data = mtcars)
expect_silent(summary(model))
