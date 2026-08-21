# Extracted from test_ivregranks_vcov.R:43

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "csranks", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
w <- cbind(mtcars$qsec, mtcars$qsec)
model <- expect_warning(ivregranks(r(mpg) ~ r(hp) + w | r(disp) + w, data = mtcars), "collinear")
cov2 <- vcov(model, component = "stage2")
