# Extracted from test_ivregranks_vcov.R:251

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "csranks", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
load(test_path("testdata", "ivregranks_cov_sigmahat_increasing_FALSE.rda"))
res <- ivregranks(r(Y, increasing = FALSE) ~ r(X, increasing = FALSE) + W |
  r(Z, increasing = FALSE) + W, omega = 1)
sigma2hat_ivregranks <- vcov(res)[2, 2] * n
