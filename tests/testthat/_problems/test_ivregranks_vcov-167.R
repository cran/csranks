# Extracted from test_ivregranks_vcov.R:167

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "csranks", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
load(test_path("testdata", "ivregranks_cov_sigmahat_covariates_FALSE.rda"))
res <- ivregranks(r(Y) ~ r(X) | r(Z))
sigma2hat_ivregranks <- vcov(res)[2, 2] * n
