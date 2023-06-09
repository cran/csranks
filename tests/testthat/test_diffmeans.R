# Edge case
x <- rep(1, 10)
Sigma <- diag(rep(0.5, 10))

test_that("csdiffmeans returns identical CI for identical input",{
  out <- csdiffmeans(x, Sigma)
  expected_L <- matrix(out$L[1,2], nrow = nrow(out$L), ncol = ncol(out$L))
  diag(expected_L) <- NA
  expected_U <- matrix(out$U[1,2], nrow = nrow(out$U), ncol = ncol(out$U))
  diag(expected_U) <- NA
  
  expect_equal(out$L, expected_L)
  expect_equal(out$U, expected_U)
})


### initialize_I0 ###
p <- 5
indices <- c(2,4)
expected_partial_I0 <- matrix(c(FALSE, FALSE, FALSE, FALSE, FALSE,
                                TRUE, FALSE, TRUE, TRUE, TRUE,
                                FALSE, FALSE, FALSE, FALSE, FALSE,
                                TRUE, TRUE, TRUE, FALSE, TRUE,
                                FALSE, FALSE, FALSE, FALSE, FALSE),
                              byrow = TRUE, nrow = 5)

expected_symmetric_I0 <- matrix(c(FALSE, TRUE, FALSE, TRUE, FALSE,
                                  TRUE, FALSE, TRUE, TRUE, TRUE,
                                  FALSE, TRUE, FALSE, TRUE, FALSE,
                                  TRUE, TRUE, TRUE, FALSE, TRUE,
                                  FALSE, TRUE, FALSE, TRUE, FALSE),
                                byrow = TRUE, nrow = 5)

test_that("Initialize_I0 works for patrial indices", {
  expect_equal(initialize_I0(p, indices, TRUE, "upper"),
               list(I0 = expected_partial_I0,
                    cstype = "upper"))
  expect_equal(initialize_I0(p, indices, FALSE, "symmetric"),
               list(I0 = expected_partial_I0,
                    cstype = "symmetric"))
  expect_equal(initialize_I0(p, indices, TRUE, "symmetric"),
               list(I0 = expected_symmetric_I0,
                    cstype = "lower"))
})

### reduce_I ###

I <- matrix(c(FALSE, FALSE, FALSE, FALSE, 
              FALSE, FALSE, TRUE, TRUE, 
              FALSE, TRUE, FALSE, FALSE, 
              FALSE, FALSE, TRUE, FALSE
              ), byrow = TRUE, nrow=4)

expected_needed_variables <- c(FALSE, TRUE, TRUE, TRUE)
expected_requested_differences <- matrix(c(2,1,
                                           1,2,
                                           3,2,
                                           1,3),
                                         byrow = TRUE, ncol=2)
expected_reduced_I <- list(needed_variables = expected_needed_variables,
                           requested_differences = expected_requested_differences)

test_that("reduce_I works",{
  actual_reduced_I <- reduce_I(I)
  expect_equal(actual_reduced_I, expected_reduced_I)
})

### calculate_scaled_diffs ###
Z <- matrix(c(1:3, seq(1,10,4)), byrow=TRUE, nrow=2)
scales <- c(1,1,2,3)
expected_Z_diff <- matrix(c((Z[,2] - Z[,1]) / scales[1],
                            (Z[,1] - Z[,2]) / scales[2],
                            (Z[,3] - Z[,2]) / scales[3],
                            (Z[,1] - Z[,3]) / scales[4]),
                          nrow = 2)
test_that("calculate_scaled_diffs works", {
  actual_Z_diff <- calculate_scaled_differences_in_samples(
    Z, expected_requested_differences,scales)
  expect_equal(actual_Z_diff, expected_Z_diff)
})

# Edge case: 1 bootstrap sample
Z <- matrix(1:3, byrow=TRUE, nrow=1)
scales <- c(1,1,2,3)
expected_Z_diff <- matrix(c((Z[,2] - Z[,1]) / scales[1],
                            (Z[,1] - Z[,2]) / scales[2],
                            (Z[,3] - Z[,2]) / scales[3],
                            (Z[,1] - Z[,3]) / scales[4]),
                          nrow = 1)
test_that("calculate_scaled_diffs works with 1 bootstrap sample", {
  actual_Z_diff <- calculate_scaled_differences_in_samples(
    Z, expected_requested_differences,scales)
  expect_equal(actual_Z_diff, expected_Z_diff)
})

# Compare against master version 0.2.2
test_that("calculate_scaled_diffs works as master 0.2.2", {
  variance <- seq(0.1, 0.5, by=0.1)
  sigmadiff <- sqrt(outer(variance, variance, '+'))
  I <- expected_partial_I0
  Z <- matrix(1:5, byrow=TRUE, nrow=1)
  expected_scaled_diff <- outer(as.vector(Z), as.vector(Z), '-')[I]/sigmadiff[I]
  
  reduced_I <- reduce_I(I)
  requested_differences <- reduced_I$requested_differences
  Zdiff_scaled <- calculate_scaled_differences_in_samples(Z, requested_differences,
                                                          sigmadiff[I])
  expect_equal(Zdiff_scaled[1,], expected_scaled_diff)
  expect_equal(apply(Zdiff_scaled, 1, max),
               max(expected_scaled_diff))
})