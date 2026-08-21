#' @describeIn ivregranks Calculate Variance-Covariance Matrix for a Fitted
#' \code{ivregranks} object
#'
#' Returns the variance-covariance matrix of the regression coefficients
#' (main parameters) of a fitted \code{ivregranks} object. Its result is
#' theoretically valid and asymptotically consistent, in contrast to naively
#' running \code{vcov(ivreg(...))}.
#'
#' ---------------------------------------------------------------------------
#' CHANGE NOTE:
#' ---------------------------------------------------------------------------
#' Eq. (8) of "Inference for Rank-Rank Regressions with Instrumental
#' Variables" defines (theta_l, eta_l) via a projection of W_l. The previous
#' implementation projected W_l onto the fitted values of a first stage that
#' excludes W_l (i.e. R_X ~ R_Z + W_{-l}). However, the 2SLS needs
#' the full first stage R_i^{X,fit} = pi_hat * R_i^Z + W_i'lambda_hat (i.e.
#' the same first stage used everywhere else). 
#' 
#' Functions removed (replaced by calculate_beta_projection_matrix() below,
#' which computes the projection on R^{X,fit} directly instead of via a
#' dropped-regressor first stage):
#'   - update_coefficients_when_dropping_regressors()
#'   - calculate_projection_residual_matrix_stage_2()
#'   - substitute_coefs_change_base_to_stage_1()
#'   - calculate_residuals_of_endogenous_in_exogenous_terms()
#' All other functions in this file (get_projection_residual_matrix_ivregranks,
#' get_projection_variances, get_instrument_index_after_dropping_NAs,
#' get_regressor_index_after_dropping_NAs, postprocess_sigmahat, and the three
#' calculate_H*.ivregranks methods) are unchanged from the original file.
#' ---------------------------------------------------------------------------
#'
#' @param complete logical indicating if the full variance-covariance matrix
#' should be returned also in case of an over-determined system where
#' some coefficients are undefined and \code{coef(.)} contains NAs
#' correspondingly. When \code{complete = TRUE}, \code{vcov()} is compatible
#' with \code{coef()} also in this singular case.
#' @importFrom stats vcov
#' @export
vcov.ivregranks <- function(object, component = c("stage2", "stage1"),
                            complete = TRUE, ...) {
  component <- match.arg(component, c("stage2", "stage1"))
  if (component == "stage1") {
    return(vcov(object$object_fs, complete = complete, ...))
  }

  regressor_dropped_fs <- is.na(coef(object, component = "stage1"))

  # Per eqn 8 from doc 'Inference for Rank-Rank Regressions with Instrumental Variables'
  # We're interested in projecting exogenous variable W_l on R(X) and other W's
  # However, on R(X) we project using 2SLS estimation.
  # We want to do that for all W's

  # This one has W_l ~ Z + W_-l as well as Z ~ W
  projection_residual_matrix_stage_1 <- get_projection_residual_matrix_ivregranks(object)

  # [CHANGED] The two old calls:
  #   X_coefs_without_W_l <- update_coefficients_when_dropping_regressors(...)
  #   projection_residual_matrix_stage_2 <- calculate_projection_residual_matrix_stage_2(...)
  # are replaced by a single call that projects each W_l directly onto the
  # full first-stage fitted value R^{X,fit}.
  Z <- stats::model.matrix(object, component = "instruments")[, !regressor_dropped_fs, drop = FALSE]
  stage_1_coefs <- coef(object, component = "stage1")[!regressor_dropped_fs]
  instrument_index <- get_instrument_index_after_dropping_NAs(object)

  projection_residual_matrix_stage_2_in_terms_stage_1 <- calculate_beta_projection_matrix(
    Z, stage_1_coefs, instrument_index, projection_residual_matrix_stage_1
  )

  projection_residuals_fs <- Z %*% projection_residual_matrix_stage_2_in_terms_stage_1

  H1 <- calculate_H1(object, projection_residuals_fs)
  H1_mean <- colMeans(H1)
  H2 <- calculate_H2(object, projection_residuals_fs, H1_mean)
  H3 <- calculate_H3(object, projection_residual_matrix_stage_2_in_terms_stage_1, H1_mean)

  # [CHANGED] zeta_hat = R_X - W'gamma_hat is obtained directly as
  # pi_hat * xi_hat + nu_hat (nu_hat = R_X - R^{X,fit}), since nu_hat is
  # orthogonal to W by construction of the first stage. This replaces the
  # detour through calculate_residuals_of_endogenous_in_exogenous_terms(),
  # which used the (now removed) dropped-regressor first stage.
  X_on_W <- calculate_zeta_hat(object, Z, stage_1_coefs, instrument_index)
  projection_variances <- get_projection_variances(object, X_on_W, projection_residuals_fs)

  psi <- t(t(H1 + H2 + H3) / projection_variances)

  raw_sigmahat <- (t(psi) %*% psi) / (nrow(psi)^2)

  sigmahat <- postprocess_sigmahat(object, raw_sigmahat, complete)

  sigmahat
}

#' @title Get projection residual matrix for ivregranks
#' @description In calculation of covariance matrix of parameters in ivregranks,
#' we're interested in projections of exogenous variables (W) onto other exogenous variables, as well as on intrument variable Z.
#' We really don't want to calculate the coefficients and residuals for these projections from scratch,
#' because that involves fitting p linear models (p - number of exogenous variables).
#' So we use the QR decomposition already calculated for purpose of first stage of main SLSS regression.
#' Turns out, the inverse of R^TR is closely related to coefficients of projection models.
#'
#' Important: this function returns a matrix with rows&columns corresponding only to
#' variables that were NOT colinear (their coefficients in first stage weren't NA).
#'
#' @return Suppose we have matrix U (exogenous variables + instrument, in column order as in main regression).
#' Retuned is matrix P s.t. U %*% P gives residuals of projections of exogenous variables onto other exogenous variables as well as on instrument variable.
#' First column corresponds to projection of first variable  in U etc.
#'
#' @seealso get_projection_residual_matrix - a counterpart for lmranks.
#' @noRd
#' [UNCHANGED from original file]
get_projection_residual_matrix_ivregranks <- function(object) {
  stage_1_coefficients <- coef(object, component = "stage1")
  regressor_dropped <- is.na(stage_1_coefficients)
  if (any(regressor_dropped)) {
    Z <- stats::model.matrix(object, component = "instruments")[, !regressor_dropped]
    R <- qr.R(qr(Z))
  } else if (is.null(object[["qr1"]])) {
    R <- qr.R(qr(stats::model.matrix(object, component = "instruments")))
  } else {
    R <- qr.R(object[["qr1"]])
  }

  calculate_projection_residual_matrix(R, regressor_dropped[!regressor_dropped], sum(!regressor_dropped))
}

#' Project every W_l onto the full first-stage fitted value and W_{-l}
#'
#' [NEW - replaces update_coefficients_when_dropping_regressors(),
#' calculate_projection_residual_matrix_stage_2(), and
#' substitute_coefs_change_base_to_stage_1()]
#'
#' For every covariate W_l we need the residual M_l of regressing W_l on
#' (R^X,fit, W_-l), where R^X,fit = pi_hat * R_Z + W'lambda_hat is the fitted
#' value of the FULL first stage (the same one used everywhere else in the
#' model, i.e. built from ALL of stage_1_coefs, including the coefficient on
#' W_l itself). This M_l is exactly the object needed for beta_hat_l's
#' variance (see the change note at the top of this file for why the first
#' stage must include W_l).
#'
#' We need M_l in two forms: as actual residual values (a column of numbers,
#' used in H1 and H2), and as a coefficient vector expressed in terms of the
#' original instrument columns (R_Z, W) -- because H3 later needs to swap in
#' the Hajek-projection term at the "R_Z slot" of that coefficient vector.
#' This function returns the coefficient-vector form (one column per W_l);
#' multiplying by Z afterwards gives the residual values.
#'
#' OUTPUT
#' -------
#' A (q x q) coefficient matrix, expressed in the Z basis (R_Z, W), such
#' that Z %*% result gives, in each column, either xi_hat (column
#' instrument_index) or M_l for the corresponding W_l (all other columns).
#'
#' @noRd
calculate_beta_projection_matrix <- function(Z, stage_1_coefs, instrument_index,
                                             projection_residual_matrix_stage_1) {
  q <- ncol(Z)
  RX_fitted <- drop(Z %*% stage_1_coefs)

  A <- Z
  A[, instrument_index] <- RX_fitted # This could be model.matrix(object, "projected")
  R_A <- qr.R(qr(A))
  P_A <- calculate_projection_residual_matrix(R_A, rep(FALSE, q), q)

  T_mat <- diag(q)
  T_mat[, instrument_index] <- stage_1_coefs

  coefs_in_Z_basis <- T_mat %*% P_A

  # The instrument_index column is not something we need from P_A (we already
  # have it, unaffected by the bug, from the original Z-self-projection).
  coefs_in_Z_basis[, instrument_index] <- projection_residual_matrix_stage_1[, instrument_index]

  coefs_in_Z_basis
}

#' Compute zeta_hat = R_X - W'gamma_hat for the rho-component's variance
#'
#' [NEW - replaces calculate_residuals_of_endogenous_in_exogenous_terms()]
#'
#' zeta_hat is obtained as pi_hat * xi_hat + nu_hat, where
#' nu_hat = R_X - R^{X,fit} is the first-stage residual (orthogonal to W by
#' construction) and xi_hat is the (already computed, unaffected-by-the-bug)
#' residual of R_Z on W.
#'
#' @noRd
calculate_zeta_hat <- function(object, Z, stage_1_coefs, instrument_index) {
  regressor_dropped_ss <- is.na(coef(object, component = "stage2"))
  RX <- stats::model.matrix(object, component = "regressors")[, object[["endogenous"]], drop = FALSE]
  RX <- RX[, !regressor_dropped_ss[object[["endogenous"]]], drop = FALSE]

  pi_hat <- stage_1_coefs[instrument_index]
  RX_fitted <- drop(Z %*% stage_1_coefs)
  nu_hat <- drop(RX) - RX_fitted

  xi_hat <- Z %*% get_projection_residual_matrix_ivregranks_column(Z, stage_1_coefs, instrument_index)
  pi_hat * xi_hat + nu_hat
}

#' Small helper returning xi_hat = R_Z - W'delta_hat as a plain vector.
#' [NEW - trivial convenience wrapper, avoids recomputing the QR twice.]
#' @noRd
get_projection_residual_matrix_ivregranks_column <- function(Z, stage_1_coefs, instrument_index) {
  R <- qr.R(qr(Z))
  q <- ncol(Z)
  P <- calculate_projection_residual_matrix(R, rep(FALSE, q), q)
  P[, instrument_index]
}

## ---------------------------------------------------------------------------
## The following functions from the original file are REMOVED, because the
## projection they computed (first stage of R_X excluding W_l, i.e.
## R_X ~ R_Z + W_{-l}) does not correspond to the FWL residual of the 2SLS
## estimator beta_hat_l. See the change note at the top of vcov.ivregranks().
##
##   update_coefficients_when_dropping_regressors()
##   calculate_projection_residual_matrix_stage_2()
##   substitute_coefs_change_base_to_stage_1()
##   calculate_residuals_of_endogenous_in_exogenous_terms()
## ---------------------------------------------------------------------------

#' [UNCHANGED from original file]
get_projection_variances <- function(
  object,
  projection_residuals_endogenous_on_exogenous, projection_residuals_fs
) {
  instrument_index <- get_instrument_index_after_dropping_NAs(object)

  projection_residuals_ss <- projection_residuals_fs
  projection_residuals_ss[, instrument_index] <- projection_residuals_endogenous_on_exogenous

  projection_variances_in_stage_1_order <- colMeans(projection_residuals_ss * projection_residuals_fs)
  names(projection_variances_in_stage_1_order) <- names(coef(object, "stage1"))[!is.na(coef(object, "stage1"))]
  target_names <- names(coef(object, "stage2"))
  target_names[object[["endogenous"]]] <- names(coef(object, "stage1"))[instrument_index]
  target_names <- target_names[!is.na(coef(object, "stage2"))]

  projection_variances_in_stage_1_order[target_names]
}

#' [UNCHANGED from original file]
get_instrument_index_after_dropping_NAs <- function(object) {
  # That's in model.matrix order
  stage_1_coefficients <- coef(object, component = "stage1")
  regressor_dropped <- is.na(stage_1_coefficients)

  matrix_column_corresponds_to_ranked_term <- attr(stats::model.matrix(object, "instruments"), "assign") %in% object[["rank_instruments_indices"]]
  instrument_index <- which(matrix_column_corresponds_to_ranked_term)

  instrument_index <- which((seq_along(stage_1_coefficients) == instrument_index)[!regressor_dropped])
  instrument_index
}

#' [UNCHANGED from original file]
get_regressor_index_after_dropping_NAs <- function(object) {
  stage_2_coefficients <- coef(object, component = "stage2")
  regressor_dropped <- is.na(stage_2_coefficients)

  matrix_column_corresponds_to_ranked_term <- attr(stats::model.matrix(object, "regressors"), "assign") %in% object[["rank_terms_indices"]]
  regressor_term <- which(matrix_column_corresponds_to_ranked_term)

  regressor_index <- which((seq_along(stage_2_coefficients) == regressor_term)[!regressor_dropped])
  regressor_index
}

#' [UNCHANGED from original file]
postprocess_sigmahat <- function(object, sigmahat, complete) {
  regressor_dropped_ss <- is.na(coef(object, component = "stage2"))
  if (complete && any(regressor_dropped_ss)) {
    full_sigmahat <- matrix(NA, nrow = length(regressor_dropped_ss), ncol = length(regressor_dropped_ss))
    full_sigmahat[!regressor_dropped_ss, !regressor_dropped_ss] <- sigmahat
    colnames(full_sigmahat) <- names(coef(object, component = "stage2"))
  } else {
    full_sigmahat <- sigmahat
    colnames(full_sigmahat) <- names(coef(object, component = "stage2"))[!regressor_dropped_ss]
  }

  rownames(full_sigmahat) <- colnames(full_sigmahat)
  full_sigmahat
}

#' Calculate H1 component for covariance estimation
#'
#' Originally defined as h_1(x, y, z) = (R_Y(y) - rhoR_X(x) - Wbeta)(R_Z(z) - Wgamma)
#'
#' @return n x p matrix
#' @noRd
#' @exportS3Method
#' [UNCHANGED from original file]
calculate_H1.ivregranks <- function(object, projection_residuals, ...) {
  NextMethod()
}

#' Calculate H2 component for covariance estimation
#'
#' Originally defined as `h_2(x,y) = E[(I(y,Y)-rhoI(x,X)-Wbeta)(R_Z(Z) - Wgamma)]`
#' Estimator in matrix notation:
#' `(I_Y-rhoI_X-(Wbeta)') %*% (R_Z(Z)-Wgamma) / n`
#' Equal to
#' `I_Y %*% (R_Z(Z)-Wgamma) / n -`
#' `rho \* I_X %*% (R_Z(Z)-Wgamma) / n -`
#' `(Wbeta)' %*% (R_Z(Z)-Wgamma) / n`
#'
#' @noRd
#' @exportS3Method
#' [UNCHANGED from original file]
calculate_H2.ivregranks <- function(object, projection_residuals,
                                    H1_mean = NULL, ...) {
  rank_column_index <- get_ranked_indices(
    object,
    component = "regressors"
  )
  model_matrix_seqn <- stats::model.matrix(object, component = "regressors")
  l <- get_and_separate_regressors(
    model_matrix_seqn,
    rank_column_index
  )
  RY <- stats::model.response(stats::model.frame(object))

  NextMethod(l = l, RY = RY)
}

#' Calculate H3 component for covariance estimation
#'
#' Originally defined as `h_3(x) = E[(R_Y(Y)-rhoR_X(X)-Wbeta)(I(z,Z) - Wgamma)]`;
#' The second component depends on which projection model is considered
#'
#' Estimator in matrix notation:
#' `h_3(x) = (R_Y(Y)-rhoR_X(X)-Wbeta)' %*% [I_(z,Z); W] %*% R_S / n`
#' Where R_S is the projection residual matrix.
#'
#' For a given x this higly resembles colMeans(H1).
#' The difference H3(x) - colMeans(H1)is
#' `(R_Y(Y)-rhoR_X(X)-Wbeta)'%*%(I(z,Z) - RX)%*%R_S[r,] / n`
#' (last element is a row vector from R_S matrix corresponding to ranked regressor)
#'
#' @noRd
#' @exportS3Method
#' [UNCHANGED from original file]
calculate_H3.ivregranks <- function(object, projection_residual_matrix,
                                    H1_mean, ...) {
  rank_column_index <- get_ranked_indices(object,
    component = "instruments"
  )
  model_matrix <- stats::model.matrix(object, component = "instruments")
  l <- get_and_separate_regressors(model_matrix, rank_column_index)

  NextMethod(l = l)
}

## ---------------------------------------------------------------------------
## Suggested regression test (e.g. testthat), independent of any asymptotics:
## the exact FWL identity must hold for every l up to floating-point error.
##
## fit <- ivregranks(ry ~ rx + w2 + w3 | rz + w2 + w3, data = ...)
## Z    <- model.matrix(fit, "instruments")
## s1   <- coef(fit, "stage1"); s1 <- s1[!is.na(s1)]
## instr_idx <- get_instrument_index_after_dropping_NAs(fit)
## RXfit <- drop(Z %*% s1)
## A <- Z; A[, instr_idx] <- RXfit
## RY <- model.response(model.frame(fit))
## for (l in seq_len(ncol(A))[-instr_idx]) {
##   M_l <- lm.fit(A[, -l, drop = FALSE], A[, l])$residuals
##   beta_l_fwl <- sum(RY * M_l) / sum(M_l^2)
##   # compare beta_l_fwl against the corresponding entry of coef(fit, "stage2")
## }
## ---------------------------------------------------------------------------
