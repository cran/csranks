# correct already:
# coerce, dummy.coef, family, formula, kappa, model.frame, model.matrix,
# nobs, print, qr, residuals, show, update

# probably correct:
# add1, alias, drop1, hatvalues, sigma

effects.lmranks <- function(object, ...){
  cli::cli_warn("This method might not return correct results.")
  NextMethod()
}

logLik.lmranks <- function(object, ...){
  cli::cli_warn("This method might not return correct results.")
  NextMethod()
}

proj.lmranks <- function(object, ...){
  cli::cli_warn("This method might not return correct results.")
  NextMethod()
}

rstandard.lmranks <- function(object, ...){
  cli::cli_warn("This method might not return correct results.")
  NextMethod()
}

anova.lmranks <- function(object, ...){
  cli::cli_warn("This method might not return correct results.")
  NextMethod()
}

alias.lmranks <- function()
summary.lmranks <- function(object, correlation = FALSE, symbolic.cor = FALSE, ...){
  if(correlation || symbolic.cor){
    cli::cli_abort("{.var correlation} and {.var symbolic.cor} are not yet implemented for {.class lmranks}.")
  }
  # call summary.lm
  outcome <- NextMethod()
  outcome
}