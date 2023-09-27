## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(csranks)
data(parent_child_income)
head(parent_child_income)

## ----lmranks------------------------------------------------------------------
lmr_model <- lmranks(r(c_faminc) ~ r(p_faminc), data=parent_child_income)
summary(lmr_model)

## ----lm-----------------------------------------------------------------------
c_faminc_rank <- frank(parent_child_income$c_faminc, omega=1, increasing=TRUE)
p_faminc_rank <- frank(parent_child_income$p_faminc, omega=1, increasing=TRUE)
lm_model <- lm(c_faminc_rank ~ p_faminc_rank)
summary(lm_model)

## ----lmrankscov---------------------------------------------------------------
lmr_model_cov <- lmranks(r(c_faminc) ~ r(p_faminc) + gender + race, data=parent_child_income)
summary(lmr_model_cov)

## ----grouped_lmranks_simple---------------------------------------------------
grouped_lmr_model_simple <- lmranks(r(c_faminc) ~ r(p_faminc_rank):gender, 
                             data=parent_child_income)
summary(grouped_lmr_model_simple)

## ----grouped_lm_simple--------------------------------------------------------
grouped_lm_model_simple <- lm(c_faminc_rank ~ p_faminc_rank:gender + gender - 1, #group-wise intercept
                       data=parent_child_income)
summary(grouped_lm_model_simple)


## ----grouped_lmranksgran------------------------------------------------------
parent_child_income$subgroup <- interaction(parent_child_income$gender, parent_child_income$race)
grouped_lmr_model <- lmranks(r(c_faminc) ~ r(p_faminc_rank):subgroup, 
                             data=parent_child_income)
summary(grouped_lmr_model)

## ----grouped_lm---------------------------------------------------------------
grouped_lm_model <- lm(c_faminc_rank ~ p_faminc_rank:subgroup + subgroup - 1, #group-wise intercept
                       data=parent_child_income)
summary(grouped_lm_model)

## ----plot_CIs, message = FALSE, out.width = "90%", fig.width=6, fig.height=4----
library(ggplot2)
theme_set(theme_minimal())
ci_data <- data.frame(estimate=coef(lmr_model), 
                    parameter=c("Intercept", "slope"),
                    group="Whole sample",
                    method="csranks", 
                    lower=confint(lmr_model)[,1], 
                    upper=confint(lmr_model)[,2])
  
  ci_data <- rbind(ci_data, data.frame(
    estimate = coef(grouped_lmr_model),
    parameter = rep(c("Intercept", "slope"), each=6),
    group = rep(c("Hispanic female", "Hispanic male", "Black female", "Black male", 
                  "Other female", "Other male"), times=2),
    method="csranks",
    lower=confint(grouped_lmr_model)[,1],
    upper=confint(grouped_lmr_model)[,2]
  ))
  
  ci_data <- rbind(ci_data, data.frame(
    estimate = coef(lm_model),
    parameter = c("Intercept", "slope"),
    group = "Whole sample",
    method="naive",
    lower=confint(lm_model)[,1],
    upper=confint(lm_model)[,2]
  ))
  
  ci_data <- rbind(ci_data, data.frame(
    estimate = coef(grouped_lm_model),
    parameter = rep(c("Intercept", "slope"), each=6),
    group = rep(c("Hispanic female", "Hispanic male", "Black female", "Black male", 
                  "Other female", "Other male"), times=2),
    method="naive",
    lower=confint(grouped_lm_model)[,1],
    upper=confint(grouped_lm_model)[,2]
  ))
  
  ggplot(ci_data, aes(y=estimate, x=group, ymin=lower, ymax=upper,col=method, fill=method)) +
    geom_point(position=position_dodge2(width = 0.9)) +
    geom_errorbar(position=position_dodge2(width = 0.9)) +
    geom_hline(aes(yintercept=estimate), data=subset(ci_data, group=="Whole sample"),
               linetype="dashed",
               col="gray") +
    coord_flip() +
    labs(title="95% confidence intervals of intercept and slope\nin rank-rank regression")+
    facet_wrap(~parameter)

