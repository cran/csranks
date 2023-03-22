## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(csranks)
data(pisa)
head(pisa)

## ----math_irank---------------------------------------------------------------
math_rank <- irank(pisa$math_score)
math_rank

## ----math_Sigma---------------------------------------------------------------
math_Sigma <- diag(pisa$math_se^2)

## ----math_taubest-------------------------------------------------------------
CS_5best <- cstaubest(pisa$math_score, math_Sigma, tau = 5)

pisa[CS_5best, "jurisdiction"]

## ----math_taubest_90----------------------------------------------------------
CS_5best_90 <- cstaubest(pisa$math_score, math_Sigma, tau = 5, coverage = 0.9)

pisa[CS_5best_90, "jurisdiction"]

## ----math_marg----------------------------------------------------------------
uk_i <- which(pisa$jurisdiction == "United Kingdom")
CS_marg <- csranks(pisa$math_score, math_Sigma, simul=FALSE, indices = uk_i)
CS_marg

## ----math_simul, fig.width=7, fig.height=7------------------------------------
CS_simul <- csranks(pisa$math_score, math_Sigma, simul=TRUE)
plot(CS_simul, popnames=pisa$jurisdiction, 
     title="Ranking of OECD Countries by 2018 PISA Math Score", 
     subtitle="(with 95% simultaneous confidence sets)")

