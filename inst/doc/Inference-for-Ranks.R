## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(csranks)
library(ggplot2)
data(pisa)
head(pisa)

## -----------------------------------------------------------------------------
ggplot(pisa, aes(x=reorder(jurisdiction,math_score,decreasing=TRUE), y=math_score)) + 
    geom_errorbar(aes(ymin=math_score-2*math_se, ymax=math_score+2*math_se)) +
    geom_point() + 
    theme_minimal() +
    labs(y="math score", x="", title="2018 PISA math score for OECD countries", subtitle="(with 95% marginal confidence intervals)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## -----------------------------------------------------------------------------
math_rank <- irank(pisa$math_score)
head(pisa[order(math_rank),])

## ----math_Sigma---------------------------------------------------------------
math_cov_mat <- diag(pisa$math_se^2)

## ----math_taubest-------------------------------------------------------------
CS_5best <- cstaubest(pisa$math_score, math_cov_mat, tau = 5, coverage = 0.95)

pisa[CS_5best, "jurisdiction"]

## ----math_taubest_90----------------------------------------------------------
CS_5best_90 <- cstaubest(pisa$math_score, math_cov_mat, tau = 5, coverage = 0.9)

pisa[CS_5best_90, "jurisdiction"]

## ----math_marg----------------------------------------------------------------
uk_i <- which(pisa$jurisdiction == "United Kingdom")
CS_marg <- csranks(pisa$math_score, math_cov_mat, simul=FALSE, indices = uk_i, coverage=0.95)
CS_marg

## ----math_simul, fig.width=7, fig.height=7------------------------------------
CS_simul <- csranks(pisa$math_score, math_cov_mat, simul=TRUE, coverage=0.95)
plotsimul <- plot(CS_simul, popnames=pisa$jurisdiction, 
     title="Ranking of OECD Countries by 2018 PISA Math Score", 
     subtitle="(with 95% simultaneous confidence sets)")
plotsimul

## ----eval = FALSE-------------------------------------------------------------
#  ggplot2::ggsave("mathsimul.pdf", plot=plotsimul)

## ----math_simul75, fig.width=7, fig.height=7----------------------------------
CS_simul75 <- csranks(pisa$math_score, math_cov_mat, simul=TRUE, coverage=0.75)
plotsimul75 <- plot(CS_simul75, popnames=pisa$jurisdiction, 
     title="Ranking of OECD Countries by 2018 PISA Math Score", 
     subtitle="(with 75% simultaneous confidence sets)")
plotsimul75

