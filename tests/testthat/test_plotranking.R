# setup
n <- 10 # not larger than 23
x <- seq(1, 3, length = n)
sd <- diag(rep(0.02, n))
ranks <- irank(x)
CS <- csranks(x, sd)
CS_ind <- csranks(x, sd, indices=c(5,6,7))
popnames <- rev(LETTERS[1:n])

test_that("default plotranking returns a ggplot object", {
  expect_s3_class(plotranking(ranks, CS$L, CS$U), "ggplot")
})

test_that("plotranking accepts popnames as factor", {
  expect_s3_class(plotranking(ranks, CS$L, CS$U, popnames = factor(popnames)), "ggplot")
})

test_that("custom plotranking returns a ggplot object", {
  expect_s3_class(plotranking(ranks, CS$L, CS$U, popnames = popnames), "ggplot")
  expect_s3_class(plotranking(ranks, CS$L, CS$U, title = "title"), "ggplot")
  expect_s3_class(plotranking(ranks, CS$L, CS$U, subtitle = "subtitle"), "ggplot")
  expect_s3_class(plotranking(ranks, CS$L, CS$U, caption = "caption"), "ggplot")
  expect_s3_class(plotranking(ranks, CS$L, CS$U, colorbins = 2), "ggplot")
  expect_s3_class(plotranking(ranks, CS$L, CS$U, horizontal = FALSE), "ggplot")
})

N <- 100 # not larger than 23
X <- seq(1, 3, length = n)
V <- diag(rep(0.04, n))
Ranks <- irank(x)
CS_large <- csranks(x, sd)

test_that("default plotranking works for larger dataset", {
  expect_s3_class(plotranking(Ranks, CS_large$L, CS_large$U), "ggplot")
})

test_that("S3 method for csranks works", {
  expect_s3_class(plot(CS), "ggplot")
})

test_that("S3 method for csranks with nondefault indices works", {
  expect_s3_class(plot(CS_ind), "ggplot")
})
