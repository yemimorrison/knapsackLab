## ----include = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup------------------------------------------------------------------------------------------
library(knapsackLab)
library(parallel)
library(utils)

## ----message=FALSE, warning=FALSE-------------------------------------------------------------------

#install_github("yemimorrison/knapsackLab")


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------

brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel = FALSE)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------

#Question: How much time does it take to run the algorithm for n = 16 objects?

system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel=FALSE))
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel=TRUE))


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------

knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------

#Question: How much time does it take to run the algorithm for n = 500 objects?

system.time(knapsack_dynamic(knapsack_objects[1:500,], W = 3500))


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------

greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------

#Question: How much time does it take to run the algorithm for n = 1000000 objects?

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 1000000

knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))

#Run the code below to check the answer:
system.time(greedy_knapsack(knapsack_objects[1:1000000,], W = 3500))


