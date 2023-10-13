RNGversion(min(as.character(getRversion(), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 20
knapsack_objects <-
  data.frame(
    w=sample(1:50, size = n, replace = TRUE),
    v=runif(n = n, 0, 30)
  )
