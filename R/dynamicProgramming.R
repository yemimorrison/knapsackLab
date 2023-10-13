#' Solve the knapsack problem using dynamic programming
#' @param weights A vector of item weights
#' @param values A vector of item values
#' @param W Maximum weight capacity of the knapsack
#' @return A list with two elements: the maximum value and a vector of selected item indices
knapsack_dynamic_programming <- function(weights, values, W) {
  n <- length(weights)
  dp <- matrix(0, n + 1, W + 1)
  
  for (i in 1:(n + 1)) {
    for (w in 0:(W + 1)) {
      if (i == 0 || w == 0) {
        dp[i, w] <- 0
      } else if (weights[i] <= w) {
        dp[i, w] <- max(dp[i - 1, w], dp[i - 1, w - weights[i]] + values[i])
      } else {
        dp[i, w] <- dp[i - 1, w]
      }
    }
  }
  
  selected_indices <- integer(n)
  i <- n
  w <- W
  
  while (i > 0 && w > 0) {
    if (dp[i, w] != dp[i - 1, w]) {
      selected_indices[i] <- 1
      w <- w - weights[i]
    }
    i <- i - 1
  }
  
  return(list(max_value = dp[n, W], selected_indices = which(selected_indices == 1)))
}