#' Solve the knapsack problem using a brute-force algorithm
#' @param weights A vector of item weights
#' @param values A vector of item values
#' @param W Maximum weight capacity of the knapsack
#' @return A list with two elements: the maximum value and a vector of selected item indices

knapsack_bruteforce <- function(weights, values, W) {
  n <- length(weights)
  best_value <- 0
  best_set <- integer(n)
  
  for (i in 0:(2^n - 1)) {
    set <- as.integer(intToBits(i))[1:n]
    if (sum(weights * set) <= W) {
      total_value <- sum(values * set)
      if (total_value > best_value) {
        best_value <- total_value
        best_set <- set
      }
    }
  }
  
  return(list(max_value = best_value, selected_indices = which(best_set == 1)))
}

knapsack_bruteforce(weights = knapsack_objects$w, values = knapsack_objects$v, W = 2000)
