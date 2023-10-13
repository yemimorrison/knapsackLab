#' Solve the knapsack problem using a greedy algorithm
#' @param weights A vector of item weights
#' @param values A vector of item values
#' @param W Maximum weight capacity of the knapsack
#' @return A list with two elements: the maximum value and a vector of selected item indices
knapsack_greedy <- function(weights, values, W) {
  n <- length(weights)
  value_per_weight <- values / weights
  order_by_ratio <- order(-value_per_weight)
  selected_indices <- integer(n)
  current_weight <- 0
  total_value <- 0
  
  for (i in order_by_ratio) {
    if (current_weight + weights[i] <= W) {
      selected_indices[i] <- 1
      current_weight <- current_weight + weights[i]
      total_value <- total_value + values[i]
    }
  }
  
  return(list(max_value = total_value, selected_indices = which(selected_indices == 1)))
}