#' Solve the knapsack problem using a greedy algorithm
#' @param x A dataframe called knapsack_objects with items with two columns: v- values and w-weights per items.
#' @param W a numeric variable which represents the size of the knapsack
#' @return A list with two elements: the maximum value and a vector of selected item indices
#' @export

greedy_knapsack <- function(x, W) {
  if(!is.data.frame(x)){
    stop("Invalid entry.'x' must be a dataframe")
  }
  if(any(x<0,na.rm=TRUE)){
    stop("Invalid entry. 'x' must contain positive values")
  }
  if(!(W>=0 && length(W)==1 && is.numeric(W))){
    stop("Invalid variable. 'W' must be one positive numeric value")
  }
  if(!(length(x)==2)){
    stop("x must have two columns")
  }
  if(!(all(names(x)==c("w","v")))){
    stop("x columns' names must be 'v' and 'w'")
  }
  if(!(W>=0 && length(W)==1 && is.numeric(W))){
    stop("W must be one positive numeric value")
  }
  
  n <- dim(x)[1]
  weight <- x$v / x$w
  value <- 0
  elements <- rep(0, n)
  q <- 1
  
  for (q in 1:n) {
    if ((sum(x$w[elements]) + x$w[which.max(weight)]) <= W && any(weight > 0)) {
      i <- which.max(weight)
      value <- value + x$v[i]
      elements[q] <- i
      weight[i] <- 0
    } else {
      break
    }
  }
  
  value <- round(value)
  elements <- elements[which(elements > 0)]
  
  total_values <- list(value = value, elements = elements)
  
  return(total_values)
}

#greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500)

#Check time for greedy
#system.time(greedy_knapsack(knapsack_objects[1:1000000,], W = 3500))

