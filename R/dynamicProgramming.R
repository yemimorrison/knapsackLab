#' Solve the knapsack problem using dynamic programming
#' @param x A dataframe called knapsack_objects with items with two columns: v- values and w-weights per items.
#' @param W a numeric variable which represents the size of the knapsack
#' 
#' @return A list with two elements: the maximum value and a vector of selected items
#' @export

knapsack_dynamic <- function(x, W) {
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
  dp <- matrix(nrow=n + 1, ncol=W + 1)
  dp[1,] <- rep(0,W+1)
  values <- x$v
  weights <- x$w
  
  
  for (i in 1:n) {
    for (j in 0:W) {
      if (weights[i] > j) {
        dp[i+1,j+1]<-dp[i,j+1] 
      }else {
         dp[i+1, j+1] <- max(dp[i , j+1], dp[i - 1, j+1 - weights[i]] + values[i])
      }
    }
  }
  
  j=j+1
  
  i<-which.max(dp[,j])
  elements <- length(n)
  q <-1
  elements[q]<-i-1
  
  while(dp[i,j]!=0 && j!=1 && i!=0){
    q<-q+1
    j<-(j-weights[i-1])
    i<-which(dp[,j] == dp[i-1,j])[1]
    elements[q]<-i-1
  }
  
  value<-round(dp[n+1,W+1])
  elements<-sort(elements[which(elements>0)])
  
  vals<-list(value=value,elements=elements)  
  
  return(vals)
}

#knapsack_dynamic(knapsack_objects[1:8,], W = 3500)

#Check Time for dynamic programming
#system.time(knapsack_dynamic(knapsack_objects[1:500,], W = 3500))
#profvis({knapsack_dynamic(knapsack_objects[1:500,], W = 3500)})


