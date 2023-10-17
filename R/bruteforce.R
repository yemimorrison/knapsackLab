#' Solve the knapsack problem using a brute-force algorithm
#' @param x A dataframe called knapsack_objects with items with two columns: v- values and w-weights per items.
#' @param W a numeric variable which represents the size of the knapsack
#' @param parallel a variable with boolean values (TRUE or FALSE). If set to true, the function should parallelize.
#' @return A list with two elements: the maximum value and a vector of selected items.
#' @importFrom utils combn
#' 
#' @export

brute_force_knapsack <- function(x, W, parallel=FALSE) {
  if(!is.data.frame(x)){
    stop("Invalid entry.'x' must be a dataframe")
  }
  if(any(x<0,na.rm=TRUE)){
    stop("Invalid entry. 'x' must contain positive values")
  }
  if(!(W>=0 && length(W)==1 && is.numeric(W))){
    stop("Invalid variable. 'W' must be a positive numeric value")
  }
  if(!missing(parallel) && !is.logical(parallel)){
    stop("Error: parallel is missing & must be logical")
  }
  
  if(!(length(x)==2)){
    stop("Error: the dataframe 'x' must have two columns")
  }
  if(!(all(names(x)==c("w","v")))){
    stop("Error: the dataframe 'x' columns' names must be 'v' and 'w'")
  }
  
  
  val_max<-0
  value<-0
  n<-dim(x)[1]
  elements<-length(n)
  
  
  if(parallel==TRUE){
    num_cores <- detectCores()
    num_cores <-2
    
    cl <-  makeCluster(num_cores)
    
    clusterExport(cl, varlist=c("x","W","n","elements","val_max","value"), envir=environment())
    clusterEvalQ(cl, library(utils))
    
    parallel_fn <- function(i, x,W) {
      
      comb<-utils::combn(n,i)
      j<-1
      
      while(j<=ncol(comb)){
        if(sum(x$w[comb[,j]])<=W){
          value<-sum(x$v[comb[,j]])
          if(val_max<value){
            elements<-comb[,j] 
            val_max<-value
          }
        }
        j<-j+1
      }
      
      return(list(value=round(val_max),elements=elements))
      
    }
    
    vals<- parLapply(cl, 1:n, parallel_fn, x, W )
    
    i=1
    while(vals[[i]]["value"]!=0){
      value<-vals[[i]]["value"]
      elements<-vals[[i]]["elements"]
      i<-i+1
    }
    return(c(value,elements))
    stopCluster(cl)
    
  }else{
    
    lapply(1:n, function(i){
      comb<-combn(n,i) 
      j<-1
      while(j<=ncol(comb)){
        if(sum(x$w[comb[,j]])<=W){
          value<-sum(x$v[comb[,j]])
          if(val_max<value){
            elements<<-comb[,j] 
            val_max<<-value 
          }
        }
        j<-j+1
      }
    })
    
    value<-round(val_max)
    vals<-list(value=value,elements=elements)
    return(vals)
  }
}

#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel=FALSE)
#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel=TRUE)

#Check time for brute force programming
#system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel=FALSE))
#system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel=TRUE))

#profvis({brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel=TRUE)})
#profvis({brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel=FALSE)})

  