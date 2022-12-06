
### Logistic Function to be optimized.

logit_fun <- function(y, X, theta){
  
  # if there are multiple independent variables:
  if(!is.null(ncol(X))){
    
    beta <- theta[1:ncol(X)]
    
    mu <- X %*% beta
    
    
    # if there is just one:
  } else {
    
    beta <- theta[1]
    
    mu <- X * beta
    
    
  }
  
  # caluclating probabilities
  p <- 1 / ( 1 + exp(-mu) )
  
  # summing logged likelihood
  logll <-  sum( y * log(p) + (1 - y) * log ( 1- p) )
  
  #
  return(logll)
  
}
