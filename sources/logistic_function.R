logit_fun <- function(y, X, theta){
    
    ## This is a logit function to be optimised with optim-function.
    ### Input: dependent variable vector y, independent variable matrix X. theta will be optimised and does not require
    ### an input.
    ### Output: summed maximum logged likelihood of function with given parameter values as a scalar.
  
  # if there are >= 2 independent variables:
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
  
  # return sum loglik value
  return(logll)

}
