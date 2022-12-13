logistic_pooled <- function(df, coefvcov_only = FALSE){
    
    ## The function below estimates a pooled logistic regression model from Dion et al. (2018).
    ### Input: dataframe from Dion et al. (2018).
    #### optionally: boolean if we only want coefficients and variance covariance matrix.
    ### Output: Model table containing logistic models estimating probability of authors citing a female research team.    
  
  require(tidyverse)
  require(MASS)
  require(rms)
  require(optimx)
    
          if(!(all(c("Female", "Mixed", "APSR", "PG", "PA", "Econ.", "SMR", "newjnlid", "reffemonly", "newartid") %in% colnames(df)))){
        
        stop("Your dataframe does not contain one of the variables for the analysis.")
    }
    
    y <- df$reffemonly
    
    X <- cbind(1,
               df$Female,
               df$Mixed,
               df$PG,
               df$PA,
               df$Econ.,
               df$SMR)
    
    
    
    # start values
    startvals <- rep(0, ncol(X))
    
    # optimise
    res <- optim(
      par = startvals,
      fn = logit_fun,
      y = y,
      X = X,
      control = list(fnscale = -1),
      hessian = TRUE,
      method = "BFGS"
    )
    
        coef <- res$par

    
    # optimise restricted model
    startvals2 <- c(0, 0)
    
    restricted <- optim(
      startvals2,
      logit_fun,
      y = y,
      X = X[, 1],
      # restricted model
      control = list(fnscale = -1),
      method = "BFGS"
    )
    
    
    # Robust standard errors computed using the robcov-package.
    vcov <- vcov(robcov(lrm(data = df, reffemonly ~ Female + Mixed + PG + PA + Econ. + SMR, x=T, y=T),
                        cluster = df$newartid))
    
    # calculate standard error as square of diagonal values of vcov matrix
    se <- sqrt(diag(vcov))
  
  Names <- c("Intercept", "Female", "Mixed", "P&G", "PA", "Econ", "SMR", "Pseudo R2", "NullLL",  "LL", "Clusters", "Observations")
    
    ModelTable <- data.frame(Name = c(paste0(round(coef, 2), " (", round(se, 2), ")"), # coefficients
                                      round( 1- (restricted$value / res$value), 6), # pseudo r2
                                      round(restricted$value, 0), # nullLL
                                      round(res$value,0), # LL
                                      length(unique(df$newartid)), # Clusters
                                      nrow(df)) # Observations
    )
    
  
  rownames(ModelTable) <- Names
  colnames(ModelTable) <- "Pooled"
  
  
  if(coefvcov_only == FALSE){
    
    return(ModelTable)
    
  } else {
    
    return(list(coef = coef,
                vcov = vcov))
  }

}

