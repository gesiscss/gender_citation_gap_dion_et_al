logistic_per_journal <- function(df, journal){
  
  require(tidyverse)
  require(MASS)
  require(rms)
  require(optimx)
    
    if(!(all(c("newjnlid", "reffemonly", "newartid", "Female", "Mixed") %in% colnames(df)))){
        
        stop("Your dataframe does not contain one of the variables for the analysis.")
    }
  
    
    df_ana <- df %>%
      filter(newjnlid %in% journal)
    
    
    y <- df_ana$reffemonly
    
    X <- cbind(1,
               df_ana$Female,
               df_ana$Mixed)
    
    # start values
    startvals <- rep(0, ncol(X))
    
    # optimize
    res <- optim(
      par = startvals,
      fn = logit_fun,
      y = y,
      X = X,
      control = list(fnscale = -1),
      hessian = TRUE,
      method = "BFGS"
    )
    
    # calculate restricted model.
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
    
    coef <- res$par
    # vcov <- solve(-res$hessian)
    # se <- sqrt(diag(vcov))
    
    # Unfortunately, I am not yet able to compute robust standard errors
    # clustered at article level by hand. But I am on it.
    fit=lrm(data = df_ana, reffemonly ~ Female + Mixed, x=T, y=T)
    vcov <- vcov(robcov(lrm(data = df_ana, reffemonly ~ Female + Mixed, x=T, y=T),
                        cluster = df_ana$newartid) 
    )
    se <- sqrt(diag(vcov))
    ##robust standard error
    
  
  Names = c("Intercept", "Female", "Mixed", "Pseudo R2", "NullLL",  "LL", "Clusters", "Observations")
    
    ModelTable <- data.frame(Name = c(paste0(round(coef, 2), " (", round(se, 2), ")"),
                                      round( 1- (restricted$value / res$value), 4),
                                      round(restricted$value, 0),
                                      round(res$value,0),
                                      length(unique(df_ana$newartid)),
                                      nrow(df_ana)))
  
  rownames(ModelTable) <- Names
  colnames(ModelTable) <- journal
  
  
    return(list(ModelTable = ModelTable,
                coef = coef,
                vcov = vcov))

}