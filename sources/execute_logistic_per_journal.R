logistic_per_journal <- function(df, journal, coefvcov_only = FALSE){
    
    ## The function below estimates a logistic regression model for a specific journal from Dion et al. (2018).
    ### Input: dataframe from Dion et al. (2018), name of journal as in newjnlid variable fro data
    #### optionally: boolean if we only want coefficients and variance covariance matrix.
    ### Output: Model table containing logistic models estimating probability of authors citing a female research team.
    
    
  
  # required packages for function
  require(tidyverse)
  require(MASS)
  require(rms)
  require(optimx)
    
    # check whether our dataset contains the variables we need for the function
    if(!(all(c("newjnlid", "reffemonly", "newartid", "Female", "Mixed") %in% colnames(df)))){
        
        stop("Your dataframe does not contain one of the variables for the analysis.")
    }
  
    # just consider one journal
    df_ana <- df %>%
      filter(newjnlid %in% journal)
    
    
    # create our dependent variable
    y <- df_ana$reffemonly
    
    # create the matrix of our independent variables
    X <- cbind(1,
               df_ana$Female,
               df_ana$Mixed)
    
    # start values. The algorithm will start from these values to maximise the log likelihood of all of our parameters. 
    startvals <- rep(0, ncol(X))
    
    # optimize:
    res <- optim(
      par = startvals, # start values
      fn = logit_fun, # function to optimise: the function argument not stated below (theta) will be optimised.
      y = y, # y is our dv
      X = X, # X are our ivs
      control = list(fnscale = -1), # a negative value in fnscale will lead to a maximisation of the function, rather than a minimmisation.
      hessian = FALSE, # do not include hessian matrix of 2nd derivatives. What be necessary if we calculated se by hand.
      method = "BFGS" # which hill-climbing algorithm to use.
    )
    
    # at which values for theta did the optim() function reach its maximum? Those are the coefficients.
    coef <- res$par
    # vcov <- solve(-res$hessian)
    # se <- sqrt(diag(vcov))
    
    # clustered standard errors are not computed by hand. So we run a quick model and calculate standard errors using
    # robcov function. It may make our own function redundant, but that function enhances understanding.
    fit <- lrm(data = df_ana, reffemonly ~ Female + Mixed, x=T, y=T)
    vcov <- vcov(robcov(lrm(data = df_ana, reffemonly ~ Female + Mixed, x=T, y=T),
                        cluster = df_ana$newartid) 
    )
    
    # Standard errors as diagonals of variance covariance matrix
    se <- sqrt(diag(vcov))
    
    # estimate restricted model. We need that to calculate pseudo r2.
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
    
  
    # names for our regression table
  Names = c("Intercept", "Female", "Mixed", "Pseudo R2", "NullLL",  "LL", "Clusters", "Observations")
    
    # calculating regression diagnostics and generate table
  ModelTable <- data.frame(Name = c(paste0(round(coef, 2), " (", round(se, 2), ")"), # coefficients
                                      round( 1- (restricted$value / res$value), 6), # pseudo r2
                                      round(restricted$value, 0), # NullLL
                                      round(res$value,0), # LL
                                      length(unique(df_ana$newartid)), # Clusters
                                      nrow(df_ana))) # Observations
  
    #row- and column-names for table
  rownames(ModelTable) <- Names
  colnames(ModelTable) <- journal
  
    
  
    # if we wanted to visualise regression via simulation, we may not need a table but just the coefs and vcov. 
    # Then we can run the model using coefvoc_only = TRUE and get only those.
    if(coefvcov_only == TRUE){
        return(list(coef = coef,
                    vcov = vcov))

        
    } else{
        # we get a model table for a regression for one journal as output
        return(ModelTable)

    }

}