logistic_per_journal <- function(journal, vcovcoef = FALSE){
  
  require(tidyverse)
  require(MASS)
  require(rms)
  require(optimx)
  
  
  if(journal != "Pooled"){
    df_ana <- df %>%
      filter(newjnlid %in% journal & refauthcomplete == 1) %>%
      dplyr::select(newjnlid, authorteam, reffemonly, newartid) %>%
      na.omit() %>%
      mutate(Female = ifelse(authorteam == "Female", 1, 0),
             Mixed = ifelse(authorteam == "Mixed", 1, 0)) %>%
      dplyr::select(-authorteam)
    
    
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
    
    startvals2 <- c(0, 0) # Why three this time?
    
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
    
  } else {
    
    df_ana <- df %>%
      filter(refauthcomplete == 1) %>%
      dplyr::select(newjnlid, authorteam, reffemonly, newartid) %>%
      na.omit() %>%
      mutate(Female = ifelse(authorteam == "Female", 1, 0),
             Mixed = ifelse(authorteam == "Mixed", 1, 0),
             APSR = ifelse(newjnlid == "APSR", 1, 0),
             PG = ifelse(newjnlid == "Politics & Gender", 1, 0),
             PA = ifelse(newjnlid == "Political Analysis", 1, 0),
             Econ. = ifelse(newjnlid == "Econometrica", 1, 0),
             SMR = ifelse(newjnlid == "Soc. Methods & Res.", 1, 0)) %>%
      dplyr::select(-authorteam)    
    
    y <- df_ana$reffemonly
    
    X <- cbind(1,
               df_ana$Female,
               df_ana$Mixed,
               df_ana$PG,
               df_ana$PA,
               df_ana$Econ.,
               df_ana$SMR)
    
    
    
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
    
    startvals2 <- c(0, 0) # Why three this time?
    
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
    vcov <- vcov(robcov(lrm(data = df_ana, reffemonly ~ Female + Mixed + PG + PA + Econ. + SMR, x=T, y=T),
                        cluster = df_ana$newartid))
    #              )
    se <- sqrt(diag(vcov))
    ##robust standard error
  }
  
  Names = c("Intercept", "Female", "Mixed", "P&G", "PA", "Econ", "SMR", "Pseudo R2", "NullLL",  "LL", "Clusters", "Observations")
  if(journal == "Pooled"){
    
    ModelTable <- data.frame(Name = c(paste0(round(coef, 2), " (", round(se, 2), ")"),
                                      round( 1- (restricted$value / res$value), 4),
                                      round(restricted$value, 0),
                                      round(res$value,0),
                                      length(unique(df_ana$newartid)),
                                      nrow(df_ana))
    )
    
  } else {
    
    ModelTable <- data.frame(Name = c(paste0(round(coef, 2), " (", round(se, 2), ")"),
                                      rep("", 4),
                                      round( 1- (restricted$value / res$value), 4),
                                      round(restricted$value, 0),
                                      round(res$value,0),
                                      length(unique(df_ana$newartid)),
                                      nrow(df_ana)))
  }
  
  rownames(ModelTable) <- Names
  colnames(ModelTable) <- journal
  
  
  if(vcovcoef == FALSE){
    
    return(ModelTable)
    
  } else {
    
    return(list(coef = coef,
                vcov = vcov))
  }
  
}

