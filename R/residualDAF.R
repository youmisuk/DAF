if(! "SuperLearner" %in% installed.packages()){
  install.packages("SuperLearner", dependencies = TRUE)
}
library("SuperLearner")

# ::: define a function for residual-based DAF ::::
residualDAF <- function(decision, group, focal.name = 1, fair.att, SL.library=c("SL.glm", "SL.randomForest", "SL.nnet"), ...) {
  
  # decision ... decision 
  # group ... group membership (focal versus reference)
  # focal.name ... the name of focal group
  # fair.att ... fair attribute 
  # SL.library ... Either a character vector of prediction algorithms. Note function, listWrappers() prints a list of functions available in the SuperLearner package.
  
  sl_model <- SuperLearner(Y=decision, X=data.frame(fair.att),
                               family=binomial(),
                               SL.library=SL.library, ...)

  d.pred <- as.numeric(predict.SuperLearner(sl_model, newdata=data.frame(fair.att))$pred)
  
  # find the location of examinees for the reference and the focal groups 
  loc_ref <- which(group != focal.name)
  loc_foc <- which(group == focal.name)
  
  # divide the decision and prediction data into the two group data 
  resp_ref <- decision[loc_ref]; d.pred_ref <- d.pred[loc_ref]
  resp_foc <- decision[loc_foc]; d.pred_foc <- d.pred[loc_foc]
  
  # check sample sizes
  n_ref <- length(resp_ref)
  n_foc <- length(resp_foc)
  
  # compute the raw residuals   
  resid_ref <- resp_ref - d.pred_ref
  resid_foc <- resp_foc - d.pred_foc
  
  # compute the first two residual-based DAF statistics: RDAF_R and RDAF_S
  rdaf_r <- mean(resid_foc) - mean(resid_ref) # the difference of mean raw residuals
  rdaf_s <- mean(resid_foc^2) - mean(resid_ref^2) # the difference of mean squared residuals
  
  # compute the mean, variance, and covariance 
  rdaf_r_mu <- 0 
  rdaf_r_var <- sum(d.pred_foc*(1-d.pred_foc))/(n_foc^2) + sum(d.pred_ref*(1-d.pred_ref))/(n_ref^2)
  
  rdaf_s_mu <- mean(d.pred_foc*(1-d.pred_foc)) - mean(d.pred_ref*(1-d.pred_ref))
  rdaf_s_var <- sum(d.pred_foc*(1-d.pred_foc)*(1-2*d.pred_foc)^2)/(n_foc^2) + 
    sum(d.pred_ref*(1-d.pred_ref)*(1-2*d.pred_ref)^2)/(n_ref^2)
  rdaf_r_s_cov <- sum(d.pred_foc*(1-d.pred_foc)*(1-2*d.pred_foc))/(n_foc^2) + 
    sum(d.pred_ref*(1-d.pred_ref)*(1-2*d.pred_ref))/(n_ref^2)
  
  # standardize the two statistics of RDAF_R and RDAF_S
  z_stat_rdaf_r <- (rdaf_r - rdaf_r_mu) / sqrt(rdaf_r_var)
  z_stat_rdaf_s <- (rdaf_s - rdaf_s_mu) / sqrt(rdaf_s_var)
  
  # create a covariance matrix between RDAF_R and RDAF_S
  cov_mat <- array(NA, c(2, 2))
  
  # replace NAs with the analytically computed covariance
  cov_mat[col(cov_mat) != row(cov_mat)] <- rdaf_r_s_cov
  
  # replace NAs with the analytically computed variances
  diag(cov_mat) <- c(rdaf_r_var, rdaf_s_var)
  
  # create a vector of mean RDAF_R and RDAF_S
  mu_vec <- cbind(rdaf_r_mu, rdaf_s_mu)
  
  # create a vector of RDAF_R and RDAF_S
  est_mu_vec <- cbind(rdaf_r, rdaf_s)
  
  # compute the last residual-based DIF statistics: RDAF_RS
  chisq <- as.numeric((est_mu_vec - mu_vec) %*% solve(cov_mat) %*% t(est_mu_vec - mu_vec)) 
  
  # calculate p-values for all three statistics
  pval_rdaf_r <- round(2 * stats::pnorm(q=abs(z_stat_rdaf_r), mean=0, sd=1, lower.tail=FALSE), 4)
  pval_rdaf_s <- round(2 * stats::pnorm(q=abs(z_stat_rdaf_s), mean=0, sd=1, lower.tail=FALSE), 4)
  pval_rdaf_rs <- round(stats::pchisq(chisq, df=2, lower.tail=FALSE), 4)
  
  # summarize the results
  pval_rdaf_all <- c(pval_rdaf_r, pval_rdaf_s, pval_rdaf_rs)
  stat_all <- c(z_stat_rdaf_r, z_stat_rdaf_s, chisq) 
  rst <- as.matrix(cbind(stat_all, pval_rdaf_all))
  
  colnames(rst) <- c("Statistics (z/chisq)", "P value")
  rownames(rst) <- c("RDAF_R", "RDAF_S", "RDAF_RS")
  
  resi_summmary <- matrix(c(0, 1, mean(resid_ref), mean(resid_foc), mean(resid_ref^2), mean(resid_foc^2)), nrow=2)
  
  colnames(resi_summmary) <- c("group", "resid_mean", "resid_square_mean")
  rownames(resi_summmary) <- c("ref", "foc")
  
  # return the results  
  return(list(resi_summary=round(resi_summmary, 4), stat_summary=rst))
  
}
