#' Build 5-parameter Concavex JAGS code
#' 
#' This function creates JAGS code within R for fitting the 5 parameter Concavex model. It has default non-informative priors for model parameters, but allows for specification of custom priors written in JAGS syntax
#' 
#' @param prior.theta_0  Prior for theta_0 specified in terms of JAGS code. The default is a non-informative normal prior: 'theta_0 ~ dnorm(0, 1E-4)'
#' @param prior.theta_1  Prior for theta_1 specified in terms of JAGS code. The default is a non-informative normal prior: 'theta_1 ~ dnorm(0, 1E-4)'
#' @param prior.gamma_1   Prior for gamma_1 specified in terms of JAGS code. The default is a non-informative uniform prior: 'gamma_1 ~ dunif(-.001, .999)' 
#' @param prior.gamma_2   Prior for gamma_2 specified in terms of JAGS code. The default is a non-informative uniform prior: 'gamma_2 ~ dunif(-.001, .999)' 
#' @param prior.alpha   Prior for alpha specified in terms of JAGS code. The default is a non-informative uniform prior on (0, pi/2): 'alpha ~ dunif(0.001, 3.1415926/2 - .001)' 
#' @param predictive.probs  Indicator for whether to compute posterior predictive probabilites for a future study. default is FALSE. if set to TRUE, the ccvx_fit function requires additional inputs
#' 
#' @export
#' @examples 
#' ccvx.jags.mod <- ccvx5_build_jags()
#' cat(ccvx.jags.mod)

ccvx5_build_jags <- function(prior.theta_0 = "theta_0 ~ dnorm(0, 1E-4)", 
                             prior.theta_1 = "theta_1 ~ dnorm(0, 1E-4)", 
                             prior.gamma_1 = "gamma_1 ~ dunif(.001, .999)",
                             prior.gamma_2 = "gamma_2 ~ dunif(.001, .999)",
                             prior.alpha   = "alpha ~ dunif(0.001, 3.1415926/2 - .001)",
                             predictive.probs = FALSE) {
  
  if(predictive.probs){
    cat("Including JAGS code to compute posterior predictive probabilities
        Please provide values for 'sd.ph3' and 'n.per.arm.ph3' arguments when using ccvx_fit()")
    pred.prob.code <- 
      "
    ## compute posterior predictive probabilities
    pred.pbo ~ dnorm(theta_0, tau.ph3)
    
    for(ii in 1:length(pred.doses)){
    mu.tilde.pred[ii] ~ dnorm(mu.tilde[ii], tau.ph3)
    trt.post.pred[ii] <- mu.tilde.pred[ii] - pred.pbo
    }
    
    for(ii in 1:length(dose)){
    mu.tilde.pred.dose[ii] ~ dnorm(dose.post[ii], tau.ph3)
    trt.post.pred.dose[ii] <- mu.tilde.pred.dose[ii] - pred.pbo
    }
    "
  }else{
    pred.prob.code <- ""
  }
  paste0(
    "
    model{
    
    # 5-parameter concavex spline model

    # likelihood -------------------------------------------------------------
    
    for(ii in 1:length(dose)){
    # concavex function
      ccvx1[ii] <- theta_0 +  (theta_0 + gamma_2*theta_1) *  (dose[ii] / gamma_1) *((lambda_1+((1-lambda_1)/2)^2)/((dose[ii]/gamma_1)*lambda_1+((1-lambda_1)/2)^2))
      ccvx2[ii] <- (theta_0+gamma_2*theta_1) +  ((1-gamma_2)*theta_1) *  ((dose[ii] - gamma_1) / (1 - gamma_1)) * ((lambda_2+((1-lambda_2)/2)^2)/(((dose[ii] - gamma_1) / (1 - gamma_1))*lambda_2+((1-lambda_2)/2)^2))
      ccvx.fit[ii] <- ccvx1[ii]*step(gamma_1 - dose[ii]) + ccvx2[ii]*step(dose[ii] - gamma_1)
    
    # effects are randomly distributed with assumed known dose-specific point estimates
      eff[ii] ~ dnorm(ccvx.fit[ii], tau[ii]) 
    }
    
    # specify priors --------------------------------------------------------
    
    ", 

    prior.theta_0, "\n\n    ", 
    prior.theta_1, "\n\n    ",
    prior.gamma_1, "\n\n    ", 
    prior.gamma_2, "\n\n    ", 
    prior.alpha, "\n",
  
  "
  # transformations -------------------------------------------------------
  k <- tan(alpha)
  lambda_1 <- 1 - 2*k/(k + sqrt((gamma_2/gamma_1)*k))
  lambda_2 <- 1 - 2*1/(1+sqrt(((1-gamma_1)/(1-gamma_2))*k))
  
  ## posterior credible intervals for d-r curve
  for(ii in 1:length(pred.doses)){
    ccvx1.t[ii] <- theta_0 +  (theta_0 + gamma_2*theta_1) *  (pred.doses[ii] / gamma_1) *((lambda_1+((1-lambda_1)/2)^2)/((pred.doses[ii]/gamma_1)*lambda_1+((1-lambda_1)/2)^2))
    ccvx2.t[ii] <- (theta_0+gamma_2*theta_1) +  ((1-gamma_2)*theta_1) *  ((pred.doses[ii] - gamma_1) / (1 - gamma_1)) * ((lambda_2+((1-lambda_2)/2)^2)/(((pred.doses[ii] - gamma_1) / (1 - gamma_1))*lambda_2+((1-lambda_2)/2)^2))
    mu.tilde[ii] <- ccvx1.t[ii]*step(gamma_1 - pred.doses[ii]) + ccvx2.t[ii]*step(pred.doses[ii] - gamma_1)
    trt.post[ii] <- ccvx1.t[ii]*step(gamma_1 - pred.doses[ii]) + ccvx2.t[ii]*step(pred.doses[ii] - gamma_1) - theta_0
  }

    ## posterior distns for doses tested
  for(ii in 1:length(dose)){
    dose.post[ii] <- ccvx1[ii]*step(gamma_1 - dose[ii]) + ccvx2[ii]*step(dose[ii] - gamma_1)
    trt.post.dose[ii] <- ccvx1[ii]*step(gamma_1 - dose[ii]) + ccvx2[ii]*step(dose[ii] - gamma_1) - theta_0
  }
  ",
  pred.prob.code,
  
  "
    }
  "
  )
}
