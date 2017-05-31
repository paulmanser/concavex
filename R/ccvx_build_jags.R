#' Build Concavex JAGS code
#' 
#' This function creates JAGS code within R for fitting the Concavex model. It has default non-informative priors for model parameters, but allows for specification of custom priors written in JAGS syntax
#' 
#' @param prior.theta_0  Prior for theta_0 specified in terms of JAGS code. The default is a non-informative normal prior: 'theta_0 ~ dnorm(0, 1E-4)'
#' @param prior.theta_1  Prior for theta_1 specified in terms of JAGS code. The default is a non-informative normal prior: 'theta_1 ~ dnorm(0, 1E-4)'
#' @param prior.lambda   Prior for lambda specified in terms of JAGS code. The default is a non-informative uniform prior: 'lambda ~ dunif(-.999, .999)' 
#' @param predictive.probs  Indicator for whether to compute posterior predictive probabilites for a future study. default is FALSE. if set to TRUE, the ccvx_fit function requires additional inputs
#' 
#' @export
#' @examples 
#' ccvx.jags.mod <- ccvx_build_jags()
#' cat(ccvx.jags.mod)


ccvx_build_jags <- function(prior.theta_0 = "theta_0 ~ dnorm(0, 1E-4)", 
                            prior.theta_1 = "theta_1 ~ dnorm(0, 1E-4)", 
                            prior.lambda = "lambda ~ dunif(-.999, .999)",
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
    
    # likelihood -------------------------------------------------------------
    
    for(ii in 1:length(dose)){
      # concavex function
      ccvx.fit[ii] <- theta_0 + theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*dose[ii]))*dose[ii])
      
      # effects are randomly distributed with assumed known dose-specific point estimates
      eff[ii] ~ dnorm(ccvx.fit[ii], tau[ii]) 
    }

    # specify priors --------------------------------------------------------
    
    ", 
  
    prior.theta_0, "\n\n    ", 
    prior.theta_1, "\n\n    ", 
    prior.lambda, "\n",

    "
    # transformations --------------------------------------------------------

    ## posterior credible intervals for d-r curve
    for(ii in 1:length(pred.doses)){
      mu.tilde[ii] <- theta_0 + theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*pred.doses[ii]))*pred.doses[ii])
      trt.post[ii] <- theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*pred.doses[ii]))*pred.doses[ii])
    }
    
    ## posterior distns for doses tested
    for(ii in 1:length(dose)){
      dose.post[ii] <- theta_0 + theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*dose[ii]))*dose[ii])
      trt.post.dose[ii] <- theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*dose[ii]))*dose[ii])
    }
  ",
  pred.prob.code,
  
  "
  }
  "
  )
}
