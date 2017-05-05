#' Build Concavex JAGS code
#' 
#' This function creates JAGS code within R for fitting the Concavex model. It has default non-informative priors for model parameters, but allows for specification of custom priors written in JAGS syntax
#' 
#' @param prior.theta_0  Prior for theta_0 specified in terms of JAGS code. The default is a non-informative normal prior: 'theta_0 ~ dnorm(0, 1E-4)'
#' @param prior.theta_1  Prior for theta_1 specified in terms of JAGS code. The default is a non-informative normal prior: 'theta_1 ~ dnorm(0, 1E-4)'
#' @param prior.lambda   Prior for lambda specified in terms of JAGS code. The default is a non-informative uniform prior: 'lambda ~ dunif(-.999, .999)' 
#' 
#' @export
#' @examples 
#' ccvx.jags.mod <- ccvx_build_jags()
#' cat(ccvx.jags.mod)


ccvx_build_jags <- function(prior.theta_0 = "theta_0 ~ dnorm(0, 1E-4)", 
                            prior.theta_1 = "theta_1 ~ dnorm(0, 1E-4)", 
                            prior.lambda = "lambda ~ dunif(-.999, .999)") {
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
    }
    
  }
  "
  )
}
