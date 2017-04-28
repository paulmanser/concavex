#' Build Concavex JAGS code
#' 
#' This function creates the JAGS code within R for fitting the Concavex model. Will eventually allow for flexible specification of priors
#' 
#' @export
#' @examples 
#' ccvx.jags.mod <- ccvx_build_jags()
#' cat(ccvx.jags.mod)


ccvx_build_jags <- function() {
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
    
    theta_0 ~ dnorm(0, 1E-4)   # non-informative normal prior on theta_0
    
    theta_1 ~ dnorm(0, 1E-4)   # non-informative normal prior on theta_1
    
    # flat prior on lambda over (-1, 1)
    lambda ~ dunif(-.999, .999)  
    
    # transformations --------------------------------------------------------

    for(ii in 1:length(pred.doses)){
      
      ## posterior credible interval for d-r curve
      mu.tilde[ii] <- theta_0 + theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*pred.doses[ii]))*pred.doses[ii])
      trt.post[ii] <- theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*pred.doses[ii]))*pred.doses[ii])
      
    }
    
    ## posterior distns for actual doses tested
    for(ii in 1:length(dose)){
      dose.post[ii] <- theta_0 + theta_1 * (((((1-lambda)/2)^2 + lambda)/(((1-lambda)/2)^2 + lambda*dose[ii]))*dose[ii])
    }
    
  }
  "
}
