#' Plot Concavex model fit 
#' 
#' This function plots posterior distributions for the 3 parameters of the Concavex model
#' 
#' @param ccvx.samples   List containg Gibbs samples output by ccvx_fit() function
#' @param cred.int.width Specify width of credible interval for posteriors. Default is .9 (90\% credible interval)
#' @param params Specify for which model parameters to show posterior densities. Allowable values are "theta_0", "theta_1", and "lambda"
#' 
#' @export
#' @examples 
#' ccvx.mod <- ccvx_build_jags()
#' ccvx.samples <- ccvx_fit(ccvx.mod, doses = 0:4, mu.hat = c(1, 20, 50, 60, 65), std.err = rep(10, 5))
#' par(mfrow=c(1, 3))
#' ccvx_hist_post(ccvx.samples, params = c("theta_0", "theta_1", "lambda"))

ccvx_hist_post <- function(ccvx.samples, params = c("theta_0", "theta_1", "lambda"), cred.int.width = 0.9) {
  
  if (any(!params %in% c("theta_0", "theta_1", "lambda"))) 
    stop("params must be one of three values: 'theta_0', 'theta_1', or 'lambda'")
  
  delta <- (1 - cred.int.width) / 2

  # theta 0
  if ("theta_0" %in% params) {
    hist(ccvx.samples$jags.samples$theta_0, 
         main = bquote("Posterior for" ~ theta[0] ~ "with" ~ .(100*cred.int.width) * "% C.I."), freq=FALSE, 
         xlab = bquote(theta[0]), cex.main = 1.3,
         cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
    
    abline(v = quantile(ccvx.samples$jags.samples$theta_0, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  }
  
  # theta 1
  if ("theta_1" %in% params) {
    hist(ccvx.samples$jags.samples$theta_1, 
         main = bquote("Posterior for" ~ theta[1] ~ "with" ~ .(100*cred.int.width) * "% C.I."), freq=FALSE,
         xlab = bquote(theta[1]), cex.main = 1.3,
         cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
    
    abline(v = quantile(ccvx.samples$jags.samples$theta_1, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  }
  
  if("lambda" %in% params & any(grep('3-parameter concavex model', ccvx.samples$ccvx.mod))){ 
    # lambda
    hist(ccvx.samples$jags.samples$lambda, 
         main = bquote("Posterior for" ~ lambda ~ "with" ~ .(100*cred.int.width) * "% C.I."), 
         freq = FALSE, cex.main = 1.3,
         xlab = bquote(lambda), cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
  
    abline(v = quantile(ccvx.samples$jags.samples$lambda, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  }
  
  # if(any(grep('5-parameter concavex spline model', ccvx.samples$ccvx.mod))){ 
  #   # gamma_1
  #   hist(ccvx.samples$jags.samples$gamma_1, 
  #        main = bquote("Posterior for" ~ gamma[1] ~ "with" ~ .(100*cred.int.width) * "% C.I."), 
  #        freq = FALSE, cex.main = 1.3,
  #        xlab = bquote(gamma[1]), cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
  #   
  #   abline(v = quantile(ccvx.samples$jags.samples$gamma_1, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  #   
  #   # gamma_2
  #   hist(ccvx.samples$jags.samples$gamma_2, 
  #        main = bquote("Posterior for" ~ gamma[2] ~ "with" ~ .(100*cred.int.width) * "% C.I."), 
  #        freq = FALSE, cex.main = 1.3,
  #        xlab = bquote(gamma[2]), cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
  #   
  #   abline(v = quantile(ccvx.samples$jags.samples$gamma_2, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  #   
  #   # alpha
  #   hist(ccvx.samples$jags.samples$alpha, 
  #        main = bquote("Posterior for" ~ alpha ~ "with" ~ .(100*cred.int.width) * "% C.I."), 
  #        freq = FALSE, cex.main = 1.3,
  #        xlab = bquote(alpha), cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
  #   
  #   abline(v = quantile(ccvx.samples$jags.samples$alpha, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  # }
}


