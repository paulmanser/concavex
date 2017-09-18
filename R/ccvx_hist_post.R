#' Plot Concavex model fit 
#' 
#' This function plots posterior distributions for the 3 parameters of the Concavex model
#' 
#' @param ccvx.samples   List containg Gibbs samples output by ccvx_fit() function
#' @param cred.int.width Specify width of credible interval for posteriors. Default is .9 (90\% credible interval)
#' 
#' @export
#' @examples 
#' ccvx.mod <- ccvx_build_jags()
#' ccvx.samples <- ccvx_fit(ccvx.mod, doses = 0:4, mu.hat = c(1, 20, 50, 60, 65), std.err = rep(10, 5))
#' par(mfrow=c(1,3))
#' ccvx_hist_post(ccvx.samples)

ccvx_hist_post <- function(ccvx.samples, cred.int.width = 0.9) {
  
  delta <- (1 - cred.int.width) / 2

  # theta 0
  hist(ccvx.samples$jags.samples$theta_0, 
       main = bquote(paste("Posterior for ", theta[0])), freq=FALSE, 
       xlab = bquote(theta[0]), cex.main = 1.3,
       cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
  
  abline(v = quantile(ccvx.samples$jags.samples$theta_0, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  
  # theta 1
  hist(ccvx.samples$jags.samples$theta_1, 
       main = bquote("Posterior for" ~ theta[1] ~ "with" ~ .(100*cred.int.width) * "% C.I."), freq=FALSE,
       xlab = bquote(theta[1]), cex.main = 1.3,
       cex.lab = 1.65, cex.axis = 1.3, breaks = 100)
  
  abline(v = quantile(ccvx.samples$jags.samples$theta_1, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
  
  # lambda
  hist(ccvx.samples$jags.samples$lambda, 
       main = bquote("Posterior for" ~ lambda ~ "with" ~ .(100*cred.int.width) * "% C.I."), 
       freq = FALSE, cex.main = 1.3,
       xlab = bquote(lambda), cex.lab = 1.65, cex.axis = 1.3, breaks = 100)

  abline(v = quantile(ccvx.samples$jags.samples$lambda, probs = c(delta, cred.int.width + delta)), col = 2, lwd = 2.5)
}


