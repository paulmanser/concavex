#' Sample from Concavex model posteriors
#' 
#' This function samples from posterior densities of concavex model parameters as well as transformations of these parameters. 
#' 
#' @param ccvx.mod  JAGS model file as specified by ccvx_build_jags()
#' @param doses A vector of dose strengths
#' @param mu.hat A vector of parameter estimates for each dose
#' @param stderr A vector of standard errors for parameter estimates
#' @param n.chains Number of chains used to sample. Default is 4
#' @param gibbs.samples Number of samples to draw for each chain. Default is 5000
#' 
#' @export
#' @examples 
#' ccvx.mod <- ccvx_build_jags()
#' ccvx.samples <- ccvx_fit(ccvx.mod, doses = 0:4, mu.hat = c(1, 20, 50, 60, 65), stderr = rep(10, 5))
#' ccvx_plot_fit(ccvx.samples)

ccvx_fit <- function(ccvx.mod, doses, mu.hat, stderr, n.chains = 4, gibbs.samples = 5000) {
  
  tc.ccvx <- textConnection(ccvx.mod)

  model.init <- jags.model(tc.ccvx,
                           data = list('dose' = doses / max(doses),
                                       'eff' = mu.hat,
                                       'tau' = 1 / stderr^2,      # jags deals in precisions
                                       'pred.doses' = seq(0, 1, length.out = 250)),
                           n.chains = n.chains)
  close(tc.ccvx)
  
  # gibbs sampling
  ccvx.samples <- jags.samples(model.init, 
                               c("theta_0", "theta_1", "lambda", "mu.tilde", "dose.post", "trt.post"),
                               n.iter = gibbs.samples)
  
  # diagnostics with coda
  coda.samples <- coda.samples(model.init, c("theta_0", "theta_1", "lambda"), gibbs.samples)
  
  out <- list(ccvx.mod = ccvx.mod, jags.samples = ccvx.samples, coda.samples = coda.samples,
              doses = doses, mu.hat = mu.hat, stderr = stderr)
  out
}
