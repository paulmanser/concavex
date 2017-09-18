#' Plot Concavex model posterior predictive risks for Phase 3 (AKA data-driven conditional probabilities)
#' 
#' Function to plot Concavex model risk profiles. Specifically, posterior predictive probabilities of exceeding efficacy thresholds
#' are plotted against doses tested, and against specific thresholds of interest. If no thresholds are specified in 
#' 'eff.thresholds' argument, only the plot against dose is generated.
#' 
#' @param ccvx.samples List containg Gibbs samples output by ccvx_fit() function
#' @param eff.thresholds A list of efficacy thresholds of specific interest for which to compute probabilities
#' 
#' @export
#' @examples 
#' ccvx.mod <- ccvx_build_jags(predictive.probs = TRUE)
#' ccvx.samples <- ccvx_fit(ccvx.mod, doses = 0:4, mu.hat = c(1, 20, 50, 60, 65), std.err = rep(20, 5), sd.ph3 = 200, n.per.arm.ph3 = 300)
#' par(mfrow=c(1,2))
#' ccvx_ddcp_plot(ccvx.samples, eff.thresholds = c(-5, 0, 5, 10, 20, 40))


ccvx_ddcp_plot <- function(ccvx.samples, eff.thresholds = NULL) {
  
  if(!is.null(eff.thresholds)) {
    
    # dose prob plot fix eff ---------------------------------------------------------------
    eff.mat <- matrix(nr = dim(ccvx.samples$jags.samples$trt.post.pred)[1],
                      nc = length(eff.thresholds))
    
    for(jj in 1:ncol(eff.mat)) {
      for(ii in 1:nrow(eff.mat)) {
        eff.mat[ii, jj] <- mean(unlist(ccvx.samples$jags.samples$trt.post.pred[ii, , ]) > eff.thresholds[jj])
      }
    }
    
    plot(0, type = 'n', ylim = c(0, 1), xlim = range(ccvx.samples$doses), cex.lab = 1.4,
         ylab = "Probability",
         main = "Probability of Exceeding Efficacy Thresholds \n by Threshold",
         xlab = "Dose")
    grid(lwd = 2)
    
    for(jj in 1:ncol(eff.mat)) {
      lines(seq(min(ccvx.samples$doses), max(ccvx.samples$doses), length.out = nrow(eff.mat)),
            eff.mat[, jj],
            col = jj, lwd = 2)
    }
    
    legend("bottomright",
           legend = eff.thresholds,
           title = "Threshold",
           fill = 1:length(eff.thresholds), border = NA)
  }
  
  
  # p for tested doses ------------------------------------------------------
  doses <- ccvx.samples$doses
  trt.eff.range <- seq(0, quantile(ccvx.samples$jags.samples$theta_1, probs = .95), length.out = 250)
  
  eff.mat <- matrix(nr = length(trt.eff.range), nc = length(doses))
  
  for(jj in 1:ncol(eff.mat)) {
    for(ii in 1:nrow(eff.mat)) {
      eff.mat[ii, jj] <- mean(unlist(ccvx.samples$jags.samples$trt.post.pred.dose[jj, , ]) > trt.eff.range[ii])
    }
  }
  
  plot(0, type = 'n', xlim = range(trt.eff.range), ylim = c(0, 1), cex.lab = 1.4,
       ylab = "Probability",
       main = "Probability of Exceeding Efficacy Thresholds \n by Dose",
       xlab = "Treatment Effect over Placebo")
  
  grid(lwd=2)
  
  for(jj in 1:ncol(eff.mat)) {
    lines(trt.eff.range, eff.mat[, jj], col = jj, lwd = 2)
  }
  
  legend("bottomleft", legend = doses, fill = 1:length(doses), border = NA,
         title = "Dose")
  
}



