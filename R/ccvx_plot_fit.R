#' Plot Concavex model fit 
#' 
#' Function to plot Concavex model fit overlaid on actual points
#' 
#' @param ccvx.samples List containg Gibbs samples output by ccvx_fit() function
#' @param placebo.adjusted Should the estimated dose-response curve be placebo adjusted? Default is FALSE
#' @param cred.int.width Specify width of credible interval for curve. This is also applied to confidence intervals of point estimates. Default is 0.9 (90\%)
#' @param title Plot title
#' @param xlab Label for x-axis
#' @param ylab Label for y-axis
#' @author Paul Manser, \email{manser.paul@gene.com}
#' @export
#' @examples 
#' ccvx.mod <- ccvx_build_jags()
#' ccvx.samples <- ccvx_fit(ccvx.mod, doses = 0:4, mu.hat = c(1, 20, 50, 60, 65), std.err = rep(10, 5))
#' par(mfrow=c(1,2))
#' ccvx_plot_fit(ccvx.samples, placebo.adjusted = FALSE)
#' ccvx_plot_fit(ccvx.samples, placebo.adjusted = TRUE)

ccvx_plot_fit <- function(ccvx.samples, placebo.adjusted = FALSE,
                          title = "Concavex Model Fit", xlab = "Dose", ylab = "Effect", cred.int.width = .9) {
  
  # define some convenience variables
  dose.range <- seq(0, 1, length.out = 250)
  dose.range.orig <- seq(0, max(ccvx.samples$dose), length.out = 250)
  orig.ind <- sapply(ccvx.samples$dose, function(x) which.min(abs(dose.range.orig - x)))
  
  # compute median parameter estimates
  theta_0_median <- median(ccvx.samples$jags.samples$theta_0)
  theta_1_median <- median(ccvx.samples$jags.samples$theta_1)
  lambda_median <- median(ccvx.samples$jags.samples$lambda)
  
  # compute credible interval bounds, plot points, and standard errors
  pci.ul <- pci.ll <- numeric(length(dose.range.orig))
  delta <- (1 - cred.int.width)/2
  
  if (!placebo.adjusted){
    for(ii in 1:length(pci.ul)) {
      quantiles <- quantile(unlist(ccvx.samples$jags.samples$mu.tilde[ii, , ]), probs = c(delta, cred.int.width + delta))
      pci.ll[ii] <- quantiles[1]
      pci.ul[ii] <- quantiles[2]
    }
    y <- theta_0_median + theta_1_median * (((((1-lambda_median)/2)^2 + lambda_median)/(((1-lambda_median)/2)^2 + lambda_median*dose.range))*dose.range)
    y.vals <- ccvx.samples$mu.hat
    x.vals <- ccvx.samples$doses
    std.err <- ccvx.samples$std.err
  }
  
  if (placebo.adjusted){
    for(ii in 1:length(pci.ul)) {
      quantiles <- quantile(unlist(ccvx.samples$jags.samples$trt.post[ii, , ]), probs = c(delta, cred.int.width + delta))
      pci.ll[ii] <- quantiles[1]
      pci.ul[ii] <- quantiles[2]
    }
    y <- theta_1_median * (((((1-lambda_median)/2)^2 + lambda_median)/(((1-lambda_median)/2)^2 + lambda_median*dose.range))*dose.range)
    y.vals <- ccvx.samples$mu.hat[-1] - ccvx.samples$mu.hat[1]
    x.vals <- ccvx.samples$doses[-1]
    std.err <- sqrt(ccvx.samples$std.err[1]^2 + (ccvx.samples$std.err[-1]^2))
  }

  # plot point estimates with std err bars
  plot(x.vals, y.vals, type = 'n', 
       xlab = xlab, ylab = ylab,
       xlim = range(ccvx.samples$doses),
       ylim = c(min(y.vals - abs(qnorm(delta)) * std.err), max(y.vals + abs(qnorm(delta)) * std.err)),
       main = paste0("Concavex Dose-Response Curve with \n", 100*cred.int.width, "% Credible Interval"))
  grid(lwd = 2)
  
  points(x.vals, y.vals, pch = 16, cex = 1.3)
  arrows(x.vals, 
         y.vals - abs(qnorm(delta)) * std.err, 
         x.vals, 
         y.vals + abs(qnorm(delta)) * std.err, 
         length=0.05, angle=90, code=3, lwd = 2)
  
  # add ccvx model fit and credible interval
  
  lines(dose.range.orig, y, lwd = 2)
  lines(dose.range.orig, pci.ll,  lty = 2, lwd = 2)
  lines(dose.range.orig, pci.ul, lty = 2, lwd = 2)
  
}



