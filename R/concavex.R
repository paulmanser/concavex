#' concavex: A package for Bayesian dose-response modeling
#'
#' concavex is an R package for fitting dose response curves. It conceptually borrows from the MCPMod approach to dose response 
#' curve fitting and only requires point estimates of effects and their standard errors at tested doses. Model fitting uses a fully 
#' Bayesian approach, and the concavex package is largely a wrapper for easily implementing and generating outputs with JAGS and the 
#' rjags package.
#' 
#' 
#' @section Functions:
#' \link{ccvx_build_jags}    Build JAGS code for 3-parameter model
#' \link{ccvx5_build_jags}   Build JAGS code for 5-parameter model
#' \link{ccvx_fit}           Fit concavex model with JAGS Gibbs Sampler
#' \link{ccvx_plot_fit}      Plot fitted model vs data
#' \link{ccvx_hist_post}     Plot histograms of parameter posteriors
#' \link{ccvx_risk_profile}  Plot risk profiles
#' \link{ccvx_ddcp_plot}     Plot DDCP Phase 3 risks
#' \link{available_docs}     List available supporting documentation
#' \link{open_docs}          Open .pdf doc files listed in \link{available_docs}
#'
#' @section Examples
#' 
#' See Concavex User Guide for examples:
#' 
#' ## Not run:
#' 
#' open_docs("User Guide")
#' 
#' ## End (Not run)
#'
#' @section See Also
#'
#' Link to eventual concavex paper
#' Other Bayesian propaganda
#'
#' @docType package
#' @name concavex
NULL