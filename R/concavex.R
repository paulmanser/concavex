#' concavex: A package for Bayesian dose-response modeling
#'
#' concavex is an R package for fitting dose response curves. It conceptually borrows from the MCPMod approach to dose response 
#' curve fitting and only requires point estimates of effects and their standard errors at tested doses. Model fitting uses a fully 
#' Bayesian approach, and the concavex package is largely a wrapper for easily implementing and generating outputs with JAGS and the 
#' rjags package.
#' 
#' 
#' @section Functions:
#' \itemize{
#' \item{\link{ccvx_build_jags}}    {Build JAGS code for 3-parameter model }
#' \item{\link{ccvx5_build_jags}}   {Build JAGS code for 5-parameter model }
#' \item{\link{ccvx_fit}}          {Fit concavex model with JAGS Gibbs Sampler }
#' \item{\link{ccvx_plot_fit}}      {Plot fitted model vs data }
#' \item{\link{ccvx_hist_post}}     {Plot histograms of parameter posteriors }
#' \item{\link{ccvx_risk_profile}}  {Plot risk profiles }
#' \item{\link{ccvx_ddcp_plot}}     {Plot DDCP Phase 3 risks }
#' \item{\link{available_docs}}     {List available supporting documentation }
#' \item{\link{open_doc}}          {Open .pdf doc files listed in \link{available_docs} }
#' }
#' @section Examples:
#' 
#' Open User Guide for examples:
#' 
#' ## Not run:
#' 
#' open_doc("User Guide")
#' 
#' ## End (Not run)
#'
#' @section See Also:
#'
#' Link to eventual concavex paper
#' 
#' Other Bayesian propaganda
#'
#' @docType package
#' @name concavex
NULL