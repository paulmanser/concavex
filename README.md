# concavex

`concavex` is an R package for fitting dose response curves. It conceptually borrows from the MCPMod approach to dose response curve fitting and only requires point estimates and standard errors of the endpoint at tested doses. Model fitting uses a fully Bayesian approach, and the `concavex` package is largely a wrapper for implementing and generating outputs using JAGS software and the `rjags` package.

As such, `concavex` requires a local JAGS installation, which may be downloaded from this link: http://mcmc-jags.sourceforge.net/.


After the `concavex` package has been installed, a list of available help files can be displayed using the `available_docs()` function. These files can be opened directly from within R using the `open_doc()` function. By default, `open_doc()` opens the main vignette if no docs are otherwise specified.