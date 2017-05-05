#' Show available documents
#' 
#' Show available documents.
#' 
#' @author Kwame Okrah, \email{okrah.kwame@gene.com}
#' 
#' @export

available_docs <- function() {
  cat("=============================================================\n")
  cat("The following documents are currently available in concavex:\n")
  cat("-------------------------------------------------------------\n")
  allowed.docs = c("1. User Guide")
  allowed.docs = paste(allowed.docs, collapse="\n")
  cat(allowed.docs)
  cat("\n")
  cat("-------------------------------------------------------------\n")
  cat("To open a document, call the function **open_doc(doc)**, where\n")
  cat("doc is any of the documents specified above;\n")
  cat('e.g. open_doc("User Guide")\n')
}



#' Access package documents
#' 
#' This function allows the user the get access to pdf documents associated
#' with this package.
#' 
#' @author Kwame Okrah, \email{okrah.kwame@gene.com}
#' 
#' @param doc a character vector specifying the document to open;
#' see \code{\link{available_docs}}
#' @param verbose a logical vector indicating whether the path of the 
#' document should be printed (Default = FALSE)
#' 
#' @export

open_doc <- function(doc = "User Guide", verbose = FALSE) {
  allowed.docs = c("User Guide")
  
  check.doc <- doc %in% allowed.docs
  
  if (!check.doc) {
    m0 = "\n==================================="
    m1 = "\n*doc* must be one of the following:"
    m2 = "\n-----------------------------------\n"
    m3 = paste(allowed.docs, collapse="\n")
    msg = paste0(m0, m1, m2, m3)
    stop(msg, call. = FALSE)
  }
  
  if (doc == "User Guide") doc = "ccvx-vignette.pdf"
  
  f = system.file("doc", doc, package = "concavex")
  
  if (.Platform$OS.type == "windows") {
    shell.exec(f)
  }else{
    system(paste(Sys.getenv("R_PDFVIEWER"), f, "&"))
  }
  
  if (verbose) {
    return(f)
  }
}