#' @docType package
#' @useDynLib collections, .registration = TRUE, .fixes = "C_"
"_PACKAGE"

# helper to identify if default is missing
missing_arg <- function() .Call(C_missing_arg)
