#' @docType package
#' @useDynLib collections, .registration = TRUE, .fixes = "C_"
"_PACKAGE"

# helper to identify if default is missing
missing_arg <- function(default) if (missing(default)) .Call(C_missing_arg) else default
