#' @docType package
#' @useDynLib collections, .registration = TRUE, .fixes = "C_"
"_PACKAGE"

dict_hash <- function(key) .Call(C_dict_hash, key)
