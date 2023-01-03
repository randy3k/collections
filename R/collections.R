#' @docType package
#' @useDynLib collections, .registration = TRUE, .fixes = "C_"
"_PACKAGE"

# dict_hash <- function(key) .Call(C_dict_hash, key)

#' @title Inspect objects
#' @description
#' `cls` is a replacement for the `class` function
#' which also works for the collection objects. It falls back to the ordinary `class` function
#' for other objects.
#' @param x a collection object
#' @examples
#' d <- dict()
#' cls(d)
#' @export
cls <- function(x) {
    if (is.environment(x) && identical(parent.env(x), asNamespace("collections"))) {
        return(x$.__class__)
    }
    return(class(x))
}
