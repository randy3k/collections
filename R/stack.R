#' @title Stack
#' @description
#' The `Stack` function creates a stack.
#' @section Usage:
#' \preformatted{
#' Stack(items = NULL)
#' .$push(item)
#' .$pop()
#' .$peek()
#' .$clear()
#' .$size()
#' .$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
#' * `item`: any R object
#' @examples
#' s <- Stack()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#'
#' s <- Stack(list("foo", "bar"))
#' s$push("baz")$push("bla")
#' @seealso [StackL]
#' @export
Stack <- function(...) {
    self <- environment()
    q <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        .Call("stack_push", PACKAGE = "collections", self, item)
        invisible(self)
    }
    pop <- function() {
        .Call("stack_pop", PACKAGE = "collections", self)
    }
    peek <- function() {
        if (is.null(q)) stop("stack is empty")
        .Call("pairlist_car", PACKAGE = "collections", q)
    }
    clear <- function() {
        q <<- NULL
        invisible(self)
    }
    size <- function() length(q)
    as_list <- function() as.list(q)

    initialize(...)
    class(self) <- "Stack"
    self
}


#' @method print Stack
#' @export
print.Stack <- function(self) {
    n <- self$size()
    cat("Stack object with", n, "item(s)\n")
}


#' @title StackL (R implementation)
#' @description
#' The `StackL` function creates a stack.
#' Pure R implementation for benchmarking.
#' @section Usage:
#' \preformatted{
#' StackL(items = NULL)
#' .$push(item)
#' .$pop()
#' .$peek()
#' .$clear()
#' .$size()
#' .$as_list()
#' }
#' @section Argument:
#' * `items`: initialization list
#' * `item`: any R object
#' @examples
#' s <- StackL()
#' s$push("first")
#' s$push("second")
#' s$pop()  # second
#' s$pop()  # first
#'
#' s <- stack(list("foo", "bar"))
#' s$push("baz")$push("bla")
#' @seealso [Stack]
#' @export
StackL <- function(...) {
    self <- environment()

    q <- NULL
    n <- NULL

    initialize <- function(items = NULL) {
        clear()
        for (i in seq_along(items)) {
            push(items[[i]])
        }
    }
    push <- function(item) {
        q <<- c(item, q)
        n <<- n + 1
        invisible(self)
    }
    pop <- function() {
        if (n == 0) stop("stack is empty")
        v <- q[[1]]
        q <<- q[-1]
        n <<- n - 1
        v
    }
    peek <- function() {
        if (n == 0) stop("stack is empty")
        q[[1]]
    }
    clear <- function() {
        q <<- list()
        n <<- 0
        invisible(self)
    }
    size <- function() n
    as_list <- function() q

    initialize(...)
    class(self) <- "StackL"
    self
}


#' @method print StackL
#' @export
print.StackL <- function(self) {
    n <- self$size()
    cat("StackL object with", n, "item(s)\n")
}
