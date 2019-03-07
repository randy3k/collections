#' @title Double Ended Queue
#' @description
#' The `Deque` class creates a double ended queue with pairlist backend.
#' @section Usage:
#' \preformatted{
#' Deque$new(items = NULL)
#' Deque$push(item)
#' Deque$pushleft(item)
#' Deque$pop()
#' Deque$popleft()
#' Deque$peek()
#' Deque$peekleft()
#' Deque$extend(q)
#' Deque$extendleft(q)
#' Deque$remove(item)
#' Deque$clear()
#' Deque$size()
#' Deque$as_list()
#' }
#' @section Arguments:
#' * `items`: initialization list
#' * `item`: any R object
#' * `q`: a Deque object
#' @examples
#' q <- Deque$new()
#' q$push("foo")
#' q$push("bar")
#' q$pushleft("baz")
#' q$pop()  # bar
#' q$popleft()  # baz
#'
#' q <- Deque$new(list("foo", "bar"))
#' q$push("baz")$pushleft("bla")
#' @seealso [DequeL]
#' @export
Deque <- R6::R6Class("Deque",
    cloneable = FALSE,
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        initialize = function(items = NULL) {
            self$clear()
            for (i in seq_along(items)) {
                self$push(items[[i]])
            }
        },
        push = function(item) {
            .Call("deque_push", PACKAGE = "collections", private, item)
            invisible(self)
        },
        pushleft = function(item) {
            .Call("deque_pushleft", PACKAGE = "collections", private, item)
            invisible(self)

        },
        pop = function() {
            .Call("deque_pop", PACKAGE = "collections", private)
        },
        popleft = function() {
            .Call("deque_popleft", PACKAGE = "collections", private)
        },
        peek = function() {
            if (is.null(private$last)) stop("deque is empty")
            .Call("pairlist_car", PACKAGE = "collections", private$last)[[2]]
        },
        peekleft = function() {
            if (is.null(private$q)) stop("deque is empty")
            .Call("pairlist_car", PACKAGE = "collections", private$q)[[2]]
        },
        extend = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$q
            while (!is.null(q)) {
                v <- .Call("pairlist_car", PACKAGE = "collections", q)
                self$push(v[[2]])
                q <- .Call("pairlist_cdr", PACKAGE = "collections", q)
            }
            invisible(self)
        },
        extendleft = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$q
            while (!is.null(q)) {
                v <- .Call("pairlist_car", PACKAGE = "collections", q)
                self$pushleft(v[[2]])
                q <- .Call("pairlist_cdr", PACKAGE = "collections", q)
            }
            invisible(self)
        },
        clear = function() {
            private$q <- NULL
            private$last <- NULL
        },
        remove = function(item) {
            .Call("deque_remove", PACKAGE = "collections", private, item)
            invisible(self)
        },
        size = function() length(private$q),
        as_list = function() {
            n <- self$size()
            ret <- vector("list", n)
            x <- private$q
            for (i in seq_len(n)) {
                ret[[i]] <- .Call("pairlist_car", PACKAGE = "collections", x)[[2]]
                x <- .Call("pairlist_cdr", PACKAGE = "collections", x)
            }
            ret
        },
        print = function() {
            n <- self$size()
            cat("Deque object with", n, "item(s).\n")
        }
    )
)


#' @title Double Ended Queue (list based)
#' @description
#' The `DequeL` class creates a double ended queue with list backend.
#' Pure R implementation, mainly for benchmark.
#' @section Usage:
#' \preformatted{
#' DequeL$new(items = NULL)
#' DequeL$push(item)
#' DequeL$pushleft(item)
#' DequeL$pop()
#' DequeL$popleft()
#' DequeL$peek()
#' DequeL$peekleft()
#' DequeL$extend(q)
#' DequeL$extendleft(q)
#' DequeL$clear()
#' DequeL$remove(item)
#' DequeL$size()
#' DequeL$as_list()
#' }
#' @section Arguments:
#' * `items`: initialization list
#' * `item`: any R object
#' * `q`: a DequeL object
#' @examples
#' q <- DequeL$new()
#' q$push("foo")
#' q$push("bar")
#' q$pushleft("baz")
#' q$pop()  # bar
#' q$popleft()  # baz
#'
#' q <- DequeL$new(list("foo", "bar"))
#' q$push("baz")$pushleft("bla")
#' @seealso [Deque]
#' @export
DequeL <- R6::R6Class("DequeL",
    cloneable = FALSE,
    private = list(
        q = NULL,
        n = NULL
    ),
    public = list(
        initialize = function(items = NULL) {
            self$clear()
            for (i in seq_along(items)) {
                self$push(items[[i]])
            }
        },
        push = function(item) {
            private$q[[private$n + 1]] <- item
            private$n <- private$n + 1
            invisible(self)
        },
        pushleft = function(item) {
            private$q <- c(item, private$q)
            private$n <- private$n + 1
            invisible(self)
        },
        pop = function() {
            if (private$n == 0) stop("deque is empty")
            v <- private$q[[private$n]]
            private$q <- private$q[-private$n]
            private$n <- private$n - 1
            v
        },
        popleft = function() {
            if (private$n == 0) stop("deque is empty")
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        peek = function() {
            if (private$n == 0) stop("deque is empty")
            private$q[[private$n]]
        },
        peekleft = function() {
            if (private$n == 0) stop("deque is empty")
            private$q[[1]]
        },
        extend = function(deque) {
            !inherits(deque, "DequeL") && stop("expect DequeL object")
            q <- deque$.__enclos_env__$private$q
            private$q <- c(private$q, q)
            private$n <- length(private$q)
            invisible(self)
        },
        extendleft = function(deque) {
            !inherits(deque, "DequeL") && stop("expect DequeL object")
            q <- deque$.__enclos_env__$private$q
            private$q <- c(rev(q), private$q)
            private$n <- length(private$q)
            invisible(self)
        },
        clear = function() {
            private$q <- list()
            private$n <- 0
        },
        remove = function(value) {
            ind <- match(value, private$q)
            if (is.na(ind)) stop("value not found")
            private$q <- private$q[-ind]
            private$n <- private$n - 1
            invisible(self)
        },
        size = function() length(private$q),
        as_list = function() private$q,
        print = function() {
            n <- self$size()
            cat("DequeL object with", n, "item(s).\n")
        }
    )
)
