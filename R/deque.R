#' @export
Deque <- R6::R6Class("Deque",
    cloneable = FALSE,
    private = list(
        q = NULL,
        last = NULL
    ),
    public = list(
        push = function(item) {
            invisible(.Call("deque_push", PACKAGE = "collections", private, item))
        },
        pushleft = function(item) {
            invisible(.Call("deque_pushleft", PACKAGE = "collections", private, item))
        },
        pop = function() {
            .Call("deque_pop", PACKAGE = "collections", private)
        },
        popleft = function() {
            .Call("deque_popleft", PACKAGE = "collections", private)
        },
        extend = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$q
            while (!is.null(q)) {
                v <- pairlist_car(q)
                self$push(v[[2]])
                q <- pairlist_cdr(q)
            }
            invisible(self)
        },
        extendleft = function(deque) {
            !inherits(deque, "Deque") && stop("expect Deque object")
            q <- deque$.__enclos_env__$private$last
            while (!is.null(q)) {
                v <- pairlist_car(q)
                self$pushleft(v[[2]])
                q <- v[[1]]
            }
            invisible(self)
        },
        remove = function(value) {
            invisible(.Call("deque_remove", PACKAGE = "collections", private, value))
        },
        size = function() length(private$q),
        as_list = function() {
            ret <- list()
            i <- 0
            x <- private$q
            while (!is.null(x)) {
                i <- i + 1
                ret[[i]] <- pairlist_car(x)[[2]]
                x <- pairlist_cdr(x)
            }
            ret
        }
    )
)


#' @export
DequeL <- R6::R6Class("DequeL",
    cloneable = FALSE,
    private = list(
        q = list(),
        n = 0
    ),
    public = list(
        push = function(item) {
            private$q[[private$n + 1]] <- item
            private$n <- private$n + 1
            invisible(item)
        },
        pushleft = function(item) {
            private$q <- c(item, private$q)
            private$n <- private$n + 1
            invisible(item)
        },
        pop = function() {
            v <- private$q[[private$n + 1]]
            private$q <- private$q[-private$n]
            private$n <- private$n - 1
            v
        },
        popleft = function() {
            v <- private$q[[1]]
            private$q <- private$q[-1]
            private$n <- private$n - 1
            v
        },
        extend = function(deque) {
            !inherits(deque, "DequeL") && stop("expect DequeL object")
            q <- deque$.__enclos_env__$private$q
            private$q <- c(private$q, q)
            invisible(self)
        },
        extendleft = function(deque) {
            !inherits(deque, "DequeL") && stop("expect DequeL object")
            q <- deque$.__enclos_env__$private$q
            private$q <- c(rev(q), private$q)
            invisible(self)
        },
        remove = function(value) {
            ind <- match(value, private$q)
            if (is.na(ind)) stop("value not found")
            private$q <- private$q[-ind]
            invisible(value)
        },
        size = function() length(private$q),
        as_list = function() private$q
    )
)
