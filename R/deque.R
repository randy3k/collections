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
