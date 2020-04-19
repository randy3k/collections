patch_symbols <- function(expr, public, private) {
    if (is.symbol(expr)) {
        if (as.character(expr) %in% public) {
            return(as.call(list(as.name("$"), as.name("self"), expr)))
        } else if (as.character(expr) %in% private) {
            return(as.call(list(as.name("$"), as.name("self"), expr)))
        } else {
            return(expr)
        }
    } else if (is.expression(expr)) {
        as.expression(lapply(expr, patch_symbols, public = public, private = private))
    } else if (is.call(expr)) {
        if (expr[[1]] == "function") {
            # remove arguments from known variables
            argnames <- names(as.list(expr[[2]]))
            public <- setdiff(public, argnames)
            private <- setdiff(private, argnames)
        }
        as.call(lapply(expr, patch_symbols, public = public, private = private))
    } else {
        return(expr)
    }
}


resolve_objects <- function(constructor) {
    stopifnot(is.function(constructor))
    args <- as.list(formals(constructor))
    exprs <- body(constructor)

    fields <- list()
    methods <- list()

    for (i in seq_along(exprs)) {
        expr <- exprs[[i]]
        if (is.call(expr) && class(expr) == "<-") {
            lhs <- as.character(expr[[2]])
            rhs <- expr[[3]]
            if (length(rhs) > 1 && rhs[[1]] == "function" && !startsWith(lhs, ".")) {
                methods [lhs] <- list(rhs)
            } else if (!hasName(args, lhs) && lhs != "self" && !startsWith(lhs, ".")) {
                fields[lhs] <- list(rhs)
            }
        }
    }

    public <- c(names(methods), names(fields))
    private <- list()

    for (m in seq_along(methods)) {
        methods[[m]] <- patch_symbols(methods[[m]], public, private)
    }

    list(fields = fields, methods = methods)
}


R6Class <- function(classname, constructor) {
    env <- environment(constructor)
    objects <- resolve_objects(constructor)

    fields <- objects$fields
    methods <- objects$methods
    cls <- R6::R6Class(classname, parent_env = env)
    for (n in names(fields)) {
        x <- eval(fields[[n]], envir = env)
        cls$set("public", n, x)
    }
    for (n in names(methods)) {
        f <- eval(methods[[n]], envir = env)
        cls$set("public", n, f)
    }
    cls
}
