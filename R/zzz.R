class(Stack) <- "StackMeta"

#' @method $ StackMeta
#' @export
`$.StackMeta` <- function(x, key) {
    .Deprecated("Stack", old = "Stack$new")
    x
}


class(StackL) <- "StackLMeta"

#' @method $ StackLMeta
#' @export
`$.StackLMeta` <- function(x, key) {
    .Deprecated("StackL", old = "StackL$new")
    x
}


class(Queue) <- "QueueMeta"

#' @method $ QueueMeta
#' @export
`$.QueueMeta` <- function(x, key) {
    .Deprecated("Queue", old = "Queue$new")
    x
}


class(QueueL) <- "QueueLMeta"

#' @method $ QueueLMeta
#' @export
`$.QueueLMeta` <- function(x, key) {
    .Deprecated("QueueL", old = "QueueL$new")
    x
}


class(PriorityQueue) <- "PriorityQueueMeta"

#' @method $ PriorityQueueMeta
#' @export
`$.PriorityQueueMeta` <- function(x, key) {
    .Deprecated("PriorityQueue", old = "PriorityQueue$new")
    x
}


class(Deque) <- "DequeMeta"

#' @method $ DequeMeta
#' @export
`$.DequeMeta` <- function(x, key) {
    .Deprecated("Deque", old = "Deque$new")
    x
}


class(DequeL) <- "DequeLMeta"

#' @method $ DequeLMeta
#' @export
`$.DequeLMeta` <- function(x, key) {
    .Deprecated("DequeL", old = "DequeL$new")
    x
}


class(OrderedDict) <- "OrderedDictMeta"

#' @method $ OrderedDictMeta
#' @export
`$.OrderedDictMeta` <- function(x, key) {
    .Deprecated("OrderedDict", old = "OrderedDict$new")
    x
}


class(OrderedDictL) <- "OrderedDictLMeta"

#' @method $ OrderedDictLMeta
#' @export
`$.OrderedDictLMeta` <- function(x, key) {
    .Deprecated("OrderedDictL", old = "OrderedDictL$new")
    x
}


class(Dict) <- "DictMeta"

#' @method $ DictMeta
#' @export
`$.DictMeta` <- function(x, key) {
    .Deprecated("Dict", old = "Dict$new")
    x
}

class(DictL) <- "DictLMeta"

#' @method $ DictLMeta
#' @export
`$.DictLMeta` <- function(x, key) {
    .Deprecated("DictL", old = "DictL$new")
    x
}
