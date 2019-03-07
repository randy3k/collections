for (container in c("Deque", "DequeL")) {

context(container)

Container <- eval(as.name(container))

test_that("push, peek and pop", {
    q <- Container$new()
    q$push(1)$push(2)
    expect_equal(q$size(), 2)
    expect_equal(q$peek(), 2)
    expect_equal(q$pop(), 2)
    expect_equal(q$size(), 1)
    q$push(3)
    expect_equal(q$size(), 2)
    expect_equal(q$peek(), 3)
    expect_equal(q$pop(), 3)
    expect_equal(q$peek(), 1)
    expect_equal(q$pop(), 1)
    expect_equal(q$size(), 0)
    expect_error(q$peek(), "empty")
    expect_error(q$pop(), "empty")
})


test_that("push, peek and pop", {
    q <- Container$new()
    q$pushleft(1)$pushleft(2)
    expect_equal(q$size(), 2)
    expect_equal(q$peekleft(), 2)
    expect_equal(q$popleft(), 2)
    expect_equal(q$size(), 1)
    q$pushleft(3)
    expect_equal(q$size(), 2)
    expect_equal(q$peekleft(), 3)
    expect_equal(q$popleft(), 3)
    expect_equal(q$peekleft(), 1)
    expect_equal(q$popleft(), 1)
    expect_equal(q$size(), 0)
    expect_error(q$popleft(), "empty")
    expect_error(q$peekleft(), "empty")
})

test_that("random push and pop", {
    q <- Container$new()
    q$pushleft(1)$push("a")$pushleft(2)$push("b")
    expect_equal(q$size(), 4)
    expect_equal(q$popleft(), 2)
    expect_equal(q$size(), 3)
    expect_equal(q$pop(), "b")
    expect_equal(q$size(), 2)
    expect_equal(q$popleft(), 1)
    expect_equal(q$size(), 1)
    expect_equal(q$pop(), "a")
    expect_equal(q$size(), 0)
    expect_error(q$pop(), "empty")
    expect_error(q$popleft(), "empty")
})

test_that("random push and pop 2", {
    q <- Container$new()
    q$pushleft(1)$push("a")$pushleft(2)$push("b")
    expect_equal(q$size(), 4)
    expect_equal(q$pop(), "b")
    expect_equal(q$size(), 3)
    expect_equal(q$popleft(), 2)
    expect_equal(q$size(), 2)
    expect_equal(q$pop(), "a")
    expect_equal(q$size(), 1)
    expect_equal(q$popleft(), 1)
    expect_equal(q$size(), 0)
    expect_error(q$pop(), "empty")
    expect_error(q$popleft(), "empty")
})

test_that("remove", {
    q <- Container$new()
    q$push("a")$pushleft(1)$push("b")$pushleft(2)
    expect_equal(q$size(), 4)
    q$remove("a")
    expect_equal(q$size(), 3)
    expect_equal(q$pop(), "b")
    expect_equal(q$size(), 2)
    q$remove(1)
    expect_equal(q$size(), 1)
    expect_equal(q$pop(), 2)
})


test_that("extend", {
    q <- Container$new()
    q2 <- Container$new()
    q2$push("a")$push("b")
    q$extend(q2)
    expect_equal(q$size(), 2)
    expect_equal(q$pop(), "b")
    q$extendleft(q2)
    expect_equal(q$popleft(), "b")
})

test_that("clear", {
    q <- Container$new()
    q$push("a")$push("b")
    q$clear()
    expect_equal(q$size(), 0)
})

}
