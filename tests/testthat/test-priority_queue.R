context("priority_queue")

test_that("push and pop", {
    q <- priority_queue()
    q$push("a", 2)$push("b", 3)$push("c", 1)
    expect_equal(q$size(), 3)
    expect_equal(q$pop(), "b")
    expect_equal(q$size(), 2)
    q$push("d", 5)
    expect_equal(q$size(), 3)
    expect_equal(q$pop(), "d")
    expect_equal(q$pop(), "a")
    expect_equal(q$pop(), "c")
    expect_equal(q$size(), 0)
    expect_error(q$pop(), "empty")
})

test_that("push and pop with items", {
    q <- priority_queue(list("a", "b", "c"), c(2, 3, 1))
    expect_equal(q$size(), 3)
    expect_equal(q$pop(), "b")
    expect_equal(q$size(), 2)
    q$push("d", 5)
    expect_equal(q$size(), 3)
    expect_equal(q$pop(), "d")
    expect_equal(q$pop(), "a")
    expect_equal(q$pop(), "c")
    expect_equal(q$size(), 0)
    expect_error(q$pop(), "empty")
})

test_that("as_list", {
    q <- priority_queue()
    q$push("a", 2)$push("b", 3)$push("c", 1)
    expect_equal(q$as_list(), list("b", "a", "c"))
})

test_that("as_list", {
    q <- priority_queue()
    q$push("a", 2)$push("b", 3)$push("d", 2)$push("c", 1)
    expect_equal(q$as_list(), list("b", "a", "d", "c"))
})

test_that("clear", {
    q <- priority_queue()
    q$push("a", 2)$push("b", 3)$push("d", 2)$push("c", 1)
    q$clear()
    expect_equal(q$size(), 0)
})

test_that("push NULL", {
    q <- priority_queue()
    q$push(NULL)$push(NULL)
    expect_null(q$pop())
    expect_equal(q$size(), 1)
})


test_that("grow and shrink", {
    q <- priority_queue()
    for (i in 1:100) q$push(i)
    len <- q$size()
    expect_gt(length(q$self$h), len)
    for (i in 1:99) q$pop()
    expect_lt(length(q$self$h), len)
})


test_that("serialization", {
    q <- priority_queue()
    q$push(1)$push(2)
    q2 <- unserialize(serialize(q, NULL))
    expect_equal(q2$size(), 2)
    q2$push(3)
    expect_equal(q2$size(), 3)
})
