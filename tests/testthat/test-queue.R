context("queue")


test_that("push, peek and pop", {
    q <- queue()
    q$push(1)$push(2)
    expect_equal(q$size(), 2)
    expect_equal(q$peek(), 1)
    expect_equal(q$pop(), 1)
    expect_equal(q$size(), 1)
    q$push(3)
    expect_equal(q$size(), 2)
    expect_equal(q$peek(), 2)
    expect_equal(q$pop(), 2)
    expect_equal(q$peek(), 3)
    expect_equal(q$pop(), 3)
    expect_equal(q$size(), 0)
    expect_error(q$peek(), "empty")
    expect_error(q$pop(), "empty")
})

test_that("push, peek and pop with items", {
    q <- queue(list(2, 3))
    expect_equal(q$size(), 2)
    expect_equal(q$peek(), 2)
    expect_equal(q$pop(), 2)
    expect_equal(q$peek(), 3)
    expect_equal(q$pop(), 3)
    expect_equal(q$size(), 0)
    expect_error(q$peek(), "empty")
    expect_error(q$pop(), "empty")
})

test_that("clear", {
    q <- queue()
    q$push("a")$push("b")$push("c")
    q$clear()
    expect_equal(q$size(), 0)
})

test_that("push NULL", {
    q <- queue()
    q$push(NULL)$push(NULL)
    expect_null(q$pop())
    expect_equal(q$size(), 1)
})
