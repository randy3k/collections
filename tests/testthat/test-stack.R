context("stack")

test_that("push, peek and pop", {
    s <- stack()
    s$push(1)$push(2)
    expect_equal(s$size(), 2)
    expect_equal(s$peek(), 2)
    expect_equal(s$pop(), 2)
    expect_equal(s$size(), 1)
    s$push(3)
    expect_equal(s$size(), 2)
    expect_equal(s$peek(), 3)
    expect_equal(s$pop(), 3)
    expect_equal(s$peek(), 1)
    expect_equal(s$pop(), 1)
    expect_equal(s$size(), 0)
    expect_error(s$pop(), "empty")
})

test_that("push, peek and pop with items", {
    q <- stack(list(1, 3))
    expect_equal(q$size(), 2)
    expect_equal(q$peek(), 3)
    expect_equal(q$pop(), 3)
    expect_equal(q$peek(), 1)
    expect_equal(q$pop(), 1)
    expect_equal(q$size(), 0)
    expect_error(q$peek(), "empty")
    expect_error(q$pop(), "empty")
})

test_that("clear", {
    s <- stack()
    s$push("a")$push("b")$push("c")
    s$clear()
    expect_equal(s$size(), 0)
})

test_that("push NULL", {
    q <- stack()
    q$push(NULL)$push(NULL)
    expect_null(q$pop())
    expect_equal(q$size(), 1)
})


test_that("serialization", {
    s <- stack()
    s$push(1)$push(2)
    s2 <- unserialize(serialize(s, NULL))
    expect_equal(s2$size(), 2)
    s2$push(3)
    expect_equal(s2$size(), 3)
})
