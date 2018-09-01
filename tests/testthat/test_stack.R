context("Stack")

test_that("push and pop", {
    s <- Stack$new()
    s$push(1)
    s$push(2)
    expect_equal(s$size(), 2)
    expect_equal(s$pop(), 2)
    expect_equal(s$size(), 1)
    s$push(3)
    expect_equal(s$size(), 2)
    expect_equal(s$pop(), 3)
    expect_equal(s$pop(), 1)
    expect_equal(s$size(), 0)
    expect_error(s$pop(), "empty")
})


context("StackL")

test_that("push and pop", {
    s <- Stack$new()
    s$push(1)
    s$push(2)
    expect_equal(s$size(), 2)
    expect_equal(s$pop(), 2)
    expect_equal(s$size(), 1)
    s$push(3)
    expect_equal(s$size(), 2)
    expect_equal(s$pop(), 3)
    expect_equal(s$pop(), 1)
    expect_equal(s$size(), 0)
    expect_error(s$pop(), "empty")
})
