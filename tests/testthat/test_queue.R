context("Queue")

test_that("push and pop", {
    q <- Queue$new()
    q$push(1)
    q$push(2)
    expect_equal(q$size(), 2)
    expect_equal(q$pop(), 1)
    expect_equal(q$size(), 1)
    q$push(3)
    expect_equal(q$size(), 2)
    expect_equal(q$pop(), 2)
    expect_equal(q$pop(), 3)
    expect_equal(q$size(), 0)
    expect_error(q$pop(), "empty")
})


context("QueueL")

test_that("push and pop", {
    q <- QueueL$new()
    q$push(1)
    q$push(2)
    expect_equal(q$size(), 2)
    expect_equal(q$pop(), 1)
    expect_equal(q$size(), 1)
    q$push(3)
    expect_equal(q$size(), 2)
    expect_equal(q$pop(), 2)
    expect_equal(q$pop(), 3)
    expect_equal(q$size(), 0)
    expect_error(q$pop(), "empty")
})
