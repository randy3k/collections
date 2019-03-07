context("PriorityQueue")

test_that("push and pop", {
    q <- PriorityQueue$new()
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

test_that("as_list", {
    q <- PriorityQueue$new()
    q$push("a", 2)$push("b", 3)$push("c", 1)
    expect_equal(q$as_list(), list("b", "a", "c"))
})

test_that("as_list", {
    q <- PriorityQueue$new()
    q$push("a", 2)$push("b", 3)$push("d", 2)$push("c", 1)
    expect_equal(q$as_list(), list("b", "a", "d", "c"))
})

test_that("clear", {
    q <- PriorityQueue$new()
    q$push("a", 2)$push("b", 3)$push("d", 2)$push("c", 1)
    q$clear()
    expect_equal(q$size(), 0)
})
