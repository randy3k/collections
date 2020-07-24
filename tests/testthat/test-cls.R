context("cls")

test_that("cls works", {
    expect_equal(cls(deque()), "deque")
    expect_equal(cls(dict()), "dict")
    expect_equal(cls(ordered_dict()), "ordered_dict")
    expect_equal(cls(priority_queue()), "priority_queue")
    expect_equal(cls(queue()), "queue")
    expect_equal(cls(stack()), "stack")
    expect_true("matrix" %in% cls(matrix()))
})
