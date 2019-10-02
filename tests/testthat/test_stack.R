for (container in c("Stack", "StackL")) {

context(container)

Container <- eval(as.name(container))

test_that("push, peek and pop", {
    s <- Container()
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

test_that("clear", {
    s <- Container()
    s$push("a")$push("b")$push("c")
    s$clear()
    expect_equal(s$size(), 0)
})

test_that("push NULL", {
    q <- Container()
    q$push(NULL)$push(NULL)
    expect_null(q$pop())
    expect_equal(q$size(), 1)
})

}
