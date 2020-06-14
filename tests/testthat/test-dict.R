
context("dict")

test_that("push and pop", {
    d <- dict()
    d$set("b", 2)
    d$set("a", 1)
    d$set("c", 3)
    expect_equal(d$size(), 3)
    expect_equal(d$get("b"), 2)
    expect_equal(d$get("a"), 1)
    expect_equal(d$get("c"), 3)
    expect_error(d$get("d"), "not found")
    expect_error(d$pop("d"), "not found")
    d$remove("b")
    expect_equal(d$size(), 2)
    expect_equal(d$pop("a"), 1)
    expect_equal(d$size(), 1)
    d$set("c", 5)
    expect_equal(d$get("c"), 5)
    expect_equal(d$size(), 1)
    expect_equal(d$pop("c"), 5)
})

test_that("push and pop with items", {
    d <- dict(list(b = 2, a = 1, c = 3))
    expect_equal(d$size(), 3)
    expect_equal(d$get("b"), 2)
    expect_equal(d$get("a"), 1)
    expect_equal(d$get("c"), 3)
    expect_error(d$get("d"), "not found")
    expect_error(d$pop("d"), "not found")
    d$remove("b")
    expect_equal(d$size(), 2)
    expect_equal(d$pop("a"), 1)
    expect_equal(d$size(), 1)
    d$set("c", 5)
    expect_equal(d$get("c"), 5)
    expect_equal(d$size(), 1)
    expect_equal(d$pop("c"), 5)
})

test_that("push and pop with items and keys", {
    d <- dict(list(2, 1, 3), list("b", "a", "c"))
    expect_equal(d$size(), 3)
    expect_equal(d$get("b"), 2)
    expect_equal(d$get("a"), 1)
    expect_equal(d$get("c"), 3)
    expect_error(d$get("d"), "not found")
    expect_error(d$pop("d"), "not found")
    d$remove("b")
    expect_equal(d$size(), 2)
    expect_equal(d$pop("a"), 1)
    expect_equal(d$size(), 1)
    d$set("c", 5)
    expect_equal(d$get("c"), 5)
    expect_equal(d$size(), 1)
    expect_equal(d$pop("c"), 5)
})

test_that("clear", {
    d <- dict()
    d$set("b", 2)
    d$set("a", 1)
    d$set("c", 3)

    d$clear()
    expect_equal(d$size(), 0)
})

test_that("NULL and default", {
    d <- dict()
    d$set("b", NULL)
    expect_equal(d$get("b"), NULL)
    expect_equal(d$get("a", "default"), "default")
})

test_that("size is correctly calculated after remove", {
    d <- dict()
    for (l in LETTERS) {
        d$set(l, 1)
    }
    expect_equal(d$size(), 26)
    for (l in LETTERS) {
        d$remove(l)
    }
    expect_equal(d$size(), 0)
    d$set("A", 1)
    expect_equal(d$size(), 1)
})

test_that("set a key twice", {
    d <- dict()
    d$set("a", 1)
    d$set("b", 2)
    d$set("a", 3)
    expect_equal(d$get("a"), 3)
    expect_length(d$keys(), 2)
    expect_length(d$values(), 2)
})

test_that("serialize and unserialized", {
    d <- dict()
    d$set("b", 2)
    d$set("a", 1)
    d$set("c", 3)
    d$remove("c")
    d2 <- unserialize(serialize(d, NULL))
    expect_equal(d2$get("a"), 1)

    d$remove("a")
    d2 <- unserialize(serialize(d, NULL))
    expect_error(d2$get("a"), "not found")
})


# test_that("hole is not popped if key error", {
#     d <- dict()
#     d$set("a", 1)$set("b", 2)$set("c", 3)
#     d$remove("b")
#     expect_equal(d$size(), 2)
#     expect_equal(tryCatch(
#         d$set("", 2),
#         error = function(e) NULL
#     ), NULL)
#     expect_equal(d$size(), 2)
#     d$set("b", 2)
#     expect_equal(d$vs[[2]], 2L)
# })

test_that("grow and shrink", {
    d <- dict()
    for (i in 1:100) d$set(paste0("key", i), i)
    len <- d$size()
    expect_gt(length(d$ks), len)
    for (i in 1:99) d$remove(paste0("key", i))
    expect_lt(length(d$ks), len)
    expect_length(d$keys(), 1)
})

test_that("object indexing works", {
    d <- dict()
    s <- stack()
    q <- queue()
    f <- function() {
        NULL
    }
    g <- function(x) {
        x
    }
    d$set(s, 1)$set(q, 2)$set(f, 3)$set(g, 4)
    expect_equal(d$size(), 4)
    expect_equal(d$get(s), 1)
    expect_equal(d$get(q), 2)
    expect_equal(d$get(f), 3)
    expect_equal(d$get(g), 4)
    expect_equal(d$keys(), list(s, q, f, g))
})


test_that("vector indexing works", {
    d <- dict()
    a <- c(1L, 2L)
    b <- list(key1 = "value1", key2 = 2)
    d$set(a, 1)$set(b, 2)
    c <- list(key1 = "value1", key2 = 2, key3 = list(1, 2, LETTERS))
    d$set(a, 1)$set(b, 2)$set(c, 3)
    expect_equal(d$size(), 3)
    expect_equal(d$get(c(1L, 2L)), 1)
    expect_equal(d$get(list(key1 = "value1", key2 = 2)), 2)
    expect_error(d$get(c(1, 2)))
    expect_equal(d$get(c), 3)
})


test_that("attributes must be hashable", {
    d <- dict()
    a <- 1:4
    e <- new.env()
    d$set(a, 1)
    expect_equal(d$get(a), 1)
    attr(a, "e") <- e
    expect_error(d$set(a, 2), "not hashable")
})
