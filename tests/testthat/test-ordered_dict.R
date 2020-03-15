for (container in c("OrderedDict", "OrderedDictL")) {

context(container)

Container <- eval(as.name(container))

test_that("push and pop", {
    d <- Container()
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
    d <- Container(list(b = 2, a = 1, c = 3))
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
    d <- Container(list(2, 1, 3), list("b", "a", "c"))
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
    d <- Container()
    d$set("b", 2)
    d$set("a", 1)
    d$set("c", 3)
    d$clear()
    expect_equal(d$size(), 0)
})

test_that("size is correctly calculated after remove", {
    d <- Container()
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
    d <- Container()
    d$set("a", 1)
    d$set("b", 2)
    d$set("a", 3)
    expect_equal(d$get("a"), 3)
    expect_length(d$keys(), 2)
    expect_length(d$values(), 2)
})


test_that("NULL and default", {
    d <- Container()
    d$set("b", NULL)
    expect_equal(d$get("b"), NULL)
    expect_equal(d$get("a", "default"), "default")
})

}

test_that("object indexing works", {
    d <- OrderedDict()
    s <- Stack()
    q <- Queue()
    f <- function() {
        NULL
    }
    d$set(s, 1)$set(q, 2)$set(f, 3)
    expect_equal(d$size(), 3)
    expect_equal(d$get(s), 1)
    expect_equal(d$get(q), 2)
    expect_equal(d$get(f), 3)
    expect_equal(d$keys(), list(s, q, f))
})
