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

test_that("clear", {
    d <- Container()
    d$set("b", 2)
    d$set("a", 1)
    d$set("c", 3)
    d$clear()
    expect_equal(d$size(), 0)
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
