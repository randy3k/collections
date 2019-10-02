for (container in c("Dict", "DictL")) {

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

test_that("NULL and default", {
    d <- Container()
    d$set("b", NULL)
    expect_equal(d$get("b"), NULL)
    expect_equal(d$get("a", "default"), "default")
})

if (container == "Dict") {
    test_that("grow and shrink", {
        d <- Dict()
        for (i in 1:100) d$set(paste0("key", i), i)
        len <- d$size()
        expect_gt(length(d$ks), len)
        for (i in 1:99) d$remove(paste0("key", i))
        expect_lt(length(d$ks), len)
    })
}

test_that("serialize and unserialized", {
    d <- Container()
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

}
