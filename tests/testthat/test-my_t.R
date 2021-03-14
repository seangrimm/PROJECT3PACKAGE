test_that("T-test works", {
  expect_equal(my_t.test(c(1, 2, 3, 4, 5), "two.sided", 1)[[1]], 2.828,
               tolerance = .001)
  expect_equal(my_t.test(c(1, 2, 3, 4, 5), "two.sided", 1)[[2]], 4)
  expect_equal(my_t.test(c(1, 2, 3, 4, 5), "two.sided", 1)[[3]], "two.sided")
  expect_equal(my_t.test(c(1, 2, 3, 4, 5), "two.sided", 1)[[4]], 0.0474,
               tolerance = .0005)
  expect_equal(my_t.test(c(1, 2, 3, 4, 5), "greater", 1)[[4]], .0237,
               tolerance = .0005)
  expect_equal(my_t.test(c(1, 2, 3, 4, 5), "less", 1)[[4]], .9763,
               tolerance = .0005)
})
test_that("Errors/messages thrown correctly", {
  expect_error(my_t.test(c(1, 2, 3), "two.side", 1))
  expect_error(my_t.test(c(1, 2, 3), "two.sided", "string"))
})
