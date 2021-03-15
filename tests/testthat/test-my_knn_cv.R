test_that("proper output", {
  expect_equal(my_knn_cv(na.omit(my_penguins)[,3:6], na.omit(my_penguins)[1],
                         5, 1)[[2]], 1)
})

test_that("errors received when appropriate", {
  expect_error(my_knn_cv(2, na.omit(my_penguins)[1], 1, 5))
  expect_error(my_knn_cv(na.omit(my_penguins)[,3:6], 2, 1, 5))
  expect_error(my_knn_cv(na.omit(my_penguins)[,3:6], na.omit(my_penguins)[1],
                         .1, 1))
})
