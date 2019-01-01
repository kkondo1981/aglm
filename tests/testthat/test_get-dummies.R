context("get-dummies")
library(aglm)

test_that("getUDummyMatForOneVec()'s outputs are correct.", {
  expect_equal(getUDummyMatForOneVec(1:3)$dummy_mat, matrix(c(1, 0, 0, 0, 1, 0), 3, 2))
  expect_equal(getUDummyMatForOneVec(1:3, drop_last=FALSE)$dummy_mat, matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3))
  expect_equal(getUDummyMatForOneVec(c("a", "b", "b", "e", "d", "f"))$levels, c("a", "b", "d", "e", "f"))
})
