context("get-dummies")
library(aglm)

test_that("getUDummyMatForOneVec()'s outputs are correct.", {
  expect_equal(getUDummyMatForOneVec(1:3)$dummy_mat, matrix(c(1, 0, 0, 0, 1, 0), 3, 2))
  expect_equal(getUDummyMatForOneVec(1:3, drop_last=FALSE)$dummy_mat, matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3))
  expect_equal(getUDummyMatForOneVec(c("a", "b", "b", "e", "d", "f"))$levels, c("a", "b", "d", "e", "f"))
  expect_equal(getUDummyMatForOneVec(c("a", "b", "b", "e", "d", "f"), levels=c("a", "b", "c", "d", "e", "f"))$levels, c("a", "b", "c", "d", "e", "f"))
})

test_that("getODummyMatForOneVec()'s outputs are correct.", {
  expect_equal(getODummyMatForOneVec(1:3, nbin.max=5)$dummy_mat,
               matrix(c(1, 0, 0, 1, 1, 0, 1, 1, 1), 3, min(3, 5)))
  expect_equal(getODummyMatForOneVec(1:3, breaks=c(0, 0.5, 1.2, 3, 4))$dummy_mat,
               matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1), 3, 4))
})
