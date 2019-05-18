context("get-dummies")
library(aglm)

test_that("getUDummyMatForOneVec()'s outputs are correct.", {
  expect_equal(getUDummyMatForOneVec(1:3)$dummy_mat, matrix(c(1, 0, 0, 0, 1, 0), 3, 2))
  expect_equal(getUDummyMatForOneVec(1:3, drop_last=FALSE)$dummy_mat, matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3))
  expect_equal(getUDummyMatForOneVec(c("a", "b", "b", "e", "d", "f"))$levels, c("a", "b", "d", "e", "f"))
  expect_equal(getUDummyMatForOneVec(c("a", "b", "b", "e", "d", "f"), levels=c("a", "b", "c", "d", "e", "f"))$levels, c("a", "b", "c", "d", "e", "f"))
  expect_equal(getUDummyMatForOneVec(ordered(c(1, 1, 2, 3)))$dummy_mat, matrix(c(1, 1, 0, 0, 0, 0, 1, 0), 4, 2))
})

test_that("getODummyMatForOneVec()'s outputs are correct.", {
  ## Tests dummy_type='C'
  expect_equal(getODummyMatForOneVec(1:3, dummy_type="C")$dummy_mat,
               matrix(c(0, 1, 1, 0, 0, 1), 3, 2))
  expect_equal(getODummyMatForOneVec(c(1, 1.5, 2, 2.3, 3), breaks=1:3, dummy_type="C")$dummy_mat,
               matrix(c(0, 0.5, 1, 1, 1, 0, 0, 0, 0.3, 1), 5, 2))
  expect_equal(getODummyMatForOneVec(c(1, 1.5, 2, 2.3, 3), breaks=c(1, 2, 10), dummy_type="C")$dummy_mat,
               matrix(c(0, 0.5, 1, 1, 1, 0, 0, 0, 0.3/8, 1/8), 5, 2))

  ## Tests dummy_type='J'
  expect_equal(getODummyMatForOneVec(1:3, dummy_type="J")$dummy_mat,
               matrix(c(1, 1, 1, 0, 1, 1, 0, 0, 1), 3, 3))
  expect_equal(getODummyMatForOneVec(1:3, breaks=c(0, 0.5, 1.2, 3, 4), dummy_type="J")$dummy_mat,
               matrix(c(1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0), 3, 5))
  expect_equal(getODummyMatForOneVec(ordered(c(1, 1, 2, 3)), dummy_type="J")$dummy_mat,
               matrix(c(1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1), 4, 3))
})
