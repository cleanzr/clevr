
test_that("pairwise confusion matrix is correct for a simple example", {
  pred_pairs <- rbind(c(1, 2), c(1, 3), c(4, 5))
  true_pairs <- rbind(c(1, 2), c(1, 5))
  result <- pairwise_confusion_matrix(pred_pairs, true_pairs, num_pairs = 25)
  dnames <- dimnames(cm)
  dimnames(result) <- NULL
  expect_equal(result,
               rbind(c(1,1), c(2,21)))
  expect_equal(dnames,
               list("Prediction" = c("TRUE", "FALSE"), "Truth" = c("TRUE", "FALSE")))
})
