
test_that("pairwise contingency table is correct for a simple example", {
  pred_pairs <- rbind(c(1, 2), c(1, 3), c(4, 5))
  true_pairs <- rbind(c(1, 2), c(1, 5))
  result <- contingency_table_pairs(pred_pairs, true_pairs, num_pairs = 25)
  true_result <- rbind("TRUE" = c("TRUE" = 1,"FALSE" = 1), "FALSE" = c("TRUE" = 2, "FALSE" = 21))
  true_result <- as.table(true_result)
  names(dimnames(true_result)) <- c("Prediction", "Truth")
  expect_equal(result, true_result)
})
