context("Clusters to membership vector")

test_that("un-named list of integer vectors correctly transformed to membership vector", {
  clusters <- list(c(100L, 1L), c(2L))
  clust_ids <- c("A", "B")
  elem_ids <- c(1L, 2L, 100L)
  expect_equal(clusters_to_membership(clusters),
               c("1" = 1L, "100" = 1L, "2" = 2L))
  expect_equal(clusters_to_membership(clusters, elem_ids = elem_ids),
               c("1" = 1L, "2" = 2L, "100" = 1L))
  expect_equal(clusters_to_membership(clusters, clust_ids = clust_ids),
               c("1" = "A", "100" = "A", "2" = "B"))
  expect_equal(clusters_to_membership(clusters, elem_ids = elem_ids, clust_ids = clust_ids),
               c("1" = "A", "2" = "B", "100" = "A"))
})

test_that("un-named list of character vectors correctly transformed to membership vector", {
  clusters <- list(c("ELEM3", "ELEM1"), c("ELEM2"))
  clust_ids <- c("A", "B")
  elem_ids <- c("ELEM3", "ELEM2", "ELEM1")
  expect_equal(clusters_to_membership(clusters),
               c("ELEM1" = 1L, "ELEM2" = 2L, "ELEM3" = 1L))
  expect_equal(clusters_to_membership(clusters, elem_ids = elem_ids),
               c("ELEM3" = 1L, "ELEM2" = 2L, "ELEM1" = 1L))
  expect_equal(clusters_to_membership(clusters, clust_ids = clust_ids),
               c("ELEM1" = "A", "ELEM2" = "B", "ELEM3" = "A"))
  expect_equal(clusters_to_membership(clusters, elem_ids = elem_ids, clust_ids = clust_ids),
               c("ELEM3" = "A", "ELEM2" = "B", "ELEM1" = "A"))
})

test_that("named list of integer vectors correctly transformed to membership vector", {
  clusters <- list("A" = c(100L, 1L), "B" = c(2L))
  clust_ids <- c("A", "B")
  elem_ids <- c(1L, 2L, 100L)
  expect_equal(clusters_to_membership(clusters),
               c("1" = "A", "100" = "A", "2" = "B"))
  expect_equal(clusters_to_membership(clusters, elem_ids = elem_ids),
               c("1" = "A", "2" = "B", "100" = "A"))
  expect_equal(clusters_to_membership(clusters, clust_ids = clust_ids),
               c("1" = "A", "100" = "A", "2" = "B"))
  expect_equal(clusters_to_membership(clusters, elem_ids = elem_ids, clust_ids = clust_ids),
               c("1" = "A", "2" = "B", "100" = "A"))
})


context("Membership vector to clusters")

test_that("un-named integer membership vector correctly transformed to list of vectors", {
  membership <- c(1L, 2L, 1L)
  clust_ids <- c(2L, 1L)
  elem_ids <- c(1L, 2L, 100L)
  expect_equal(membership_to_clusters(membership),
               list("1" = c(1L, 3L), "2" = 2L))
  expect_equal(membership_to_clusters(membership, elem_ids = elem_ids),
               list("1" = c(1L, 100L), "2" = 2L))
  expect_equal(membership_to_clusters(membership, clust_ids = clust_ids),
               list("2" = 2L, "1" = c(1L, 3L)))
  expect_equal(membership_to_clusters(membership, elem_ids = elem_ids, clust_ids = clust_ids),
               list("2" = 2L, "1" = c(1L, 100L)))
})

test_that("un-named character membership vector correctly transformed to list of vectors", {
  membership <- c("B", "A", "B")
  clust_ids <- c("B", "A")
  elem_ids <- c(1L, 2L, 100L)
  expect_equal(membership_to_clusters(membership),
               list("A" = 2L, "B" = c(1L, 3L)))
  expect_equal(membership_to_clusters(membership, elem_ids = elem_ids),
               list("A" = 2L, "B" = c(1L, 100L)))
  expect_equal(membership_to_clusters(membership, clust_ids = clust_ids),
               list("B" = c(1L, 3L), "A" = 2L))
  expect_equal(membership_to_clusters(membership, elem_ids = elem_ids, clust_ids = clust_ids),
               list("B" = c(1L, 100L), "A" = 2L))
})

test_that("named character membership vector correctly transformed to list of vectors", {
  membership <- c("1" = "B", "2" = "A", "100" = "B")
  clust_ids <- c("B", "A")
  elem_ids <- c(1L, 2L, 100L)
  expect_equal(membership_to_clusters(membership),
               list("A" = "2", "B" = c("1", "100")))
  expect_equal(membership_to_clusters(membership, elem_ids = elem_ids),
               list("A" = 2L, "B" = c(1L, 100L)))
  expect_equal(membership_to_clusters(membership, clust_ids = clust_ids),
               list("B" = c("1", "100"), "A" = "2"))
  expect_equal(membership_to_clusters(membership, elem_ids = elem_ids, clust_ids = clust_ids),
               list("B" = c(1L, 100L), "A" = 2L))
})


context("Pairs to membership vector")

test_that("integer matrix of pairs correctly transformed to membership vector", {
  pairs <- rbind(c(1L, 2L), c(1L, 3L), c(2L, 3L), c(4L, 5L))
  elem_ids <- seq_len(5)
  expect_equal(pairs_to_membership(pairs, elem_ids),
               c("1" = 1, "2" = 1, "3" = 1, "4" = 2, "5" = 2))
})

test_that("special case of no pairs handled correctly", {
  pairs <- matrix(0L, nrow = 0, ncol = 2)
  elem_ids <- seq_len(5)
  expect_equal(pairs_to_membership(pairs, elem_ids),
               c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5))
})

test_that("character matrix of pairs correctly transformed to membership vector", {
  pairs <- rbind(c("A", "B"), c("B", "C"), c("A", "C"), c("D", "E"))
  elem_ids <- LETTERS[1:5]
  expect_equal(pairs_to_membership(pairs, elem_ids),
               c("A" = 1, "B" = 1, "C" = 1, "D" = 2, "E" = 2))
})

test_that("missing element identifiers in pairs produces a warning", {
  pairs <- rbind(c(NA, 2L), c(1L, 3L), c(2L, 3L))
  elem_ids <- seq_len(5)
  expect_warning(pairs_to_membership(pairs, elem_ids))
})

test_that("missing element identifiers in `elem_ids` results in error", {
  pairs <- rbind(c(1L, 2L), c(1L, 3L), c(2L, 3L))
  elem_ids <- c(1L, NA, 3L)
  expect_error(pairs_to_membership(pairs, elem_ids))
})

test_that("passing pairs with incorrect dimensions results in error", {
  pairs <- rbind(c(1L, 2L), c(1L, 3L), c(2L, 3L))
  elem_ids <- c(1L, 2L, 3L)
  expect_error(pairs_to_membership(pairs[,0], elem_ids))
})


context("Canonicalize pairs")

test_that("rows are ordered lexicographically by first column then second column", {
  pairs <- rbind(c(3,4), c(1,5), c(1,2))
  expect_equal(canonicalize_pairs(pairs),
               rbind(c(1,2), c(1,5), c(3,4)))
})

test_that("identifiers are ordered lexicographically within each row", {
  pairs <- rbind(c(4,3), c(1,5), c(2,1))
  expect_equal(canonicalize_pairs(pairs),
               rbind(c(1,2), c(1,5), c(3,4)))
})

test_that("duplicate pairs are removed", {
  pairs <- rbind(c(1,2), c(2,1))
  expect_equal(canonicalize_pairs(pairs),
               rbind(c(1,2)))
})
