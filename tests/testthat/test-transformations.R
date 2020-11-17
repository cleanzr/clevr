context("Cluster to membership vector")

test_that("un-named list of integer vectors correctly transformed to membership vector", {
  clusters <- list(c(100L, 1L), c(2L))
  clust_ids <- c("A", "B")
  rec_ids <- c(1L, 2L, 100L)
  expect_equal(clusters_to_membership(clusters),
               c("1" = 1L, "100" = 1L, "2" = 2L))
  expect_equal(clusters_to_membership(clusters, rec_ids = rec_ids),
               c("1" = 1L, "2" = 2L, "100" = 1L))
  expect_equal(clusters_to_membership(clusters, clust_ids = clust_ids),
               c("1" = "A", "100" = "A", "2" = "B"))
  expect_equal(clusters_to_membership(clusters, rec_ids = rec_ids, clust_ids = clust_ids),
               c("1" = "A", "2" = "B", "100" = "A"))
})

test_that("un-named list of character vectors correctly transformed to membership vector", {
  clusters <- list(c("REC3", "REC1"), c("REC2"))
  clust_ids <- c("A", "B")
  rec_ids <- c("REC3", "REC2", "REC1")
  expect_equal(clusters_to_membership(clusters),
               c("REC1" = 1L, "REC2" = 2L, "REC3" = 1L))
  expect_equal(clusters_to_membership(clusters, rec_ids = rec_ids),
               c("REC3" = 1L, "REC2" = 2L, "REC1" = 1L))
  expect_equal(clusters_to_membership(clusters, clust_ids = clust_ids),
               c("REC1" = "A", "REC2" = "B", "REC3" = "A"))
  expect_equal(clusters_to_membership(clusters, rec_ids = rec_ids, clust_ids = clust_ids),
               c("REC3" = "A", "REC2" = "B", "REC1" = "A"))
})

test_that("named list of integer vectors correctly transformed to membership vector", {
  clusters <- list("A" = c(100L, 1L), "B" = c(2L))
  clust_ids <- c("A", "B")
  rec_ids <- c(1L, 2L, 100L)
  expect_equal(clusters_to_membership(clusters),
               c("1" = "A", "100" = "A", "2" = "B"))
  expect_equal(clusters_to_membership(clusters, rec_ids = rec_ids),
               c("1" = "A", "2" = "B", "100" = "A"))
  expect_equal(clusters_to_membership(clusters, clust_ids = clust_ids),
               c("1" = "A", "100" = "A", "2" = "B"))
  expect_equal(clusters_to_membership(clusters, rec_ids = rec_ids, clust_ids = clust_ids),
               c("1" = "A", "2" = "B", "100" = "A"))
})


context("Membership vector to clusters")

test_that("un-named integer membership vector correctly transformed to list of vectors", {
  membership <- c(1L, 2L, 1L)
  clust_ids <- c(2L, 1L)
  rec_ids <- c(1L, 2L, 100L)
  expect_equal(membership_to_clusters(membership),
               list("1" = c(1L, 3L), "2" = 2L))
  expect_equal(membership_to_clusters(membership, rec_ids = rec_ids),
               list("1" = c(1L, 100L), "2" = 2L))
  expect_equal(membership_to_clusters(membership, clust_ids = clust_ids),
               list("2" = 2L, "1" = c(1L, 3L)))
  expect_equal(membership_to_clusters(membership, rec_ids = rec_ids, clust_ids = clust_ids),
               list("2" = 2L, "1" = c(1L, 100L)))
})

test_that("un-named character membership vector correctly transformed to list of vectors", {
  membership <- c("B", "A", "B")
  clust_ids <- c("B", "A")
  rec_ids <- c(1L, 2L, 100L)
  expect_equal(membership_to_clusters(membership),
               list("A" = 2L, "B" = c(1L, 3L)))
  expect_equal(membership_to_clusters(membership, rec_ids = rec_ids),
               list("A" = 2L, "B" = c(1L, 100L)))
  expect_equal(membership_to_clusters(membership, clust_ids = clust_ids),
               list("B" = c(1L, 3L), "A" = 2L))
  expect_equal(membership_to_clusters(membership, rec_ids = rec_ids, clust_ids = clust_ids),
               list("B" = c(1L, 100L), "A" = 2L))
})

test_that("named character membership vector correctly transformed to list of vectors", {
  membership <- c("1" = "B", "2" = "A", "100" = "B")
  clust_ids <- c("B", "A")
  rec_ids <- c(1L, 2L, 100L)
  expect_equal(membership_to_clusters(membership),
               list("A" = "2", "B" = c("1", "100")))
  expect_equal(membership_to_clusters(membership, rec_ids = rec_ids),
               list("A" = 2L, "B" = c(1L, 100L)))
  expect_equal(membership_to_clusters(membership, clust_ids = clust_ids),
               list("B" = c("1", "100"), "A" = "2"))
  expect_equal(membership_to_clusters(membership, rec_ids = rec_ids, clust_ids = clust_ids),
               list("B" = c(1L, 100L), "A" = 2L))
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
