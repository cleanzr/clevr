#' Transform Clustering Representations
#'
#' @description
#' Transform between different representations of a clustering.
#'
#' @param clusters a representation of a clustering as a list of vectors,
#'   where the i-th vector contains the identifiers of elements assigned to the
#'   i-th cluster. If `clust_ids` is specified (see below), the i-th cluster
#'   is identified according to the corresponding entry in `clust_ids`.
#'   Otherwise the i-th cluster is identified according it's name (if
#'   `clusters` is a named list) or its integer index i.
#' @param membership a representation of a clustering as a membership vector,
#'   where the i-th entry contains the cluster identifier for the i-th element.
#'   If `elem_ids` is specified (see below), the i-th element is identified
#'   according to the corresponding entry in `elem_ids`. Otherwise the i-th
#'   element is identified according it's name (if `members` is a named vector)
#'   or its integer index i.
#' @param pairs a representation of a clustering as a matrix or data.frame
#'   containing all pairs of elements that are co-clustered. The rows index
#'   of the matrix/data.frame index pairs and columns index the identifiers
#'   of the constituent elements. The `elem_ids` argument (see below) must be
#'   specified in order to recover singleton clusters (containing a single
#'   element).
#' @param elem_ids a vector specifying the complete set of identifiers for the
#'   cluster elements in canonical order. Optional for all functions excluding
#'   `pairs_to_membership` and `pairs_to_clusters`.
#' @param clust_ids a vector specifying the complete set of identifiers for
#'   the clusters in canonical order. Optional for all functions.
#' @return `clusters_to_membership` and `pairs_to_membership` both return a
#'   membership vector representation of the clustering. The order of the
#'   elements is taken from `elem_ids` if specified, otherwise the elements are
#'   ordered lexicographically by their identifiers. For
#'   `pairs_to_membership`, the cluster identifiers cannot be recovered and
#'   are taken to be integers.
#'
#'   `membership_to_clusters` and `pairs_to_clusters` both return a
#'   representation of the clustering as a list of vectors. The order of the
#'   clusters is taken from `clust_ids` if specified, otherwise the clusters
#'   are ordered lexicographically by their identifiers. For
#'   `pairs_to_clusters`, the cluster identifiers cannot be recovered and
#'   are taken to be integers.
#'
#'   `clusters_to_pairs` and `membership_to_pairs` both return a
#'   representation of the clustering as a matrix of element pairs that are
#'   co-clustered. This representation results in loss of information, as
#'   singleton clusters (with one element) and cluster identifiers are not
#'   represented.
#'
#' @examples
#' ## A clustering of three items represented as a membership vector
#' m <- c("Item1" = 1, "Item2" = 2, "Item3" = 1)
#'
#' # Transform to list of clusters
#' membership_to_clusters(m)
#' # Specify different identifiers for the items
#' membership_to_clusters(m, elem_ids = c(1, 2, 3))
#' # Transform to array of pairs that are co-clustered
#' membership_to_pairs(m)
#'
#' ## A clustering represented as a list of clusters
#' cl <- list("ClustA" = c(1,3), "ClustB" = c(2))
#'
#' # Transform to membership vector representation
#' clusters_to_membership(cl)
#' # Transform to array of pairs that are co-clustered
#' clusters_to_pairs(cl)
#'
#' ## A clustering (incompletely) represented as an array of pairs that
#' ## are co-clustered
#' p <- rbind(c(1,3)) # pairs of elements in the same cluster
#' ids <- c(1,2,3)    # necessary to specify set of all elements
#'
#' # Transform to membership vector representation
#' pairs_to_membership(p, ids)
#' # Transform to list of clusters
#' pairs_to_clusters(p, ids)
#'
#' @export
#' @importFrom stats na.fail
#' @rdname clustering_representations
clusters_to_membership <- function(clusters, elem_ids = NULL, clust_ids = NULL)
{
  if (!is.null(clust_ids)) {
    # Check provided clust_ids for consistency
    if (length(clust_ids) != length(clusters))
      stop("`clust_ids` must be the same length as `clusters`")
    tryCatch(na.fail(clust_ids), error = function(e)
      stop("`clust_ids` cannot contain NA values"))
  } else {
    # Infer clust_ids from names first, falling back to integer ids
    if (!is.null(names(clusters))) {
      clust_ids <- names(clusters)
    } else {
      clust_ids <- seq_along(clusters)
    }
  }

  clust_sizes <- sapply(clusters, length)
  if (!is.null(elem_ids)) {
    if (sum(clust_sizes) != length(elem_ids))
      stop("`elem_ids` does not match number of elements in `clusters`")
    tryCatch(na.fail(elem_ids), error = function(e)
      stop("`elem_ids` cannot contain NA values"))
  }

  membership <- rep(clust_ids, times=clust_sizes)
  names(membership) <- as.character(unlist(clusters))

  # Reorder membership vector
  if (!is.null(elem_ids)) {
    # Use order in elem_ids
    membership <- membership[as.character(elem_ids)]
  } else {
    # Order lexicographically by name
    ordered_idx <- order(names(membership))
    membership <- membership[ordered_idx]
  }

  return(membership)
}


#' @importFrom stats na.fail
#' @export
#' @rdname clustering_representations
membership_to_clusters <- function(membership, elem_ids = NULL, clust_ids = NULL) {
  if (!is.null(elem_ids)) {
    # Check provided elem_ids for consistency
    if (length(elem_ids) != length(membership))
      stop("`elem_ids` must be the same length as `membership`")
    tryCatch(na.fail(elem_ids), error = function(e)
      stop("`elem_ids` cannot contain NA values"))
  } else {
    # Infer elem_ids from names first, falling back to integer ids
    if (!is.null(names(membership))) {
      elem_ids <- names(membership)
    } else {
      elem_ids <- seq_along(membership)
    }
  }

  clusters <- split(elem_ids, membership)

  # Reorder clusters list
  if (!is.null(clust_ids)) {
    # Use order in clust_ids, but first check consistency
    tryCatch(na.fail(clust_ids), error = function(e)
      stop("`clust_ids` cannot contain NA values"))
    clusters <- clusters[as.character(clust_ids)]
  } else {
    # Order lexicographically by name
    ordered_idx <- order(names(clusters))
    clusters <- clusters[ordered_idx]
  }

  return(clusters)
}


#' @importFrom utils combn
#' @export
#' @rdname clustering_representations
clusters_to_pairs <- function(clusters) {

  non_singletons <- Filter(function(x) length(x) > 1, clusters)

  if (length(non_singletons) == 0) {
    if (length(clusters) == 0) {
      # No clusters
      pairs <- array(dim = c(0, 2), data = 0L)

    } else {
      # All clusters are singletons: no pairs to return
      element_id_type <- typeof(clusters[[1]])
      pairs <- array(dim = c(0, 2), data = vector(mode = element_id_type))
    }
    return(pairs)
  }

  # Make ? x 2 array of pairs for each cluster and store in a list
  pairs <- lapply(non_singletons, function(x) t(combn(x, 2)))
  # Merge pairs from all clusters into single ? x 2 array
  pairs <- do.call(rbind, pairs)

  pairs <- canonicalize_pairs(pairs)
  return(pairs)
}


#' @export
#' @rdname clustering_representations
membership_to_pairs <- function(membership, elem_ids = NULL) {
  clusters <- membership_to_clusters(membership, elem_ids = elem_ids)
  pairs <- clusters_to_pairs(clusters)
  return(pairs)
}


#' @importFrom stats na.fail na.omit na.action
#' @export
#' @rdname clustering_representations
pairs_to_membership <- function(pairs, elem_ids) {
  # Need to convert to matrix in order for factor to work below
  pairs <- as.matrix(pairs)
  pairs <- na.omit(pairs)

  if (ncol(pairs) != 2) stop("`pairs` must have exactly two columns")
  if (length(na.action(pairs))!= 0)
    warning("rows with NA values were removed from `pairs`")

  tryCatch(na.fail(elem_ids), error = function(e) stop("`elem_ids` cannot contain NA values"))

  # Transform pairs so that elem_ids are represented as integers starting at 0
  original_dim <- dim(pairs)
  pairs <- factor(pairs)
  pairs <- unclass(pairs) - 1
  dim(pairs) <- original_dim

  # Save mapping to original elem_ids used in pairs
  pairs_elem_ids <- levels(pairs)

  membership <- pairs_to_membership_cpp(pairs, length(elem_ids))
  # R indexing starts at 1
  membership <- membership + 1

  # Fill names with elem_ids
  char_elem_ids <- as.character(elem_ids)
  singleton_elem_ids <- setdiff(char_elem_ids, pairs_elem_ids)
  names(membership) <- c(pairs_elem_ids, singleton_elem_ids)

  # Sort according to elem_ids
  membership <- membership[char_elem_ids]

  return(membership)
}


#' @export
#' @rdname clustering_representations
pairs_to_clusters <- function(pairs, elem_ids) {
  membership <- pairs_to_membership(pairs, elem_ids)
  clusters <- membership_to_clusters(membership, elem_ids = elem_ids)
  names(clusters) <- NULL
  return(clusters)
}


#' Canonicalize element pairs
#'
#' @description
#' Coerce a collection of element pairs into canonical form. Facilitates
#' testing of equivalence.
#'
#' @param pairs a matrix or data.frame of element pairs where rows correspond
#'   to element pairs and columns correspond to element identifiers.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#' @return Returns the element pairs in canonical form, so that:
#'   * the first element id precedes the second element id lexicographically
#'     if `ordered = FALSE`---i.e. pair (3, 2) becomes pair (2, 3);
#'   * duplicate pairs are removed; and
#'   * the rows in the matrix/data.frame pairs are sorted lexicographically
#'     by the first element id, then by the second element id.
#'
#' @examples
#' messy_pairs <- rbind(c(2,1), c(1,2), c(3,1), c(1,2))
#' clean_pairs <- canonicalize_pairs(messy_pairs)
#' all(rbind(c(1,2), c(1,3)) == clean_pairs) # duplicates removed and order fixed
#'
#' @export
canonicalize_pairs <- function(pairs, ordered=FALSE) {
  if (ncol(pairs) != 2) stop("`pairs` must have exactly two columns")

  pairs <- as.matrix(pairs)

  if (nrow(pairs) == 0) return(pairs)

  # Sort entries in each row lexicographically
  if (!ordered) {
    pairs <- t(apply(pairs, 1, sort))
  }

  # Remove duplicate rows
  pairs <- unique(pairs)

  # Sort rows lexicographically, by first column then second
  ordered_row <- order(pairs[,1], pairs[,2])
  pairs <- pairs[ordered_row, , drop=FALSE]

  return(pairs)
}
