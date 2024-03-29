% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures_clusterings.R
\name{rand_index}
\alias{rand_index}
\title{Rand Index Between Clusterings}
\usage{
rand_index(true, pred)
}
\arguments{
\item{true}{ground truth clustering represented as a membership
vector. Each entry corresponds to an element and the value identifies
the assigned cluster. The specific values of the cluster identifiers
are arbitrary.}

\item{pred}{predicted clustering represented as a membership
vector.}
}
\description{
Computes the Rand index (RI) between two clusterings, such
as a predicted and ground truth clustering.
}
\details{
The Rand index (RI) can be expressed as:
\deqn{\frac{a + b}{{n \choose 2}}.}{(a + b)/binom(n, 2).}
where
\itemize{
\item \eqn{n} is the number of elements,
\item \eqn{a} is the number of pairs of elements that appear in the
same cluster in both clusterings, and
\item \eqn{b} is the number of pairs of elements that appear in distinct
clusters in both clusterings.
}

The RI takes on values between 0 and 1, where 1 denotes exact agreement
between the clusterings and 0 denotes disagreement on all pairs of
elements.
}
\examples{
true <- c(1,1,1,2,2)  # ground truth clustering
pred <- c(1,1,2,2,2)  # predicted clustering
rand_index(true, pred)

}
\references{
Rand, W. M. "Objective Criteria for the Evaluation of Clustering Methods."
\emph{Journal of the American Statistical Association} 66(336), 846-850 (1971).
\doi{10.1080/01621459.1971.10482356}
}
