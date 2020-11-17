#include <Rcpp.h>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/adjacency_list.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector pairs_to_membership_cpp(const IntegerMatrix &pairs, int num_records) {
  using namespace boost;

  typedef adjacency_list <vecS, vecS, undirectedS> Graph;

  Graph G(num_records);
  for (int i = 0; i < pairs.nrow(); i++) {
    add_edge(pairs.at(i, 0), pairs.at(i, 1), G);
  }

  IntegerVector membership(num_records);
  connected_components(G, &membership[0]);

  return membership;
}
