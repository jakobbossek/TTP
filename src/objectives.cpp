#include <Rcpp.h>
#include "TTPInstance.h"

using namespace Rcpp;

// Helper function which locates node with node number 1
// and shifts the permutation such that this node is the
// first in the permutation
// NumericVector rotateTour(NumericVector tour) {
//   int n = tour.size();
//   NumericVector rt(n);

//   // locate node "NUMBER 1"
//   int loc = 0;
//   for (int i = 0; i < n; ++i) {
//     if (tour[i] == 1) {
//       loc = i;
//       break;
//     }
//   }

//   // now shift
//   for (int i = 0; i < n; ++i) {
//     int locInTour = (i + loc) % n;
//     rt[i] = tour[locInTour];
//   }

//   return rt;
// }

// [[Rcpp::export]]
NumericVector tspC(IntegerVector tour, NumericMatrix D) {
  int n = tour.size();
  NumericVector tlo(1);

  double tl = 0.0;
  for (int i = 0; i < n - 1; ++i) {
    // Note: each time -1 since indexing in C(++) is zero-based
    tl += D(tour[i] - 1, tour[i + 1] - 1);
  }
  tl += D(tour[n - 1] - 1, tour[0] - 1);

  tlo[0] = tl;
  return tlo;
}

// [[Rcpp::export]]
NumericVector wtspC(IntegerVector tour, List problem, IntegerVector itemPackingPlan, NumericVector itemWeights, IntegerVector itemAssignedNodes) {
  NumericMatrix D = as<NumericMatrix>(problem["distance.matrix"]);
  NumericVector tlo(1);

  int n = (int)(problem["n"]);
  int m = (int)(problem["m"]);

  // calculate packing by node, i.e. sum of weights for each single node
  std::vector<int> pbn(n);
  for (int i = 0; i < n; ++i) {
    pbn[i] = 0.0;
  }

  // first node has ALWAYS weight 1 by definition of the node-weighted TSP
  // see our paper GECCO2020 paper for details
  pbn[0] = 1.0;

  // nr. of items per node
  // int nripn = (int)(m / (n - 1));

  // add up
  for (int i = 0; i < m; ++i) { // m is equal to packing.size()
    int assignedNode = itemAssignedNodes[i] - 1;
    pbn[assignedNode] += (itemPackingPlan[i] * itemWeights[i]);
  }

  // rotate tour such that node #1 is the first one in the permutation
  // NumericVector rt = rotateTour(tour);
  NumericVector rt(n);

  // locate node "NUMBER 1"
  int loc = 0;
  for (int i = 0; i < n; ++i) {
    if (tour[i] == 1) {
      loc = i;
      break;
    }
  }

  // now shift
  for (int i = 0; i < n; ++i) {
    int locInTour = (i + loc) % n;
    rt[i] = tour[locInTour];
  }

  // eventually calculate WTSP tour
  double Wxi = 0.0;
  double tl = 0.0;
  for (int i = 0; i < n - 1; ++i) {
    // accumulate weight
    Wxi += pbn[rt[i] - 1];
    // add up to overall tour length
    tl  += Wxi * D(rt[i] - 1, rt[i + 1] - 1);
  }
  // do not forget about closing the tour, i.e. from the last node back to the start
  Wxi += pbn[rt[n - 1] - 1];
  tl += Wxi * D(rt[n - 1] - 1, rt[0] - 1);

  tlo[0] = tl;
  return tlo;
}

// [[Rcpp::export]]
NumericVector ttpC(IntegerVector tour, List problem, IntegerVector itemPackingPlan, NumericVector itemProfits, NumericVector itemWeights, IntegerVector itemAssignedNodes) {
  NumericMatrix D = as<NumericMatrix>(problem["distance.matrix"]);

  int n = (int)(problem["n"]);
  int m = (int)(problem["m"]);

  double vmin = (double)(problem["vmin"]);
  double vmax = (double)(problem["vmax"]);
  double R    = (double)(problem["R"]);

  // rotate tour such that node #1 is the first one in the permutation
  // NumericVector rt = rotateTour(tour);
  NumericVector rt(n);

  // locate node "NUMBER 1"
  int loc = 0;
  for (int i = 0; i < n; ++i) {
    if (tour[i] == 1) {
      loc = i;
      break;
    }
  }

  // now shift
  for (int i = 0; i < n; ++i) {
    int locInTour = (i + loc) % n;
    rt[i] = tour[locInTour];
  }

  // sum of profits
  double profit = 0.0;
  for (int i = 0; i < m; ++i) {
    profit += (itemPackingPlan[i] * itemProfits[i]);
  }

// calculate packing by node, i.e. sum of weights for each single node
  std::vector<int> pbn(n);
  for (int i = 0; i < n; ++i) {
    pbn[i] = 0.0;
  }

  // first node has ALWAYS weight 1 by definition of the node-weighted TSP
  // see our paper GECCO2020 paper for details
  // NOTE: this is 0.0 for the first city in the TTP (not 1.0 as it is in the WTSP).
  pbn[0] = 0.0;

  // add up
  for (int i = 0; i < m; ++i) { // m is equal to packing.size()
    int assignedNode = itemAssignedNodes[i] - 1;
    pbn[assignedNode] += (itemPackingPlan[i] * itemWeights[i]);
  }

  // total weight (this is our capacity since we allow to pack all items)
  // NOTE: We decided to add up ALL items and not just the packed ones?
  // double W = 0.0;
  // for (int i = 0; i < pbn.size(); ++i) {
  //   W += pbn[i];
  // }
  double W = 0.0;
  for (int i = 0; i < m; ++i) {
    W += itemWeights[i];
  }

  double nu = (vmax - vmin) / W;
  double Wxi = 0.0;
  double tl = 0.0;
  for (int i = 0; i < n - 1; ++i) {
    Wxi += pbn[rt[i] - 1];
    tl += D(rt[i] - 1, rt[i + 1] - 1) / (vmax - nu * Wxi);
  }

  Wxi += pbn[rt[n - 1] - 1];
  tl += D(rt[n - 1] - 1, rt[0] - 1) / (vmax - nu * Wxi);

  // final TTP value
  tl = profit - R * tl;

  // build output
  NumericVector tlo(1);
  tlo[0] = tl;

  return tlo;
}
