#include <Rcpp.h>
#include <cstdlib>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector monotonC(NumericVector seq, CharacterVector relation) {
  int n = seq.size();
  LogicalVector mon(n - 1);

  for (int i = 0; i < n - 1; ++i) {
    if (relation[0] == "<") {
      mon[i] = (seq[i] < seq[i+1]);
    } else if (relation[0] == ">") {
      mon[i] = (seq[i] > seq[i+1]);
    } else if (relation[0] == ">=") {
      mon[i] = (seq[i] >= seq[i+1]);
    } else if (relation[0] == "<=") {
      mon[i] = (seq[i] <= seq[i+1]);
    } else if (relation[0] == "<<=") {
      if ((i > 0 && !mon[i-1]) || i == 0) {
        mon[i] = (seq[i] < seq[i+1]);
      } else {
        mon[i] = (seq[i] <= seq[i+1]);
      }
    } else if (relation[0] == ">>=") {
      if ((i > 0 && !mon[i-1]) || i == 0) {
        mon[i] = (seq[i] > seq[i+1]);
      } else {
        mon[i] = (seq[i] >= seq[i+1]);
      }
    }
  }

  return mon;
}

// [[Rcpp::export]]
List getMonotonicBlocksC(NumericVector mon) {
  // storage for (start, end) pairs
  std::vector<int> starts;
  std::vector<int> ends;
  std::vector<int> lengths;

  int i = 0;
  int n = mon.size();
  bool inBlock = false;
  int start = -1;

  while (i < n) {
    // new block or inside block
    if (mon[i] == 1) {
      if (!inBlock) {
        inBlock = true;
        start = i + 1; // +1 because we return to R
      }
    // not in block or block ended
    } else {
      if (inBlock) {
        // found block
        int end = i + 1; // +1 because we return to R
        starts.push_back(start);
        ends.push_back(end);
        lengths.push_back(end - start + 1);
        inBlock = false;
      }
    }
    ++i;
  }

  // Now translate into R data structure
  NumericVector startsR = wrap(starts);
  NumericVector endsR = wrap(ends);
  NumericVector lengthsR = wrap(lengths);

  return List::create(
    _["start"] = startsR,
    _["end"] = endsR,
    _["length"] = lengthsR,
    _["n"] = starts.size()
  );
}

// [[Rcpp::export]]
NumericVector shiftTourC(NumericVector tour) {
  int n = tour.size();

  // shift tour such that node #1 is the first one in the permutation
  // NumericVector rt = rotateTour(tour);
  NumericVector stour(n);

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
    stour[i] = tour[locInTour];
  }

  return stour;
}

// [[Rcpp::export]]
double getUncommonEdges(NumericVector tour1, NumericVector tour2, bool normalize) {
  double overlap = 0;
  unsigned int n = tour1.size();

  for (unsigned int i = 0; i < n; ++i) {
    // start and end in first tour
    int tour1s = tour1[i];
    int tour1d = tour1[(i + 1) % n];
    for (unsigned int j = 0; j < n; ++j) {
      // start and end in second tour
      int tour2s = tour2[j];
      int tour2d = tour2[(j + 1) % n];
      // now check
      if (((tour1s == tour2s) && (tour1d == tour2d)) || ((tour1s == tour2d) && (tour1d == tour2s))) {
        overlap += 1;
        break;
      }
    }
  }

  double uncommon = n - overlap;

  if (normalize) {
    uncommon /= n;
  }

  return uncommon;
}

// [[Rcpp::export]]
double getNumberOfInversionsC(IntegerVector v1, IntegerVector v2, bool normalize) {
  double inversions = 0;
  unsigned int n = v1.size();

  for (unsigned int i = 0; i < n; ++i) {
    for (unsigned int j = i + 1; j < n; ++j) {
      int v1a = v1[i];
      int v1b = v1[j];

      if (((v1a < v1b) & (v2[v1a-1] > v2[v1b-1])) || ((v1a > v1b) & (v2[v1a-1] > v2[v1b-1]))) {
        //printf("v1[%d] = %d < v1[%d] = %d & v2[%d] = %d > v2[%d] = %d\n", i+1, v1a, j+1, v1b, v1a, v2[v1a], v1b, v2[v1b]);
        inversions += 1;
      }
      //else if (((v1a > v1b) & (v2[v1a-1] > v2[v1b-1]))) {
        //printf("v1[%d] = %d > v1[%d] = %d & v2[%d] = %d > v2[%d] = %d\n", i+1, v1a, j+1, v1b, v1a, v2[v1a], v1b, v2[v1b]);
        //inversions += 1;
      //}
    }
  }

  if (normalize) {
    inversions /= (n * (n - 1) * 0.5);
  }

  return inversions;
}

// Maximum distance an element must travel to be in its sorted position (O(n log(n)))
// [[Rcpp::export]]
double getMaximumDistanceC(IntegerVector tour1, IntegerVector tour2, bool normalize) {
  int maxdist = 0;
  int n = tour1.size();

  for (int i = 0; i < n; ++i) {
    // element
    int elem = tour1[i];
    // the elements position in tour2 (-1 cause permutations are in {1, ..., n} in R)
    int posInTour2 = tour2[elem - 1] - 1;
    // update dist
    maxdist = std::max(maxdist, abs(i - posInTour2));
  }

  if (normalize)
    maxdist /= (n - 1);

  return maxdist;
}

double getRunsC(IntegerVector tour1, IntegerVector tour2, bool normalize) {
  return 1.0;
}
