#include <Rcpp.h>

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
