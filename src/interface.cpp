#include <Rcpp.h>
#include "TTPInstance.h"

using namespace Rcpp;


// [[Rcpp::export]]
List runWTSPSolverC(String pathToFile, IntegerVector packing, IntegerMatrix initTours, int mu, int mutation, int objectiveType, int survivalStrategy, int maxEvaluations) {
  TTPInstance instance(pathToFile);

  // Now perform hill-climbing
  int lambda = 1;

  int n = instance.getNumberOfNodes();
  //int k = instance.getNumberOfItems();

  // ugly way to copy initTours
  std::vector<int> packingC = as<std::vector<int>>(packing);
  std::vector<std::vector<int>> toursC(mu);
  for (int i = 0; i < mu; ++i) {
    std::vector<int> tour(n);
    for (int j = 0; j < n; ++j) {
      tour[j] = initTours(i, j);
      //std::cout << tour[j] << ", ";
    }
    //std::cout << std::endl;
    toursC[i] = tour;
  }

  //std::vector<int> initTourC = TTPAlgorithm::generateRandomPermutation(n);

  TTPAlgorithmOutput o = TTPAlgorithm::doEAMagic(
    instance,
    packingC,
    toursC,
    mu,
    lambda,
    mutation,
    survivalStrategy,
    maxEvaluations,
    objectiveType);

  TTPSolution s = o.getSolution();
  std::vector<std::vector<int>> finalTours = o.getFinalTours();

  IntegerMatrix finalToursR(mu, n);
  for (int i = 0; i < mu; ++i) {
    for (int j = 0; j < n; ++j) {
      finalToursR(i, j) = finalTours[i][j];
    }
  }

  // calculate WTSP and TTP solution on final tour
  double tourLengthWTSP = instance.evaluate(s, 0);
  double tourLengthTTP  = instance.evaluate(s, 1);

  return List::create(
    _["tour.length"] = s.getValue(),
    _["tour"] = s.getTour(),
    _["tour.length.wtsp"] = tourLengthWTSP,
    _["tour.length.ttp"] = tourLengthTTP,
    _["finalTours"] = finalToursR,
    _["trajectory"] = o.getTrajectory()
  );
}
