// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// runWTSPSolverC
List runWTSPSolverC(String pathToFile, IntegerVector packing, IntegerMatrix initTours, int mu, int mutation, int objectiveType, int survivalStrategy, int maxEvaluations);
RcppExport SEXP _TTP_runWTSPSolverC(SEXP pathToFileSEXP, SEXP packingSEXP, SEXP initToursSEXP, SEXP muSEXP, SEXP mutationSEXP, SEXP objectiveTypeSEXP, SEXP survivalStrategySEXP, SEXP maxEvaluationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type pathToFile(pathToFileSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type packing(packingSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type initTours(initToursSEXP);
    Rcpp::traits::input_parameter< int >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type mutation(mutationSEXP);
    Rcpp::traits::input_parameter< int >::type objectiveType(objectiveTypeSEXP);
    Rcpp::traits::input_parameter< int >::type survivalStrategy(survivalStrategySEXP);
    Rcpp::traits::input_parameter< int >::type maxEvaluations(maxEvaluationsSEXP);
    rcpp_result_gen = Rcpp::wrap(runWTSPSolverC(pathToFile, packing, initTours, mu, mutation, objectiveType, survivalStrategy, maxEvaluations));
    return rcpp_result_gen;
END_RCPP
}
// tspC
NumericVector tspC(IntegerVector tour, NumericMatrix D);
RcppExport SEXP _TTP_tspC(SEXP tourSEXP, SEXP DSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type tour(tourSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type D(DSEXP);
    rcpp_result_gen = Rcpp::wrap(tspC(tour, D));
    return rcpp_result_gen;
END_RCPP
}
// wtspC
NumericVector wtspC(IntegerVector tour, List problem, IntegerVector itemPackingPlan, NumericVector itemWeights, IntegerVector itemAssignedNodes);
RcppExport SEXP _TTP_wtspC(SEXP tourSEXP, SEXP problemSEXP, SEXP itemPackingPlanSEXP, SEXP itemWeightsSEXP, SEXP itemAssignedNodesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type tour(tourSEXP);
    Rcpp::traits::input_parameter< List >::type problem(problemSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type itemPackingPlan(itemPackingPlanSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type itemWeights(itemWeightsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type itemAssignedNodes(itemAssignedNodesSEXP);
    rcpp_result_gen = Rcpp::wrap(wtspC(tour, problem, itemPackingPlan, itemWeights, itemAssignedNodes));
    return rcpp_result_gen;
END_RCPP
}
// ttpC
NumericVector ttpC(IntegerVector tour, List problem, IntegerVector itemPackingPlan, NumericVector itemProfits, NumericVector itemWeights, IntegerVector itemAssignedNodes, bool weightOnly);
RcppExport SEXP _TTP_ttpC(SEXP tourSEXP, SEXP problemSEXP, SEXP itemPackingPlanSEXP, SEXP itemProfitsSEXP, SEXP itemWeightsSEXP, SEXP itemAssignedNodesSEXP, SEXP weightOnlySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type tour(tourSEXP);
    Rcpp::traits::input_parameter< List >::type problem(problemSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type itemPackingPlan(itemPackingPlanSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type itemProfits(itemProfitsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type itemWeights(itemWeightsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type itemAssignedNodes(itemAssignedNodesSEXP);
    Rcpp::traits::input_parameter< bool >::type weightOnly(weightOnlySEXP);
    rcpp_result_gen = Rcpp::wrap(ttpC(tour, problem, itemPackingPlan, itemProfits, itemWeights, itemAssignedNodes, weightOnly));
    return rcpp_result_gen;
END_RCPP
}
// monotonC
LogicalVector monotonC(NumericVector seq, CharacterVector relation);
RcppExport SEXP _TTP_monotonC(SEXP seqSEXP, SEXP relationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type seq(seqSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type relation(relationSEXP);
    rcpp_result_gen = Rcpp::wrap(monotonC(seq, relation));
    return rcpp_result_gen;
END_RCPP
}
// getMonotonicBlocksC
List getMonotonicBlocksC(NumericVector mon);
RcppExport SEXP _TTP_getMonotonicBlocksC(SEXP monSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type mon(monSEXP);
    rcpp_result_gen = Rcpp::wrap(getMonotonicBlocksC(mon));
    return rcpp_result_gen;
END_RCPP
}
// shiftTourC
NumericVector shiftTourC(NumericVector tour);
RcppExport SEXP _TTP_shiftTourC(SEXP tourSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tour(tourSEXP);
    rcpp_result_gen = Rcpp::wrap(shiftTourC(tour));
    return rcpp_result_gen;
END_RCPP
}
// getUncommonEdges
double getUncommonEdges(NumericVector tour1, NumericVector tour2, bool normalize);
RcppExport SEXP _TTP_getUncommonEdges(SEXP tour1SEXP, SEXP tour2SEXP, SEXP normalizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type tour1(tour1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tour2(tour2SEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    rcpp_result_gen = Rcpp::wrap(getUncommonEdges(tour1, tour2, normalize));
    return rcpp_result_gen;
END_RCPP
}
// getNumberOfInversionsC
double getNumberOfInversionsC(IntegerVector v1, IntegerVector v2, bool normalize);
RcppExport SEXP _TTP_getNumberOfInversionsC(SEXP v1SEXP, SEXP v2SEXP, SEXP normalizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type v1(v1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type v2(v2SEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    rcpp_result_gen = Rcpp::wrap(getNumberOfInversionsC(v1, v2, normalize));
    return rcpp_result_gen;
END_RCPP
}
// getMaximumDistanceC
double getMaximumDistanceC(IntegerVector tour1, IntegerVector tour2, bool normalize);
RcppExport SEXP _TTP_getMaximumDistanceC(SEXP tour1SEXP, SEXP tour2SEXP, SEXP normalizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type tour1(tour1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type tour2(tour2SEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    rcpp_result_gen = Rcpp::wrap(getMaximumDistanceC(tour1, tour2, normalize));
    return rcpp_result_gen;
END_RCPP
}
// getRunsC
List getRunsC(IntegerVector tour1, IntegerVector tour2, bool normalize);
RcppExport SEXP _TTP_getRunsC(SEXP tour1SEXP, SEXP tour2SEXP, SEXP normalizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type tour1(tour1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type tour2(tour2SEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    rcpp_result_gen = Rcpp::wrap(getRunsC(tour1, tour2, normalize));
    return rcpp_result_gen;
END_RCPP
}
// getWeightByNodeC
NumericVector getWeightByNodeC(List problem, IntegerVector itemPackingPlan, NumericVector itemWeights, IntegerVector itemAssignedNodes);
RcppExport SEXP _TTP_getWeightByNodeC(SEXP problemSEXP, SEXP itemPackingPlanSEXP, SEXP itemWeightsSEXP, SEXP itemAssignedNodesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type problem(problemSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type itemPackingPlan(itemPackingPlanSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type itemWeights(itemWeightsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type itemAssignedNodes(itemAssignedNodesSEXP);
    rcpp_result_gen = Rcpp::wrap(getWeightByNodeC(problem, itemPackingPlan, itemWeights, itemAssignedNodes));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_TTP_runWTSPSolverC", (DL_FUNC) &_TTP_runWTSPSolverC, 8},
    {"_TTP_tspC", (DL_FUNC) &_TTP_tspC, 2},
    {"_TTP_wtspC", (DL_FUNC) &_TTP_wtspC, 5},
    {"_TTP_ttpC", (DL_FUNC) &_TTP_ttpC, 7},
    {"_TTP_monotonC", (DL_FUNC) &_TTP_monotonC, 2},
    {"_TTP_getMonotonicBlocksC", (DL_FUNC) &_TTP_getMonotonicBlocksC, 1},
    {"_TTP_shiftTourC", (DL_FUNC) &_TTP_shiftTourC, 1},
    {"_TTP_getUncommonEdges", (DL_FUNC) &_TTP_getUncommonEdges, 3},
    {"_TTP_getNumberOfInversionsC", (DL_FUNC) &_TTP_getNumberOfInversionsC, 3},
    {"_TTP_getMaximumDistanceC", (DL_FUNC) &_TTP_getMaximumDistanceC, 3},
    {"_TTP_getRunsC", (DL_FUNC) &_TTP_getRunsC, 3},
    {"_TTP_getWeightByNodeC", (DL_FUNC) &_TTP_getWeightByNodeC, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_TTP(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
