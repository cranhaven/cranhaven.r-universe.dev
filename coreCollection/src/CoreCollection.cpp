#include <Rcpp.h>
#include "CoreSelection.h"
#include "CoreMethodAccessionNearestEntry.h"
#include "CoreMethodEntryNearestEntry.h"
#include "CoreMethodEntryEntry.h"
#include "CoreAlgorithmRandomDescent.h"

using namespace Rcpp;

//' Compute a random selection
//'
//' The function \code{computeRandomSelection} is used internally by
//' the \code{\link{CoreCollection}} object.
//'
//' This function returns a random selection of approximately size
//' \code{requiredN} by choosing entries sequentually and randomly, while excluding all entries
//' within a certain radius of an entry chosen before, and by finding iteratively the most
//' appropiate radius to end up with a number close to \code{requiredN} of selected entries.
//'
//' @param dist distance matrix, used for distances and implicitly defining the set of entries
//' @param requiredN the required size of the random selection
//' @param preselected a list of preselected entries, referring to the row/column of \code{dist}
//' @param seed the applied seed for the randomizer
//'
//' @keywords internal
//'
// [[Rcpp::export]]
Rcpp::IntegerVector computeRandomSelection(Rcpp::NumericMatrix & dist, int requiredN, Rcpp::IntegerVector & preselected, int seed) {
  CoreSelection::initialise(seed);
  return CoreSelection::computeRandomSelection(dist, requiredN, preselected);
}

//' Compute selection - recompute method
//'
//' The function \code{computeAdjustedSelectionUsingRecomputeMethod} is used internally by
//' the \code{\link{CoreCollection}} object to compute an adjusted selection using the recompute
//' method.
//'
//' This function returns a list describing for each of the row/columns entries of \code{dist} the
//' closest selected entry. The entries are implicetly defined by the row/columns of
//' \code{dist} and referred to by a zero-based integer describing the position.
//'
//' @param dist distance matrix, used for distances and implicitly defining the set of entries
//' @param adjustedSelected the selected entries defined as a list of zero-based integers referring to the row/columns of \code{dist}
//'
//' @keywords internal
//'
// [[Rcpp::export]]
Rcpp::IntegerVector computeAdjustedSelectionUsingRecomputeMethod(Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & adjustedSelected) {
  return CoreSelection::createSelectionResult(dist, adjustedSelected);
}

//' Compute selection - split method
//'
//' The function \code{computeAdjustedSelectionUsingSplitMethod} is used internally by
//' the \code{\link{CoreCollection}} object to compute an adjusted selection using the split
//' method.
//'
//' This function returns a list describing for each of the row/columns entries of \code{dist} the
//' corresponding entry referred to in \code{groups}. However, groups with one or multiple
//' \code{preselected} entries are divided, and the returned list wil contain references to
//' the closest preselected entry within this group, implying a split if multiple preselected
//' entries occur within one group. The entries are implicetly defined by the row/columns of
//' \code{dist} and referred to by a zero-based integer describing the position.
//'
//' @param dist distance matrix, used for distances and implicitly defining the set of entries
//' @param groups the initial division into group defined as a list of zero-based integers referring to the row/columns of \code{dist}
//' @param preselected the set of preselected entries
//'
//' @keywords internal
//'
// [[Rcpp::export]]
Rcpp::IntegerVector computeAdjustedSelectionUsingSplitMethod(Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & groups, Rcpp::IntegerVector & preselected) {
  if(preselected.length()>0) {
    int i,j,nl,selectedGroup;
    //full number
    int N = dist.nrow();
    //initialise new
    Rcpp::IntegerVector newGroups(N);
    double newDistance;
    std::vector<double> newDistances(N);
    for(i=0; i<N; i++) {
      newGroups[i] = groups[i];
      newDistances[i] = 0.0;
    }
    //split existing groups
    int preselectedLength = preselected.length();
    for(j=0; j<preselectedLength; j++) {
      selectedGroup = groups[preselected[j]];
      nl=0;
      for(i=0; i<N; i++) {
        if(i==preselected[j]) {
          newGroups[i]=preselected[j];
          newDistances[i] = 0.0;
        } else if(groups[i]==selectedGroup) {
          newDistance = dist[nl+preselected[j]];
          if((newGroups[i]==selectedGroup) && (newDistance<newDistances[i])) {
            newGroups[i]=preselected[j];
            newDistances[i]=newDistance;
          }
        }
        nl+=N;
      }
    }
    return newGroups;
  } else {
    return groups;
  }
}

//' Compute the core
//'
//' The function \code{computeCore} is used internally by
//' the \code{\link{CoreCollection}} object to compute the core.
//'
//' The \code{A-NE} method requires the core to minimize the average distance between each accession and
//' the nearest entry within the core. The \code{E-NE} method requires the core to maximize the
//' average distance between each core entry and its nearest neighbouring entry within the core.
//' The \code{E-E} method requires the core to maximize the average distance between all core entries.
//'
//' @param algorithm applied algorithm to find solution with method: currently, only \code{randomDescent} is available
//' @param method required method for choosing the entries within the groups: \code{A-NE} (accession nearest entry), \code{E-NE} (entry nearest entry) or \code{E-E} (entry entry)
//' @param dist distance matrix, used for distances and implicitly defining the set of entries
//' @param groups the initially created subdivision into groups
//'
//' @keywords internal
//'
// [[Rcpp::export]]
Rcpp::IntegerVector computeCore(std::string algorithm, std::string method, Rcpp::NumericMatrix & dist, Rcpp::List & groups) {
  CoreAlgorithm* a;
  CoreMethod* m;
  if(algorithm==ALGORITHM_RANDOM_DESCENT) {
    a = new CoreAlgorithmRandomDescent();
  } else {
    return Rcpp::IntegerVector(0);
  }
  if(method==METHOD_ACCESSION_NEAREST_ENTRY) {
    m = new CoreMethodAccessionNearestEntry(dist, groups);
  } else if(method==METHOD_ENTRY_NEAREST_ENTRY) {
    m = new CoreMethodEntryNearestEntry(dist, groups);
  } else if(method==METHOD_ENTRY_ENTRY) {
    m = new CoreMethodEntryEntry(dist, groups);
  } else {
    return Rcpp::IntegerVector(0);
  }
  Rcpp::IntegerVector result(clone(a->getCore(*m)));
  delete m;
  delete a;
  return result;
}

// [[Rcpp::export]]
double computeMeasure(std::string method, Rcpp::NumericMatrix & dist, Rcpp::IntegerVector & c) {
  if(method==METHOD_ACCESSION_NEAREST_ENTRY) {
    return CoreMethodAccessionNearestEntry::measure(dist,c);
  } else if(method==METHOD_ENTRY_NEAREST_ENTRY) {
    return CoreMethodEntryNearestEntry::measure(dist,c);
  } else if(method==METHOD_ENTRY_ENTRY) {
    return CoreMethodEntryEntry::measure(dist,c);
  } else {
    return 0;
  }
}
