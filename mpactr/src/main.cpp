#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <string>

#include "Math.h"
#include "CvFilter.h"

// [[Rcpp::export]]
Rcpp::List FilterMispickedIons(const Rcpp::DataFrame &peakTable, const double ringWin, const double isoWin,
                               const double trWin, const double maxIsoShift) {
    const auto rows = static_cast<size_t>(peakTable.nrows());
    const Rcpp::NumericVector mzVector = peakTable["mz"];
    const Rcpp::NumericVector rtVector = peakTable["rt"];
    const Rcpp::CharacterVector compoundVector = peakTable["Compound"];
    std::vector<std::string> cutIons;
    cutIons.reserve(rows);
    std::vector<bool> cutIonsChecked(rows, false);
    std::unordered_map<std::string, size_t> cutIonDict;
    std::vector<std::vector<std::string> > mergeGroupList(rows);
    std::vector<std::string> nameVector;
    std::vector<size_t> indexSwap(rows);
    nameVector.reserve(rows);
    mergeGroupList.reserve(rows);
    for (size_t i = 0; i < rows - 1; i++) {
        const double mz = mzVector[i];
        const double rt = rtVector[i];
        const Rcpp::String compound = compoundVector[i];
        bool addName = false;
        for (size_t j = i + 1; j < rows; j++) {
            const Rcpp::String nextCompound = compoundVector[j];
            if (cutIonsChecked[j]) continue;
            const double nextMz = mzVector[j];
            const double nextRt = rtVector[j];
            const double massDifference = nextMz - mz;
            const double kmdDifference = massDifference - std::floor(massDifference);
            const bool shiftDifference = std::abs(massDifference) > maxIsoShift - 0.4;
            const double rtDifference = nextRt - rt;
            const double ringBand = std::fmod(std::floor(std::abs(massDifference) * (1 / ringWin)), (1 / ringWin));
            const double doubleBand = kmdDifference - 0.5004 - (std::floor(massDifference) * 0.007);
            const bool betweenIonCalculation = std::abs(rtDifference) <= trWin && (massDifference <= maxIsoShift - 0.4)
                                               &&
                                               (ringBand == 0 || doubleBand < isoWin);

            if (!shiftDifference && betweenIonCalculation) {
                cutIons.emplace_back(nextCompound);
                cutIonsChecked[j] = true;
                mergeGroupList[i].emplace_back(nextCompound);
                addName = true;
            }
        }
        if (addName)
            nameVector.emplace_back(compound);
    }
    size_t size = nameVector.size();
    Rcpp::List mergeGroups(static_cast<int>(size));
    size_t count = 0;
    for (size_t i = 0; i < rows; i++) {
        if (mergeGroupList[i].empty()) continue;
        mergeGroups[count++] = mergeGroupList[i];
    }
    mergeGroups.attr("names") = nameVector;


    return Rcpp::List::create(Rcpp::Named("cut_ions") = cutIons,
                              Rcpp::Named("merge_groups") = mergeGroups);
}
// [[Rcpp::export]]
Rcpp::DataFrame FilterCV(const Rcpp::DataFrame& peakTable, const std::vector<std::string>& uniqueSampleList,
    const double cvCutOff, const size_t replicates) {
    // Map the peak table data to a class
    CvFilter table;
    table.CalculateCV(peakTable, uniqueSampleList, cvCutOff, replicates);
    return table.GetCvTable();
}

// [[Rcpp::export]]
Rcpp::StringVector UniqueDuplicates(Rcpp::StringVector &compoundNames) {
    const size_t size = compoundNames.size();
    std::unordered_set<Rcpp::String> duplicates;
    for (size_t i = 0; i < size; i++) {
        const Rcpp::String& compound = compoundNames[i];
        if(duplicates.find(compound) == duplicates.end()) {
            duplicates.insert(compound);
            continue;
        }
        bool duplicate = true;
        int count = 1;
        while(duplicate) {
            Rcpp::String newCompound = compound;
            newCompound.push_back("_" + std::to_string(count++));
            if(duplicates.find(newCompound) == duplicates.end()) {
                duplicates.insert(newCompound);
                compoundNames[i] = newCompound;
                duplicate = false;
                break;
            }
        }
    }
    return compoundNames;
}
