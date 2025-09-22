//
// Created by Gregory Johnson on 8/22/25.
//

#include "CvFilter.h"
#include <unordered_set>
#include "Math.h"

void CvFilter::CalculateCV(const Rcpp::DataFrame& peakTable, const std::vector<std::string>& uniqueSampleList,
                     const double cvCutOff, const size_t replicates) {
    const std::vector<std::string>& compounds = peakTable["Compound"];
    const std::vector<std::string>& sampleCodes = peakTable["Sample_Code"];
    const std::vector<std::string>& biologicalGroups = peakTable["Biological_Group"];
    const std::vector<double> intensity = peakTable["intensity"];
    const size_t size = compounds.size();
    features = std::vector<FeatureData>(size);
    int index = 0;
    for (const auto& sample : uniqueSampleList) {
        sampleCodesToIndex[sample] = index++;
    }
    for (size_t i = 0, featureIndex = 0; i < size; i++) {
        const std::string& currentCompound = compounds[i];
        features[featureIndex].compoundName = currentCompound;
        features[featureIndex].metaData.intensityPerSample = std::vector<std::vector<double>>(uniqueSampleList.size());
        std::unordered_set<std::string> previousSampleCodes;
        while (compounds[i] == currentCompound) {
            if (previousSampleCodes.find(sampleCodes[i]) == previousSampleCodes.end()) {
                previousSampleCodes.insert(sampleCodes[i]);
                biologicalGroupsList.emplace_back(biologicalGroups[i]);
            }
            // fill in meta data, all the data should be sorted and we should
            // Get all the data for one compound as we iterate
            const int sampleIndex = sampleCodesToIndex[sampleCodes[i]];
            features[featureIndex].metaData.intensityPerSample[sampleIndex].emplace_back(intensity[i]);
            i++;
        }
        int idx = 0;
        for (const auto& intensityList : features[featureIndex].metaData.intensityPerSample) {
            bool hasPassedCv = false;
            compoundNamesToCV.emplace_back(currentCompound);
            sampleCodeList.emplace_back(uniqueSampleList[idx++]);
            double cvScore = VectorMath::CoefficientOfVarianceCalculation(intensityList);
            if (cvScore < cvCutOff && cvScore > 0) {
                hasPassedCv = true;
            }
            coefficientOfVariance.emplace_back(cvScore);
            passesCV.emplace_back(hasPassedCv);
        }
        featureIndex++;
        i--;
    }

}

Rcpp::DataFrame CvFilter::GetCvTable() const {
    return Rcpp::DataFrame::create(
        Rcpp::Named("Compound") = compoundNamesToCV,
        Rcpp::Named("Biological_Group") = biologicalGroupsList,
        Rcpp::Named("Sample_Code") = sampleCodeList,
        Rcpp::Named("PassesCvFilter") = passesCV,
        Rcpp::Named("cv") = coefficientOfVariance);
}
