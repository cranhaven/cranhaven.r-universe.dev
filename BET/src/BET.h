/*
 * BET.h
 *
 *  Created on: Oct 20, 2020
 *      Author: wanzh
 */

#ifndef BET_H_
#define BET_H_

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <random>
#include <Rcpp.h>

namespace N
{
	class BETfunction
	{
	   public:
		int d;
		int getStats();
		double getPvalue();
		std::string getInteraction();
		std::map<std::string, int, std::greater<std::string>> getGrids();
		std::vector<std::vector<std::string>> getSymmInteraction();
		std::vector<int> getSymmStats();
		std::vector<std::vector<int>> getAllColor();
		std::vector<std::string> getBinary();
		void Beast(std::size_t m, std::size_t B, double lambda, bool test_uniformity, bool test_independence, std::vector<std::vector<std::size_t>>& independence_index);
		double getBeastStat();
		// std::vector<std::vector<int>> getBeastInteraction();
		std::string getBeastInteraction();
		BETfunction(std::vector<std::vector<double>>& X_R, int depth, bool unif, bool asymptotic, bool test_uniformity, bool test_independence, std::vector<std::vector<std::size_t>>& independence_index);

	   private:
		bool unifMargin;
    bool asympt;
    bool testUnif;
    bool testIndep;
    std::vector<std::vector<std::size_t>> indepIndex;
		std::size_t numThread = 1;
		std::size_t p;
		std::size_t n;
		std::vector<std::vector<double>> X;
		std::vector<std::string> binary_inter;
		// std::vector<std::string> symminter;
		std::vector<std::vector<std::string>> out_symminter;
		std::vector<int> out_symmstats;
		std::vector<long long> bid;
		std::vector<std::string> inter;
		std::vector<std::vector<int>> inter_mat;
		std::vector<std::vector<std::size_t>> allidx;
		std::vector<std::size_t> thread;
		// new: ecdf & count cells
		std::vector<std::vector<int>> cell;
		std::map<std::vector<int>, int> countGrid;
		std::unordered_map<std::string, int> countInteraction;
		std::vector<std::vector<int>> pat;
		bool ifIter = 0;
		// std::vector<int> freqInter;
		// std::vector<std::vector<int>> BeastInter;
		std::string freqInter;
		int Stats = 0;
		double BeastStat = 0;
		double pvalue = 0;
		// std::vector<std::vector<double>> imp(Rcpp::NumericMatrix& X);
		std::string interaction_str;
		std::vector<std::vector<int>> interactions();
		std::vector<std::vector<int>> interaction_mat();
		std::vector<std::string> interaction_index();
		std::string binaryToNum(std::string binary);
		std::vector<long long> BIDs();
		std::vector<std::vector<int>> ecdf_loc(std::vector<std::vector<double>>& X);
		std::map<std::vector<int>, int> groupC(std::vector<std::vector<int>>& c);
		int locate(int c, long long b);
		std::vector<std::vector<std::vector<int>>> CBIDs(std::map<std::vector<int>, int>& count);
		//new func: all 1/-1 for n points
		std::vector<std::vector<int>> allColor(std::vector<std::vector<int>>& Cij);
		std::vector<std::vector<std::size_t>> allComb(size_t p, int dep);
		bool isIndex(std::vector<std::size_t>& idx);
		std::vector<int> symmstats(std::vector<int>& countValues, std::vector<std::vector<std::vector<int>>>& matrix, size_t sampleSize);
		//new version for symmstats: iterBET
		std::map<std::vector<int>, int> allPosition(int dep);
		std::vector<std::vector<int>> getPattern();
		std::string convert(size_t idx);
		std::vector<std::string> idxFilter();
		std::vector<int> iterBET(std::map<std::vector<int>, int>& posi);


		std::vector<std::size_t> unreplaceShuffle(std::size_t size, std::size_t max_size);
		std::vector<double> subsample(size_t m, size_t B);
		std::vector<double> softthreshold(std::vector<double>& S, double lambda);
		double logHypergeometricProb(double* logFacs, int a, int b, int c, int d);
		double fisherExact(int a, int b, int c, int d);
		double binomial(int n, int k, double p);
		double pbinom(int n, int k, double p);
	};
}



#endif /* BET_H_ */
