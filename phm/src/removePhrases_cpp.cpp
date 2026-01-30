/*******************************************************************************
 *
 * Remove phrases from a phraseDoc
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#include "phraseDoc.h"
#include <Rcpp.h>
#include <unordered_set>
#include <algorithm>  // for transform

using namespace Rcpp;
/*******************************************************************************
 * Classes
 ******************************************************************************/
/*******************************************************************************
 * Internal Functions
 ******************************************************************************/
/*******************************************************************************
 * Main function (removePhrases_cpp)
 ******************************************************************************/
// [[Rcpp::export]]
List removePhrases_cpp(const std::vector<int> &idx,
                       const std::vector<int> &doc,
                       const std::vector<int> &block,
                       const std::vector<int> &pos,
                       const std::vector<std::string> &phrs,
                       const std::vector<int> &frq,
                       const std::vector<int> &wrds,
                       const std::vector<std::string> &exclude_phrs) {
  
  // Initialization
  robin_hood::unordered_map<std::string, size_t> phrase_pool;
  std::vector<std::string> pool_idx_to_phrase;
  std::vector<Phrase> phrases;
  std::vector<Position> positions;
  std::unordered_set<std::string> exclude_set;

  //Reserve space
  phrase_pool.reserve(phrs.size());
  pool_idx_to_phrase.reserve(phrs.size());
  phrases.reserve(phrs.size());
  positions.reserve(idx.size());
  exclude_set.reserve(exclude_phrs.size());
  
  // Create exclude_set, a lowercase, trimmed version of excluded phrases
  for (const auto& ex : exclude_phrs) {
    std::string trimmed = ex;
    trimmed.erase(0, trimmed.find_first_not_of(" \t\n\r"));
    trimmed.erase(trimmed.find_last_not_of(" \t\n\r") + 1);
    std::transform(trimmed.begin(), trimmed.end(), trimmed.begin(), ::tolower);
    exclude_set.insert(trimmed);
  }
  
  // Load phrases and pool_idx_to_phrase
  for (size_t i = 0; i < phrs.size(); ++i) {
    std::string phr = phrs[i];
    auto it = phrase_pool.find(phr);
    size_t pool_idx;
    
    if (it == phrase_pool.end()) {
      pool_idx = pool_idx_to_phrase.size();
      phrase_pool[phr] = pool_idx;
      pool_idx_to_phrase.push_back(phr);
    } else {
      pool_idx = it->second;
    }
    
    Phrase p;
    p.pool_idx = pool_idx;
    p.pwrds = wrds[i];
    p.freq = static_cast<size_t>(frq[i]);    
    // keep = true if not excluded
    p.keep = exclude_set.find(phr) == exclude_set.end(); // keep = true if not excluded  
    phrases.push_back(p);
  }
  
  // Load positions
  positions.reserve(idx.size());
  for (size_t i = 0; i < idx.size(); ++i) {
    Position p;
    p.phrase_idx = static_cast<size_t>(idx[i] - 1); // R to C++ index
    p.doc = static_cast<size_t>(doc[i] - 1);
    p.block = static_cast<size_t>(block[i] - 1);
    p.pos = static_cast<size_t>(pos[i] - 1);
    positions.push_back(p);
  }
  
  //Remove phrases and positions if needed (keep=false)
  clean_phrases_cpp(positions,phrases);

  //Get indices of phrases in alphabetical order
  std::vector<size_t> alpha_idx(phrases.size());
  std::iota(alpha_idx.begin(), alpha_idx.end(), 0);
  std::sort(alpha_idx.begin(), alpha_idx.end(),[&](size_t a, size_t b) {
    return pool_idx_to_phrase[phrases[a].pool_idx] < pool_idx_to_phrase[phrases[b].pool_idx];
  });
  //Also need a reverse lookup
  std::vector<size_t> new_idx(phrases.size(), static_cast<size_t>(-1));
  for (size_t i = 0; i < alpha_idx.size(); ++i) {
    new_idx[alpha_idx[i]] = i;
  }
  
  //Prepare for output
  std::vector<int> p_idx, pdoc, pblock, ppos, freq, pwrds;
  std::vector<std::string> phr;
  //Reserve space for the variables
  p_idx.reserve(positions.size());
  pdoc.reserve(positions.size());
  pblock.reserve(positions.size());
  ppos.reserve(positions.size());
  phr.reserve(phrases.size());
  freq.reserve(phrases.size());
  pwrds.reserve(phrases.size());
  
  //Get info from the phrases
  for (const auto& ph : alpha_idx) {
    phr.push_back(pool_idx_to_phrase[phrases[ph].pool_idx]);
    freq.push_back(phrases[ph].freq);
    pwrds.push_back(phrases[ph].pwrds);
  }

  //Prepare positions data, but note all indices need to be 1 higher in R
  for (const auto& pos : positions) {
    //p_idx must have the alpha numeric R index of the phrase 
    p_idx.push_back(new_idx[pos.phrase_idx]+1);          
    pdoc.push_back(static_cast<int>(pos.doc+1));
    pblock.push_back(static_cast<int>(pos.block+1));
    ppos.push_back(static_cast<int>(pos.pos+1));
  }
  
  List pr = List::create(
    Named("phrase") = wrap(phr),
    Named("freq")   = wrap(freq),
    Named("pwrds")  = wrap(pwrds)
  );

  return List::create(
    Named("phrase")    = wrap(p_idx),
    Named("doc")       = wrap(pdoc),
    Named("block")     = wrap(pblock),
    Named("pos")       = wrap(ppos),
    Named("phrases")   = pr
  );
}
