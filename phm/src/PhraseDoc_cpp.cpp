/*******************************************************************************
 *
 * Obtain a list with the information to be contained in a Phrase Document
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#include "phraseDoc.h"
#include <Rcpp.h>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>
#include "robin_hood.h"

using namespace Rcpp;
/*******************************************************************************
 * Classes
 ******************************************************************************/
/*******************************************************************************
 * Internal Functions
 ******************************************************************************/
/*******************************************************************************
 * collect: Determine all n-grams within the blocks that do not start with a 
 * word in ssw, do not end with a word in sew, and are not equal to phrases in 
 * sp. Record the position of each n-gram found, in a list called ps containing
 * the n-gram, the number of words in the n-gram (pwrds), and the document,
 * block, and position numbers.
 * Input:  text, mn, mx, ssw, sew, sp, and Empty phrases and positions vectors
 * Output: positions and phrases are updated
 ******************************************************************************/
void collect_phrases_cpp(const std::vector<std::string> &text,
                         const int mn,
                         const int mx,
                         const std::vector<std::string> &ssw_vec,
                         const std::vector<std::string> &sew_vec,
                         const std::vector<std::string> &sp_vec,
                         int minFreq,
                         robin_hood::unordered_map<std::string, size_t> &phrase_pool,
                         std::vector<Phrase> &phrases,
                         std::vector<Position> &positions)
{
  // Convert once at the top for fast lookups
  robin_hood::unordered_set<std::string> ssw(ssw_vec.begin(), ssw_vec.end());
  robin_hood::unordered_set<std::string> sew(sew_vec.begin(), sew_vec.end());
  robin_hood::unordered_set<std::string> sp(sp_vec.begin(), sp_vec.end());
  
//  print_time_cpp("Start texts: ");
  for (size_t j=0;j < text.size();++j) { // iterate over text
    std::vector<std::string> bl=get_blocks_cpp(text[j]); //split text[j] into blocks
    if (bl.empty()) continue;
    
    for (size_t b=0; b < bl.size();++b) { //iterate over each block
      std::vector<std::string> wds=get_words_cpp(bl[b]); //split block into words
      
      for (size_t w=0; w < wds.size();++w) { //iterate over each word in wds
        //w is the index of the first word in a phrase
        // #Phrase may not start with a stopStartWord
        if (ssw.find(wds[w]) != ssw.end()) continue;
        
        //Create phrases of length l
        for (int l=mn;l<=mx;++l) {
          size_t lst=w+l-1; //Index of last word in the phrase
          //Phrase cannot go past the end of block
          if (lst >= wds.size()) break; //Go to the next w
          // #Phrase may not end with a stopendWord
          if (sew.find(wds[lst]) != sew.end()) continue; //go to the next l
          //Create a phrase
          std::string phr=wds[w];
          for (size_t i=w+1;i <= lst;++i) phr+=" " + wds[i];
          // Ignore if phrase is in stopPhrases
          if (sp.find(phr) != sp.end()) continue; //go to the next l
          
          //We found a position of a phrase to keep
          
          // --- check if phrase already exists ---
          // Look up or create phrase
          size_t pool_idx;
          auto it = phrase_pool.find(phr);
          if (it != phrase_pool.end()) { //Found a phrase in the lookup
            //In the lookup, first refers to the phrase, second to the index
            pool_idx = it->second;
            Phrase &ph = phrases[pool_idx];
            ph.freq += 1;
            if (ph.freq == static_cast<size_t>(minFreq)) ph.keep = true;
          } else { //New phrase, in pool and in phrases
            pool_idx = phrases.size();
            phrase_pool[phr] = pool_idx;
            bool keep = (minFreq == 1); // only true if freq will be 1 and threshold is 1
            Phrase ph_tmp = {pool_idx, l, 1, keep};
            phrases.push_back(ph_tmp);
          }
          Position pos_tmp = {pool_idx, j, b, w, lst};
          positions.push_back(pos_tmp);
        }
      }
    } //End of block
  } //All texts processed
  // Rcpp::Rcout << "#positions=" << positions.size() <<std::endl;
  // Rcpp::Rcout << "#phrases=" << phrases.size() <<std::endl;
}
/*******************************************************************************
 * Main function (PhraseDoc_cpp)
 ******************************************************************************/
// [[Rcpp::export]]
List PhraseDoc_cpp(const std::vector<std::string> &text,
                   const int mn,
                   const int mx,
                   const std::vector<std::string> &ssw_vec,
                   const std::vector<std::string> &sew_vec,
                   const std::vector<std::string> &sp_vec,
                   int minFreq,
                   int maxPhrases,
                   const bool shiny,
                   const bool silent,
                   const double sz) {
  
  if (!shiny&&!silent) print_time_cpp("Collection: ");
  
  //Vectors with our main structures
  std::vector<Phrase> phrases;
  std::vector<Position> positions;
  phrases.reserve(static_cast<size_t>(sz*235));
  positions.reserve(static_cast<size_t>(sz*225));
  //Each element in an unordered_map is an std::pair with variables first and
  //second. In this case, first points to a string, second to a size_t integer
  robin_hood::unordered_map<std::string, size_t> phrase_pool; // map phrase → index
  phrase_pool.reserve(static_cast<size_t>(sz*225));
  
  //Collection
  collect_phrases_cpp(text,mn,mx,ssw_vec,sew_vec,sp_vec,minFreq,phrase_pool,
                      phrases,positions);

  if (positions.empty()) stop("No phrases found");
  
  //Aim to have not much more than max.phrases left in the end, so we update
  //minFreq if needed
  std::vector<int> pfreq;            
  if (phrases.size() > static_cast<size_t>(maxPhrases)) {
    for (const auto& ph : phrases) {
      if (ph.freq >= static_cast<size_t>(minFreq)) pfreq.push_back(ph.freq);
    }
    std::sort(pfreq.begin(), pfreq.end(), std::greater<int>());
    if (pfreq.size() > static_cast<size_t>(maxPhrases) && pfreq[maxPhrases]>minFreq) {
      minFreq=pfreq[maxPhrases];
    }
  }
  
  // Set keep to false for phrases with frequencies that are too low.
  for (auto& ph : phrases) {
    if (ph.keep && ph.freq < static_cast<size_t>(minFreq)) ph.keep = false;
  }

  if (!shiny&&!silent) print_time_cpp("Pre_clean: ");
  //Remove phrases and positions if needed
  clean_phrases_cpp(positions,phrases);
  if (!shiny&&!silent) print_time_cpp("Post_clean: ");
  
  //Check if there are any left
  if (positions.size() == 0) {
    stop(std::string("No phrases have frequency greater than ") 
           + std::to_string(minFreq));
  }
  
  //Rectification 
  if (!shiny&&!silent) print_time_cpp("Rectification: ");
  rectify_phrases_cpp(positions,phrases,minFreq);

  //Post-processing
  if (!shiny&&!silent) print_time_cpp("Post-processing: ");

  //Create a map from phrase_id to phrase
  robin_hood::unordered_map<size_t, size_t> pool_idx_to_phrase_idx;
  pool_idx_to_phrase_idx.reserve(phrases.size());
  for (size_t i = 0; i < phrases.size(); ++i) {
    pool_idx_to_phrase_idx[phrases[i].pool_idx] = i;
  }
  robin_hood::unordered_map<size_t, std::string> phrase_idx_to_phrase;
  phrase_idx_to_phrase.reserve(phrases.size());
  for (const auto& pair : phrase_pool) {
    const std::string& phrase_str = pair.first;
    size_t pool_idx = pair.second;
    
    auto it = pool_idx_to_phrase_idx.find(pool_idx);
    if (it != pool_idx_to_phrase_idx.end()) { //Found it
      size_t phrase_idx = it->second;
      phrase_idx_to_phrase[phrase_idx] = phrase_str;
    }
  }
  
  //Get indices of phrases in alphabetical order
  std::vector<size_t> alpha_idx(phrases.size());
  std::iota(alpha_idx.begin(), alpha_idx.end(), 0);
  
  std::sort(alpha_idx.begin(), alpha_idx.end(), [&](size_t a, size_t b) {
    return phrase_idx_to_phrase[a] < phrase_idx_to_phrase[b];
  });  
  //Also need a reverse lookup
  std::vector<size_t> new_idx(phrases.size(), static_cast<size_t>(-1));
  for (size_t i = 0; i < alpha_idx.size(); ++i) { //new -> old
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
    size_t pool_idx = phrases[ph].pool_idx;
    size_t phrase_idx = pool_idx_to_phrase_idx[pool_idx];
    phr.push_back(phrase_idx_to_phrase[phrase_idx]);
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
