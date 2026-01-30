/*******************************************************************************
 *
 * Functions common to the getPhraseDoc function and the getPhraseDocParallel 
 * function
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#include "phraseDoc.h"
#include <chrono>
#include <ctime>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>
#include <sstream>
#include <Rcpp.h>
/*******************************************************************************
 * Internal Functions
 ******************************************************************************/
/*******************************************************************************
 * Get the current time
 ******************************************************************************/
void print_time_cpp(const std::string &txt) {
  auto now = std::chrono::system_clock::now();
  auto now_ms = std::chrono::time_point_cast<std::chrono::milliseconds>(now);
  auto value = now_ms.time_since_epoch();
  long ms = value.count() % 1000;
  
  std::time_t t = std::chrono::system_clock::to_time_t(now);
  
  // Format the time and print it
  // Rcout is used for R instead of std::cout
  Rcpp::Rcout << txt << std::put_time(std::localtime(&t), "%Y-%m-%d %H:%M:%S")
        << "." << std::setfill('0') << std::setw(3) << ms<<std::endl;  
}
/*******************************************************************************
 * getBlocks: Split a document into blocks
 * Input:  txt: contents of a document to be split into pieces
 * Output: A character vector containing a block of text in each field
 ******************************************************************************/
std::vector<std::string> get_blocks_cpp(const std::string &txt) {
  std::vector<std::string> blocks;
  std::string block;
  //Reserve space in memory (to make it faster)
  block.reserve(128);
  
  size_t i = 0;
  const size_t n = txt.size();
  
  //Go through all characters in text
  while (i < n) {

    char c = txt[i++];
    
    // skip en dash and em dash
    unsigned char uc = static_cast<unsigned char>(c);
    if (uc == 0xE2 && i + 2 < n) {
      // Check for en dash U+2013 (E2 80 93) or em dash U+2014 (E2 80 94)
      if (static_cast<unsigned char>(txt[i]) == 0x80 &&
          (static_cast<unsigned char>(txt[i+1]) == 0x93 ||
          static_cast<unsigned char>(txt[i+1]) == 0x94)) {
        i += 2;  // skip the remaining two bytes
        continue; // skip this character
      }
    }
    
    // Handle dashes
    if (c == '-') {
      bool space_before = (i < 2) || txt[i - 2]==' ';
      bool space_after  = (i == n) || txt[i]==' ';
      
      if (space_before && space_after) {
        if (!block.empty() && block.find_first_not_of(" \t\n\r") != std::string::npos) {
          blocks.push_back(block);
        }
        block.clear();
        ++i; // skip the space
        continue;
      } else {
        // dash is internal (like +/- or word-word)
        block += c;
        continue;
      }
    }
    
    // Handle closing braces and quotes: End block immediately
    if (c == ')' || c == ']' || c == '}' || c == '"' || c == '\'') {
      if (!block.empty() && block.find_first_not_of(" \t\n\r") != std::string::npos)
        blocks.push_back(block);
      block.clear();
      continue;
    }
    
    //Handle punctuation
    if (c == '.' || c == ',' || c == ':'|| c == ';' || c == '?' || c == '!') {
      //Create a block only if followed by a space, end of text, or a quote
      if (i >= n || txt[i] == ' ' || txt[i] == '"' || txt[i] == '\'') {
        if (!block.empty() && block.find_first_not_of(" \t\n\r") != std::string::npos) 
          blocks.push_back(block);
        block.clear();
        ++i; // skip the space
        continue;    
      } else { // Punctuation is not followed by space → keep it
        block += c;
        continue;
      }
    }

    //Handle matched pairs
    if (c == '(' || c == '[' || c == '{') {
      char closing;
      switch (c) {
      case '(': closing = ')'; break;
      case '[': closing = ']'; break;
      case '{': closing = '}'; break;
      default: closing = 0;    // Should not happen
      }
      
      size_t j = i;
      while (j < n) {
        if (txt[j] == ' ') break;
        if (txt[j] == closing) {
          block += c;
          while (i <= j) {
            block += txt[i++];
          }
          break;
        }
        ++j;
      }
      continue;
    }
    
    block += c; //No punctuation found, so we keep building the block
  }
  
  //Create a block only if there is at least one non-space character 
  if (!block.empty() && block.find_first_not_of(" \t\n\r") != std::string::npos) 
    blocks.push_back(block);  
  return blocks;
}
/*******************************************************************************
 * getWords: Split a block of words into words
 * Input:  txt: A block of text
 * Output: A character vector containing a a word in each field
 ******************************************************************************/
std::vector<std::string> get_words_cpp(const std::string &txt) {
  // Convert to lowercase
  std::string lower;
  lower.reserve(txt.size()); // Avoid repeated reallocs
  for (char c : txt)
    lower += std::tolower(static_cast<unsigned char>(c));
  
  // Split on spaces
  std::vector<std::string> words;
  //Create iss as an input string stream on lower
  std::istringstream iss(lower);
  std::string word;
  while (iss >> word) {   //operator>> automatically skips whitespace
    words.push_back(word);
  }
  
  return words;
}
/*******************************************************************************
 * cleanPhrases: Remove phrases with keep=false and their positions; also
 * remove positions with keep=false 
 * Input:  vectors with positions and phrases
 * Output: positions and phrases are updated
 ******************************************************************************/
void clean_phrases_cpp(std::vector<Position>& positions,
                       std::vector<Phrase>& phrases) {
  
  std::vector<Phrase> new_phrases;
  //Create a vector of integers called remap, same size as phrases,
  //but with each value initialized to the largest possible size
  std::vector<size_t> remap(phrases.size(), static_cast<size_t>(-1));
  //Fill new_phrases, and set the new index for each old
  //phrase in remap.
  for (size_t i = 0; i < phrases.size(); ++i) {
    if (!phrases[i].keep) continue; 
    remap[i] = new_phrases.size(); // old -> new index
    new_phrases.push_back(std::move(phrases[i]));
  }
  
  // Use remap to update phrase_idx in kept positions
  std::vector<Position> new_positions;
  for (auto& pos : positions) {
    if (pos.keep && remap[pos.phrase_idx] != static_cast<size_t>(-1)) {
      pos.phrase_idx = remap[pos.phrase_idx];
      new_positions.push_back(pos);
    }
  }
  
  // Replace originals
  phrases = std::move(new_phrases);
  positions = std::move(new_positions);
}
/*******************************************************************************
 * rectifyPhrases: Determine which phrases are principal, then set keep to false
 * for all positions that start or end in positions of the principal phrases.
 * Input:  vectors with positions and phrases
 * Output: positions and phrases are updated
 ******************************************************************************/
void rectify_phrases_cpp(std::vector<Position>& positions,
                         std::vector<Phrase>& phrases,
                         const int &minFreq) {
  
  // Create sorted list of indices (not modifying original vector)
  // We sort by pwrds descending and freq within that.
  std::vector<size_t> sorted_indices(phrases.size());
  std::iota(sorted_indices.begin(), sorted_indices.end(), 0);
  
  std::sort(sorted_indices.begin(), sorted_indices.end(),
            [&](size_t a, size_t b) {
              if (phrases[a].pwrds != phrases[b].pwrds)
                return phrases[a].pwrds > phrases[b].pwrds;
              return phrases[a].freq > phrases[b].freq;
            });
  
  //Map positions by doc/block for faster access
  robin_hood::unordered_map<int, robin_hood::unordered_map<int, std::vector<Position*>>> block_map;
  for (auto &pos : positions) {
    block_map[pos.doc][pos.block].push_back(&pos);
  }
  //This means that block_map[0][0] contains an unordered collection of pointers
  //to all positions in doc 0 and block 0.
  
  //Now order them by position
  for (auto &doc_pair : block_map) {
    for (auto &block_pair : doc_pair.second) {
      auto &vec = block_pair.second;
      std::sort(vec.begin(), vec.end(),
                [](Position* a, Position* b){ return a->pos < b->pos; });
    }
  }
  
  //Map positions by phrase_idx
  robin_hood::unordered_map<size_t, std::vector<Position*>> ph_pos;
  for (auto &pos : positions) {
    ph_pos[pos.phrase_idx].push_back(&pos);
  }
  
  // Now loop over phrases in sorted order via index
  for (size_t i : sorted_indices) {
    Phrase& ph = phrases[i];
    size_t cnt = 0;
    
    auto &pos_list=ph_pos[i];
    
    for (auto &pos : pos_list) {
      // i is the index of current phrase
      if (pos->keep) ++cnt; 
    }
    ph.freq = cnt;
    if (cnt < static_cast<size_t>(minFreq)) {
      ph.keep=false;
      continue; //Process the next phrase
    }
    //The phrase is a principal phrase

    // Invalidate overlapping positions within the same doc/block
    for (auto &pos1 : pos_list) {
      if (!pos1->keep) continue; 
      
      //For each position of the principal phrase, we run though all positions
      //in the same doc and block, and if any start or end within the first 
      //position, we do not keep it.
      auto &block_positions = block_map[pos1->doc][pos1->block];
      for (auto &pos2 : block_positions) {
        if (pos2->keep && pos2->phrase_idx != i) {
          // If pos2 overlaps pos1, mark it false
          // If the first position of pos2 starts after the last position of
          // pos1, none of the pos2 positions that come after overlap, so break
          if (pos2->pos > pos1->lst) break;
          if (pos2->keep && pos2->phrase_idx != i && pos2->lst >= pos1->pos) {
            pos2->keep = false;
          }
        }
      }
    }
  }
  //Remove phrases and positions after rectification if needed
  clean_phrases_cpp(positions,phrases);
}
