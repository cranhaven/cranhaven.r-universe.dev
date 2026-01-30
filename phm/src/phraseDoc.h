/*******************************************************************************
 *
 * Functions common to the phm function and the phmParallel functions
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#pragma once
#include <string>
#include <vector>
#include <cctype>
#include "robin_hood.h"
/*******************************************************************************
 * Classes
 ******************************************************************************/
//Stores a phrase and information about the phrase
struct Phrase {
  size_t pool_idx;        // Index of the phrase in the pool
  int pwrds;              // number of words in the phrase
  size_t freq = 0;        // Number of positions for the phrase
  bool keep = false;      // Should the phrase be kept?
};

// Stores a phrase location (in block, doc, etc.)
struct Position {
  size_t phrase_idx;      // Index into the phrases
  size_t doc;             // document index
  size_t block;           // block index
  size_t pos;             // starting word index
  size_t lst;             // Last word index
  bool keep = true;       // Should the position be kept?
};
/*******************************************************************************
 * Internal Function Declarations
 ******************************************************************************/
void print_time_cpp(const std::string &txt);
std::vector<std::string> get_blocks_cpp(const std::string &text);
std::vector<std::string> get_words_cpp(const std::string &block);
void clean_phrases_cpp(std::vector<Position>& positions,
                       std::vector<Phrase>& phrases);
void rectify_phrases_cpp(std::vector<Position>& positions,
                         std::vector<Phrase>& phrases,
                         const int &minFreq);
