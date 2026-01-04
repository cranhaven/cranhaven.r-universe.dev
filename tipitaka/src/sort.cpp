#include "cpp11.hpp"
#include <string>
#include <vector>
#include <iostream>

using namespace cpp11;

// How many bytes is the UTF-8 character at byte p in string s?
std::size_t char_size(const std::string s, int p) {
   if (s.size() < p) return 0;
   std::size_t length = 1;
   while ((s[p + length] & 0b11000000) == 0b10000000) {
      ++length;
   }
   return length;
}

// The full Pali alphabet in Pali alphabetical order.
// Note that "gh", "jh", etc are considered single characters.
const std::vector<std::string> c_pali_alphabet =
   {"a", "ā", "i", "ī", "u", "ū", "e", "o",
    "k", "kh", "g", "gh", "ṅ",
    "c", "ch", "j", "jh", "ñ",
    "ṭ", "ṭh", "ḍ", "ḍh", "ṇ",
    "t", "th", "d", "dh", "n",
    "p", "ph", "b", "bh", "m",
    "y", "r", "l", "v", "s", "h", "ḷ", "ṃ"};

const std::vector<std::string> c_pali_alphabet_upper =
   {"A", "Ā", "I", "Ī", "U", "Ū", "E", "O",
    "K", "KH", "G", "GH", "Ṅ",
    "C", "CH", "J", "JH", "Ñ",
    "Ṭ", "ṬH", "Ḍ", "ḌH", "Ṇ",
    "T", "TH", "D", "DH", "N",
    "P", "PH", "B", "BH", "M",
    "Y", "R", "L", "V", "S", "H", "Ḷ", "Ṃ"};




// c is a Pali character (ie, 1 or 2 actual letters)
// returns pali_alphabet.size() + 1 if c is not a valid Pali character
// Note we check twice so that comparisons are case-insensitive.
int pali_position(std::string c) {
   auto lower = distance(c_pali_alphabet.begin(),
                        find(c_pali_alphabet.begin(),
                             c_pali_alphabet.end(), c));
   auto upper = distance(c_pali_alphabet_upper.begin(),
                        find(c_pali_alphabet_upper.begin(),
                             c_pali_alphabet_upper.end(), c));
   return std::min(lower, upper);
}

// Return true if c is a valid Pali letter, in upper or lower case.
bool is_letter(std::string c) {
   if (find(c_pali_alphabet.begin(),
           c_pali_alphabet.end(), c) != c_pali_alphabet.end() &&
       find(c_pali_alphabet_upper.begin(),
                   c_pali_alphabet_upper.end(), c) != c_pali_alphabet_upper.end())
      return true;
   else
      return false;
}

// Does word1 come before word2 in Pali alphabetical word?
// This function is a bit hairy because it has to handle
// three complications:
//   1. Pali alphabetical order is different than standard C character order.
//   2. UTF-8 characters can be more than one byte.
//   3. Pali letters like "dh" and "th" have to be treated as one character.

[[cpp11::register]]
bool c_pali_lt(const std::string word1, const std::string word2) {

   // Total bytes in each word
   const auto word1_total_bytes = word1.size();
   const auto word2_total_bytes = word2.size();

   // Bytes compared already
   auto word1_byte_count = 0;
   auto word2_byte_count = 0;

   while (word1_byte_count < word1_total_bytes &&
          word2_byte_count < word2_total_bytes) {

      // How big is the next character in each word?
      const auto word1_next_char_size = char_size(word1, word1_byte_count);
      const auto word2_next_char_size = char_size(word2, word2_byte_count);

      // Get the next character in each word
      std::string word1_next_char =
         word1.substr(word1_byte_count, word1_next_char_size);
      std::string word2_next_char =
         word2.substr(word2_byte_count, word2_next_char_size);

      // What's the byte count to the next character?
      auto word1_next_byte_count = word1_byte_count + word1_next_char_size;
      auto word2_next_byte_count = word2_byte_count + word2_next_char_size;

      // Is there an "h" next in word1? And does that form a valid Pali letter?
      if (word1_next_byte_count < word1_total_bytes &&
          word1.substr(word1_next_byte_count, 1) == "h" &&
          is_letter(word1.substr(word1_byte_count, word1_next_char_size + 1))) {

            word1_next_char =
               word1.substr(word1_byte_count, word1_next_char_size + 1);
            word1_next_byte_count++;
      }

      // Same as above for word2.
      if (word2_next_byte_count < word2_total_bytes &&
          word2.substr(word2_next_byte_count, 1) == "h" &&
          is_letter(word1.substr(word2_byte_count, word2_next_char_size + 1))) {

         word2_next_char =
            word2.substr(word2_byte_count, word2_next_char_size + 1);
         word2_next_byte_count++;
      }
      /* Useful debugging:
      std::cout << "Comparing " << word1_next_char << " and " <<
         word2_next_char << ": " << pali_position(word1_next_char) <<
         " and " << pali_position(word2_next_char) << "\n";
      */

      // If word1 character comes before word2 character, return true.
      if (pali_position(word1_next_char) < pali_position(word2_next_char))
         return true;
      // If word1 character comes after word2 character, return false.
      else if (pali_position(word1_next_char) > pali_position(word2_next_char))
         return false;

      // Otherwise, increment the byte counts and repeat.
      word1_byte_count = word1_next_byte_count;
      word2_byte_count = word2_next_byte_count;
      }

      // If we get all the way through the above while loop,
      // one word is a subset of the other, so the shorter word
      // is less than the longer word.

      if (word1_total_bytes < word2_total_bytes)
         return true;
      else
         return false;
   }


// Sort vector of words into Pali alphabetical order.

[[cpp11::register]]
std::vector<std::string> c_pali_sort(std::vector<std::string> words) {
   std::sort(words.begin(), words.end(), c_pali_lt);
   return(words);
}


