/*******************************************************************************
 *
 * Get information from a PubMed file
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#include <Rcpp.h>
using namespace Rcpp;
#include <fstream>
#include <string>
#include <vector>
/*******************************************************************************
 * Classes
 ******************************************************************************/
/*******************************************************************************
 * Internal Functions
 ******************************************************************************/
// [[Rcpp::export]]
List getPubMed_cpp(const std::string &filename) {
  std::ifstream file(filename, std::ios::binary | std::ios::ate);
  if (!file.is_open()) {
    Rcpp::stop("Error: Could not open the file.");
  }
  std::streamsize file_size = file.tellg();
  file.seekg(0);  // reset to beginning
  
  //Estimate number of documents
  double docs_per_kb = 0.42; // maximum
  size_t estimated_n = static_cast<size_t>((file_size / 1024.0) * docs_per_kb);
  if (estimated_n < 100) estimated_n = 100; // ensure some minimal reserve
  
  //Initialization
  std::vector<std::string> pmids, titles, abs, dates, authors, author1s;
  pmids.reserve(estimated_n);
  titles.reserve(estimated_n);
  abs.reserve(estimated_n);
  dates.reserve(estimated_n);
  authors.reserve(estimated_n);
  author1s.reserve(estimated_n);
  std::string x, title, bti, auth;
  std::ostringstream ab;  //Faster for long abstracts
  bool isTitle=false,  titleNow=false,
       isBti=false,    btiNow=false,
       isAb=false,     abNow=false,
       isDate=false,   isAuthor=false,
       isFirst=true;
  
  //Perform stuff that needs to happen at the end of each paper
  auto finalize_paper = [&]() {
    // Handle title, abstract, date, and authors
    if (isTitle) titles.push_back(title);
    else if (isBti) titles.push_back(bti);
    else titles.push_back("NA");
    
    if (isAb) abs.push_back(ab.str());
    else if (isTitle) abs.push_back(title);
    else if (isBti) abs.push_back(bti);
    else abs.push_back("NA");
    
    if (!isDate) dates.push_back("NA");
    
    if (isAuthor) authors.push_back(auth);
    else {
      authors.push_back("NA");
      author1s.push_back("NA");
    }
  };
  
  //Go through the file line by line  
  while (std::getline(file, x)) {
    if (!x.empty() && x.back() == '\r') x.pop_back();
    if (x.empty() || x.size() < 7) continue;
    
    //Continuations
    if (x.compare(0,2,"  ")==0) {
      if (titleNow) {
        title+=" "+x.substr(6);
        continue;
      } 
      if (btiNow) {
        bti+=" "+x.substr(6);
        continue;
      }
      if (abNow) {
        ab<<" "<<x.substr(6);
        continue;
      }
    } else {
      titleNow=false, btiNow=false, abNow=false;
    }//End of continuations
    
    //New paper starts
    if (x.compare(0,4,"PMID")==0) {
      if (!isFirst) {
        finalize_paper();
        
        //Reset variables for a new paper 
        isAb=false, isDate=false, isTitle=false,
            isBti=false, isAuthor=false;
        title.clear();            // reset for next title
        bti.clear();              // reset for next bti
        auth.clear();             // reset for a new set of authors;
        ab.str("");               // reset for next abstract
        ab.clear();               
      }
      //store the PMID
      pmids.push_back(x.substr(6));
      if (isFirst) isFirst=false;
      continue;
    }
    if (x.compare(0,2,"TI")==0) {
      isTitle=true;
      titleNow=true;
      title=x.substr(6); // start new title
      continue;
    }
    if (x.compare(0,3,"BTI")==0) {
      isBti=true;
      btiNow=true;
      bti=x.substr(6); // start new bti
      continue;
    }
    if (x.compare(0,2,"AB")==0) {
      isAb=true;
      abNow=true;
      ab<<x.substr(6); // start new abstract
      continue;
    }
    if (x.compare(0,5,"AU  -")==0) {
      if (!isAuthor) { //No author yet for this paper
        author1s.push_back(x.substr(6));
        auth=x.substr(6);
        isAuthor=true;
      } else auth+=", "+x.substr(6);
      continue;
    }
    if (x.compare(0,4,"EDAT")==0 && !isDate) {
      isDate=true;
      dates.push_back(x.substr(6,10));
    }
  }

  finalize_paper();  // Finish the last paper
  
  return List::create(
    Named("id")      = wrap(pmids),
    Named("text")    = wrap(abs),
    Named("PMID")    = wrap(pmids),
    Named("date")    = wrap(dates),
    Named("title")   = wrap(titles),
    Named("author")  = wrap(authors),
    Named("author1") = wrap(author1s)
  );
}
