//define guards, so headers are declare only once.
//#ifndef READFILE_H
//#define READFILE_H

#include "network.h"

class readfile {

 public:
  string* getDataSet();
  readfile();
  readfile(network *, string*, int, int, int=0);
  ~readfile();

  static inline void removeDuplicates(std::vector<string>& vec){
    
    std::sort(vec.begin(), vec.end());
    vec.erase(std::unique(vec.begin(), vec.end()), vec.end());
    
  };

  static inline void removeDuplicates(std::vector<int>& vec){
    
    std::sort(vec.begin(), vec.end());
    vec.erase(std::unique(vec.begin(), vec.end()), vec.end());

  };

  
 private:
  int OFFSET;
  int NROWS;
  int NCOLS;
  bool Header;
  int Skip;
  int isLabel;
  network *net;
  string  *dataset;
  vector<string> labels;
  vector<int>    labelsI;
  
  int count_vertices();
  void create_network();
  int find_vertex( int, int, string );
  int find_vertex( int, int, int );
  void get_degrees();
  void read_edges();

  // Constants
  #define LINELENGTH 1000
  
};

//#endif
