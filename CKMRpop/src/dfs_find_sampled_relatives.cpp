#include <Rcpp.h>
#include <stdlib.h>
#include <cstring>
using namespace Rcpp;




// Definitions of structures, etc

struct node { // for variable nodes (individuals)
  int idx;
  char name[30];   // the ID of the individual
  int is_sample;  // 1 if the individual is part of the sample
  int n_up;       // number of parents.  This will be 0 for founders (i.e. those whose parents are "0")
  int n_down;     // number of children (i.e., downward edges)
  int up[2];      // idxs of the pa (up[0]) and ma (up[1])
  int *down;      // idxs of the children of this individual
  int been_up;    // a flag that gets set to 1 when the node has been reached by the search_up()
                  // part of the depth first search.  It gets set back to 0 before returning out
                  // of search_up(). I don't think this really gets used anymore.
};


struct pedigree { // struct to hold all the nodes and info of a pedigree
  int N;               // the number of nodes
  int S;               // the number of samples
  struct node* nodes;  // the array of nodes, 1 for each indiv in the pedigree
};






//' Exponentiation function for integer arguments
//'
//' Solaris' compiler freaked hard on the pow function---couldn't figure
//' out whether it should return an int or a float or something, when
//' I used it to get very small powers of two. So I
//' am going to write a silly, simple function (that only gets used a couple
//' of times in an entire execution, and only with very
//' small arguments, so the fact that it is not super efficient
//' should not be a big problem).
//' @param x the integer to raise 2 to.
//' @name int_pow2
//' @keywords internal
int int_pow2(int x) {
  int i;
  int ret = 1;
  for(i=0;i<x;i++) {
    ret *= 2;
  }
  return(ret);
}


//' Depth first search down the pedigree to N generations.
//'
//' When you call this
//' from within search_up(), c should be set at 0,
//' and the algorithm will run down for, n generations from there.
//' Unlike in R, this is 0-based.  So, n = 0 is self, n = 1 is kids,
//' n = 2 is grandkids, and so forth.
//' @param i the index of the node to call this on
//' @param c the current generation level.  0 = the first (i.e. the starting individual).
//' @param n the number of generations back to down.  1 means go no further than the offspring.
//' 2 means go no further than the grandkids.
//' @param P the pedigree structure
//' @param C a reference to a character vector to which sampled individuals' IDs will get
//' pushed on.  At the end, we can unique them.
//' @name search_down
//' @keywords internal
void search_down(
    int i,
    int c,
    int n,
    struct pedigree *P,
    CharacterVector &C
) {

  int j;

  if(P->nodes[i].is_sample == 1) {
    C.push_back(P->nodes[i].name);
  }


  //Rcout << "SearchingDown: i= " << i << " c= " << c << " n= " << n << " name= " << P->nodes[i].name << " C= " << C << " n_down= " << P->nodes[i].n_down << "\n";

  if(c < n) {
    for(j=0;j<P->nodes[i].n_down;j++) {
      search_down(P->nodes[i].down[j], c + 1, n, P, C);
      //Rcout << "  Would proceed down to j: " << j << " i: " << P->nodes[i].down[j] << " c: " << c + 1 <<  " n: " << n << "\n";
    }
  }


  return;
}



//' Depth first search up the pedigree to N generations.
//'
//' Call this
//' with c = 0 for the original individual, and it will go back, n generations.
//' Unlike in R, this is 0-based.  So, n = 1 is parents, n = 2 is grandparents
//' and so on.
//' @param i the index of the node to call this on
//' @param c the current generation level.  0 = the first (i.e. the sampled individual)
//' @param n the number of generations back to go.  1 means go no further than the parents.
//' 2 means go no further than the grandparents.
//' @param P the pedigree structure
//' @param C a reference to a character vector to which sampled individuals' IDs will get
//' pushed on.  At the end, we can unique them.
//' @name search_up
//' @keywords internal
void search_up(
  int i,
  int c,
  int n,
  struct pedigree *P,
  CharacterVector &C
) {

  int j;

  //Rcout << "SearchingUp: i= " << i << " c= " << c << " n= " << n << " name= " << P->nodes[i].name << " C= " << C << "\n";

  search_down(i, 0, n, P, C);

  if(c < n) {
    for(j=0;j<P->nodes[i].n_up;j++) {
      search_up(P->nodes[i].up[j], c + 1, n, P, C);
    }
  }

  return;
}



// Function to initialize a pedigree from information compiled in
// matrices and vectors
// this allocates memory to a pedigree factor graph.  It passes in static
// information to the nodes and edge structs, but does not initialize the messages.
struct pedigree *init_ped_graph(
    int N,                       // number of nodes
    int S,                       // number of samples
    IntegerMatrix node_matrix,   // First column is 0-based index of the node,
                                 // Second column is 0-based index of pa, or -1 if this node is a founder
                                 // Third column is 0-based index of ma, or -1 if this node is a founder
                                 // Fourth column is a 1 if indiv is sampled, and 0 otherwise
                                 // Fifth column is n_down, i.e. the number of children that the node has

    IntegerMatrix down_matrix,   // First column in 0-based index of the node
                                 // Second column is which child of the node (i.e. 0, 1, 2, etc.)
                                 // Third column is the 0-based index of the node of that child
    IntegerVector sample_vec,    // length-S vector of 0-based indexes of the samples
    CharacterVector names_vec   // The names of the nodes, in the order of their idxs.
) {
  int i,r;
  int pa, ma, s, ndown, cn, kid;
  struct pedigree *P = (struct pedigree *) malloc(sizeof(struct pedigree));

  P->N = N;
  P->S = S;
  P->nodes = (struct node *)calloc(N, sizeof(node));

  // cycle over the nodes and set default and initial values
  for(i=0;i<N;i++) {
    P->nodes[i].idx = i;
    P->nodes[i].n_down = 0;
    P->nodes[i].n_up = 0;
    P->nodes[i].is_sample = 0;
    P->nodes[i].been_up = 0;
    strcpy(P->nodes[i].name, names_vec(i));
    // Rcout << "i: " << i << " node_name: " << P->nodes[i].name << "\n";
  }

  // now, cycle over the rows of node_matrix and add add n_up, n_down, up, and sampled, and allocate to down
  for(r=0;r<N;r++) {
    i = node_matrix(r, 0);
    pa = node_matrix(r, 1);
    ma = node_matrix(r, 2);
    s = node_matrix(r, 3);
    ndown = node_matrix(r, 4);

    if(pa > -1 && ma > -1) {
      P->nodes[i].n_up = 2;
    }

    P->nodes[i].up[0] = pa;
    P->nodes[i].up[1] = ma;
    P->nodes[i].n_down = ndown;
    if(ndown > 0) {
      P->nodes[i].down = (int *)calloc(ndown, sizeof(int));
    }
    if(s == 1) {
      P->nodes[i].is_sample = 1;
      //Rcout << "i: " << i << " node_name: " << P->nodes[i].name << " ndown: " << P->nodes[i].n_down << " pa: " << P->nodes[i].up[0] << " ma: " << P->nodes[i].up[1] << "\n";
    }
  }

  // finally, cycle over the rows of down matrix and set the values in each node's down vector
  for(r=0;r<down_matrix.nrow();r++) {
    i = down_matrix(r, 0);
    cn = down_matrix(r, 1);
    kid = down_matrix(r, 2);
    P->nodes[i].down[cn] = kid;
  }

  return(P);
}



// Function to free memory allocated to a pedigree struct
//
// Gets called after the pedigree struct has been all used.
void free_ped_graph(pedigree *P) {
  int N = P->N;
  int i;

  for(i=0;i<N;i++) {
    if(P->nodes[i].n_down > 0) {
      free(P->nodes[i].down);
    }
  }

  free(P->nodes);

  free(P);

  return;
}


//' Function to make a vector of all the ancestors of an individual out to n generations.
//'
//' This is a replacement for the R implementation of
//' `ancestor_vectors()` which was too slow. This will get called
//' from with a C function in which the pedigree has been assembled.
//' @param sv vector of sample indexes
//' @param nv vector of names of all samples
//' @param Ped pedigree struct
//' @param n the number of generations.  0 = self, 1 = parent, 2 = grandparent, etc.
//' @name ancestor_vectors_cpp
//' @keywords internal
List ancestor_vectors_cpp(
  IntegerVector sv,
  CharacterVector nv,
  struct pedigree *Ped,
  int n
) {
  int i, j, A1, A2, T;
  int OutL, TopJ;
  int SV_length = sv.size();
  List ret;

  OutL = int_pow2(n + 1) -  1;  // int_pow2 is my own function 'cuz Solaris couldn't handle an overloaded pow function
  TopJ = int_pow2(n) - 2;

  for(i=0;i<SV_length;i++) {

    CharacterVector C(OutL);
    IntegerVector AncIdxs(OutL);

    //Rcout << i << "\n";

    //Rcout << "i: " << i << " n: " << n << "  sv.length(): " << SV_length << "  OutL: " << OutL << "  AncIdxs.size(): " << AncIdxs.size() << "\n";

     AncIdxs(0) = sv(i);  // initialize

    // cycle over individuals in all but the last generation,
    // filling forward in pairs from these.
    for(j=0;j<=TopJ;j++) {  // more sillyness to get it to compile on Solaris.
      T = AncIdxs(j);
      if(T == -1 || Ped->nodes[T].n_up == 0) {
        A1 = -1;
        A2 = -1;
      } else {
        A1 = Ped->nodes[T].up[0];
        A2 = Ped->nodes[T].up[1];
      }
      AncIdxs(2 * j + 1) = A1;
      AncIdxs(2 * j + 2) = A2;

    }

    //Rcout << "Sample_idx: " << i << "   AncIdx: " << AncIdxs << "\n";
    // cycle again and put names in the character vector
    for(j=0;j<C.size();j++) {
      if(AncIdxs(j) == -1) {
        C(j) = NA_STRING;
      } else {
        C(j) = Ped->nodes[AncIdxs(j)].name;
      }
    }
    //Rcout << "Sample_idx: " << i << "   C: " << C << "\n";
    ret.push_back(C);  // push that vector onto a list

  }
  return(ret);
}




//' function to test and use DFS stuff
//' @param L list of inputs
//' @param n the number of generations back to go when computing the ancestor vectors
//' and finding relatives.
//' @keywords internal
//' @export
// [[Rcpp::export]]
List rcpp_ancestors_and_relatives(List L, int n) {

  int N = L["N"];
  int S = L["S"];
  IntegerMatrix nm = L["node_matrix"];
  IntegerMatrix dm = L["down_matrix"];
  IntegerVector sv = L["sample_vec"];
  CharacterVector nv = L["names_vec"];
  int i;
  List REL;
  List AV;

  struct pedigree *Ped;

  Ped = init_ped_graph(N, S, nm, dm, sv, nv);

  for(i=0;i<sv.length();i++) {
    CharacterVector C;
    search_up(sv(i), 0, n, Ped, C);

    REL.push_back(unique(C));
    //Rcout << "Sample_idx: " << i << "   Unique C: " << unique(C) << "\n";
  }

  // now, test anestor vectors function
  AV = ancestor_vectors_cpp(sv, nv, Ped, n);

  // free the pedigree structure.  Otherwise memory leaks.
  free_ped_graph(Ped);

  return(
    List::create(
      _["AV"] = AV,
      _["REL"] = REL
    )
  );

}
