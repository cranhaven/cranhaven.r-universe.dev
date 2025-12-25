#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;
#include "checkconstraints.h"

//1 = head, 2 = spouse, 3 = biological child,
//4 = adopted child, 5 = stepchild, 6 = sibling,
//7 = parent, 8 = grandchild, 9 = parent-in-law,
//10 = child-in-law, 11 = other relative,
//12 = boarder, roommate or partner,
//13 = other non-relative or foster child
inline bool IsHead(int relate, int age) {
    return (relate == HEAD && age >=16);
}

inline int GetHead(int *record, int hhsize) {
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]==HEAD) {
            return i;
        }
    }
    return -1;
}

inline bool MoreThanOneHead(int *record, int hhsize) {
    int nhead = 0;
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]==HEAD) {
            nhead++;
        }
    }
    return (nhead >1);
}

inline int GetValidSpouse(int *record, int hhsize) {
    int nspouse = 0;
    int spouse = -1;
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]==SPOUSE) {
            nspouse++;
            spouse = i;
        }
    }
    if (nspouse > 1) {return 0;} //too many spouse
    if (nspouse == 0) { return -1;} //no spouse
    if (record[hhsize+spouse]<16) {return 0;} //spouse is under-age
    return spouse;
}

inline bool IsValidCouple(int *record, int hh_size, int spouse, int head) {
    if (spouse ==0) { //bad spouse or too many spouses
        return false;
    } else { //valid spouse or no spouse
        if (spouse>0) {//the only spouse, so check sex, and age difference
            if (record[head] == record[spouse]) {return false;}
            if (std::abs((float)record[hh_size + head] - (float)record[hh_size + spouse]) > 49) {return false;}
        }
    }
    return true;
}

//return -1 if no biological child
//return the record index of the oldest child otherwise
inline int GetOldestBiologicalChild(int *record, int hhsize) {
    int age = -1;
    int child = -1;  //no childen
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]==BIOLOGICALCHILD) {
            if (record[hhsize+i] > age) {
                age = record[hhsize+i];
                child = i;
            }
        }
    }
    return child;
}

inline bool IsValidBiologicalChild(int *record, int hh_size, int child, int head) {
    if (child>0) {//get a child, check age difference
        if (record[hh_size + head] - record[hh_size + child] <7) {return false;}
    }
    return true;
}


//return -1 if no adopted child
//return the record index of the oldest child otherwise
inline int GetOldestAdoptedChild(int *record, int hhsize) {
  int age = -1;
  int child = -1;  //no childen
  for (int i = 1; i <= hhsize; i++) {
    if (record[2*hhsize+i]==ADOPTEDCHILD) {
      if (record[hhsize+i] > age) {
        age = record[hhsize+i];
        child = i;
      }
    }
  }
  return child;
}

inline bool IsValidAdoptedChild(int *record, int hh_size, int child, int head) {
  if (child>0) {//get a child, check age difference
    if (record[hh_size + head] - record[hh_size + child] <11) {return false;}
  }
  return true;
}

//return -1 if no adopted child
//return the record index of the oldest child otherwise
inline int GetOldestStepChild(int *record, int hhsize) {
  int age = -1;
  int child = -1;  //no childen
  for (int i = 1; i <= hhsize; i++) {
    if (record[2*hhsize+i]==STEPCHILD) {
      if (record[hhsize+i] > age) {
        age = record[hhsize+i];
        child = i;
      }
    }
  }
  return child;
}

inline bool IsValidStepChild(int *record, int hh_size, int child, int head) {
  if (child>0) {//get a child, check age difference
    if (record[hh_size + head] - record[hh_size + child] <9) {return false;}
  }
  return true;
}


//return -1 if no child
//return the record index of the oldest child otherwise
//inline int GetOldestChildInLaw(int *record, int hhsize) {
//    int age = -1;
//    int child = -1;  //no childen
//    for (int i = 1; i <= hhsize; i++) {
//        if (record[2*hhsize+i]==CHILDINLAW) {
//            if (record[hhsize+i] > age) {
//                age = record[hhsize+i];
//                child = i;
//            }
//        }
//    }
//    return child;
//}

//inline bool IsValidChildInLaw(int *record, int hh_size, int child, int head) {
//    if (child>0) {//get a child, check age difference
//        if (record[hh_size + head] - record[hh_size + child] <10) {return false;}
//    }
//    return true;
//}

//return -1 if no parent
//return the record index of the youngest parent otherwise
inline int GetYoungestParent(int *record, int hhsize) {
    int age = 1000;
    int parent = -1;  //no childen
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]==PARENT) {
            if (record[hhsize+i] < age) {
                age = record[hhsize+i];
                parent = i;
            }
        }
    }
    return parent;
}

inline bool IsValidParent(int *record, int hh_size, int parent, int head) {
    if (parent>0) {//get a child, check age difference
        if (record[hh_size + parent] -record[hh_size + head] < 4) {return false;}
    }
    return true;
}

inline int GetYoungestParentInLaw(int *record, int hhsize) {
    int age = 1000;
    int parent = -1;  //no childen
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]==PARENTINLAW) {
            if (record[hhsize+i] < age) {
                age = record[hhsize+i];
                parent = i;
            }
        }
    }
    return parent;
}

inline bool IsValidParentInLaw(int *record, int hh_size, int parent, int head) {
    if (parent>0) {//get a child, check age difference
        if (record[hh_size + parent] -record[hh_size + head] <4) {return false;}
    }
    return true;
}

inline bool IsValidSibling(int *record, int hhsize, int head) {
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]== SIBLING) {
            if (std::abs(record[hhsize + i] - record[hhsize + head]) >37) {return false;}
        }
    }
    return true;
}

inline bool IsValidGrandChild(int *record, int hhsize, int spouse, int head) {
    for (int i = 1; i <= hhsize; i++) {
        if (record[2*hhsize+i]== GRANDCHILD) {
            if (record[hhsize + head] < 31) {return false;} //too young to be grand parent for the HEAD
            if (spouse > 0) { //make sure the spouse(if any) is not too young
                if (record[hhsize + spouse] < 17) {return false;}
            }
            if (record[hhsize + head] - record[hhsize + i] <26 ) {return false;}
        }
    }
    return true;
}


int isValid(int *datah, int hh_size) {

    int head = GetHead(datah,hh_size);
    if (head <=0) {return 0;}

    if (!IsHead(datah[2 * hh_size + head], datah[hh_size + head])) {return 0;}
    if (MoreThanOneHead(datah,hh_size)) {return 0;}

    int spouse = GetValidSpouse(datah,hh_size);
    if (!IsValidCouple(datah,hh_size,spouse, head)) {return 0;}

    int oldestBiologicalChild = GetOldestBiologicalChild(datah,hh_size);
    if (!IsValidBiologicalChild(datah,hh_size,oldestBiologicalChild,head)) {return 0;}

    int oldestAdoptedChild = GetOldestAdoptedChild(datah,hh_size);
    if (!IsValidAdoptedChild(datah,hh_size,oldestAdoptedChild,head)) {return 0;}

    int oldestStepChild = GetOldestStepChild(datah,hh_size);
    if (!IsValidStepChild(datah,hh_size,oldestStepChild,head)) {return 0;}

    int youngestParent = GetYoungestParent(datah,hh_size);
    if (!IsValidParent(datah,hh_size,youngestParent,head)) {return 0;}

    int youngestParentInLaw = GetYoungestParentInLaw(datah,hh_size);
    if (!IsValidParentInLaw(datah,hh_size,youngestParentInLaw,head)) {return 0;}

    if (!IsValidSibling(datah,hh_size,head)) {return 0;}

    if (!IsValidGrandChild(datah,hh_size,spouse,head)) {return 0;}

    return 1;

}

int checkconstraints_imp(int *data, int *isPossible,int hh_size, int DIM, int nHouseholds) {

    int totalpossible = 0;
    int *datah = new int[hh_size * COL + 1];
    //column 3, 6, 7 = sex, age and relte
    int column[COL]; column[0] = 3; column[1] = 6; column[2] = 7;

	for (int m = 1; m <= nHouseholds; m++){
        for (int j = 1; j <= hh_size; j++) {
            for (int k = 0; k < COL; k++) {
                datah[k * hh_size + j] = data[((j-1) * DIM + column[k] -1) * nHouseholds + (m-1)];
            }
        }
		isPossible[m-1] = isValid(datah, hh_size);
        totalpossible+= isPossible[m-1];
	}

	delete [] datah;
    return totalpossible;
}

// [[Rcpp::export]]
IntegerVector checkSZ(IntegerMatrix Data_to_check, int h){
  int n0 = Data_to_check.nrow();
  int p = Data_to_check.ncol()/h;

  IntegerVector Data_checked(n0);
  int *datah = new int[h * COL + 1];
  for (int i = 0; i < n0; i++) {
    for(int j = 0; j < h; j++) { //0 sex, sex,sex,...age,age,age,...relte,relte,relate...
      int base = p * j;
      datah[j+1] = Data_to_check(i, base + GENDER);
      datah[j+1 + h] = Data_to_check(i, base + AGE);
      datah[j+1 + 2*h] = Data_to_check(i, base + RELATE);
    }
    Data_checked[i] = isValid(datah,h);
  }
  delete [] datah;
  return Data_checked;
}

//only return the index (one-based of the first valid data point)
// [[Rcpp::export]]
IntegerVector checkSZ2(IntegerMatrix Data_to_check, int h){
  int n0 = Data_to_check.nrow();
  int p = Data_to_check.ncol()/h;

  IntegerVector First_Valid(1);
  First_Valid[0] = 0;
  int *datah = new int[h * COL + 1];
  for (int i = 0; i < n0; i++) {
    for(int j = 0; j < h; j++) { //0 sex, sex,sex,...age,age,age,...relte,relte,relate...
      int base = p * j;
      datah[j+1] = Data_to_check(i, base + GENDER);
      datah[j+1 + h] = Data_to_check(i, base + AGE);
      datah[j+1 + 2*h] = Data_to_check(i, base + RELATE);
    }
    if (isValid(datah,h)) {
      First_Valid[0] = i+1;
      break;
    }
  }
  delete [] datah;
  return First_Valid;
}
