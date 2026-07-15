#if !defined(RFUTIL_H)
#define RFUTIL_H

void bootstrapSample(int size, marray<int> &data, marray<int> &ib, marray<booleanT> &oob, marray<int> &oobIdx) ;
void randomSample(int size, double prop, marray<int> &data, marray<int> &ib, marray<booleanT> &oob, marray<int> &oobIdx) ;
void shuffleChange(int noValues, marray<int> &valArray) ;


#endif
