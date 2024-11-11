#ifndef __RANDOM_H__
#define __RANDOM_H__

typedef struct{
  unsigned int m_i1;
  unsigned int m_i2;
} uniformGenerator;

double genrunif(uniformGenerator* generator);
#endif
