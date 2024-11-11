#include "random.h"
/* the state of generator changed after generating one random number */
double genrunif(uniformGenerator* generator)
{
  unsigned int m_i1, m_i2;
  double runif;
  m_i1 = generator -> m_i1;
  m_i2 = generator -> m_i2;
  m_i1 = 36969*(m_i1 & 0177777) + (m_i1>>16);
  m_i2= 18000*(m_i2 & 0177777) + (m_i2>>16);
  runif = ((m_i1 << 16)^(m_i2 & 0177777)) * 2.328306437080797e-10;
  generator -> m_i1 = m_i1;
  generator -> m_i2 = m_i2;
  return runif;
}
