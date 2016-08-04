#include "../ref/poly.h"
#include "../ref/fips202.h"

#include "batcher.h"

static int discardtopoly(poly* a, uint16_t *x)
{
  int32_t i, r=0;

  for(i=0;i<16;i++)
  batcher84(x+i);

  // Check whether we're safe:
  for(i=1008;i<1024;i++)
    r |= 61444 - x[i];
  if(r >>= 31) return -1;

  // If we are, copy coefficients to polynomial:
  for(i=0;i<PARAM_N;i++)
    a->coeffs[i] = x[i];
  
  return 0;
}

void poly_tor_uniform(poly *a, const unsigned char *seed)
{
  uint64_t state[25];
  unsigned int nblocks=16;
  uint8_t buf[SHAKE128_RATE*nblocks];

  shake128_absorb(state, seed, NEWHOPE_SEEDBYTES);

  do
  {
    shake128_squeezeblocks((unsigned char *) buf, nblocks, state);
  }
  while (discardtopoly(a, (uint16_t *)buf));
}
