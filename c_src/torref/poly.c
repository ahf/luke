#include "../ref/poly.h"
#include "../ref/fips202.h"

#include "batcher.h"

static int discardtopoly(poly* a, unsigned char *buf, unsigned int nblocks)
{
  int r=0;
  unsigned int i;
  uint16_t x[SHAKE128_RATE*nblocks/2];

  for(i=0;i<SHAKE128_RATE*nblocks/2;i++)
    x[i] = buf[2*i] | (uint16_t)buf[2*i+1] << 8; //handle endianess

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
  while (discardtopoly(a, buf, nblocks));
}
