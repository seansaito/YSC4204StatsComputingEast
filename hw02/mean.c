#include <stdio.h>
#include <R.h>

/*
* Function that calculates the mean of vector x
* Inputs:
*     x   : vector of doubles
*     n   : length of vector
*     res : placeholder for result
*/
void meanC(double *x, int *n, double *res) {

  int i;
  double temp = 0.0;
  for (i = 0; i < *n; i++) {
    temp += x[i];
  }

  *res = temp / *n;
  return;
}
