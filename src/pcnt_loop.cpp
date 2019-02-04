#include <Rcpp.h>
using namespace Rcpp;

//' For and while loops for getting percentiles
//'
//' @name pcnt_loop
//' @param pop Population in an area
//' @param var Deprivation variable
//' @param p number of percentiles
//' @param cp Population cut-off point for percentiles
//'
//' @keywords internal
//'
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
NumericVector pcnt_loop(NumericVector pop, NumericVector var, int p, double cp) {

  // number of areas
  int n = pop.size();

  // percentile values
  IntegerVector v(p);
  for (int i = 0; i < p; i++) v[i] = i + 1;

  // variable for percentiles
  IntegerVector pcnt(n);

  // loop to create the percentile variable
  double s = 0;
  int i = 0;

    for(int j=0; j < p-1; ++j) {

      while(s <= cp) {
        s = s + pop[i];
        pcnt[i] =  v[j];
        i = i + 1;
      }
      s = 0;
      i = i - 1;
    }


    // last percentile category gets value
    for(i = i; i < n; ++i) {
      pcnt[i] = v[p-1];
    }

  return wrap(pcnt);
}




