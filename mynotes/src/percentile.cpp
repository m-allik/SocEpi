#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
// [[Rcpp::plugins(cpp11)]]

NumericVector decile(NumericVector pop, NumericVector var, int p) {

  int n = pop.size();
  IntegerVector indx(n);
  for (int i = 0; i < n; i++) indx[i] = i + 1;

  //https://www.geeksforgeeks.org/sorting-2d-vector-in-c-set-1-by-row-and-column/
  //https://stackoverflow.com/questions/31370380/c-method-to-order-a-matrix-by-elements-in-a-column-the-same-of-sortrows-in-ma

  NumericMatrix tble (n, 3);
  tble(_,0) = indx; tble(_,1)=pop; tble(_,2)=var;

  std::sort(var.begin(), var.end());

  std::sort(pop.begin(), pop.end());

  //calculate total population and cut-points/size of the decile
  double total=0;

  for(int i=0; i < n; ++i) {
    total += pop[i];
  }

  double cp = total/p + total/(n*2);

  IntegerVector v(p);
  for (int i = 0; i < p; i++) v[i] = i + 1;

  IntegerVector pcnt(n);
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

    for(i = i; i < n; ++i) {
      pcnt[i] = v[9];
    }

  return pcnt;
}



/*** R
x <- round(runif(10, min=10, max=50), 0)
y <- runif(10)
decile(x, y)
*/
