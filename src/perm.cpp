#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

NumericMatrix perm(int n, NumericVector v){
  /*
   * Takes a number n and vector v and returns a matrix of n rows, each of which
   * is a permutation of the original vector. As far as I can tell, there is no
   * native R way to do this without calling the sample function inside of a loop,
   * which is very slow.
   * 
   * 
   * ISSUE: Does not currently work with set.seed in R. 
   */
  int l = v.size();
  NumericMatrix m(n, l);
  for(int i=0; i<n; i++){
    std::random_shuffle(v.begin(), v.end()); // Permutes v.
    for(int j=0; j<l; j++){                  
      m(i, j) = v[j];                        // Fills a line of m with the permuted
    }                                        // vector.
  }
  return m;
}

