#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericMatrix is_inside(NumericMatrix points) {
  LogicalVector inside(points.nrow());

  inside <-  function(B= 5000, seed = 10, make_plot = TRUE ) {
    //Set seed
    set.seed(seed)

    // Generate two vectors for random points in unit circle
    x <- runif(B, min=-1, max=1)
    y <- runif(B, min=-1, max=1)

    //Simulate point B
    point = matrix(runif(2*B, -1, 1), nrow = B, ncol = 2)

    // Test if draws are inside the unit circle
    Z <- ifelse(x^2 + y^2 <= 1, TRUE, FALSE)}


  return(Inside);
}


