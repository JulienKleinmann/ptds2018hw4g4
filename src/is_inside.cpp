
#include <Rcpp.h>
using namespace Rcpp;



//  find_pi <- function(B= 5000, seed = 10, make_plot = TRUE ) {
//#Set seed
//    set.seed(seed)

//# Generate two vectors for random points in unit circle
//    x <- runif(B, min=-1, max=1)
//      y <- runif(B, min=-1, max=1)

//#Simulate point B
//      point = matrix(runif(2*B, -1, 1), nrow = B, ncol = 2)

//# Test if draws are inside the unit circle
//        Z <- ifelse(x^2 + y^2 <= 1, TRUE, FALSE)

// 10 rows, 5 columns
//NumericMatrix mat(10, 5);


//NumericMatrix rngCpp(const int N) {
//  NumericMatrix X(N, 4);
//  X(_, 0) = runif(N);
//  X(_, 1) = rnorm(N);
//  X(_, 2) = rt(N, 5);
//  X(_, 3) = rbeta(N, 1, 1);
//  return X;
//}
//set.seed(42)     # setting seed
//  M1 <- rngCpp(5)
 // M1


// [[Rcpp::export]]
LogicalVector is_inside(NumericMatrix points) {

  LogicalVector inside(points.nrow());

  for(int i=0 ; i< points.nrow(); i++){

    if ( pow(points(i,0), 2) + pow(points(i,1), 2)  <= 1 ){

      inside[i] =true;

    } else { inside[i]=false; } }

    return inside;

  }
