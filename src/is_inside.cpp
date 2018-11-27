#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericMatrix is_inside(NumericMatrix points) {
  LogicalVector inside(points.nrow());

}

Is_Inside <- function(B= 5000, seed = 10, make_plot = TRUE ) {
//Set seed
  set.seed(seed)

// Generate two vectors for random points in unit circle
  x <- runif(B, min=-1, max=1)
    y <- runif(B, min=-1, max=1)

//Simulate point B
    point = matrix(runif(2*B, -1, 1), nrow = B, ncol = 2)

// Test if draws are inside the unit circle
      Z <- ifelse(x^2 + y^2 <= 1, TRUE, FALSE)

//plot
        SquareCircle <- ggplot() +
          geom_rect(aes(xmin= -1.00, ymin= -1.00, xmax= 1.00, ymax=1.00),
                    fill = NA, color = 'blue') +
                      geom_circle(aes(x0=0, y0=0, r=1),color = 'red') +
                      geom_point(aes(x, y), color = ifelse(Z == TRUE, "firebrick2", "dodgerblue3")) +
                      ggtitle("Rectangle & Circle plot") +
                      theme(aspect.ratio=1 , axis.title.x = element_blank(), axis.title.y = element_blank(),
                            plot.title = element_text(hjust = 0.5))

          return(Inside)
}
