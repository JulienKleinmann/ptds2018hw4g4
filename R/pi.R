#' @name estimate_pi
#' @title Simple estimate of pi using a simulation
#'
#' @description Compute an approximation of pi using simulation. The idea is to
#'  take a certain number of points (B is the number) that goes from -1 to 1 and
#'   store they coordinate (x and y). Then using the fact that a circle has an area of pi
#'   we can calculate an aproximation of pi. We take the area of the square which
#'   is 4, times the total of point that are in the circle, divided by the total
#'   of point (B).
#' @param B A \code{numeric} (integer) used to denote the number of points in the
#'  simulations.
#' @param seed A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{estimated_pi}{Estimated value of pi}
#'      \item{points}{list of all the point that have been generated, we have
#'       their x and y coordinates and inside which tell us if the points is in
#'       the circle or outside}
#' }
#' @author Julien Kleinmann, Constance Havret, Lumia Claramunt, Sana Ghali,
#' Azza Akamoun
#' @import ggforce ggplot2 ptds2018 ArgumentCheck
#' @export estimate_pi
#' @examples
#' estimate_pi( B=5000, seed = 10)
#'




library(ggplot2)
library(ggforce)
library(ptds2018)
library(ArgumentCheck)

estimate_pi <- function(B = 5000, seed = 10) {

  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add an stop if B is not a numeric of length one
  if (length(B)!=1 | !is.numeric(B)){
    ArgumentCheck::addError(
      msg = "'B' must be a numeric number of length 1.",
      argcheck = Check
    )

  }

  #* Add an stop if seed is not a numeric of length one
  if (length(seed)!=1 | !is.numeric(seed)){
    ArgumentCheck::addError(
      msg = "'seed' must be a numeric number of length 1.",
      argcheck = Check
    )

  }

  #* Add an warning if B is not an integer and round it up if it's the case
  if (B %% 1 != 0){
    ArgumentCheck::addWarning(
      msg = "'B' must be an integer. Its value has been rounded up with the function as.integer()",
      argcheck = Check
    )

    B <- as.integer(B)
  }

  #* Add an warning if seed is not an integer and round it up if it's the case
  if (seed %% 1 != 0){
    ArgumentCheck::addWarning(
      msg = "'seed' must be an integer. Its value has been rounded up with the function as.integer()",
      argcheck = Check
    )
    seed <- as.integer(seed)
  }

  #* Add an warning if B is negative and transform it to a positive number
  if (B < 0){
    ArgumentCheck::addWarning(
      msg = "'B' must be a positive number. Its value has been taken as positive",
      argcheck = Check
    )

    B <- -B
  }

  #* Add an warning if seed is negative and transform it to a positive number
  if (seed < 0 ){
    ArgumentCheck::addWarning(
      msg = "'seed' must be a positive number. Its value has been taken as positive",
      argcheck = Check
    )
    seed <- -seed
  }

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)


  # set a seed
  set.seed(seed)

  # simulate B points
  points <- data.frame(
    x = runif(n = B, min = -1, max = 1),
    y = runif(n = B, min = -1, max = 1),
    inside = rep(NA, B)
  )

  # Test if point are inside or not  and store a boolean in inside :

  for (i in 1:B){
    points$inside[[i]] <- ifelse(points$x[[i]]^2 + points$y[[i]]^2 <= 1, TRUE, FALSE)
  }


  estimated_pi <- 4 * sum(points$inside == TRUE) / B

  # create a new list
  rval <- list(
    estimated_pi = estimated_pi,
    points = points
  )

  # assign pi class to rval
  class(rval) <- "pi"

  # return rval
  return(rval)


}
#' @name plot.pi
#' @title Simple estimate of pi using a simulation
#'
#' @description Plot an objet of class "pi" usually created by estimate_pi
#' @param x A \code{class} (pi) used to pass the points of a simulation into a graph

#' @return A \code{plot} containing the circle,square and all the points
#'
#' @author Julien Kleinmann, Constance Havret, Lumia Claramunt, Sana Ghali,
#' Azza Akamoun
#' @import ggforce ggplot2 ptds2018 ArgumentCheck
#' @export plot.pi
#' @examples
#' x <- estimate_pi( B=5000, seed = 10)
#'
#' plot.pi(x)

plot.pi <- function(x) {

  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add an warning if seed is negative and transform it to a positive number
  if (class(x) != "pi" ){
    ArgumentCheck::addError(
      msg = "Argument is not of the class 'pi', this function only plot 'pi' class argument. ",
      argcheck = Check
    )
  }

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  ## take point of argument
  points <- x[["points"]]

  # plot points
  SquareCircle <- ggplot() + geom_rect(aes(xmin= -1.00, ymin= -1.00, xmax= 1.00, ymax=1.00), fill = NA, color =   'blue') + geom_circle(aes(x0=0, y0=0, r=1), color = 'red') + geom_point(aes(points$x,points$y), color = ifelse(points$inside == TRUE, "yellow", "lightblue")) + theme(aspect.ratio=1)

  return(SquareCircle)
}


#' @name estimate_pi2
#' @title Simple estimate of pi using a rcpp and our inside function
#'
#' @description Compute an approximation of pi using simulation and rcpp package to make it faster. The idea is to
#'  take a certain number of points (B is the number) that goes from -1 to 1 and
#'   store they coordinate (x and y). Then using the fact that a circle has an area of pi
#'   we can calculate an aproximation of pi. We take the area of the square which
#'   is 4, times the total of point that are in the circle, divided by the total
#'   of point (B).
#' @param B A \code{numeric} (integer) used to denote the number of points in the
#'  simulations.
#' @param seed A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{estimated_pi2}{Estimated value of pi}
#'      \item{points}{list of all the point that have been generated, we have
#'       their x and y coordinates and inside which tell us if the points is in
#'       the circle or outside}
#' }
#' @author Julien Kleinmann, Constance Havret, Lumia Claramunt, Sana Ghali,
#' Azza Akamoun
#' @import ggforce ggplot2 ptds2018 ArgumentCheck
#' @export estimate_pi2
#' @examples
#' estimate_pi2( B=5000, seed = 10)
#'

sourceCpp("is_inside.cpp")

estimate_pi2<- function(B = 5000, seed = 10) {

  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add an stop if B is not a numeric of length one
  if (length(B)!=1 | !is.numeric(B)){
    ArgumentCheck::addError(
      msg = "'B' must be a numeric number of length 1.",
      argcheck = Check
    )

  }

  #* Add an stop if seed is not a numeric of length one
  if (length(seed)!=1 | !is.numeric(seed)){
    ArgumentCheck::addError(
      msg = "'seed' must be a numeric number of length 1.",
      argcheck = Check
    )

  }

  #* Add an warning if B is not an integer and round it up if it's the case
  if (B %% 1 != 0){
    ArgumentCheck::addWarning(
      msg = "'B' must be an integer. Its value has been rounded up with the function as.integer()",
      argcheck = Check
    )

    B <- as.integer(B)
  }

  #* Add an warning if seed is not an integer and round it up if it's the case
  if (seed %% 1 != 0){
    ArgumentCheck::addWarning(
      msg = "'seed' must be an integer. Its value has been rounded up with the function as.integer()",
      argcheck = Check
    )
    seed <- as.integer(seed)
  }

  #* Add an warning if B is negative and transform it to a positive number
  if (B < 0){
    ArgumentCheck::addWarning(
      msg = "'B' must be a positive number. Its value has been taken as positive",
      argcheck = Check
    )

    B <- -B
  }

  #* Add an warning if seed is negative and transform it to a positive number
  if (seed < 0 ){
    ArgumentCheck::addWarning(
      msg = "'seed' must be a positive number. Its value has been taken as positive",
      argcheck = Check
    )
    seed <- -seed
  }

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  # set a seed
  set.seed(seed)

  # simulate B points
  points <- data.frame(
    point = matrix(runif(2*B, -1, 1), nrow = B, ncol = 2),
    inside = rep(NA, B)
  )


  points$inside <- is_inside(points$point)


  estimated_pi <- 4 * sum(points$inside == TRUE) / B

  # create a new list
  rval <- list(
    estimated_pi = estimated_pi,
    points = points
  )

  # assign pi class to rval
  class(rval) <- "pi"

  # return rval
  return(rval)


}




