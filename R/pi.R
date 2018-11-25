# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


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
