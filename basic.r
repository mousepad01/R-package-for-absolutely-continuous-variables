myClass <- function(x = 3, y, z){
  
  newObj <- list(first = x, second = y, third = z)
  
  class(newObj) <- "myClass"
  
  attr(newObj, "fourth") = 1000
  
  return(newObj)
}

add <- function(obj, k){
  UseMethod("add")
}

add.myClass <- function(obj, k){
  
  obj$first <- obj$first + k
  obj$third <- obj$third + k
  
  return(obj)
  
}

a <- myClass(y = 4, z = 9)
a <- add(a, 3)

print(a@fourth)

a <- 5
b <- 2

print(a / b)
