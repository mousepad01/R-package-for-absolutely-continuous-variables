ComputeMarginalDistributionX <- function (fct){
  
  MarginalX <- function(y) {
    sapply(y, function(y) {
      integrate(Vectorize(function(x) fct(x,y)), -Inf, Inf)$value
    })
  }
  
  return (MarginalX)
}

ComputeMarginalDistributionY <- function (fct){
  
  MarginalY <- function(x) {
    sapply(x, function(x) {
      integrate(Vectorize(function(y) fct(x,y)), -Inf, Inf)$value
    })
  }
  
  return (MarginalY)
}


ComputeConditionalDensityXbyY <- function (fct){
  MarginalY <- ComputeMarginalDistributionY(fct)
  ConditionalDensity <- function (x, y) {
    return (fct(x, y) / MarginalY (y))
  }
  return (ConditionalDensity)
}


ComputeConditionalDensityYbyX <- function (fct) {
  MarginalX <- ComputeMarginalDistributionX(fct)
  ConditionalDensity <- function (x, y) {
    return (fct(x,y) / MarginalX(y))
    
  }
  return (ConditionalDensity)
}