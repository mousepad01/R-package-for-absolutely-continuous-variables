ExecuteMeanDispersion <- function(g, densityFunction, lowerBound, upperBound){
  
  f1 <- function (x){
    return (g(x) * densityFunction(x))
  }
  
  f2 <- function (x){
    return(g(x) * g(x) * densityFunction(x))
  }
  
  meanValue <- integrate(f1, lower = lowerBound, upper = upperBound)$value
  dispValue <- integrate(f2, lower = lowerBound, upper = upperBound)$value - meanValue^2
  
  str <- paste("meanValue : ", as.character(meanValue), "  Dispersion Value:",as.character(dispValue), sep=" ")
  return (str)
  
}


ComputeMeanDispersion <- function (g ,nume = "", a = 0, b = 0, n = 0, lambda = 0, sigma = 0, m = 0){
  if (nume == "rUniforma"){
    rUniforma <- function (x) {
      if (x >= a & x <= b)
        return (1/(b - 1))
      return (0)
    }
    lowerBound = a
    upperBound = b
    return (ExecuteMeanDispersion(g, rUniforma, lowerBound, upperBound))
  }
  
  if (nume == "rGamma"){
    rGamma = function (x) {
      if (x > 0){
        return (1/((b^a)*gamma(a)) * x^(a - 1) * exp(-(x / b)))
      }
      return (0)
    }
    lowerBound = 0
    upperBound = Inf
    return (ExecuteMeanDispersion(g, rGamma, lowerBound, upperBound))
  }
  
  if (nume == "rBeta"){
    rBeta <- function (x) {
      if (x > 0 & x < 1){
        return (1/(beta(a, b)) * x^(a - 1) * (1 - x) ^ (b - 1))
      }
      return (0)
    }
    lowerBound = 0
    upperBound = 1
    return (ExecuteMeanDispersion(g, rBeta, lowerBound, upperBound))
  }
  
  if (nume == "rExponential"){
    rExponential <- function (x) {
      if (x >= 0)
        return ((1/lambda)*exp((-x)/lambda))
      return (0)
    }
    lowerBound = 0
    upperBound = Inf
    return (ExecuteMeanDispersion(g, rExponential, lowerBound, upperBound))
  }
  
  if (nume == "rStudents"){
    
    rStudents <- function (x) {
      return (gamma((n + 1) / 2) / (sqrt(n * pi) * gamma(n / 2)) * (1 + x^2 / n) ^ (-(n + 1) / 2))
    }
    lowerBound = 0
    upperBound = Inf
    return (ExecuteMeanDispersion(g, rStudents, lowerBound, upperBound))
  }
  
  if (nume == "rChiSquare"){
    rChiSquare <- function (x) {
      if (x >= 0){
        return (1/(2^(n/2)*gamma(n/2)) * (x^(n/2 - 1)) * exp(-x / 2))
      }
      return (0)
    }
    lowerBound = 0
    upperBound = Inf
    return (ExecuteMeanDispersion(g, rChiSquare, lowerBound, upperBound))
  }
  
  if (nume == "rNormal"){
    
    rNormal <- function (x){
      return(1 / (sigma * sqrt(2 * pi)) * exp (- (x-m)^2/(2*sigma^2)))
    }
    lowerBound = -Inf
    upperBound = Inf
    return (ExecuteMeanDispersion(g, rNormal, lowerBound, upperBound))
  }
  
  if (nume == "rWeibull"){
    
    rNormal <- function (x){
       if(x>lambda){
         return((b/n)*(((x-lambda)/n)^(b-1))*exp(-(((x-lambda)/n)^b)))
       }
     
    }
    lowerBound = lambda
    upperBound = Inf
    return (ExecuteMeanDispersion(g, rWeibull, lowerBound, upperBound))
  }
  
  if (nume == "rLogNormal"){
    
    rNormal <- function (x){
        return((1/(sigma*x*sqrt(2*pi)))*exp(-(((ln(x)-m))^2)/(2*sigma^2)))
      }
      
    
    lowerBound = -Inf
    upperBound = Inf
    return (ExecuteMeanDispersion(g, rLogNormal, lowerBound, upperBound))
  }
}












