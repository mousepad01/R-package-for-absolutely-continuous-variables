getNormCt <- function(f){
  
  tryCatch({
    
    normCt <- integrate(f, -Inf, Inf)$value
    
    return(normCt)
    
  },
  error = function(err){
    print("Nu se poate determina constanta de integrare! eroarea aruncata de integrate: ")
    print(err)
  })
}


exponential <- function(x){
  
  ifelse(x <= 0, return(0), return(2 * exp(-2 * x)))
}

exponentialBroken <- function(x){
  
  return(2 * exp(-2 * x))
}

print(getNormCt(exponential))
