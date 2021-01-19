getNormCt <- function(f, suport = list(c(-Inf, Inf))){
  
  # constanta de normalizare este fix valoarea integralei functiei pe R
  
  tryCatch({
    
    normCt <- 0
    
    for(interval in suport){
      
      normCt <- normCt + integrate(Vectorize(f), lower = interval[1], upper = interval[2])$value
    }
    
    return(1 / normCt)
    
  },
  error = function(err){
    message(paste("Nu se poate determina constanta de integrare! eroarea aruncata de integrate:", err))
  })
}

exponentialScaled <- function(x){
  
  ifelse(x <= 0, 0, 10 * exp(-2 * x))
}


print(getNormCt(exponential))
print(getNormCt(exponentialScaled))
