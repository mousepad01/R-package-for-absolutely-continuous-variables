getNormCt <- function(f){
  
  # constanta de normalizare este fix valoarea integralei functiei pe R
  
  tryCatch({
    
    normCt <- integrate(Vectorize(f), -Inf, Inf)$value
    
    return(normCt)
    
  },
  error = function(err){
    message("Nu se poate determina constanta de integrare! eroarea aruncata de integrate: ")
    print(err)
  })
}


print(getNormCt(exponentialBroken))
