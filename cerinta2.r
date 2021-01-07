isPdf <- function(f){
  
  # o functie este PDF <=> integrata pe R = 1 si pt. oricare x, f(x) > 0
  #
  # se incearca integrarea pe R (nu doar pe suport)
  # daca apare o eroare, inseamna ca integrate a esuat din diferite motive
  # in acel caz, indiferent de unde e eroarea, clar f nu este PDF
  # deci captez eroarea si returnez FALSE
  #
  # daca f trece de testul de integrare, se incearca unele valori 
  # pentru a verifica daca este negativa in unele puncte
  # cred ca voi face asa 
  
  tryCatch({
    
    integratedVal <- integrate(Vectorize(f), -Inf, Inf)$value
    
    if(integratedVal - 1 < 5e-07){
      
      return(TRUE)
    }
    else{
      return(FALSE)
    }
    
  },
  error = function(err){
    print("here")
    return(FALSE)
  })
  
}

exponential <- function(x){
  
  ifelse(x <= 0, return(0), return(2 * exp(-2 * x)))
}

exponentialBroken <- function(x){
  
  return(2 * exp(-2 * x))
}

print(isPdf(exponential))
print(isPdf(exponentialBroken))

