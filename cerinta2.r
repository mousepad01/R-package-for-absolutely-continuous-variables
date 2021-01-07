# o functie este PDF <=> integrata pe R = 1 si pt. oricare x, f(x) > 0
#
# se incearca integrarea pe suport 
# daca apare o eroare, inseamna ca integrate a esuat din diferite motive
# in acel caz, indiferent de unde e eroarea, clar f nu este PDF
# deci captez eroarea si returnez FALSE
#
# daca f trece de testul de integrare, 
# se incearca RAND_TEST_CNT teste de integrare pe intervale cu capete random
# ce formeaza intervalele suport
# pentru a verifica daca se obtin valor negative
# in cazul in care nu se obtin, se considera ca a trebut testul
#
#     NOTA: aleg sa integrez in loc sa verific direct valori,
#           deoarece in cel mai rau caz, daca am doar cateva valori negative rare
#           nu vor afecta calculul probabilitatilor
#

RAND_TEST_CNT <- 10000

isPdf <- function(f, suport = list(c(-Inf, Inf))){
  
  tryCatch({
    
    integratedVal <- 0
    
    for(interval in suport){

      integratedVal <- integratedVal + integrate(Vectorize(f), lower = interval[1], upper = interval[2])$value
    }
    
    if(integratedVal - 1 < 5e-07){
      
      for(interval in suport){
        
        lowerBound <- sample(-.Machine$integer.max:.Machine$integer.max, 1)
        upperBound <- lower_bound + 10

        if(lowerBound < upperBound){
          
          swapAux <- upperBound
          upperBound <- lowerBound
          lowerBoud <- swapAux
        }
        
        if(integrate(Vectorize(f), lower = lowerBound, upper = upperBound)$value < 0)
          return(FALSE)
        
      }
      
      return(TRUE)
    }
    else
      return(FALSE)
    
  },
  error = function(err){
    
    message("(eroare a functiei isPdf)")
    print(err)
    return(FALSE)
  })
  
}

exponential <- function(x){
  
  ifelse(x <= 0, return(0), return(2 * exp(-2 * x)))
}

exponentialBroken <- function(x){
  
  return(2 * exp(-2 * x))
}

print(isPdf(exponential, list(c(0, Inf))))
print(isPdf(exponentialBroken))

