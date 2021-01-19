# o functie este PDF <=> integrata pe R = 1 si pt. oricare x, f(x) > 0
#
# se incearca integrarea pe suport 
# daca apare o eroare, inseamna ca integrate a esuat din diferite motive
# in acel caz, indiferent de unde e eroarea, clar f nu este PDF
# deci captez eroarea si returnez FALSE
#
# daca f trece de testul de integrare, 
# se incearca TCNT teste de integrare pe intervale cu capete random
# din intervalele suport
# pentru a verifica daca se obtin valor negative
# in cazul in care nu se obtin, se considera ca a trebut testul
#
#     NOTA: aleg sa integrez in loc sa verific direct valori,
#           deoarece in cel mai rau caz, daca am doar cateva valori negative rare
#           nu vor afecta calculul probabilitatilor
# 

isPdf <- function(f, suport = list(c(-Inf, Inf)), normCt = 1, TCNT = 1000){
  
  tryCatch({
    
    integratedVal <- 0
    
    for(interval in suport){

      integratedVal <- integratedVal + integrate(Vectorize(f), lower = interval[1], upper = interval[2])$value
    }
    
    if((integratedVal / normCt) < 1 + 10e-9 & (integratedVal / normCt) > 1 - 10e-9){
      
      cnt <- TCNT / length(suport)
      
      for(interval in suport){
        
        lowRand <- 0
        upperRand <- 0
        
        if(interval[1] == -Inf){
          lowRand <- -.Machine$integer.max
        }
        else{
          lowRand <- interval[1]
        }
        
        if(interval[2] == Inf){
          upperRand <- .Machine$integer.max
        }
        else{
          upperRand <- interval[2]
        }
        
        testLen <- upperRand / 10 - lowRand / 10
        
        rands <- runif(cnt, lowRand, upperRand - testLen)
        
        for(x in c(1:cnt)){
          
          lowerBound <- rands[x]
          upperBound = lowerBound + testLen
          
          if(lowerBound > upperBound){
            
            swapAux <- upperBound
            upperBound <- lowerBound
            lowerBoud <- swapAux
          }
          
          if(integrate(Vectorize(f), lower = lowerBound, upper = upperBound)$value < 0)
            return(FALSE)
        }
        
      }
      
      return(TRUE)
    }
    else
      return(FALSE)
    
  },
  error = function(err){
    
    message(paste("eroare a functiei isPdf: ", err))
    return(FALSE)
  })
  
}

exponential <- function(x){
  
  ifelse(x <= 0, return(0), return(2 * exp(-2 * x)))
}

exponentialBroken <- function(x){
  
  return(2 * exp(-2 * x))
}

print(isPdf(exponential, normCt = 1))
print(isPdf(exponentialBroken, suport = list(c(0, Inf))))
print(isPdf(sin))

