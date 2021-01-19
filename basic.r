isPdfExpr <- function(expr, suport = list(c(-Inf, Inf)), normCt = 1, TCNT = 1000){
  
  tryCatch({
    
    fct <- function(x){
      return(eval(expr))
    }
    
    derivate <- function(x){
      return(eval(deriv(expr, 'x')))
    }
    
    integratedVal <- 0
    
    for(interval in suport){
      
      integratedVal <- integratedVal + integrate(Vectorize(fct), lower = interval[1], upper = interval[2])$value
    }

    if((integratedVal / normCt) < Inf & (integratedVal / normCt) > -Inf){
      
      cnt = TCNT / length(suport)
      
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

        randsL <- runif(cnt, lowRand, upperRand)
        randsR <- runif(cnt, lowRand, upperRand)
        
        for(x in c(1:cnt)){
          
          lowerBound <- randsL[x]
          upperBound <- randsR[x]
          
          if(lowerBound > upperBound){
            
            swapAux <- upperBound
            upperBound <- lowerBound
            lowerBoud <- swapAux
          }
          
          if((derivate(lowerBound) < 0 & derivate(upperBound) > 0) | (derivate(lowerBound) > 0 & derivate(upperBound) < 0)){
            print("----")
            print(uniroot(derivate, lower = lowerBound, upper = upperBound)$root)
            print(lowerBound)
            print(upperBound)
            if(fct(uniroot(derivate, lower = lowerBound, upper = upperBound)$root) < 0)
              return(FALSE)
          }
          
        }
        
      }
      
      return(TRUE)
    }
    else
      return(FALSE)
    
  },
  error = function(err){
    
    message(paste("eroare a functiei isPdfExpr: ", err))
    return(FALSE)
  })
}

expPdf <- expression(2 * exp(-2 * x))

expr <- expression(x^2)

sinexpr <- expression(sin(x))


dv <- function(x, expr){
  return(eval(deriv(expr, 'x')))
}

print(dv(5, expr))
print(dv(1, expPdf))

print(isPdfExpr(expPdf, suport = list(c(0, Inf))))
print(isPdfExpr(sinexpr, suport = list(c(0, 2 * pi)), TCNT = 100))

print(uniroot(cos, lower = 0, upper = 4)$root)

print(dv(pi/2, sinexpr))
print(cos(pi/2) < 1e-9)

