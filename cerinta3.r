normalPdf <- function(x, expectedVal = 0, deviation = 1){
  return((1 / sqrt(2 * pi)) * exp((-1 / 2) * ((x - expectedVal) / (deviation * deviation))))
}

E <- function(pdf, normCt = 1){
  
  toIntegrate <- function(x){
    
    return(x * pdf(x) / normCt)
  }
  
  tryCatch({
    
    integrate(Vectorize(toIntegrate), lower = -Inf, upper = Inf)
  },
  error = function(err){
    
    message("Eroare la calcularea mediei:")
    print(err)
  })
}

ConRV <- setClass(
  
  "ConRV",
  
  slots = c(
    
    pdf = "function",
    normCt = "numeric",
    
    expectedVal = "numeric",
    variance = "numeric",
    deviation = "numeric",
    
    firstInitialMoment = "numeric",
    firstCentratedMoment = "numeric",
    
    secondInitialMoment = "numeric",
    secondCentratedMoment = "numeric",
    
    thirdInitialMoment = "numeric",
    thirdCentratedMoment = "numeric",
    
    fourthInitialMoment = "numeric",
    fourthCentratedMoment = "numeric"
  )
)

setMethod("initialize", "ConRV", function(.Object, pdfCandidate, normCt, normalize){
  
  tryCatch({
    
    rv <- .Object
    
    if(missing(pdfCandidate)){
      
      rv@pdf = normalPdf
      rv@normCt = 1
      
      rv@expectedVal = 0
      rv@variance = 1
      rv@deviation = 1
      
      rv@firstInitialMoment = 0
      rv@firstCentratedMoment = 0
      
      rv@secondInitialMoment = 1
      rv@secondCentratedMoment = 1
      
      rv@thirdInitialMoment = 0
      rv@thirdCentratedMoment = 0
      
      rv@fourthInitialMoment = 3
      rv@fourthCentratedMoment = 3
      
    }
    else{

      if(missing(normalize) == FALSE){
        
        if(normalize == TRUE){
          
          rv@normCt <- getNormCt(pdfCandidate)
        }
        
      }
      else if(missing(normalize) == FALSE & missing(normCt) == TRUE){
        
        if(normalize == FALSE){
          
          rv@normCt = normCt
        }
        else{
          
          rv@normCt = 1
        }
        
      }
      else{
        
        rv@normCt = 1
      }

      checkDensity <- isPdf(pdfCandidate, normCt = rv@normCt)
      
      if(checkDensity == FALSE){
        
        message("Eroare la initializarea variabilei: nu s-a oferit o densitate valida!")
      }
      else{
        
        rv@pdf = pdfCandidate
        
        rv@expectedVal = E(pdf = pdfCandidate, normCt = rv@normCt)$value
        
        # DE COMPLETAT CU FUNCTIILE CARE CALCULEAZA VARIANTA, DEVIATIA SI MOMENTELE
        #
        # -------------------------------------------------------------------------
        
      }
    }
    
    return(rv)
    
  },
  error = function(err){
    
    message("eroare la initializarea variabilei:")
    print(err)
  })
  
})

v <- ConRV(pdfCandidate = exponential)

print(v)






