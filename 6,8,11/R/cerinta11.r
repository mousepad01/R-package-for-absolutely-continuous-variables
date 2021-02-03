#' Calculeaza densitatea marginala
#'
#' Primeste ca parametru o densitatate comuna a doua variabile returneaza densitatea marginala
#' @param f Functia de densitate comuna
#' @param Which carei variabile sa i se calculeze densitatea
#' @return functia densitate marginala
#' @examples
#' ComputeMarginalDistribution(function (x, y) <- {return (x + y)}, "X")
ComputeMarginalDistribution <- function (fct, Which = "X"){
  if (Which == "X"){
  MarginalX <- function(y) {
    sapply(y, function(y) {
      integrate(Vectorize(function(x) fct(x,y)), -Inf, Inf)$value
    })
  }
  return (MarginalX)
  }
  else if (Which == "Y"){
    MarginalY <- function(x) {
      sapply(x, function(x) {
        integrate(Vectorize(function(y) fct(x,y)), -Inf, Inf)$value
      })
    }

    return (MarginalY)
  }
  else {
    return ("Invalid")
  }

}

#' Calculeaza densitatea conditionala
#'
#' Primeste ca parametru o densitatate comuna a doua variabile returneaza densitatea conditionala
#' @param f Functia de densitate comuna
#' @param Which carei variabile sa i se calculeze densitatea conditionala
#' @return functia densitate marginala
#' @examples
#' ComputeConditionalDensity(function (x, y) <- {return (x + y)}, "X")
ComputeConditionalDensity <- function (fct, Which = "X"){
  if (Which == "X") {
  MarginalY <- ComputeMarginalDistributionY(fct)
  ConditionalDensity <- function (x, y) {
    return (fct(x, y) / MarginalY (y))
  }
  return (ConditionalDensity)
  }
  else if (Which == "Y"){
    MarginalX <- ComputeMarginalDistributionX(fct)
    ConditionalDensity <- function (x, y) {
      return (fct(x,y) / MarginalX(y))

    }
    return (ConditionalDensity)

  }
}

