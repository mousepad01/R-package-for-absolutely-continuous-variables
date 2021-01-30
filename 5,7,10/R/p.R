#' Probability
#'
#' This function calculates (conditional) probabilities of a continuous random variable.
#' @param f(x) A probability density function
#' @param p A string, indicating a (conditional) probability.
#' @keywords p prob probability
#' @export
#' @examples
#' f <- function (x) {
#'      fun <- 3 * x ^ 2
#'      fun[x < 0] = 0
#'      fun[x > 1] = 0
#'      return ( fun )
#' }
#'
#' P(test, "X < 1 | X < 3")  # good
#' P(test, "X < 2")          # good
#' P(test, "X < 1 | X > 1")  # good
#' P(test, "X < 1 | X Z")    # wrong
#' P(test, "X < 1 | Y < 2")  # wrong
#' P(test, "X < -1 & X > 1") # wrong

P <- function(f, p) {

  getOperationAndArgs <- function (inp) {
    inp <- gsub(" ", "", inp)
    for (op in c("<=", ">=", "<", ">", "=")) {

      split <- unlist(strsplit(inp, op, fixed = TRUE))
      splitSize <- length(split)

      if (splitSize > 1) {

        if (nchar(split[1]) != 1)
          return (NULL)

        usedVariables <<- c(usedVariables, split[1])
        return (c(op, split[2]))
      }
    }
  }

  cdf <- function(a) {
    return ((integrate(f, -Inf, a) $ value))
  }

  eval <- function(op, upperBound) {

    if (is.null(op) || is.null(upperBound) || grepl("\\D", upperBound))
      return (noquote("Parsing error."))

    if (upperBound %in% c("+Inf", "-Inf")) {
      upperBound <- if (upperBound == "+Inf") +Inf else -Inf
    }

    else {
      upperBound <- as.double(upperBound)
    }


    cdfAns <- round(cdf(upperBound), 4)

    ans <- switch(
      op,
      "=" = 0,
      "<=" = cdfAns,
      "<" = cdfAns,
      ">=" = 1 - cdfAns,
      ">" = 1 - cdfAns
    )

    return (ans)
  }

  sameKind <- function (a, b) {
    return (
      a == b  || (a == "<" && b == "<=") || (a == "<=" && b == "<")
      || (a == ">" && b == ">=") || (a == ">=" && b == ">")
    )
  }

  split <- unlist(strsplit(p, "|", fixed = TRUE))
  splitSize <- length(split)

  if (splitSize > 2)
    return(noquote("Parsing error."))

  usedVariables <- c()

  if (splitSize == 1) {
    cond <- getOperationAndArgs(p)
    return (eval(cond[1], cond[2]))
  }

  cond1 <- getOperationAndArgs(split[1])
  cond2 <- getOperationAndArgs(split[2])

  if (is.null(usedVariables) || length(usedVariables) != 2)
    return (noquote("Parsing error."))

  if (usedVariables[1] != usedVariables[2])
    return (noquote("Two different variables have been used."))

  op1 <- cond1[1]
  op2 <- cond2[1]

  upperBound1 <- as.double(cond1[2])
  upperBound2 <- as.double(cond2[2])

  if (is.null(op1) || is.null(upperBound1) || is.null(op2) || is.null(upperBound2))
    return (noquote("Parsing error."))

  p1 <- eval(op1, upperBound1)
  p2 <- eval(op2, upperBound2)

  if (p1 == p2 && !p1)
    return (noquote("Undefined."))

  if (p1 == 0)
    return (0)

  if (p2 == 0)
    return (noquote("Impossible: division by zero."))

  if (sameKind(op1, op2))
    return (if (op1 %in% c("<=", "<"))
      cdf(min(upperBound1, upperBound2)) / p2
      else
        cdf(max(upperBound1, upperBound2)) / p2 )

  if (op1 %in% c("<=", "<"))
    if (upperBound1 <= upperBound2)
      return (0)

  if (op1 %in% c(">=", ">"))
    if (upperBound1 >= upperBound2)
      return (0)

  return ((p2 + p1 - 1) / p2)
}
