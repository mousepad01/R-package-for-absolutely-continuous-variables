integrate2 <- function (f, suppY, suppX) {
  return (
    integrate(Vectorize(function (y) {
      integrate(function (x) { f(x, y) }, suppX[1], suppX[2]) $ value
    }), suppY[1], suppY[2]) $ value
  )
}
