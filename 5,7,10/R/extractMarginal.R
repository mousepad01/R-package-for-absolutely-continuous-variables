extractMarginal <- function (f, support, type) {
  if (type == "x") {
    return (
      Vectorize(function (y) {
        integrate(function (x) { f(x, y) }, support[1], support[2]) $ value
      })
    )
  }

  return (
    Vectorize(function (x) {
      integrate(function (y) { f(x, y) }, support[1], support[2]) $ value
    })
  )
}
