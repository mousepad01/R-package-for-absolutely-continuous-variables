var2 <- function (f, suppY, suppX, type, mean) {
  if (type == "x") {
    return (
      integrate(
        function(x) { return ((x - mean) ^ 2 * extractMarginal(f, suppY, "y")(x)) },
        suppX[1], suppX[2]
      ) $ value
    )
  }

  return (
    integrate(
      function (y) { return ((y - mean) ^ 2 * extractMarginal(f, suppX, "x")(y)) },
      suppY[1], suppY[2]
    ) $ value
  )
}
