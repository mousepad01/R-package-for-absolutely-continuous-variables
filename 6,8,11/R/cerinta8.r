#' Functie ce ofera informatii despre diverse distributii cunoscute
#'
#' Functia primeste un nume si afiseaza informatii relevante si interesante
#' despre repartitia in cauza
#' @param name Numele repartitiei despre care vrem sa aflam informatii
#' @return un sir de caractere ce afiseaza informatii despre repartitia in cauza
#' @examples
#' DistributionsSynthesis ("Uniform")

DistributionsSynthesis <- function(name) {
  if (name == "Uniform") {
    parameters <- "a, b"
    range <- "[a,b]"
    notation <- "uniform(a, b) or U(a, b)"
    pdf <- "f(x) = 1 / (b - a) for x in [a, b] \n otherwise f(x) = 0"
    cdf <- " F(x) = 0 for x < a \n  F(x) = (x - a) / (b - a) for x in [a, b] \n otherwise F(x) = 1"
    mean <- "(a + b)/2"
    median <- "(a + b)/2"
    variance <- "1/12 * (b - a) ^ 2"
    use <- "The Uniform distribution is the simplest probability distribution, but it plays an important role in statistics since it is very useful in modeling random variables.
    The uniform distribution is a continuous probability distribution and is concerned with events that are equally likely to occur. The continuous random variable X is said to be
    uniformly distributed, or having rectangular distribution on the interval [a,b]. "
  }
  else if (nume == "Gamma") {
    parameters <- "alpha, beta"
    rage <- "(0, inf)"
    notation <- "Gamma(alpha, beta)"
    pdf <- "f(x) = 1 / ((beta ^alpha) * (gamma(alpha)) * x ^ (alpha - 1) * e ^ (-x / beta) for x > 0, where gamma is Euler's Gamma function \n otherwise f(x) = 0"
    cdf <- "F(x) = 1 / gamma(alpha) * gamma(alpha, beta * x)"
    mean <- "alpha * beta"
    median <- "No simple closed form"
    variance <- "alpha * beta ^ 2"
    use <- "Gamma distribution, in statistics, continuous distribution function with two positive parameters,
    α and β, for shape and scale, respectively, applied to the gamma function. Gamma distributions occur frequently
    in models used in engineering (such as time to failure of equipment and load levels for telecommunication services),
    meteorology (rainfall), and business (insurance claims and loan defaults) for which the variables are always positive
    and the results are skewed (unbalanced)."
  }
  else if (nume == "Beta") {
    parameters <- "alpha > 0, beta > 0"
    range <- "(0, 1)"
    notation <- "Beta(alpha, beta)"
    pdf <- "f(x) = (x ^ (alpha - 1)) * (1 - x) ^ (beta - 1) / Beta(alpha, beta) for x in (0,1) where Beta is Euler's Beta function \n otherwise 0"
    cdf <- "I_x(alpha, beta)"
    mean <- "alpha / (alpha + beta) where I is a regularized incomplete beta function"
    median <- "I_(0.5) ^ (-1) (alpha, beta) where I is a regularized incomplete beta function"
    variance <- "(alpha * beta) / (alpha + beta) ^ 2 * (alpha + beta + 1)"
    use <- "The Beta distribution is a probability distribution on probabilities. For example, we can use it to model the probabilities: the Click-Through Rate of your advertisement,
    the conversion rate of customers actually purchasing on your website, how likely readers will clap for your blog, how likely it is that Ta president will win a second term,
    the 5-year survival chance for women with breast cancer, and so on.Because the Beta distribution models a probability, its domain is bounded between 0 and 1."
  }
  else if (nume == "Exponential") {
    parameters <- "lambda = λ"
    range <- "[0, inf)"
    notation <- "exponential(λ) or exp(λ); λ>0"
    pdf <- "f(x) = λ * e ^ (λ * x) for x >= 0 \n otherwise 0"
    cdf <- "F(x) = 1 - e ^ (-λ * x) for x >= 0 \n otherwise 0"
    mean <- "1 / λ"
    median <- "(ln 2) / λ"
    variance <- "1 / λ ^ 2"
    rightTailDistribution <- "P(X > x) = 1 - F(x) = e ^ (-λ * x)"
    use <- "Exponential variables can also be used to model situations where certain events occur with a
    constant probability per unit length, such as the distance between mutations on a DNA strand, or between
    roadkills on a given road.
    In queuing theory, the service times of agents in a system (e.g. how long it takes for a bank teller etc. to
    serve a customer) are often modeled as exponentially distributed variables."
  }
  else if (nume == "Chi-Square") {
    parameters <- "k, a natural number different from 0"
    range <- "if k == 1 then (0, int) otherwise [0, inf)"
    notation <- "ChiSquare(k) or H(k)"
    pdf <- "f(x) = 1 / (2 ^ (k / 2) * gamma(k / 2)) * x ^ (k / 2 - 1) * e ^ (-x / 2) for x > 0, where gamma is Euler's Gamma function \n otherwise f(x) = 0"
    cdf <- "1 / Gamma(k / 2) * gamma(k / 2, x / 2) \n where where Gamma is Euler's Gamma function "
    mean <- "k"
    median <- "approx k * (1 - 2 / (9 * k)) ^ 3"
    variance <- "2 * k"
    use <- "The chi-square distribution is used in the common chi-square tests for goodness of fit of an observed distribution to
    a theoretical one, the independence of two criteria of classification of qualitative data, and in confidence interval
    estimation for a population standard deviation of a normal distribution from a sample standard deviation.
    Many other statistical tests also use this distribution, such as Friedman's analysis of variance by ranks."
  }
  else if (nume == "StudentsT") {
    parameters <- "v, v > 0"
    range <- "(-inf, inf)"
    notation <- "S(v)"
    pdf <- "(gamma((v + 1) / 2) / (sqrt(v * pi) * gamma(v / 2))) * (1 + x ^ 2 / v) ^ (-(v + 1) / 2) for x in (-inf, inf) where gamma is Euler's Gamma function"
    cdf <- "No simple closed form"
    mean <- "0 for x> 0 otherwise undefined"
    median <- "0"
    variance <- " v / (v - 2) for v > 2; inf for 1 < v <= 2; otherwise undefined"
    use <- "Student's t-distribution arises in a variety of statistical estimation problems where the goal is to estimate an
    unknown parameter, such as a mean value, in a setting where the data are observed with additive errors.
    If (as in nearly all practical statistical work) the population standard deviation of these errors is unknown and has to be
    estimated from the data, the t-distribution is often used to account for the extra uncertainty that results from this
    estimation. In most such problems, if the standard deviation of the errors were known, a normal distribution would be used
    instead of the t-distribution."
  }
  else if (nume == "Normal") {
    parameters <- "μ = miu, σ = sigma"
    range <- "(-inf, inf)"
    notation <- "normal(μ, σ2) or N(μ, σ2)"
    pdf <- "f(x) = 1 / ((σ * sqrt(2 * π))) * e ^ (-1/2 * ((x - μ) / σ) ^ 2) for x in (-inf, inf), μ in (-inf, inf), σ > 0"
    cdf <- "F(x) has no formula, so use tables or software such as pnorm in R to compute F(x)"
    mean <- "μ"
    median <- "μ"
    variance <- "σ ^ 2"
    use <- "The normal distribution is the most important probability distribution because it fits many natural phenomena.
    For example, heights, blood pressure, measurement error, and IQ scores follow the normal distribution.Normal distributions
    are important in statistics and are often used in the natural and social sciences to
    represent real-valued random variables whose distributions are not known. Their importance is partly due to the central
    limit theorem. It states that, under some conditions, the average of many samples (observations) of a random variable with
    finite mean and variance is itself a random variable—whose distribution converges to a normal distribution as the number of
    samples increases. Therefore, physical quantities that are expected to be the sum of many independent processes, such as
    measurement errors, often have distributions that are nearly normal"
  }
  else {
    str <- paste("No available information about this distribution")
    return(str)
  }

  cat("Sources used: https://ocw.mit.edu/courses/mathematics/18-05-introduction-to-probability-and-statistics-spring-2014/readings/MIT18_05S14_Reading5c.pdf
      \n https://www.probabilitycourse.com/chapter4/  \n https://www.britannica.com/science/gamma-distribution \n https://www.britannica.com/science/ \n https://towardsdatascience.com/ \n",
      "\n\nNotation:\n", parameters, "\n\nNotation:\n", range , "\n\nPDF:\n", notation, "\n\nRange:\n", pdf, "\n\nCDF:\n", cdf, "\n\nMean:\n", mean, "\n\nMedian:\n",
      median, "\n\nVariance:\n", variance, "\n\nUse:\n", use, "\n")

}
