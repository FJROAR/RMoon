#' @title EasterChristian
#'
#' @description Determination of the Christian Easter. The following algorithm for
#' computing the date of Easter is based on the algorithm of Oudin (1940). It is
#' valid for any Gregorian year.
#'
#' @param Year vector in integer numeric format: yyyy
#'
#' @return List of vector of: (1) vector of months (2) vector of last easter sundays
#'
#' @examples
#'
#' library(RMoon)
#'
#' Easter <- ChristianEaster(2024)
#'
#' @references
#'  J.-M. Oudin (1940) Étude sur la date de Pâques, Bulletin Astronomique, 12, Paris
#'
#' @export


ChristianEaster <- function(Year){

  M <- vector("numeric", length(Year))
  D <- vector("numeric", length(Year))

  for (i in c(1: length(Year))){

    C <- Year[i] %/% 100
    N <- Year[i] - 19 * (Year[i] %/% 19)
    K <- (C - 17) %/% 25
    I <- C - C%/% 4 - (C - K)%/%3 + 19 * N + 15
    I <- I - 30* (I %/% 30)
    I <- I - (I %/% 28)*(1 - (I %/% 28)*(29 %/% (I + 1))*((21 - N) %/% 11))
    J <- Y + Y %/% 4 + I + 2 - C + C %/% 4
    J <- J - 7 * (J %/% 7)
    L = I - J
    M[i] = 3 + (L + 40) %/% 44
    D[i] = L + 28 - 31*(M[i] %/% 4)

  }

  return(list(M, D))


  }
