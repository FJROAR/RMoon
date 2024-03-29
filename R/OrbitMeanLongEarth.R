#' @title OrbitMeanLongEarth
#'
#' @description Estimation of the mean longitude of Earth's orbit according to the VSOP87 theory
#'
#' @param julianmil Numerical vector which represents a Julian converted in millenium
#'
#' @return Mean longitud of the Earth in its orbit at that date
#'
#' @examples
#'
#' #JulianMil is computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' MeanLong<- OrbitMeanLongEarth(julianmil = -0.0077221081451)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

OrbitMeanLongEarth <- function(julianmil){

  df = EarthVSOP87

  df$A <- as.numeric(gsub(",", ".", df$A0))*10^9
  df$B <- as.numeric(gsub(",", ".", df$B))
  df$C <- as.numeric(gsub(",", ".", df$C))

  dfL0 <- df[which(df$serie_desc == "L0"),]
  L0 = dfL0$A * cos(dfL0$B + dfL0$C * julianmil)
  dfL1 <- df[which(df$serie_desc == "L1"),]
  L1 = dfL1$A * cos(dfL1$B + dfL1$C * julianmil)
  dfL2 <- df[which(df$serie_desc == "L2"),]
  L2 = dfL2$A * cos(dfL2$B + dfL2$C * julianmil)
  dfL3 <- df[which(df$serie_desc == "L3"),]
  L3 = dfL3$A * cos(dfL3$B + dfL3$C * julianmil)
  dfL4 <- df[which(df$serie_desc == "L4"),]
  L4 = dfL4$A * cos(dfL4$B + dfL4$C * julianmil)
  dfL5 <- df[which(df$serie_desc == "L5"),]
  L5 = dfL5$A * cos(dfL5$B + dfL5$C * julianmil)

  L = (sum(L0) +
         julianmil * sum(L1)  +
         julianmil^2 * sum(L2)  +
         julianmil^3 * sum(L3)  +
         julianmil^4 * sum(L4)  +
         julianmil^5 * sum(L5)) / 10^9

  return (L)

}
