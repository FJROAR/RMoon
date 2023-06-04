#' @title NutationLong
#'
#' @description Estimation of the longitude of nutation according to
#' Chapter 22 of Astronomical Algorithms (second edition)
#'
#' @param julianCent Numerical vector which represents a Julian converted in millenium
#'
#' @return Vector of varNutation (Nutation in longitude) in degrees, parameter needed for the Moon's Declination and Right Ascension
#'
#' @examples
#'
#' #juliancent is computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' varNutation <- NutationLong(juliancent = -0.077221081451)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export

NutationLong <- function(juliancent){

  varNutation = numeric(length(juliancent))

  for (i in c(1: length(juliancent))){

    #Nutation in Longitude

    D <- 297.85036 +
      445267.111480 * juliancent[i] -
      0.0019142 * juliancent[i]**2 +
      juliancent[i]**3 / 189474

    D <- D %% 360
    if (D < 0) {D <- D + 360}

    M = 357.52772 + 35999.050340 * juliancent[i] -
      0.0001603 * juliancent[i]**2 -
      juliancent[i] / 300000;

    M <- M %% 360
    if (M < 0) {M <- M + 360}

    Mprime <- 134.96298 + 477198.867398 * juliancent[i] +
      0.0086972 * juliancent[i]**2 +
      juliancent[i]**3 / 56250

    Mprime <- Mprime %% 360
    if (Mprime < 0) {Mprime <- Mprime + 360}

    F_ <- 93.27191 + 483202.017538 * juliancent[i] -
      0.0036825 * juliancent[i]**2 +
      juliancent[i]**3 / 327270;

    F_ <- F_ %% 360
    if (F_ < 0) {F_ <- F_ + 360}

    O = 125.04452 - 1934.136261 * juliancent[i] +
      0.0020708 * juliancent[i]**2 +
      juliancent[i]**3 / 450000;

    O <- O %% 360
    if (O < 0) {O <- O + 360}

    df <- PeriodicNutObliq

    df$ArgNutObliq <- pi * (df$D * D +
                              df$M * M +
                              df$M_ * Mprime +
                              df$F * F_ +
                              df$O * O) / 180

    df$NutObliq <- (df$Nut1 + df$Nut2 * juliancent[i]) *
      0.0001 * sin(df$ArgNutObliq)

    varNutation[i] = sum(df$NutObliq)/3600

  }

  return(varNutation)
}
