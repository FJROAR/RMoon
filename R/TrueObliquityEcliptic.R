#' @title TrueObliqutiyEcliptic
#'
#' @description Estimation of the obliquity of ecliptic according to
#' Chapter 22 of Astronomical Algorithms (second edition)
#'
#' @param julianCent Numerical vector which represents a Julian converted in millenium
#'
#' @return Vector of epsilons (Obliquity of the Ecliptic) in degrees, parameter needed for the Sun's Declination and Right Ascension
#'
#' @examples
#'
#' #juliancent is computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' TrueObliquity <- TrueObliqutiyEcliptic(juliancent = -0.077221081451)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export

TrueObliqutiyEcliptic <- function(juliancent){

  epsilon = numeric(length(juliancent))

  for (i in c(1: length(juliancent))){

    U = juliancent / 100
    epsilon0 <- 23 + 26 / 60 + 21.448 / 3600 -
      4680.93/ 3600 * U -
      1.55/ 3600  * U**2 +
      1999.25/ 3600 * U**3 -
      51.38/ 3600 * U**4 -
      249.67/ 3600 * U**5 -
      39.05/ 3600 * U**6 +
      7.12/ 3600 * U**7 +
      27.87/ 3600 * U**8 +
      5.79/ 3600 * U**9 +
      2.45/ 3600 * U**10

    #Nutation in Obliquity

    D <- 297.85036 +
      445267.111480 * juliancent -
      0.0019142 * juliancent**2 +
      juliancent**3 / 189474

    D <- D %% 360
    if (D < 0) {D <- D + 360}

    M = 357.52772 + 35999.050340 * juliancent -
      0.0001603 * juliancent**2 -
      juliancent / 300000;

    M <- M %% 360
    if (M < 0) {M <- M + 360}

    Mprime <- 134.96298 + 477198.867398 * juliancent +
      0.0086972 * juliancent**2 +
      juliancent**3 / 56250

    Mprime <- Mprime %% 360
    if (Mprime < 0) {Mprime <- Mprime + 360}

    F_ <- 93.27191 + 483202.017538 * juliancent -
      0.0036825 * juliancent**2 +
      juliancent**3 / 327270;

    F_ <- F_ %% 360
    if (F_ < 0) {F_ <- F_ + 360}

    O = 125.04452 - 1934.136261 * juliancent +
      0.0020708 * juliancent**2 +
      juliancent**3 / 450000;

    O <- O %% 360
    if (O < 0) {O <- O + 360}

    df <- PeriodicNutObliq

    df$ArgNutObliq <- pi * (df$D * D +
                              df$M * M +
                              df$M_ * Mprime +
                              df$F * F_ +
                              df$O * O) / 180

    df$NutObliq <- (df$Obl1 + df$Obl2 * juliancent) *
      0.0001 * cos(df$ArgNutObliq)

    varEpsilon = sum(df$NutObliq)

    epsilon[i] <- epsilon0 + varEpsilon / 3600

  }


  return(epsilon)
}
