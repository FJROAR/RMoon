#' @title MoonEcplipses
#'
#' @description Determination of the posibility of a Moon Eclipse when happen
#' a full moon phase according to Chapter 54 of Astronomical Algorithms
#'
#' @param day Date in format: "yyyy-mm-dd"
#'
#' @return List of 12 vectors of: (1) juliano or Julian day of the following full moon and
#' when the eclipse would happen (2) predDate or the date at format yyyy-mm-dd hh-mm -ss
#'  (3) isEclipse or Moon's eclipse condition (4) nearNode or closest node where the eclipse takes
#' place (5) gamma or least distance from the centre of the Moon to the axis of the Earth'shadow
#' in units of the Earth's equatorial raidius (6) u radius of the Moon's umbra cone
#' (7) rho o penumbra radius (if this has sense),
#'  (8) sigma or umbra radius (if this has sense) (9) umbraMagn or magnitud of the umbra (if
#' this value is negative not eclipse at umbra happen) (10) penumbraMag or magnitud of the
#' penumbra (11) semiDurationPartialumbra or duration in minutes of partial eclipse at
#' the umbra phase if this phase happens (12) semiDurationTotalalumbra or duration in minutes of the
#' total umbra phase if this phase happens (13) semiDurationPartialpenumbra or duration
#' in minutes of the penumbra phase if this phase happens.
#'
#' Semiduration offers non-valid or NaN values if eclipse in umbra - penumbra does not happen
#'
#' @examples
#'
#' library(RMoon)
#'
#' MoonEclipseElements <- MoonEclipses("1977-02-15")
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

MoonEclipses <- function(day){

  day = as.Date(day)

  juliano <- vector("numeric", length(day))
  predDate <- vector("character", length(day))
  isEclipse <- vector("character", length(day))
  nearNode <- vector("character", length(day))
  gamma <- vector("numeric", length(day))
  u <- vector("numeric", length(day))
  rho <- vector("numeric", length(day))
  sigma <- vector("numeric", length(day))
  umbraMagn <- vector("numeric", length(day))
  penumbraMagn <- vector("numeric", length(day))
  semiDurationPartialumbra <- vector("numeric", length(day))
  semiDurationTotalumbra <- vector("numeric", length(day))
  semiDurationPartialpenumbra <- vector("numeric", length(day))


  for (i in c(1: length(day))){

    anio = as.numeric(substr(day[i], 1, 4))
    first_day <- as.Date(paste0(anio, "-01-01"))
    past_days <- as.numeric(day[i] - first_day) + 1

    if ((anio %% 4 == 0 && anio %% 100 != 0) || anio %% 400 == 0) {

      partyear = round(past_days / 366 , 2)

    } else {

      partyear = round(past_days / 365 , 2)

    }

    #Full moon determination
    k = floor((anio + partyear - 2000) * 12.3685) + 0.5

    Tcent = k / 1236.85

    JDE <- 2451550.09766 +
      29.530588861 * k +
      0.00015437 * Tcent^2 -
      0.000000150 * Tcent^3 +
      0.00000000073 * Tcent^4

    ref <-JulianDayMeeus(anio,
                         as.numeric(substr(day[i], 6, 7)),
                         as.numeric(substr(day[i], 9, 10)),
                         0,0,0)[[1]]

    if(JDE < ref){

      k = k + 1;

      Tcent = k / 1236.85

      JDE <- 2451550.09766 +
        29.530588861 * k +
        0.00015437 * Tcent^2 -
        0.000000150 * Tcent^3 +
        0.00000000073 * Tcent^4

    }



    E <- 1 - 0.002516 * Tcent - 0.0000074 * Tcent^2
    M <- ((2.5534 + 29.10535670 * k -
             0.0000015 * Tcent^2 -
             0.00000011 * Tcent^3) %% 360 ) * pi / 180

    M_ <- ((201.5643 + 385.81693528 * k +
              0.0107582 * Tcent^2 +
              0.00001238 * Tcent^3 -
              0.000000058 * Tcent^4) %% 360 ) * pi / 180

    F_ <- ((160.7108 + 390.67050284 * k -
              0.0016118 * Tcent^2 -
              0.00000227 * Tcent^3 +
              0.000000011 * Tcent^4) %% 360 ) * pi / 180

    Om <- ((124.7746 - 1.56375588 * k +
              0.0020672 * Tcent^2 +
              0.00000215 * Tcent^3) %% 360 ) * pi / 180

    F2 <- F_ * 180 / pi
    Eclipse <- abs(F2 %% 360)
    Eclipse <- min(abs(Eclipse - 0), abs(Eclipse - 180), abs(Eclipse - 360))

    isEclipse[i] <- ifelse(Eclipse < 13.9, "Yes", "No")
    isEclipse[i] <- ifelse((Eclipse >= 13.9 &
                           Eclipse <= 21), "Indetermined", isEclipse[i])

    nearNode[i] <- ifelse((abs(F2 - 180) < abs(F2)) &
                             (abs(F2 - 180) < abs(360 - F2)),
                          "Descending",
                          "Ascending")
    F1 = (F_ * (180 / pi) - 0.02665 * sin(Om)) * pi / 180
    A1 = (299.77 + 0.107408 * k - 0.009173 * Tcent^2) * pi / 180

    correction = -0.4065 * sin(M_) + 0.1727 * E * sin(M) +
      0.0161 * sin(2*M_) - 0.0097 * sin(2*F1) + 0.0073 * E * sin(M_ - M) -
      0.0050 * E * sin(M_ + M) - 0.0023 * sin(M_ - 2 * F1) +
      0.0021 * E * sin(2 * M) + 0.0012 * sin(M_ + 2 * F1) +
      0.0006 * E * sin(2 * M_ + M) - 0.0004 * sin(3 * M_) -
      0.0003 * E * sin(M + 2 * F1) + 0.0003 * sin(A1) -
      0.0002 * E * sin(M - 2 * F1) - 0.0002 * E * sin(2 * M_ - M) -
      0.0002 * sin(Om)

    juliano[i] = JDE + correction

    base_1970 <- (juliano[i] - 2440587.5) * 24 * 3600
    predDate[i] = as.character(as.POSIXct(as.numeric(base_1970),
                                          origin = "1970-01-01", tz = "UTC"))

    P = 0.2070 * E * sin(M) +
      0.0024 * E * sin(2 * M) -
      0.0392 * E * sin(M_) +
      0.0116 * sin(2 * M_) -
      0.0073 * E * sin(M_ + M) +
      0.0067 * E * sin(M_ - M) +
      0.0118 * sin(2 * F1)

    Q = 5.2207 -
      0.0048 * E * cos(M) +
      0.002 * E * cos(2 * M) -
      0.3299 * cos(M_) -
      0.006 * E * cos(M_ + M) +
      0.0041 * E * cos(M_ - M)

    W = abs(cos(F1))

    gamma[i] = (P * cos(F1) + Q * sin(F1)) * (1 - 0.0048 * W)
    u[i] = 0.0059 + 0.0046 * E * cos(M) -
      0.0182 * cos(M_) + 0.0004 * cos(2 * M_) -
      0.0005 * cos(M + M_)

    rho[i] = 1.2848 + u
    sigma[i] = 0.7403 - u

    penumbraMagn[i] = (1.5573 + u - abs(gamma[i])) / 0.545
    umbraMagn[i] = (1.0128 - u - abs(gamma[i])) / 0.545

    #Duration at umbra
    p = 1.0128 - u
    t = 0.4678 - u
    n = 0.5458 + 0.04 * cos(M_)

    semiDurationPartialumbra[i] = (60/n) * suppressWarnings({sqrt(p^2 - gamma[i]^2)})
    semiDurationTotalumbra[i] = (60/n) * suppressWarnings({sqrt(t^2 - gamma[i]^2)})

    h = 1.5573 + u
    semiDurationPartialpenumbra[i] = (60/n) * suppressWarnings({sqrt(h^2 - gamma[i]^2)})
  }

  return(list(juliano, predDate, isEclipse, nearNode, gamma, u,
              rho, sigma,
              umbraMagn, penumbraMagn, semiDurationPartialumbra,
              semiDurationTotalumbra, semiDurationPartialpenumbra))

}
