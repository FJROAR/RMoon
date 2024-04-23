#' @title SolarEclipses
#'
#' @description Determination of the posibility of a Solar Eclipse when happen
#' a full moon phase according to Chapter 54 of Astronomical Algorithms
#'
#' @param day Date in format: "yyyy-mm-dd"
#'
#' @return List of 15 vectors: (1) juliano or Julian day of the following full moon and
#' when the eclipse would happen (2) predDate or the date at format yyyy-mm-dd hh-mm -ss
#'  (3) isEclipse if the Eclipse's condition is accomplished (4) nearNode or
#' Node where the eclipse takes place (5) gamma or the least distance from the axis of the Moon's shadow to the center of
#' the Earth in units of equatorial radius of the Earth (6) u or radius of the Moon's
#' umbral cone in units of the Earth's equatorial radius (7) radPenumbralCone or radius of
#' the penumbral cone (8) isVisible or visibility condition (9) isCentral or centrality condition
#' (10) isPartial or high likely of partiallity condition (11) isAnnularNoCentral or annular condition
#' when a eclipse is no central (12) isCentralTotal or central + total condition (13) isCentralAnnular
#' or central + annular condition and (14) isAnnularTotal or annular + total condition
#' (15) magnPartial or greatest magnitude in case of a partial solar eclipse
#'
#'
#'
#' @examples
#'
#' library(RMoon)
#'
#' SolarEclipseElements <- SolarEclipses("1977-02-15")
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

SolarEclipses <- function(day){

  day = as.Date(day)

  juliano <- vector("numeric", length(day))
  predDate <- vector("character", length(day))
  isEclipse <- vector("character", length(day))
  nearNode <- vector("character", length(day))

  gamma <- vector("numeric", length(day))
  u <- vector("numeric", length(day))

  radPenumbralCone <- vector("numeric", length(day))
  isVisible <- vector("character", length(day))
  isCentral <- vector("character", length(day))
  isPartial <- vector("character", length(day))
  isAnnularNoCentral <- vector("character", length(day))

  isCentralTotal <- vector("character", length(day))
  isCentralAnnular <- vector("character", length(day))
  isAnnularTotal <- vector("character", length(day))

  magnPartial <- vector("numeric", length(day))

  for (i in c(1: length(day))){

    anio = as.numeric(substr(day[i], 1, 4))
    first_day <- as.Date(paste0(anio, "-01-01"))
    past_days <- as.numeric(day[i] - first_day) + 1

    if ((anio %% 4 == 0 && anio %% 100 != 0) || anio %% 400 == 0) {

      partyear = round(past_days / 366 , 2)

    } else {

      partyear = round(past_days / 365 , 2)

    }

    #New moon determination
    k = floor((anio + partyear - 2000) * 12.3685)

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
    F1 = (F_ * (180 / pi) - 0.02665 * sin(Om)) * pi / 180

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


    A1 = (299.77 + 0.107408 * k - 0.009173 * Tcent^2) * pi / 180

    correction = -0.4075 * sin(M_) + 0.1721 * E * sin(M) +
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

    radPenumbralCone[i] = u[i] + 0.5461

    isVisible[i] <- ifelse(abs(gamma[i]) > 1.5433 + u[i], "No visible", "Visible")
    isCentral[i]  <- ifelse(abs(gamma[i]) <= 1.5433 + u[i] &
                              abs(gamma[i]) > 0.9972, "No central", "Central")

    isPartial[i]  <- ifelse(abs(gamma[i]) > 0.9972 &
                              abs(gamma[i]) <= 1.5433, "Likely Partial", "No partial")

    isAnnularNoCentral[i]  <- ifelse(abs(gamma[i]) > 0.9972 &
                                     abs(gamma[i]) <= 0.9972 + abs(u[i]), "Annular no central",
                                   "It is not non-central & annular")

    isCentralTotal[i]  <- ifelse(isCentral[i] == "Central" &
                                   u[i] < 0, "Central & total",
                                 "It is not central & total")

    isCentralAnnular[i]  <- ifelse(isCentral[i] == "Central" &
                                   u[i] >= 0.0047,
                                  "Central & annular",
                                 "It is not central & annular")

    omega <- 0.00464 * suppressWarnings(sqrt(1 - gamma[i]^2))
    isAnnularTotal[i]  <- ifelse(isCentral[i] == "Central" &
                                   u[i] >= 0 &
                                   u[i] < omega,
                                   "It is central & annular & total",
                                 "It is not annular")
    isAnnularTotal[i]  <- ifelse(isCentral[i] == "Central" &
                                   u[i] >= omega &
                                   u[i] < 0.0047,
                                 "It is central & annular",
                                 isAnnularTotal[i])

    magnPartial[i] <- (1.5433 + u[i] - abs(gamma[i])) / (0.5461 + 2 * u[i])

  }

  return(list(juliano, predDate, isEclipse, nearNode, gamma, u, radPenumbralCone,
              isVisible, isCentral, isPartial, isAnnularNoCentral,
              isCentralTotal, isCentralAnnular, isAnnularTotal, magnPartial
              ))

}
