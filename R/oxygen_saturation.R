#' Adjust dissolved oxygen for depth and compute oxygen saturation
#'
#' This function corrects measured dissolved oxygen concentrations for
#' hydrostatic pressure at depth in a freshwater or marine system
#' and calculates the percentage of oxygen saturation.
#'
#' @param temp Numeric vector. Water temperature in degrees Celsius (°C).
#' @param oxy Numeric vector. Measured dissolved oxygen concentration in mg/L.
#' @param depth Numeric vector. Depth of measurement in meters (m).
#' @param altitude Numeric scalar. Altitude of the lake or site above sea level in meters (m).
#'
#' @details
#' The calculation uses Henry's law and Weiss (1970) equations for oxygen solubility
#' as a function of temperature and salinity (here assumed salinity = 0 for freshwater).
#' Hydrostatic pressure at depth is estimated from water column height and added to
#' barometric pressure at the lake’s altitude. Oxygen solubility at this total pressure
#' is compared with measured concentrations to estimate oxygen saturation.
#'
#' @return A data frame with two numeric vectors:
#' \describe{
#'   \item{oxy.adj}{Oxygen concentration adjusted for hydrostatic pressure (mg/L).}
#'   \item{oxy.sat}{Oxygen saturation as percentage (%).}
#' }
#'
#' @references
#' Weiss, R. F. (1970). The solubility of nitrogen, oxygen and argon in water and
#' seawater. *Deep Sea Research and Oceanographic Abstracts*, 17(4), 721–735.
#'
#' @examples
#' temp <- c(20, 25)
#' oxy <- c(8, 7)
#' depth <- c(5, 10)
#' altitude <- 735
#' calc_oxygen(temp, oxy, depth, altitude)
#'
#' @export
oxygen_saturation <- function(temp, oxy, depth, altitude) {

  # Convert altitude (m) to barometric pressure (hPa) using empirical formula
  P0 <- 1013.25  # hPa at sea level
  Patm <- P0 * (1 - 2.25577e-5 * altitude)^5.2559

  # Hydrostatic pressure from depth (hPa)
  rho <- 1000  # kg/m3 (freshwater density)
  g <- 9.80665 # m/s2
  Pdepth <- (rho * g * depth) / 100  # Pa -> hPa (1 hPa = 100 Pa)

  # Total pressure at depth (hPa)
  Ptotal <- Patm + Pdepth

  # Oxygen solubility at STP (mg/L) using Weiss (1970) for freshwater (salinity = 0)
  TK <- temp + 273.15  # Kelvin
  lnC <- -139.34411 + (1.575701e5 / TK) - (6.642308e7 / TK^2) + (1.243800e10 / TK^3) - (8.621949e11 / TK^4)
  C0 <- exp(lnC)  # mg/L at 1 atm

  # Adjust for actual pressure
  Csat <- C0 * (Ptotal / 1013.25)

  # Oxygen saturation (%)
  oxy.sat <- (oxy / Csat) * 100

  # Adjusted oxygen (mg/L) accounting for higher solubility at depth
  oxy.adj <- oxy * (Ptotal / Patm)

  # Return as data frame
  return(data.frame(oxy.adj = oxy.adj, oxy.sat = oxy.sat))
}

