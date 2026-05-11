#' Query NASA JPL Horizons API (text output)
#'
#' Retrieve ephemeris for a given target (Sun, Moon...) from a site on Earth using the
#' Horizons API, returning the raw text output.
#' Details about parameters: https://ssd-api.jpl.nasa.gov/doc/horizons.html
#'
#' @param target Horizons target ID (e.g. 301 for the Moon, 10 for the Sun)
#' @param lon Longitude in decimal degrees (east positive)
#' @param lat Latitude in decimal degrees (north positive)
#' @param elev Altitude above ITRF93 (or WGS-84 GPS) ellipsoid (m)
#' @param start_time Start time, as POSIXct with the local time zone
#' @param stop_time Stop time, as POSIXct
#' @param step Step size (e.g. "1h", "1d")
#' @param quantities Character vector of Horizons quantity (variables) codes (https://ssd.jpl.nasa.gov/horizons/manual.html#obsquan). Default to `c("4", "8", "9")`.
#' @param refracted Logical, whether to request refracted apparent coordinates of the target, default to TRUE
#'
#' @return Character string containing the Horizons text output
#' @importFrom httr GET stop_for_status content
#' @importFrom utils read.csv
#' @export
horizons_query <- function(
    target = "301",
    lon,
    lat,
    elev = 0,
    start_time,
    stop_time,
    step = "1d",
    quantities = c("4", "8", "9"),
    refracted = TRUE
) {
  elev <- elev / 1000
  base_url <- "https://ssd.jpl.nasa.gov/api/horizons.api"

  params <- list(
    format       = "text",
    COMMAND      = sprintf("'%s'", target),
    CENTER       = "'coord@399'",
    COORD_TYPE   = "'GEODETIC'",
    SITE_COORD   = sprintf(
      "'%.5f,%.5f,%.0f'",
      lon, lat, elev
    ),
    START_TIME   = format(start_time, "'%Y-%m-%d %H:%M'"),
    STOP_TIME    = format(stop_time, "'%Y-%m-%d %H:%M'"),
    TIME_ZONE    = gsub('^(.{4})(.*)$', '\\1:\\2', format(start_time, format = "'%z'")),
    STEP_SIZE    = sprintf("'%s'", step),
    QUANTITIES   = sprintf("'%s'", paste(quantities, collapse = ",")),
    APPARENT     = if (refracted) "'REFRACTED'" else "'AIRLESS'",
    RANGE_UNITS  = "'KM'",
    CSV_FORMAT   = "'YES'",
    OBJ_DATA     = "'NO'"
  )

  response <- GET(base_url, query = params)

  stop_for_status(response)

  txt <- content(response, as = "text", encoding = "UTF-8")

  return(txt)
}

#' Parse Horizons ephemeris output (text format)
#'
#' Extract and parse the ephemeris block from a Horizons API text response.
#'
#' @param txt Character vector or single string returned by Horizons API
#'
#' @return A data.frame with ephemeris data
horizons_parse_ephemeris <- function(txt) {

  # Accept single string or character vector
  if (length(txt) == 1L) {
    txt <- unlist(strsplit(txt, "\n", fixed = TRUE))
  }

  soe <- which(trimws(txt) == "$$SOE")
  eoe <- which(trimws(txt) == "$$EOE")

  if (length(soe) != 1L || length(eoe) != 1L || eoe <= soe) {
    stop("Could not locate a valid $$SOE / $$EOE block in Horizons output")
  }

  data_lines <- c(txt[(soe - 2)],txt[(soe + 1):(eoe - 1)]) %>% gsub(",$", "", .) %>% gsub("^\\s","",.)

  if (length(data_lines) == 0) {
    stop("Ephemeris block is empty")
  }

  # Read CSV safely
  df <- read.csv(
    text = paste(data_lines, collapse = "\n"),
    header=TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    strip.white = TRUE,
    na.strings = "n.a."
  )


  # some data management
  colnames(df)[2:3] <- c("solar_presence","lunar_presence")

  return(df)
}


#' Retrieve and parse Horizons ephemeris
#'
#' @inheritParams horizons_query
#'
#' @return data.frame with parsed Horizons ephemeris and POSIXct time
horizons_get_ephemeris <- function(...) {

  # retrieve time zone
  args_list <- list(...)
  start_time <- args_list$start_time
  tz <- attr(start_time, "tzone")

  # horizons query
  txt <- horizons_query(...)

  df <- horizons_parse_ephemeris(txt)

  # Identify date column
  date_col <- grep("^Date", names(df), value = TRUE)

  if (length(date_col) != 1L) {
    stop("Could not uniquely identify Horizons date column")
  }

  time <- ymd_hms(df[[date_col]], truncated = 1, tz=tz)

  if (!any(is.na(time))) {
    df$time <- time
    return(df)
  }

  stop("Could not parse Horizons date column")
}


#' Convert Horizons magnitude to direct illuminance
#'
#' @param APmag Apparent visual magnitude from Horizons
#' @param mag_ex Atmospheric extinction (magnitudes)
#'
#' @return Illuminance in lux (NA when the target is above the horizon)
illuminance_from_horizons <- function(APmag, mag_ex = 0) {

  mag_obs <- APmag + mag_ex

  E0 <- 2.54e-6  # lux for mag 0 source

  E <- E0 * 10^(-0.4 * mag_obs)

  return(E)
}
