#' Interpolate Missing Hourly METAR Data
#'
#' This function fills missing time-stamped rows in a METAR dataset and linearly interpolates numeric weather variables
#' (e.g., wind speed, temperature, pressure) based on adjacent observations. It is useful for reconstructing hourly
#' time series when some observations are missing.
#'
#' @param df A `data.frame` containing METAR data. It must include a column named `METAR_Date` (POSIXct or coercible)
#'          and numeric columns to interpolate (e.g., Wind_speed, Temperature).
#' @param time_interval A character string specifying the time step for interpolation (default is `"1 hour"`).
#'                      Passed to `seq.POSIXt(by = time_interval)`.
#'
#' @return A `data.frame` with missing timestamps filled and numeric variables interpolated.
#'         New rows added by interpolation are flagged with `"This data were interpolated"` in the `Remark` column.
#'
#' @importFrom dplyr mutate arrange left_join across all_of
#' @importFrom tidyr replace_na
#' @importFrom lubridate ymd_hms
#' @importFrom zoo na.approx
#'
#' @examples
#' \dontrun{
#' Metar <- metar_get_historical(
#' airport = "DFOO",
#' start_date = "2018-11-01",
#' end_date = "2018-11-30",
#' from = "ogimet") %>%
#' metar_decode()
#' interpolated_data <- interpolate_metar(Metar)
#' # Check interpolated rows
#' interpolated_data %>% filter(Remark == "These data were interpolated")
#' }
#'
#' @export
interpolate_metar <- function(df, time_interval = "1 hour") {

  # Ensure date column is POSIXct and dataframe is in date order
  df <- df %>%
    mutate(METAR_Date = as.POSIXct(METAR_Date, tz = "UTC")) %>%
    arrange(METAR_Date)

  # Create a complete time sequence
  all_times <- data.frame(METAR_Date = seq.POSIXt(min(df$METAR_Date, na.rm = TRUE), max(df$METAR_Date, na.rm = TRUE), by = time_interval))

  # Join to ensure all times are present
  df_complete <- all_times %>%
    left_join(df, by = "METAR_Date")

  # Variables to interpolate
  vars_to_interp <- c("Wind_speed", "Temperature", "Dew_point", "Pressure",
                      "Longitude", "Latitude", "Elevation")

  # Interpolate numeric variables
  df_complete_interp <- df_complete %>%
    arrange(METAR_Date) %>%
    mutate(across(all_of(vars_to_interp), ~ zoo::na.approx(., x = METAR_Date, na.rm = FALSE)))

  # Add flag for interpolated rows
  df_complete_interp <- df_complete_interp %>%
    mutate(Remark = ifelse(is.na(Airport_ICAO), "This data were interpolated", Remark))

  return(df_complete_interp)
}

