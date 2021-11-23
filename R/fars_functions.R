#' Read a CSV file given a file name
#'
#' @param filename The name of the csv file to be read
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' fars_read("test_file.csv")
#' \dontrun{
#' fars_read(test_file.csv)
#' }
fars_read <- function(filename) {
  browser()
  if(!file.exists(glue::glue("R/{filename}")))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(glue::glue("R/{filename}"), progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create an accident file name for a specific year
#'
#' @param year the year of interest
#'
#' @return a string of the newly created file name
#' @export
#'
#' @examples
#' make_filename("2016")
#'
#' \dontrun{
#' make_filename()
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}

#' Read accident data for a given set of years
#'
#' @param years A vector of years
#'
#' @return a dataframe of two variables
#' @export
#'
#' @import magrittr
#'
#' @examples
#' fars_read_years(c("2013","2014"))
#'
#' \dontrun{
#' fars_read_years("2013","2014")
#' }
fars_read_years <- function(years) {
  #browser()
  lapply(years, function(year) {
    #browser()
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Accidents summarised by year
#'
#' @param years A vector of years of interest for which you want monthly
#' aggregates of accidents
#'
#' @return A tibble giving number of accidents per month for a particular year
#' @export
#'
#' @importFrom dplyr n
#'
#' @examples
#' fars_summarize_years(c(2013,2014))
#'
#' \dontrun{
#' fars_summarize_years(2013,2014)
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map prevalence of accidents for a given state
#'
#' @param state.num Number representing a US state
#' @param year Year of interest
#'
#' @return a map object
#' @export
#'
#' @examples
#' fars_map_state(1,2013)
#'
#' \dontrun{
#' fars_map_state(1,c(2013,2014))
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
