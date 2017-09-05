#' Read in FARS data
#'
#' The \code{fars_read()} function uses the \code{read_csv()} function from
#' the \code{readr} package to read in a data set and converts it to the
#' \code{tdl_df} format known from the \code{dplyr} and \code{tibble} packages.
#' The function has been developed to read in FARS data but works with every data format
#' that is accepted by \code{readr::read_csv()}. It gives a warning
#' if the specified file name does not exist.
#'
#' @param filename A character string with the name of the file you want to read in
#'
#' @return This function returns the data from the input file in \code{tbl_df} class
#'         (see R documentation for the \code{tibble} package).
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create standardised file names
#'
#' The \code{make_filename()} function generates standardised file names for FARS
#' data sets based on the year you pass to it as argument.
#'
#' @param year The year of the FARS report in any format that can be coerced to an integer
#'
#' @return This function returns a character string to be used as file name. The string has
#'         the following structure where YYYY is replaced with what has been passed as the
#'         \code{year} argument: \code{accident_YYYY.csv.bz2}
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read in FARS data for a specified range of years
#'
#' The \code{fars_read_years()} function is used to read in FARS data for a specified range of years.
#'
#' @param years A year or a vector of years (in any class that can be coerced to numeric)
#'              to specify which FARS reports are to be read in
#'
#' @return This function returns a list of FARS data where every list entry contains the data
#'         for one of the years specified in form of a \code{tbl_df}. The data frame has one
#'         line for every data point in the respective FARS data set and indicates the month
#'         and the year for this data point.\cr
#'         It produces an error warning if an invalid year is given as input.
#'
#'@details The function internally uses the \code{\link{make_filename}} function to create standardised file
#'         names from the \code{years} input. It then reads in CSV files from the working directory that
#'         correspond to this standardised file name and uses \code{dplyr} functions to reorganise the
#'         data.
#'
#'@examples
#'\dontrun{fars_read_years(c(2014, 2015)) #Read in FARS data for 2014 and 2015}
#'
#'@importFrom dplyr mutate select
#'
#'@importFrom magrittr "%>%"
#'
#'@export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~ year) %>%
                                dplyr::select_("MONTH", "year")
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#'Read in and summarise FARS data for a specified range of years
#'
#'The \code{fars_summarize_years} function is used to read in FARS data from a specified range of years
#'and return the monthly number of accident registered in FARS.
#'
#'@inheritParams fars_read_years
#'
#'@return This function returns a \code{tbl_df} object with one column for every year analysed and
#'        twelve rows, one for each month. The columns contain the sum of accidents reported to
#'        FARS in each month of the given year.
#'
#'@details The function internally uses the \code{\link{fars_read_years}} function to read in and
#'         extract the data and then uses \code{dplyr} and \code{tidyr} functions to summarise and
#'         reformat the data.\cr
#'         Like \code{\link{fars_read_years}} it reads the data from your working directory
#'         and it will produce a warning if an invalid year is given as input. Please be aware that
#'         the standard naming convention for FARS data has to be used (cf. \code{\link{make_filename}}).
#'
#'@examples
#'\dontrun{fars_summarize_years(2013:2015) #Summarize FARS data for the years 2013, 2014 and 2015}
#'
#'@importFrom dplyr bind_rows group_by summarize
#'
#'@importFrom tidyr spread
#'
#'@importFrom magrittr "%>%"
#'
#'@export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~year, ~MONTH) %>%
                dplyr::summarize_(n = ~ n()) %>%
                tidyr::spread_(key_col = 'year', value_col = 'n')
}

#'Plot FARS accident data on maps of US states
#'
#'This function makes use of the geographic data contained in FARS accident data sets to plot
#'accidents on maps of US states.
#'
#'@param state.num The numeric code (as numeric or integer) of the US state to be analysed. Numeric codes can be found e.g.
#'                 \href{https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code}{on Wikipedia}.
#'
#'@param year The year (as numeric or integer) from which the data is to be plotted.
#'
#'@return The function returns a plot with a graphical representation of the selected US state with the
#'        accidents of the indicated year plotted as dots on the map according to their geographic
#'        location. If the data set contains no accidents for the indicated year and state,
#'        the message \code{"no accidents to plot"} is printed to the console and if an invalid
#'        state number is given as input, a warning will be displayed.
#'
#'@details The function internally uses the \code{\link{make_filename}} and \code{\link{fars_read}}
#'         functions to read in the FARS data for the given year and than filters for the given state.
#'         The geographic data is extracted and plotted using the \code{maps} package.
#'
#'@examples
#'\dontrun{fars_map_state(31,2015) #Plot all accidents that happened 2015 in Nebraska.}
#'
#'@importFrom dplyr filter
#'
#'@importFrom maps map
#'
#'@export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
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
