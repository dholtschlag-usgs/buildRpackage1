#' Read file of annual data from US NHTSA Fatality Analysis Reporting System (FARS)
#'
#' This function takes a file name as an arguement, and checks to see of the 
#' file exists in the working directory.  If the does not exist, execution 
#' stops and the error message 'file <filename> does not exist' is returned.  
#' Otherwise, the read_csv function from the readr package reads a compressed 
#' file that contains comma delimited entries to form a data frame.  Progress 
#' on the reading of data is suppressed.  Column names are taken as the first
#' row in the data file.  
#' Using the tbl_df function from the dplyr package, the data frame is 
#' converted to a tibble, which is an S3 class object that provides stricter 
#' checking and better formatting than a traditional data frame.  

#' @param filename  A character string corresponding to the target file

#' @return  A tibble data frame 

#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df

#' @examples
#'   fars_read('accident_2013.csv.bz2')
#'   fars_read('accident_2014.csv.bz2')

#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create a FARS filename for annual data by integrating the generic name with year

#' Converts the specified year, either as numeric value or character string, and
#' returns a character vector that corresponds to the generic filename with
#' the specified year embedded.  

#' @param year  A four digit year to specify time period for data compilation.

#' @return  A character vector is returned that embeds the specified <year> 
#'   into the generic file name for FARS data for the corresponding calendar 

#' @examples
#'   make_filename(2013)
#'   make_filename('2013')

#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read one or more years of FARS data and returns a list of one or more tibble tables

#' This function reads a set of annual FARS data corresponding to the years specified 
#'   in the years vector and returns a list of tibble tables with the MONTH variables
#'   from the data file and a variable year corresponding to each element of the 
#'   input year vector.  Errors occur if the data file is not found.  Warnings are
#'   displayed for invalid years. 

#' @param years  A vector containing one or more entries corresponding to four-
#'    digit codes corresponding to the year.

#' @return  This function returns a list of one or more tibble tables having only
#'    the original MONTH variable and the variable year from the vector years provided
#'    as an imput arguement.
#'    The number of elements in the list corresponds to the length of the years 
#'    vector provided as input

#' @importFrom magrittr %>%
#' @importFrom dplry mutate select
#' @inheritParams make_filename fars_read
#' @export


#' @examples
#'   fars_read_years(2013)
#'   fars_read_years('2013')
#'   fars_read_years(c(2013, 2015))
#'   fars_read_years(c(2013:2015))

fars_read_years <- function(years) {
  lapply(years, function(year) {
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

#' Summarize counts of observations by month for one or more tibble table

#' The fucntion vertically concatenates each element of the list of one
#'    or more tibble tables corresponding to elements of the years vector
#'    and counts the number of accidents by year and MONTH.  The table is
#'    then converted from long to wide format so that each year forms an 
#'    individual column indexed by rows from 1 to 12 corresponding to the 
#'    month number.  

#' @param years  A vector of one or more four-digit years

#' @return  A tibble table of dimension 12 x (1 + length(years))
#' is returned with a summary count of observations. 
#' Column names are MONTH and four-digit year values.
#' All values returned are in integer format.

#' @inheritParams fars_read_years
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @export 

#' @examples
#'  fars_summarize_years(c(2013,2015))
#'  fars_summarize_years(2013:2015)

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Display map for selected state showing locations of observations

#' Retrieves outline of state or area corresponding to state.num,
#'    which must be a valid FIPS code.  (See URL below for valid
#'    codes.)  Error returned for invalid STATE code.  Forms file
#'    name from generic form and year entered.  Attempts to read
#'    file in working directory.  Error results if file not found.
#'    If there are no accident to report, as message is printed, 
#'    otherwise, locations of accidents are plotted by latitude 
#'    and longitude. 

#' @param state.num  is a two-digit integer that corresponds to the
#'    FIPS (Federal Information Processing Standards) State Numeric Code 
#'....see https://www.census.gov/geo/reference/ansi_statetables.html.

#' @param year  is a four-digit year code.

#' @return  A map is displayed with the outline of the state or area
#'    associated with the state.num, and the locations of accidents
#'    in the FARS data file for the corresponding year.

#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @inheritParams make_filename fars_read
#' @export 

#' @examples
#'   fars_map_state(12, 2014)
#'   fars_map_state('13','2013')

fars_map_state    <- function(state.num, year) {
  filename  <- make_filename(year)
  data      <- fars_read(filename)
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