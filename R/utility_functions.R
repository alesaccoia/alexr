

#' Converts a number as Ms from epoch to POSIXct
#'
#' @param val The number of ms since Epoch
#' @param tz The timezone, default CET
#'
#' @return A POSIXct object with the date/time representation
#' @export
#'
#' @examples
#'
#' dateSinceEpoch(1231908401)
#'
dateSinceEpoch <- function(val,tz="CET") {
  as.POSIXct(val, origin="1970-01-01",tz)
}

#' Returns the number of significant decimal digits
#'
#' @param x the decimal number inputtes
#'
#' @return The number of significant digits
#' @export
#'
#' @examples
#' table(sapply(c(0.2,0.33,0.1234453), significantDecimalDigits))
#'
significantDecimalDigits<-function(x) {
  min(which( x*10^(0:20)==floor(x*10^(0:20)) )) - 1
}


#' Returns the nth decimal digit from the number x
#'
#' @param x input number
#' @param position can be between 0 and ~ +20
#'
#' @return the number at the decimal position
#' @export
#'
#' @examples
#'
#' x <- 1234.1234567
#' floor((x * 10^5) %% 10)
#'
nthDecimalDigit <- function(x, position) {
  floor((x * 10^position) %% 10)
}

#' Converts an angle in radians to degrees
#'
#' @param rad The angle in radians
#'
#' @return The value of the angle in degrees
#' @export rad2deg
#'
rad2deg <- function(rad) {
  (rad * 180) / (pi)
}

#' Converts an angle in degrees to radians
#'
#' @param rad The angle in radians
#'
#' @return The value of the angle in degrees
#' @export deg2rad
#'
deg2rad <- function(deg) {
  (deg * pi) / (180)
}

#' Computes how many degrees of longitude is a distance at a latitude
#'
#' @param distance_km The distance in Km
#' @param atLatitude The degrees of latitude
#'
#' @return The angle of longitude in degrees
#' @export kmToLongitudeDegrees
#'
kmToLongitudeDegrees <- function(distance_km, atLatitude = 0) {
  distance_km / (111.321 * cos(deg2rad(atLatitude)))
}

#' returns windows aroung increasing series by group indices
#'
#' @param df Input dataframe, should contain a grouping column called groupingVariable
#' @param windowLenght Must be an odd number
#' @param startIndices Should contain the indices as start from the grouped series
#' @param groupingVariable The name of the column that will be used for the inned join
#'
#' @return A new data frame containing just the matching columns
#' @export extractWindowAroundIndices
#'
#' @examples
#' A <- data.frame(name = "A", valNumeric = c(1:9), valCategorical = c("YES","NO","MAYBE"))
#' B <- data.frame(name = "B", valNumeric = c(5:100), valCategorical = c('YES','NO'))
#' C <- data.frame(name = "C", valNumeric = c(5:7), valCategorical = c('green','green','blue'))
#' df <- rbind(A,B,C)
#' startIndices <- data.frame(name = c("A","B","C"), index = c(3,10,2))
#' ggplot(extractWindowAroundIndices(df, 5, startIndices, "name"), aes(position, valNumeric, color = name)) + geom_line()
extractWindowAroundIndices <- function(df, windowLength, startIndices, groupingVariable) {
  half_window <- ((windowLength - 1) / 2)

  df %>% inner_join(startIndices,by=groupingVariable) %>%
    dplyr::group_by(.dots = groupingVariable) %>%
    dplyr::mutate(original_index = 1:dplyr::n()) %>%
    filter((original_index >= index - half_window) & (original_index <= index + half_window)) %>%
    mutate(position = original_index - index) %>%
    select(-index)
}

#' Returns a list with the proportion of missing values per column
#'
#' @param df The input data frame
#'
#' @export checkNA
#'
#' @examples
#'
#' hist(checkNA(iris),
#'   main = paste ("Histogram of the proportion of missing values"),
#'   xlab = "Proportion of missing values",
#'   ylab = "Count of variables",
#'   labels = TRUE)
#'
#'
checkNA <- function(df) {
  sapply(X = df, FUN = function(x){sum(is.na(x))/nrow(df)})
}

#' Geometric mean of a vector
#'
#' @param x the vector
#' @param na.rm ignores NAs
#'
#' @export geometric_mean
#'
#' @examples
#'
#' geometric_mean(c(1,1,3,6,1,1,2,1))
#'
geometric_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


#' Extrapolate a number using pro
#'
#' @param population_size The true size of the population
#' @param percentage_sampled Percentage of the population sampled
#' @param number_of_positives How many positives have been found
#' @param conf_int Can be any of the classical two-sided values: 0.8, 0.9, 0.95
#'
#' @return Returns a list containing:
#'         - proportion of the sample that was marked positive
#'         - confidence interval around number_of_positives
#'         - confidence interfal around the true population
#' @export extrapolate_with_confidence
#'
#' @examples
#'
#'
extrapolate_with_confidence <- function(population_size, percentage_sampled, number_of_positives, conf_int) {
  number_of_samples <- population_size * percentage_sampled
  proportion_of_sample <- number_of_positive / number_of_samples
  standard_error = sqrt((proportion_of_sample * (1 - proportion_of_sample))/number_of_samples)
  left_interval <- (1 - conf_int) / 2
  confidence_int <- c(qt(left_interval, number_of_samples-1) * standard_error, abs(qt(left_interval, number_of_samples-1)) * standard_error)
  confint_of_proportion_sample <- proportion_of_sample + confidence_int
  samples_with_intervals <- confint_of_proportion_sample * number_of_samples
  extrapolated_sample <- samples_with_intervals  / percentage_sampled
  list(proportion_of_sample, samples_with_intervals, extrapolated_sample)
}

#' Returns the start of the month of the date x
#'
#' @param x
#'
#' @return
#' @export start_of_month
#'
#' @examples
#'
#' end_of_last_month <- som(Sys.Date()) - 1
#' start_of_lasth_month <- som(som(Sys.Date()) - 1)
#'
start_of_month <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}


#' Returns a structure containing the end of each month
#'
#' @param initial_day
#' @param end_day
#'
#' @return
#' @export ends_of_months
#'
ends_of_months <- function(initial_day = as.Date("2018-11-01"), end_day = Sys.Date(), format_string = "%b %Y") {
  dates <- NULL
  names <- NULL
  while (end_day >= initial_day) {
    names <- c(names, as.character(format(end_day, format_string)))
    start_day <- start_of_month(end_day)
    dates <- c(dates, as.character(end_day))
    end_day <- start_day - 1
  }
  return(structure(as.Date(dates), .Names = names))
}

#' Title
#'
#' @param initial_day
#' @param end_day
#'
#' @return
#' @export beginning_and_end_of_months
#'
#' @examples
beginning_and_end_of_months <- function(initial_day = as.Date("2018-11-01"), end_day = Sys.Date(), format_string = "%b %Y") {
  dates <- list()
  names <- NULL
  while (end_day >= initial_day) {
    names <- c(names, as.character(format(end_day, format_string)))
    start_day <- start_of_month(end_day)
    current_bounds <- list(begin = as.character(start_day), end = as.character(end_day))
    dates <- append(dates, list(current_bounds))
    end_day <- start_day - 1
  }
  return(structure(dates, .Names = names))
}

#' Coordinates of the bounding box of a static gmap
#'
#' The zoom level must be bigger than 8
#'
#' @param lat Center latitude
#' @param lng Center longitude
#' @param zoom Zoom Level
#' @param sx Width of the map
#' @param sy Height of the map
#'
#' @return Vector containing the bottom/left/top/right coordinates
#' @export get_google_static_map_bounds
#'
#' @examples
#'
#'
#'
get_google_static_map_bounds <- function(lat, lng, zoom, sx, sy) {
  sz = 256 * 2 ** zoom
  #resolution in degrees per pixel
  res_lat = cos(lat * pi / 180.) * 360. / sz
  res_lng = 360./sz
  d_lat = res_lat * sy / 2
  d_lng = res_lng * sx / 2
  cbind(bottom = lat-d_lat, left = lng-d_lng, top = lat+d_lat, right = lng+d_lng)
}


#' Funcion like seq that includes the outer bound
#'
#' @param from Start number
#' @param to End number
#' @param by Subdivision
#'
#' @return Vector like in base::seq
#' @export seqlast
#'
seqlast <- function (from, to, by)
{
  vec <- do.call(what = seq, args = list(from, to, by))
  if ( tail(vec, 1) != to ) {
    return(c(vec, to))
  } else {
    return(vec)
  }
}

#' Funcion like seq that includes the outer bound or the number after it
#'
#' @param from Start number
#' @param to End number
#' @param by Subdivision
#'
#' @return Vector like in base::seq
#' @export seqinclude
#'
seqinclude <- function (from, to, by) {
  vec <- do.call(what = seq, args = list(from, to, by))
  if ( tail(vec, 1) != to ) {
    return(c(vec, tail(vec, 1) + by))
  } else {
    return(vec)
  }
}


#' Removes a package from memory
#'
#' @param pkg
#' @param character.only
#'
#' @export detach_package
#'
detach_package <- function(pkg, character.only = FALSE) {
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

#' Reads a directory of files with a given extension and creates a dataset out of it
#'
#' @param input_path Input direcotry
#' @param input_pattern Input pattern to search for files (default *.csv)
#' @param recursive Reads recursively
#' @param csv The expected separator (default ',')
#' @param header Files have a header or not (default T)
#' @param add_file_column Adds a column with the file name
#'
#' @return a new data.frame that contains all columns
#' @export read_csv_dir
#' @import data.table
#'
#' @examples
#' df <- read_csv_dir('/my/path/')
#'
read_csv_dir <- function(input_path, input_pattern = "*.csv", recursive = F, sep = ',', header = T, add_file_column = T, verbose = T) {
  temp <- list.files(path = input_path, pattern = input_pattern, recursive = recursive)
  myfiles <- lapply(temp, function(x) {
    if (verbose) {
      message(sprintf("Reading file %s", paste0(input_path, x)))
    }
    tbl <- fread(paste0(input_path, x), stringsAsFactors = F, sep = sep, header = header)
    if (add_file_column) {
      tbl$filename <- x
    }
    return(tbl)
  })
  df <- do.call("rbind", myfiles)
}


#' Reads a directory of files with a given extension and creates a dataset out of it
#'
#' @param input_path Input direcotry
#' @param input_pattern Input pattern to search for files (default *.csv)
#' @param colnames Names of the columns
#'
#' @return a new data.frame that contains all columns
#' @export read_csv_dir_colnames
#'
#' @examples
#' df <- read_csv_dir('/my/path/', colnames = c('id','value'))
#'
read_csv_dir_colnames <- function(input_path, colnames, input_pattern = "*.csv") {
  temp <- list.files(path = input_path, pattern = input_pattern)
  myfiles <- lapply(temp, function(x) { read.csv(paste0(input_path, x), stringsAsFactors = F, col.names = colnames, row.names=NULL) })
  df <- do.call("rbind", myfiles)
}

#' Changes the name of a column in a data.frame
#'
#' @param the_dataframe dataset to change
#' @param oldname the old column name
#' @param newname the new column name
#'
#' @export changeColName
#'
changeColName <- function(the_dataframe, oldname, newname) {
  colnames(the_dataframe)[colnames(the_dataframe) == oldname] <- newname
}

#' Installs and loads a library
#'
#' @param libname the lobrary name
#'
#' @export install_library
#'
install_library <- function(libname) {
  if (!require(as.character(libname),character.only=TRUE)) {
    install.packages(as.character(libname))
  }
  library(as.character(libname),character.only=TRUE)
}

#' Returns the current OS
#'
#' @return the current OS (win, mac,unix)
#' @export get_os
#'
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

#' Generates a GUID
#'
#' @return a GUID
#' @export generate_guid
#'
generate_guid <- function() {
  platform <- get_os()
  if (platform == 'mac') {
    my_uuid <- system("uuidgen", intern=T)
  } else if (platform == "unix") {
    my_uuid <- system("uuid",intern=T)
  }
  return(my_uuid)
}

#' Reads a file as string
#'
#' @param fileName the name of the file
#' @param convertNlToSpaces Converts newlines and tabs to spaces (useful for SQL queries)
#'
#' @return the content of the file as string
#' @export read_file_as_text
#'
read_file_as_text <- function(fileName, convertNlToSpaces = T) {
  str <- readChar(fileName, file.info(fileName)$size)
  if (convertNlToSpaces) {
    str <- gsub('\n',' ',str)
    str <- gsub('\t',' ',str)
  }
}

#' Returns the nan elements for a data frame
#'
#' @param x the data frame
#'
#' @return a boolean matrix
#' @export is.nan.data.frame
#'
#' @examples
#' df[is.nan(df)] <- 0
#'
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

#' Returns a data frame with the porportions per row
#'
#' @param df the data frame
#' @param nan.value the value to use for divisions by zero
#'
#' @return a data frame
#' @export propByRow
#'
#' @examples
#' df <- popByRow(df)
#'
propByRow <- function(df, nan.value = 0) {
  df <- df / rowSums(df)
  df[is.nan(df)] <- nan.value
  return(df)
}

#' returns wether the current date is a weekend day or not
#'
#' @param ds the input date
#'
#' @return T or F if it's weekend or not
#' @export is_weekend
#'
is_weekend <-function(ds) {
  return(ifelse((wday(ds) == 1) | (wday(ds) == 7), T, F))
}

#' Expands a data frame by merging with a MxN permutation data.frame filled with 0 or other
#'
#' @param df input data frame
#' @param namex variable name for rows
#' @param x vector of values of rows
#' @param namey variable name for columns
#' @param y vector of values of columns
#' @param namez name of the merged field, usually a count
#' @param z.val value to fill the merged column with
#'
#' @return a filled data frame
#' @export expandDataFrame
#'
#' @examples
#' heatmap_pivot <- expandDataFrame(visit_pivot, 'hour', c(0:23), 'wday', c(1:7), 'count')
#' heatmap_visits <- reshape(heatmap_pivot,  idvar="hour",timevar="wday",direction="wide")
#' heatmap_visits$hour <- NULL
#'
expandDataFrame <-function(df, namex, x, namey, y, namez, z.val = 0) {
  grid <- expand.grid( x, y, z.val )
  colnames(grid)[1] <- namex
  colnames(grid)[2] <- namey
  colnames(grid)[3] <- namez
  merged <- merge(grid, df, by = c(namex, namey), all.x = T)
  merged$mytempvar <- ifelse(is.na(merged[,paste0(namez, '.y')]), merged[,paste0(namez, '.x')], merged[,paste0(namez, '.y')])
  colnames(merged)[ncol(merged)] <- namez
  merged[,paste0(namez, '.y')] <- NULL
  merged[,paste0(namez, '.x')] <- NULL
  merged
}


#' Expands a data frame by merging with a data.frame of length M filled with 0 or other
#'
#' @param df input data frame
#' @param namex variable name for rows
#' @param x vector of values of rows
#' @param namez name of the merged field, usually a count
#' @param z.val value to fill the merged column with
#'
#' @return a filled data frame
#' @export expandDataFrame1d
#'
expandDataFrame1d <-function(df, namex, x, namez, z.val = 0) {
  grid <- expand.grid( x, z.val )
  colnames(grid)[1] <- namex
  colnames(grid)[2] <- namez
  merged <- merge(grid, df, by = c(namex), all.x = T)
  merged$mytempvar <- ifelse(is.na(merged[,paste0(namez, '.y')]), merged[,paste0(namez, '.x')], merged[,paste0(namez, '.y')])
  colnames(merged)[ncol(merged)] <- namez
  merged[,paste0(namez, '.y')] <- NULL
  merged[,paste0(namez, '.x')] <- NULL
  merged
}

#' Samples a vector from a uniform distribution, the sum adds to the mean
#'
#' @param N the number of samples
#' @param M the mean of the samples
#' @param sd the standard deviation of the samples
#'
#' @return a vector of N numbers adding to M (can be negative)
#' @export vectorToUniformDist
#'
#' @examples
#' vectorToUniformDist(6,10, sd = 1) / 10
#'
vectorToUniformDist <- function(N, M, sd = 1) {
  vec <- rnorm(N, M/N, sd)
  vec / sum(vec) * M
}
