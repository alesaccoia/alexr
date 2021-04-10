#' Does a find nearby places API call iterating over all the points that are in the centers_lonlat.
#' Pay attention because it used 3 SKU (check billing here)
#'
#' @param centers_lonlat The centers of the searches in 50 km radius
#' @param api_key API Key for Google Places
#' @param keyword Keyword, will be URL encoded by this method
#' @param radius_mt Radius, normally 500000
#' @param wait_time Eaiting time between requests, defaults to 0 seconds
#' @param verbose Wether to be true or not
#'
#' @return A lists of all the answers
#' @export nearby_search_at_points
#'
nearby_search_at_points <- function(centers_lonlat, api_key, keyword, radius_mt = 50000, wait_time = 0, verbose  = T) {
  retList <- NULL
  for (i in 1:nrow(centers_lonlat)) {
    has_next_page_token <- F
    nr_page <- 1
    repeat {
      ret_tmp <- google_places(location = c(centers_lonlat[i,2],centers_lonlat[i,1]), radius = radius_mt,
                               keyword = URLencode(keyword, reserved = T),
                               key = api_key,
                               page_token = switch(has_next_page_token, last_token ,NULL))

      if (ret_tmp$status != "OK") {
        if (ret_tmp$status != "ZERO_RESULTS") {
          print(sprintf("Failed with status %s at (%f,%f)",ret_tmp$status,centers_lonlat[i,2],centers_lonlat[i,1]))
        } else {
          print(sprintf("No places found for %s at location %i(%f,%f), pg %i", URLencode(keyword),i, centers_lonlat[i,2], centers_lonlat[i,1], nr_page))
        }
        if (wait_time > 0){
          Sys.sleep(time)
        }
        has_next_page_token <- F
        nr_page <- 1
        break
      }

      has_next_page_token <- ("next_page_token" %in% names(ret_tmp))

      if (verbose) {
        print(sprintf("Found %i places for %s at location %i(%f,%f), pg %i",nrow(ret_tmp$results), URLencode(keyword),i, centers_lonlat[i,2], centers_lonlat[i,1], nr_page))
      }
      retList <- c(retList, list(i, nr_page, ret_tmp))
      if (wait_time > 0){
        Sys.sleep(time)
      }

      if (has_next_page_token) {
        last_token <- ret_tmp$next_page_token
        nr_page <- nr_page + 1
        Sys.sleep(3)
      } else {
        nr_page <- 1
        break
      }
    }
  }
  return(retList)
}
