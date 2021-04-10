
#' Detects the stop locations from a time series of GPS positions
#'
#' Returns a spatial data frame containing stop locations in lat/long, min and max time,
#' from a spatial frame with lat/long (must have crs set to 4326, lat/long).
#'
#' @param sdf containing lat/long (crs = 4326)
#' @param columnsToKeep chr vector containing the columns to keep for other purposes
#' @param use_centroid wether to use the centroid or the last point for distance computation
#' @param max_distance_mt the maximum distance moved before the algorithm stops clustering
#' @param min_elements_in_cluster min close points that contribute to forming a cluster
#'
#' @return Returns a spatial data frame containing the summary for the clusters
#' @import sf dplyr
#' @export
#' @note Possible extensions:
#'       - accuracy that gets lower when entering a building, allowing for
#'       clusters made by just one element
#'       - previous point computation based on a small movement from the centroid
#'       - could use speed information: whenever the speed is higher than walking speed, stop clustering
#'
find_stop_locations <- function(sdf, columnsToKeep = "", use_centroid = T, max_distance_mt = 30, min_elements_in_cluster = 2) {

  # transforms to planar coordinates
  points_utm <- st_transform(sdf, crs = 3395)

  current_cluster_id <- 1
  points_utm$clusters <- integer(length = nrow(points_utm))
  points_utm$speed <- numeric(length = nrow(points_utm))
  elements_in_cluster <- NULL
  current_speed <- 0

  # the algorithm iterates all points

  for (i in 1:nrow(points_utm)) {
    if (length(elements_in_cluster) > 0) {
      if (use_centroid) {
        previous_point <- st_centroid(points_utm[elements_in_cluster,])
      } else {
        previous_point <- points_utm[i-1,]
      }
      difference_from_previous <- as.numeric(st_distance(points_utm[i,],previous_point[1,]))
      # points_utm$speed[i] <- distance_moved_mt / as.numeric(difference_from_previous$date_time, units = "secs")
      if (difference_from_previous <= max_distance_mt) {
        elements_in_cluster <- c(elements_in_cluster, i)
        if (length(elements_in_cluster) >= min_elements_in_cluster) {
          points_utm$clusters[elements_in_cluster] <- current_cluster_id
        }
      } else {
        if (length(elements_in_cluster) >= min_elements_in_cluster) {
          current_cluster_id <- current_cluster_id + 1
        }
        elements_in_cluster <- c(i)
      }
    } else {
      elements_in_cluster <- c(i)
    }
  }

  # transforms back to lat/long
  points_w84 <- st_transform(points_utm, crs = 4326)

  if (columnsToKeep == "") {
    group_by_cols <- "clusters"
  } else {
    group_by_cols <- c("clusters", columnsToKeep)
  }

  if (length(unique(points_w84$clusters)) == 1) {
    return(st_sf(st_sfc()) %>% st_set_crs(4326))
  }

  movements_summary <- points_w84 %>%
    filter(clusters > 0) %>%
    group_by(.dots = group_by_cols) %>%
    summarise(min_time = min(date_time),
              max_time = max(date_time))

  movements_summary <- st_transform(movements_summary, crs = 3395)
  movements_summary <- st_centroid(movements_summary)
  movements_summary <- st_transform(movements_summary, crs = 4326)
  movements_summary$dwell <- as.integer(as.numeric(movements_summary$max_time - movements_summary$min_time, unit = "secs") / 60.0)
  movements_summary
}
