#' Crea una griglia di cerchi dal raggio specificato che coprano tutta la superficie
#'
#' @param shape
#' @param circleRadius
#'
#' @return
#' @export
#' @import sf
#'
#' @examples
#' 
#' circles <- cover_shape_with_circles(world.borders[world.borders$NAME=='Italy',],50000)
#'

cover_shape_with_circles <- function(shape, circleRadius = 50000) {
  shape_3395 <- shape %>% st_transform(crs = 3395)

  # bounding box in meters mercator
  shape_bbox <- st_bbox(shape_3395)

  centers <- expand.grid(x =  seqinclude(shape_bbox["xmin"]+50000, shape_bbox["xmax"], 100000),
                         y =  seqinclude(shape_bbox["ymin"]+50000, shape_bbox["ymax"], 100000))
  centers_2 <- expand.grid(x =  seqinclude(shape_bbox["xmin"], shape_bbox["xmax"], 100000),
                           y =  seqinclude(shape_bbox["ymin"], shape_bbox["ymax"], 100000))

  centers <- rbind(centers, centers_2)

  coords_sp <- st_as_sf(centers, coords = c("x","y"), remove = FALSE, crs = 3395)
  circles_sp <- st_buffer(coords_sp, circleRadius)
  circles_sp_intersecting <- circles_sp[unlist(st_intersects(shape_3395, circles_sp)),]
  circles_sp_intersecting %>% st_transform(crs = 4326)

  st_geometry(circles_sp_intersecting) <- NULL
  circles_sp_latlong <- st_as_sf(circles_sp_intersecting, coords = c("x","y"), remove = FALSE, crs = 3395)
  circles_sp_latlong <- circles_sp_latlong %>% st_transform(crs = 4326)
  st_coordinates(circles_sp_latlong)
}
