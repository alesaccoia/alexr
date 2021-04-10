
library(sf)

#' Creates a grid of polygons of a given size that covers the whole italian country
#'
#' @param linear_meters the size of the square cell in meters (10km default)
#'
#' @return an sf object of polygons, crs 4326
#' @export make_italy_grid
#'
make_italy_grid <- function(linear_meters = 10000) {
  italy_3395 <- italy.country %>% st_transform(crs = 3395)
  country_grid <- st_make_grid(italy_3395, cellsize = c(linear_meters, linear_meters))
  grid_with_ids <- st_as_sf(data.frame(id = 1:length(country_grid)), geometry = country_grid)
  grid_with_ids <- grid_with_ids[italy_3395 ,] %>% st_transform(crs = 4326)
}

#' Assigns the municipality and region codes to points in Italy
#'
#' @param df_sp a sf data frame containing the points to label
#'
#' @return a new dataframe with the filled values
#' @export assign_municipality_from_coordinates
#'
assign_municipality_from_coordinates <- function(df_sp) {
  df_sp$country <- 'Italy'
  df_sp$cod_com <- NA
  df_sp$cod_pro <- NA
  df_sp$cod_reg <- NA
  df_sp$region <- NA
  df_sp$province <- NA
  df_sp$municipality <- NA
  for (i in 1:nrow(df_sp)) {
    munic_index <- sapply(st_contains(italy.municipalities, df_sp[i,]), length) == 1
    if (sum(munic_index) == 0) {
      warning(sprintf("The address at index %i has no municipalities", i))
      next
    }
    if (sum(munic_index) != 1) {
      warning(sprintf("The address %i has associated %i municipalities, using the first one", i, sum(munic_index)))
    }
    municipality_index_used <- which(munic_index)[1]
    the_munic <- italy.municipalities[municipality_index_used,]
    df_sp$cod_com[i] <- the_munic$cod_com
    df_sp$cod_pro[i] <- the_munic$cod_pro
    df_sp$cod_reg[i] <- the_munic$cod_reg
    df_sp$region[i] <- the_munic$nom_reg
    df_sp$province[i] <- the_munic$nom_pro
    df_sp$municipality[i] <- as.character(the_munic$nom_com)
  }
  return(df_sp)
}

#' Assigns the polygon index
#'
#' @param df_polygons a sf data frame containing the polygons
#' @param df_sp a sf data frame containing the points to label
#'
#' @return a new dataframe with the column index_of_containing_polygon filled
#' @export assign_polygon_index
#'
assign_polygon_index <- function(df_polygons, df_sp) {
  df_sp$index_of_containing_polygon <-  NA
  intersections <- st_contains(df_polygons, df_sp)
  i <- 1
  sapply(intersections,
    function(x) {
      df_sp$index_of_containing_polygon[x] <<- rep(i, length(x))
      i <<- i + 1
      return(x)
    }
  )
  return(df_sp)
}


#' Creates a query that returns the visits in each polygon
#'
#' Returns visits and dwell time in each polygon. Note that the name of the IDFA column must be 'aid'
#'
#' @param places_table_name the table with the polygons
#' @param ds_from the day date for which we are retrieving the visits (for 1 day can be the same as ds_to)
#' @param ds_to the day date for which we are retrieving the visits (for 1 day can be the same as ds_from)
#' @param location_table_name the table that stores the ping (defaults emr.dataset_accumulo) (hardcoded columns lon, lat)
#' @param places_id_column_name the name of the column containing the ID of the place (defaults to id)
#' @param places_wkt_column_name the column containing the polygon in WKT format (defaults to wkt)
#' @param destination_table a destination table for the INSERT, leave default NULL if you want to fetch the rows
#' @param places_options can be a clause like WHERE places.bt_level4 = 'Q8'
#' @param aid_options can be a clause like 'AND aid in = (SELECT aid from test.audience_table)'. Should contain AND because it's placed after the select
#'
#' @return the SQL query for the visits
#' @export create_sql_visits_query
#' @examples
#' create_sql_visits_query("R.mcdonalds_eevm_places", '2019-10-04', '2019-10-27')
#'
create_sql_visits_query <- function(places_table_name,
                                    ds_from,
                                    ds_to,
                                    location_table_name = "emr.dataset_accumulo",
                                    places_id_column_name = 'id',
                                    places_wkt_column_name = 'wkt',
                                    destination_table = NULL,
                                    places_options = '',
                                    aid_options = '') {
  sql_query <- read_file_as_text(paste0(path.package("alexr"),"/visits_computation.sql"))
  sql_query <- gsub('__PLACES_TABLE_NAME__', places_table_name, sql_query)
  sql_query <- gsub('__DS_FROM__', ds_from, sql_query)
  sql_query <- gsub('__DS_TO__', ds_to, sql_query)
  sql_query <- gsub('__LOCATION_TABLE_NAME__', location_table_name, sql_query)
  sql_query <- gsub('__PLACES_ID_COLUMN_NAME__', places_id_column_name, sql_query)
  sql_query <- gsub('__PLACES_WKT_COLUMN_NAME__', places_wkt_column_name, sql_query)
  sql_query <- gsub('__PLACES_OPTIONS__', places_options, sql_query)
  sql_query <- gsub('__AID_OPTIONS__', aid_options, sql_query)

  if (!is.null(destination_table)) {
    sql_query <- paste("INSERT INTO", destination_table, sql_query)
  }

  sql_query <- gsub('\n',' ',sql_query)
  sql_query <- gsub('\t',' ',sql_query)

  return(sql_query)
}

#' Creates a matrix of daily coefficients for given points on the territory,
#'
#' The returned multiplier can be used for each analysed point and each day to simulate a common
#' population penetration rate across all points. The rate is given by the normalization_percentage.
#' parameter and it defaults to 0.14, aka 14 percent.
#' NB: this queries Athena and considers the statistics.panel table on AWS Glue being updated daily by a cronjob.
#'
#' @param input_places a data.frame containing for each POI a unique id, longitude and latitude
#' @param start_date starting date of the coefficient
#' @param end_date ending date of the coefficient
#' @param id_column_name the name of the column containing the POI unique id (default id_poi)
#' @param lon_column_name the name of the column containing the POI longitudee (default lon)
#' @param lat_column_name the name of the column containing the POI latitude (default lat)
#' @param radius_mt the radius in meters around the POI over which to compute the normalization (default 25000 mt)
#' @param normalization_percentage The simulated penetration rate. Default 0.14 or 14 percent. Set to 1 to attempt an estimate of population parameter but will have huge variance.
#' @param country Defaults to Italy, other countries are not implemented yet
#'
#' @return Returns a pivot data.frame that contains, for each POI and date, the multiplication coefficient
#' @export dailyNormalizationCoefficients
#'
#' @examples
#' library(alexr)
#' library(data.table)
#' places <- athena_execute_fetch('SELECT * from places.places where ds = (select max(ds) from places.places) limit 5')
#' places$ds <- NULL
#' input_places <- places %>% select(area_id, everything())
#' dailyNormalizationCoefficients(input_places, as.Date('2020-01-20),as.Date('2020-01-20), id_column_name = 'area_id')
#' fwrite(input_places,'input_places.csv')
#' fwrite(poi_normalization_coefficient_wide, 'poi_normalization_coefficient_wide.csv')
#'
dailyNormalizationCoefficients <- function(input_places,
                                           start_date,
                                           end_date,
                                           id_column_name = 'id_poi',
                                           lon_column_name = 'lon',
                                           lat_column_name = 'lat',
                                           radius_mt = 25000,
                                           normalization_percentage = 0.14,
                                           country = 'Italy') {
  stopifnot(id_column_name != "id")
  stopifnot(id_column_name %in% colnames(input_places))
  stopifnot(lon_column_name %in% colnames(input_places))
  stopifnot(lat_column_name %in% colnames(input_places))
  stopifnot(country == 'Italy')

  colnames(input_places)[which(colnames(input_places) == id_column_name)] <- "id_poi"

  places_sp <- st_as_sf(input_places %>% select(id_poi, !!lon_column_name, !!lat_column_name), coords = c(lon_column_name,lat_column_name), crs = 4326) %>% st_transform(crs = 3395)

  # the daily pings have been computed with presto in each cell opf italy.grid for the period
  daily_pings <- athena_execute_fetch(paste0("SELECT * FROM statistics.panel where ds between date '", start_date, "' and date '", end_date, "'"))
  daily_pings$ds <- as.Date(daily_pings$ds)
  colnames(daily_pings)[2] <- 'id'

  italy_grid <- make_italy_grid()

  # create a dataset for all the current dates

  full_grid <- expand.grid(ds = seq(start_date, end_date, by = 'day'),id = unique(italy_grid$id))
  full_grid_filled <- merge(full_grid, as.data.frame(daily_pings), all.x = T)
  full_grid_filled$unique_devices[is.na(full_grid_filled$unique_devices)] <- 0
  full_grid_filled$total_pings[is.na(full_grid_filled$total_pings)] <- 0
  full_grid_filled$log_unique_devices <- log10(full_grid_filled$unique_devices)

  ## full_grid_filled ora contiene una matrice con il numero di utenti visti in ogni cella, in ogni giorno del mese
  ## full_grid_filled_sp aggiunge anche l'informazione spaziale

  full_grid_filled_sp <- inner_join(italy_grid, full_grid_filled) %>% st_transform(crs = 3395)
  # ora serve una matrice POI-giorno in cui, per ogni POI, viene scritto il numero di unici visti in quell'area


  # creo un cerchi di 50km attorno al POI per cercare il numero di abitanti che abitano in quella zona partendo dalla densitá abitativa media
  poi_centroids <- st_centroid(places_sp)
  poi_uniques <- st_buffer(poi_centroids, dist = radius_mt )

  # intersezione della griglia sulla densitá di popolazione italiana con i buffer attorno ai POI

  italy_density_sp <- inner_join(italy.grid, italy.grid.density)
  intersections_with_italy <- st_intersection(italy_density_sp, poi_uniques)

  poi_population <- intersections_with_italy
  poi_population$area_new <- st_area(poi_population)
  poi_population$area_new <- set_units(poi_population$area_new, km^2)

  # dopo aver calcolato l'area, posso droppare la geom che velocizza la prossima
  st_geometry(poi_population) <- NULL

  # raggruppo per map_id
  population_around_poi <- poi_population %>%
    dplyr::group_by(id_poi) %>%
    summarise(total_area = sum(area_new), final_density = weighted.mean(population_density, area_new), population_in_radius = final_density * total_area)

  ###--------------
  # ora faccio la stessa cosa ma vedo quanti ping, per giorno, sono stati visti nell'area del POI
  # posso semplicemente fare un join perché i dati li ho precomputati
  st_geometry(intersections_with_italy) <- NULL
  poi_devices_per_day <- inner_join(daily_pings, intersections_with_italy) %>%
    group_by(ds,id_poi) %>%
    summarise(unique_devices = sum(unique_devices), total_pings = sum(total_pings))


  # salviamo con unique_devices nelle colonne, POI nelle righe
  poi_devices_per_day_wide <- poi_devices_per_day %>% select(ds, id_poi, unique_devices) %>% spread(ds, unique_devices)

  ## ora, calcolando che il nostro obiettivo é sempre raggiungere un 14% dei device italiani, guardiamo come farlo sul territorio
  ## giusto per vederle, calcolo le percentuali
  poi_percentage <- inner_join(poi_devices_per_day, population_around_poi %>%
                                 select(id_poi, population_in_radius)) %>%
    mutate(percentage = unique_devices / population_in_radius) %>%
    select(ds, id_poi, percentage) %>%
    spread(ds, percentage)


  ## calcolo il coefficiente di normalizzazione per giorno e per POI

  poi_normalization_coefficient <- inner_join(poi_devices_per_day, population_around_poi %>% select(id_poi, population_in_radius)) %>%
    mutate(normalization_coefficient = 0.14 / (unique_devices / population_in_radius)) %>%
    select(ds, id_poi, normalization_coefficient)
  poi_normalization_coefficient$ds <- as.Date(poi_normalization_coefficient$ds)

  poi_normalization_coefficient_wide <- poi_normalization_coefficient %>%
    spread(ds, normalization_coefficient)

  return(poi_normalization_coefficient_wide)
}
