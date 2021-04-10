
#' Given a data.frame, creates a query to create a remote hive table
#'
#' @param df the data frame
#' @param table_name the output table name
#' @param remote_path path to hdfs or s3 bucket where the CSV is stored
#' @param sep separator
#'
#' @return a string with the query
#' @export hive_create_table
#'
#' @examples
#'
#' query <- hive_create_table(impressions, 'default.impressions', 's3://alexr-bucket/impressions')
#' query <- hive_create_table(impressions, 'default.impressions', '/alexr-hdfs/impressions/')
#'
#' executeAthenaQuery(query)
#'
hive_create_table <- function(df, table_name, remote_path, sep = ',') {
  fields <- sapply(df, class)
  query <- paste("CREATE EXTERNAL TABLE", table_name, "(")
  if (length(fields) > 1) {
    for (i in 1:(length(fields)-1)) {
      query <- paste(query, names(fields)[i],
                     switch (fields[[i]],
                             "character" = "string",
                             "factor" = "string",
                             "Date" = "date",
                             "integer64" = "bigint",
                             "integer" = "bigint",
                             "numeric" = "double",
                             "logical" = "string"
                     ),',')
    }
  }
  query <- paste(query, names(fields)[length(fields)],
                 switch (fields[[length(fields)]],
                         "character" = "string",
                         "factor" = "string",
                         "Date" = "date",
                         "integer" = "bigint",
                         "integer64" = "bigint",
                         "numeric" = "double",
                         "logical" = "string"
                 ),')')

  query <- paste0(query, ' ROW FORMAT DELIMITED')
  query <- paste0(query, " FIELDS TERMINATED BY '",  sep, "'")
  query <- paste0(query, " LOCATION '",  remote_path, "';")
  return(query)
}

#' Executes a query on local hive
#'
#' @param query The query
#'
#' @return
#' @export hive_execute
#'
hive_execute <- function(query) {
  Sys.setenv(JAVA_HOME = '/usr/lib/jvm/java-8-openjdk-amd64')
  system(paste0("/usr/local/hive/bin/hive -e \"", query, "\""))
}

#' Copies the data frame to HDFS and creates a table in hive
#'
#' @param df the data frame
#' @param table_name name of the resulting table
#' @param hdfs_base_location base location
#' @param hdfsUri webhdfs URI
#' @param sep separator, defaults to tab
#' @param start_empty todo
#'
#' @export hive_createDataset
#'
hive_createDataset <- function(df, table_name, hdfs_base_location = '/R/', hdfsUri = "http://192.168.0.130:9870/webhdfs/v1", sep = '\t', start_empty = F) {
  my_uuid <- tolower(uuid::UUIDgenerate())
  hdfs_location <- paste0(hdfs_base_location, my_uuid, '/')
  file_name <- paste0(my_uuid, '.csv')
  message('Writing temp file')
  write.table(df, file_name, row.names = F, quote = F, col.names = F, sep = sep)
  message('Uploading to hdfs temp file')
  hdfs_uploadFile(file_name, paste0(hdfs_location,file_name), hdfsUri)
  file.remove(file_name)
  message('Dropping table')
  hive_execute(paste('DROP TABLE', table_name))
  message('Creating table')
  query <- hive_create_table(df, table_name, hdfs_location, sep)
  message(query)
  hive_execute(hive_create_table(df, table_name, hdfs_location, sep))
  if (start_empty) {
    stop('To be implemented with the webhdfs API')
  }
}
