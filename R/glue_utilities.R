#' Executes a remote Athena command
#'
#' @param query
#'
#' @return the AWS query_id
#' @export athena_execute
#'
#' @examples
#'
#' athena_execute(create_glue_table(df, foo, 's3://test/foo/'))
#'
athena_execute <- function(query, verbose = F) {
  query_id <- system(paste0("aws athena start-query-execution --query-string \"",
                            query,
                            "\" --result-configuration \"OutputLocation\"=\"s3://alexr-alex-home-frankfurt/athena_results/\" ",
                            "--region eu-central-1 | jq -r \".QueryExecutionId\""), intern = TRUE)
  query_exec_status <- "INITING"
  while(query_exec_status != "SUCCEEDED") {
    if (query_exec_status == 'ERROR' | query_exec_status == 'FAILED') {
      if (verbose) {
        message(system(paste0("aws athena get-query-execution",
                      " --query-execution-id \"", query_id, "\"",
                      " --region eu-central-1 | jq -r \".QueryExecution.Status.StateChangeReason\""), intern = T))
      }
      stop("Query failed")
    }
    query_exec_status <- system(paste0("aws athena get-query-execution",
                                       " --query-execution-id \"", query_id, "\"",
                                       " --region eu-central-1 | jq -r \".QueryExecution.Status.State\""), intern = T)
    Sys.sleep(3)
  }
  return(query_id)
}



#' Executes a remote Athena query and fetch the result
#'
#' @param query
#'
#' @return the result as a data frame
#' @export athena_execute_fetch
#'
#' @examples
#'
#' athena_execute(create_glue_table(df, foo, 's3://test/foo/'))
#'
#' athena_execute_fetch('select * from foo')
#'
#'
athena_execute_fetch <- function(query) {
  query_id <- athena_execute(query)
  system(paste0("aws s3 cp s3://alexr-alex-home-frankfurt/athena_results/", query_id, ".csv ."))
  result_rows <- fread(paste0(query_id, '.csv'))
  system(paste0('rm -rf ', query_id, '.csv'))
  return(result_rows)
}



#' Copies the data frame to s3
#'
#' @param df the data frame
#' @param s3_location the s3 bucket (directories will be created)
#'
#' @return
#' @export copy_to_s3
#'
#' @examples
#' copy_to_s3(nike_locations_giulia, S3_LOCATION, sep = '\t')
#'
copy_to_s3 <- function(df, s3_location, sep = ',') {
  write.table(df, 'temp_df.csv', row.names = F, quote = F, col.names = F, sep = sep)
  system(paste0('aws s3 cp temp_df.csv ', s3_location))
  system('rm -rf temp_df.csv')
}


#' Copies the data frame and makes it available in AWS Glue. Attention: if a table with the same name exists it will be deleted.
#'
#' @param df the data frame
#' @param table_name the resulting table name in aws glue
#' @param s3_base_location the base dataset locations (default s3://rdsupport/R_datasets/)
#' @param sep the delimiter used (default tab)
#' @param start_empty remove the file leaving just the path (default F)
#'
#' @return
#' @export athena_createDataset
#' @import uuid
#'
#' @examples
#' athena_createDataset(df, 'my_athena_db.my_athena_table')
#'
athena_createDataset <- function(df, 
                                 table_name, 
                                 s3_base_location = 's3://rdsupport/R_datasets/', 
                                 sep = '\t', 
                                 start_empty = F, 
                                 is_spatial = F)
{
  stopifnot(any(class(df) %in% c("data.frame", "data.table")))
  my_uuid <- tolower(uuid::UUIDgenerate())
  s3_location <- paste0(s3_base_location, my_uuid, '/')
  file_name <- paste0(my_uuid, '.csv')
  if (is_spatial) {
    message('Creating spatial temp file')
    file_name_spat <- paste0(my_uuid, '_spat.csv')
    st_write(df, file_name_spat, layer_options = 'GEOMETRY=AS_WKT', delete_dsn = T)
    df <- fread(file_name_spat)
    system(paste0('rm -rf ', file_name_spat))
  }
  message('Writing temp file')
  colnames(df) <- gsub('\\.', '', make.names(colnames(df)))
  write.table(df, file_name, row.names = F, quote = F, col.names = F, sep = sep)
  message('Uploading to S3 temp file')
  system(paste0('aws s3 cp ', file_name, ' ', s3_location))
  system(paste0('rm -rf ', file_name))
  message('Dropping table')
  athena_execute(paste('DROP TABLE', table_name))
  message('Creating table')
  athena_execute(hive_create_table(df, table_name, s3_location, sep))
  if (start_empty) {
    system(paste0('aws s3 rm ', s3_location, file_name))
  }
}


