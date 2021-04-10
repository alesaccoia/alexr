#' Uploads a local file to HDFS through the webhdfs API
#'
#' @param localPath path to the local file
#' @param remotePath path to the remote file
#' @param hdfsUri URI of the webhdfs API, default http://192.168.0.130:9870/webhdfs/v1
#'
#' @return
#' @export hdfs_uploadFile
#' @import httr
#'
hdfs_uploadFile <-function(localPath, remotePath, hdfsUri = "http://192.168.0.130:9870/webhdfs/v1") {
  # Optional parameter, with the format &name1=value1&name2=value2
  optionalParameters <- "&overwrite=true"
  
  # CREATE => creation of a file
  writeParameter <- "?op=CREATE"
  
  # Concatenate all the parameters into one uri
  uri <- paste0(hdfsUri, remotePath, writeParameter, optionalParameters)
  
  # Ask the namenode on which datanode to write the file
  response <- PUT(uri)
  
  # Get the url of the datanode returned by hdfs
  uriWrite <- response$url
  
  # Upload the file with a PUT request
  responseWrite <- PUT(uriWrite, body = upload_file(localPath))
}

  
  