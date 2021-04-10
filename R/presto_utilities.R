#' Launches an EMR Presto instanct (5 x m4.4xlarge). Synchronous and can take a while!
#'
#' @param cluster_name Cluster name defaults to R Cluster
#' @param instance_type Type defaults to c5.4xlarge
#' @param instance_count Number of slaves defaults to 4
#' @param query_max_memory Max memory (default '150GB')
#' @param query_max_memory_per_node Max memory per node (default 64GB)
#' @param max_connections Max connections (default 100)
#' @param verbose Continuously logs the cluster status while starting it
#' @param request_as_spot_instance Tries to reserve as spot instance
#'
#' @return the cluster_id
#' @export emr_presto_launch_instance
#'
#' @examples
#' cluster_id <- emr_presto_launch_instance("Test Cluster", instance_type = "c5.4xlarge", instance_count = 4)
#' instance_dns <- emr_presto_get_instance_dns_name(cluster_id)
#' con <- emr_presto_get_db_connection(instance_dns)
#' res <- dbSendQuery(con, 'SELECT 1')
#' all_rows <- dbFetch(res, -1)
#' emr_presto_stop_instance(cluster_id)
#'
emr_presto_launch_instance <- function(cluster_name = "R Cluster",
                                       instance_type = "c5.4xlarge",
                                       instance_count = 4,
                                       query_max_memory = "150GB",
                                       query_max_memory_per_node = "64GB",
                                       max_connections = 100,
                                       verbose = F,
                                       request_as_spot_instance = F) {
  #system(paste0(path.package("alexr"), "/emr_presto_launch_script.sh"))
  aws_cli <- read_file_as_text(paste0(path.package("alexr"),"/emr_presto_launch_script.sh"))
  #aws_cli <- read_file_as_text('/home/asaccoia/projects/alexr/inst/emr_presto_launch_script.sh')
  aws_cli <- gsub('__CLUSTER_NAME__', cluster_name, aws_cli)
  aws_cli <- gsub('__INSTANCE_TYPE__', instance_type, aws_cli)
  aws_cli <- gsub('__INSTANCE_COUNT__', instance_count, aws_cli)
  aws_cli <- gsub('__QUERY_MAX_MEMORY__', query_max_memory, aws_cli)
  aws_cli <- gsub('__MAX_CONNECTIONS__', max_connections, aws_cli)

  if (request_as_spot_instance) {
    aws_cli <- gsub('__OPTION_SPOT__', "\"BidPrice\":\"OnDemandPrice\",", aws_cli)
  } else {
    aws_cli <- gsub('__OPTION_SPOT__', '', aws_cli)
  }
  aws_cli <- gsub('\n','', aws_cli)
  cluster_id <- system(paste0(aws_cli, " | jq -r \".ClusterId\""), intern = TRUE)
  cluster_status <- "STARTING"
  while(cluster_status != "WAITING") {
    if (cluster_status == 'ERROR' | cluster_status == 'FAILED') {
      if (verbose) {
        cluster_status <- system(paste0("aws emr describe-cluster --cluster-id ", cluster_id, " | jq -r \".Cluster.Status.State\""), intern = T)
      }
      stop("Starting cluster failed")
    }
    cluster_status <- system(paste0("aws emr describe-cluster --cluster-id ", cluster_id, " | jq -r \".Cluster.Status.State\""), intern = T)
    if (verbose) {
      print(cluster_status)
    }
    Sys.sleep(3)
  }
  return(cluster_id)
}

#' Stops an emr presto instance
#'
#' @param cluster_id the cluster id of the instance
#'
#' @return the cluster status
#' @export emr_presto_stop_instance
#'
emr_presto_stop_instance <- function(cluster_id) {
  cluster_status <- system(paste0("aws emr terminate-clusters --cluster-ids ", cluster_id))
  return(cluster_status)
}

#' Gets the presto instance DNS
#'
#' @param cluster_id
#'
#' @return the dns of the instance
#' @export emr_presto_get_instance_dns_name
#'
emr_presto_get_instance_dns_name <- function(cluster_id) {
  cluster_dns <- system(paste0("aws emr describe-cluster --cluster-id ", cluster_id, " | jq -r \".Cluster.MasterPublicDnsName\""), intern = T)
  return(cluster_dns)
}


#' Returns a db connection for sending DBI queries to an EMR Presto DB instance
#'
#' @param instance_dns
#'
#' @return the DB connection object to use with the standard DBI API
#' @export emr_presto_get_db_connection
#'
#' @examples
#'
#' cluster_id <- emr_presto_launch_instance("Test Cluster", instance_type = "c5.4xlarge", instance_count = 4)
#' instance_dns <- emr_presto_get_instance_dns_name(cluster_id)
#' con <- emr_presto_get_db_connection(instance_dns)
#' res <- dbSendQuery(con, 'SELECT 1')
#' all_rows <- dbFetch(res, -1)
#' emr_presto_stop_instance(cluster_id)
#'
emr_presto_get_db_connection <- function(instance_dns)  {
  return(dbConnect(
    RPresto::Presto(),
    host=paste0('http://',instance_dns),
    port=8889,
    user='hadoop',
    schema='default',
    catalog='hive'
  ))
}
