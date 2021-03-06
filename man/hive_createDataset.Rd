% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hive_utilities.R
\name{hive_createDataset}
\alias{hive_createDataset}
\title{Copies the data frame to HDFS and creates a table in hive}
\usage{
hive_createDataset(
  df,
  table_name,
  hdfs_base_location = "/R/",
  hdfsUri = "http://192.168.0.130:9870/webhdfs/v1",
  sep = "\\t",
  start_empty = F
)
}
\arguments{
\item{df}{the data frame}

\item{table_name}{name of the resulting table}

\item{hdfs_base_location}{base location}

\item{hdfsUri}{webhdfs URI}

\item{sep}{separator, defaults to tab}

\item{start_empty}{todo}
}
\description{
Copies the data frame to HDFS and creates a table in hive
}
