% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hive_utilities.R
\name{hive_create_table}
\alias{hive_create_table}
\title{Given a data.frame, creates a query to create a remote hive table}
\usage{
hive_create_table(df, table_name, remote_path, sep = ",")
}
\arguments{
\item{df}{the data frame}

\item{table_name}{the output table name}

\item{remote_path}{path to hdfs or s3 bucket where the CSV is stored}

\item{sep}{separator}
}
\value{
a string with the query
}
\description{
Given a data.frame, creates a query to create a remote hive table
}
\examples{

query <- hive_create_table(impressions, 'default.impressions', 's3://alexr-bucket/impressions')
query <- hive_create_table(impressions, 'default.impressions', '/alexr-hdfs/impressions/')

executeAthenaQuery(query)

}
