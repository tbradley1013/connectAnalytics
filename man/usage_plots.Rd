% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_05_usage.R
\name{usage_by_date_tbl}
\alias{usage_by_date_tbl}
\alias{usage_by_date}
\alias{usage_by_user_tbl}
\alias{usage_by_user}
\alias{usage_by_content}
\alias{usage_by_owner_tbl}
\alias{usage_by_owner}
\title{Count usage by date and content}
\usage{
usage_by_date_tbl(usage, time_col, from, to)

usage_by_date(usage, time_col, from, to, type = "Shiny App")

usage_by_user_tbl(usage)

usage_by_user(usage, type = "Shiny App")

usage_by_content(usage, type = "Shiny App")

usage_by_owner_tbl(usage)

usage_by_owner(usage, type = "Shiny App")
}
\arguments{
\item{usage}{a usage tbl output from \code{usage_info_join}}

\item{time_col}{the quoted name of the time column that you wish to use 
to calculate the "date" used. For shiny usage either `started` or `ended` 
could be used and for static content usage `time` should be used}

\item{from}{the start date of the graph}

\item{to}{the end date of the graph}

\item{type}{Either "Shiny App" or "Static Content". This string will determine
the title of the plot}
}
\description{
Get a count of all of the usage each day for each piece of
content
}
\details{
The `usage_by_date_tbl` and `usage_by_user_tbl` functions take a usage 
dataframe output from \code{usage_info_join} and create counts of the 
usage amounts by either date or user, respectively. 

The `usage_by_date` and `usage_by_user` functions use the above functions
to create interactive visualizations for the count data
}
