% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resolve_data.R
\name{resolve_data}
\alias{resolve_data}
\title{Process RBC}
\usage{
resolve_data(grouped_data, window_size = 487, threshold_year = 365,
  parallel = FALSE, n_core = 2, include_error_columns = FALSE,
  mc.cleanup = FALSE)
}
\arguments{
\item{grouped_data}{A list of data frame objects.}

\item{window_size}{The maximum length of the scanning period.
Can be an integer giving the number of days, the result
of a call to function \code{\link[base]{difftime}}, or an object of
class \code{\link[lubridate:Duration-class]{Duration}}.}

\item{threshold_year}{The length of the yearly test period.
Can be an integer giving the number of days, the result
of a call to function \code{\link[base]{difftime}}, or an object of
class \code{\link[lubridate:Duration-class]{Duration}}.}

\item{parallel}{Optional, if it is TRUE, run on parallel.}

\item{n_core}{if \code{parallel} set to TRUE, this will specify
the number of computer cores required.}

\item{include_error_columns}{Optional, if it is TRUE, the returned 
result of \code{error_data} will contain two extra columns
 \code{error_code} and \code{error_message}.}

\item{mc.cleanup}{if set to TRUE then all children that have been 
forked by this function will be killed (by sending SIGTERM) before
this function returns. Under normal circumstances mclapply waits
for the children to deliver results, so this option usually has 
only effect when mclapply is interrupted. If set to FALSE then 
child processes are collected, but not forcefully terminated.
As a special case this argument can be set to the number of the
signal that should be used to kill the children instead of SIGTERM.}
}
\value{
A list type of object that contains a classified journey 
dataframe object and a error dataframe object.
}
\description{
This function is the key function to do the rules 
 based classification.
}
\examples{
## to suppresse log messages to the console
migrbc::initialize_logger(log_level = 1)

number_of_people = 10
person_data <- migrbc::setup_random_test_data(number_of_people, 
                                 initial_date = '2001-01-01', 
                                 numJourneys = 5,
                                 min = 0, 
                                 max = 10)
crossings <- migrbc::pre_process(person_data, n_groups = 10)
crossings
cross_spaces <- migrbc::resolve_data(crossings)
cross_spaces

}
