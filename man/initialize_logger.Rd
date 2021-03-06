% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{initialize_logger}
\alias{initialize_logger}
\title{Initialize Futile Logger}
\usage{
initialize_logger(log_level = 6, log_path = NULL)
}
\arguments{
\item{log_level}{a parameter representing a threshold, which 
affects the visibility of a given logger. If the log level is at or 
higher in priority than the logger threshold,  a message will print. 
Otherwise the command will silently return. The value of the log_level 
is a number between 1 and 9. 9 or futile.logger::TRACE will show all 
messages in details.}

\item{log_path}{A path for the output log files generated by the 
logger. If NULL, all messages will be displayed in the calling 
environment.}
}
\value{
it runs on side effects but also return a simple message.
}
\description{
This function is used to initialize the futile.logger
 so that the user can be notified with the current
 status of running RBC.
}
\examples{
## futile.logger::FATAL: 1
## futile.logger::ERROR: 2
## futile.logger::WARN:  4
## futile.logger::INFO:  6
## futile.logger::DEBUG: 8
## futile.logger::TRACE: 9

## to suppresse log messages to the console
migrbc::initialize_logger(log_level = 1)

## to display all messages to the console
migrbc::initialize_logger(log_level = 9)

}
