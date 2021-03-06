% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_functions.R
\name{check_work_spaces}
\alias{check_work_spaces}
\title{Validate the size of data (work space)}
\usage{
check_work_spaces(pre_processed_data, max_ram = 2, target_unit = "Gb")
}
\arguments{
\item{pre_processed_data}{Data that processed by the function 
\code{pre_process}.}

\item{max_ram}{A value of the maximum size of the list of 
CrossingWorkSpace instance.}

\item{target_unit}{The target unit, i.e., 'Gb', 'Tb' and  'Pb'.
The default value is 'Gb'.}
}
\value{
A NULL value if there is no issue raised.
}
\description{
A function to check the size of a 
data variable whether is in the right range.
}
