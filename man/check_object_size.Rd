% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_functions.R
\name{check_object_size}
\alias{check_object_size}
\title{Validate the size of a object}
\usage{
check_object_size(object, max_ram = 2, target_unit = "Gb")
}
\arguments{
\item{object}{An object that is required to check.}

\item{max_ram}{The maximum size of the target object.}

\item{target_unit}{The target unit that is constrained. 
The value is one of c('bytes', 'Kb', 'Mb', 'Gb', 'Tb', 'Pb').}
}
\value{
A NULL value if there is no issue raised.
}
\description{
A function to check the size of a data 
variable whether is in the right range.
}
