#' Validate an integer value
#' 
#' Check a value whether or not it is an integer.
#' 
#' @param name The name of the variable.
#' @param value The validating variable.
#' 
#' @return NULL 
#' 
check_integer <- function(name = NULL, value = NULL) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.null(name)) {
    var_name <- "unnamed object"
  } else {
    var_name <- name
  }
  
  if (!is.numeric(value)) {
    stop(gettextf("'%s' is non-numeric", var_name))
  }
  len <- length(value)
  if (!len == 1L) {
    stop(gettextf("'%s' does not have length %d", var_name, 1L))
  }
  if (as.integer(value) != value) {
    stop(gettextf("'%s' is not an integer", var_name))   
  }
  if (value < 1L) {
    stop(gettextf("'%s' is less than %d", var_name, 1L))      
  }
  NULL
}

#' Validate a logical value
#' 
#' Check a value whether or not it is a Boolean value.
#' 
#' @param check_value Boolean value to present In/Out the country.
#' 
#' @return NULL 
#' 
check_is_logic <- function(check_value) {
  if (is.numeric(check_value)) {
    if (!all(check_value %in% c(0L, 1L))) {
      stop(gettextf("'%s' is numeric, but has values not equal to 0, 1",
                    "check_value"))
    }
  } else {
    if (!is.logical(check_value)) {
      stop(gettextf("'%s' has class \"%s\"", "check_value",
                    class(check_value)))
    }
  }
  if (any(is.na(check_value))) {
    stop(gettextf("'%s' has missing values",
                  "check_value"))
  } 
  NULL
}

#' Validate dates
#' 
#' @param date The last date to compare with.
#' @param date_crossing The border crossing date.
#' @param name A name of the checking variable.
#' 
#' @return NULL 
#' 
check_and_tidy_date_first_last <- function(date, 
                                           date_crossing, name) {
  multiplier_extra <- 0.1
  name <- match.arg(name, choices = c("date_first", "date_last"))
  n <- length(date_crossing)
  is_date_first <- identical(name, "date_first")
  if (is.null(date)) {
    if (identical(n, 1L)) {
      extra <- lubridate::ddays(30L)  
    } else {
      length_interval <- date_crossing[n] - date_crossing[1L]
      extra <- multiplier_extra * length_interval / lubridate::ddays(1L)
    }
    if (is_date_first) {
      date <- date_crossing[1L] - extra
    } else {
      date <- date_crossing[n] + extra    
    }
  } else {
    if (!identical(length(date), 1L)) {
      stop(gettextf("'%s' does not have length %d", name, 1L))
    }
    if (is.na(date)) {
      stop(gettextf("'%s' is missing", name))  
    }
    if (!methods::is(date, "Date")) {
      date <- tryCatch(lubridate::ymd(date), 
                       error = function(e) e,
                       warning = function(w) w)
      if (methods::is(date, "error") || methods::is(date, "warning")) {
        stop(gettextf("'%s' has invalid year-month-date format: %s",
                      name,
                      date$message))
      }
    }
    if (is_date_first) {
      if (date > date_crossing[1L]) {
        stop(gettextf("'%s' is later than first element of '%s'",
                      name,
                      "date_crossing")) 
      }

    } else {
      if (date < date_crossing[n]) {
        stop(gettextf("'%s' is earlier than last element of '%s'",
                      name,
                      "date_crossing"))
      }
    }
  }
  date
}

#' Validate a numeric value
#' 
#' Check a value whether or not it is a number.
#' 
#' @param number The checking value.
#' @param name The name of the variable.
#' 
#' @return NULL 
#' 
check_positive_number <- function(number, name) {
  if (!identical(length(number), 1L)) {
    stop(gettextf("'%s' does not have length %d", name, 1L)) 
  } 
  if (is.na(number)) {
    stop(gettextf("'%s' is missing", name))  
  } 
  if (!is.numeric(number)) {
    stop(gettextf("'%s' is non-numeric", name))  
  }
  if (number <= 0) {
    stop(gettextf("'%s' is non-positive", name))  
  } 
  NULL
}

#' Validate dates
#' 
#' @param date_crossing The border crossing date.
#' 
#' @return NULL 
#' 
check_and_tidy_date_crossing <- function(date_crossing) {
  if (any(is.na(date_crossing))) {
    stop(gettextf("'%s' has missing values",
                  "date_crossing"))
  }
  if (is.character(date_crossing)) {
    date_crossing_original <- date_crossing
    date_crossing <- tryCatch(as.Date(date_crossing),
                              error = function(e) e)
    if (methods::is(date_crossing, "error")) {
      stop(gettextf("problem coercing '%s' to class \"%s\" : %s",
                    "date_crossing",
                    "Date",
                    date_crossing$message))   
    } 
    is_invalid_date <- is.na(date_crossing)
    if (any(is_invalid_date)) {
      first_invalid_date <- date_crossing_original[is_invalid_date][1L]
      stop(gettextf("'%s' contains invalid date(s) : %s ...",
                    "date_crossing",
                    first_invalid_date))
    }
  } else {
    if (!methods::is(date_crossing, "Date")) {
      stop(gettextf("'%s' has class \"%s\"",
                    "date_crossing",
                    class(date_crossing)))
    }
  }
  date_crossing
}

#' Validate dates
#' 
#' A function to check the date variable is the right date
#' 
#' @param date An date object in string format
#'  such as '2018-01-01'.
#' @param date_name The name of the date variable.
#' 
#' @return NULL
#' 
check_and_tidy_date <- function(date, date_name) {
  if (is.null(date)) {
    stop(gettextf("'%s' cannot be NULL",
                  date_name)) 
  }
  if (!identical(length(date), 1L)) {
    stop(gettextf("'%s' does not have length %d",
                  date_name, 1L)) 
  }
  if (is.na(date)) {
    stop(gettextf("'%s' is missing",
                  date_name)) 
  }
  if (is.character(date)) {
    date <- tryCatch(lubridate::ymd(date),
                     error = function(e) e,
                     warning = function(w) w)
    if (methods::is(date, "error") || methods::is(date, "warning")) {
      stop(gettextf("problem coercing '%s' to class \"%s\" : %s",
                    date_name,
                    "Date",
                    date$message))
    }
  } else {
    if (!methods::is(date, "Date")) {
      stop(gettextf("'%s' is a class of \"%s\" but not Character", 
                    date_name, class(date)))
    }
  }
  date
}

#' Validate the bag of CrossingWorkSpace object
#' 
#' @param pre_processed_data Data that processed by the function 
#'  \code{pre_process}.
#' @param max_ram A value of the maximum size of the list of 
#'  CrossingWorkSpace instance.
#' @param target_unit The target unit, i.e., 'Gb', 'Tb' and  'Pb'.
#' The default value is 'Gb'.
#' 
#' @return NULL
#' 
#' @export
check_work_spaces <- function(pre_processed_data,
                              max_ram = 2,
                              target_unit = "Gb") {
  if (!inherits(pre_processed_data, "migrbc_preprocess")) {
    stop(paste0("The pre_processed_data must be an object of ",
                "'migrbc_preprocess', ",
                "processed by the function 'pre_process'."))
  }
  for (cws in pre_processed_data) {
    get_names <- names(cws)
    if (is.null(get_names) || !all(get_names %in% c("group_data", 
                                                    "initial_status_group"))) {
      stop(paste0("The list must contain a list of sub group data:", 
                  " 'group_data' and 'initial_status_group'."))
    }
  }
  check_object_size(pre_processed_data,
                    max_ram = max_ram, 
                    target_unit = target_unit)
  NULL
}

#' Validate the bag of CrossingWorkSpace instances
#' 
#' @param object An object that is required to check.
#' @param max_ram The maximum size of the target object.
#' @param target_unit The target unit that is constrained. 
#' The value is one of c('bytes', 'Kb', 'Mb', 'Gb', 'Tb', 'Pb').
#' 
#' @return NULL if succeed.
#' 
#' @export
check_object_size <- function(object, max_ram = 2, target_unit = "Gb") {
  if (!is.numeric(max_ram)) {
    stop("The parameter 'max_ram' must be a number.")
  }
  
  units <- c("bytes", "Kb", "Mb", "Gb", "Tb", "Pb")
  res <- get_object_size(object)
  check_index <- which(units == res$unit)
  target_index <- which(units == target_unit)
  
  if (check_index < target_index) {
    return(NULL)
  }
  if (res$size > max_ram || check_index > target_index) {
    stop(gettextf("The maximum object size is %d %s and 
      the input variable has a size of %s %s", 
                  max_ram, 
                  target_unit,
                  res$size, 
                  res$unit))
  }
  NULL
}

#' Validate the data columns
#' 
#' @param data The journey data that should contain columns in (
#'            'journeyId', 
#'            'personId', 
#'            'date_crossing', 
#'            'is_arrival',
#'            'journey_sequence',
#'            'journeyId_prev).
#'            
#' @return NULL
#'
#' @export 
check_data_columns <- function(data) {
  if (!is.data.frame(data)) {
    stop("The input data must be a type of data frame")
  }
  data_colnames <- colnames(data)
  validated_columns <- c("journeyId",
                         "personId",
                         "date_crossing", 
                         "is_arrival",
                         "journey_sequence", 
                         "journeyId_prev")
  if (!all(validated_columns %in% data_colnames)) {
    stop("The journey data does not contain the required columns.")
  }
  nrow <- nrow(data)
  if (nrow == 0) {
      stop("No journey data provided")
  }
  NULL
}

#' Validate the data columns
#'
#' @param data The journey data that should contain columns of (
#'            'personId',
#'            'res_status_initial',
#'            'date_finalised').
#'
#' @return NULL
#'
#' @export
check_ini_res_data_columns <- function(data) {
  if (!is.data.frame(data)) {
    stop("The input data must be a type of data frame")
  }
  data_colnames <- colnames(data)
  validated_columns <- c("personId", "res_status_initial",
                         "date_finalised")
  if (!all(validated_columns %in% data_colnames)) {
    stop(paste0("The data does not contain",
                " the required columns."))
  }
  NULL
}
