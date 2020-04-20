
#' @title Plot a migration history.
#'
#' @description Given a sequence of border crossings for a person,
#' draw a diagram describing that person's migration
#' history.
#'
#' Note that, unlike elsewhere in package \code{migrbc}, the
#' \code{date_crossing} and \code{is_arrival} arguments for
#' \code{plot_mig_hist} refer to a single individual.
#'
#' If values for \code{date_first} and \code{date_last} are not supplied,
#' then defaults are calculated, based on the length of the travel history.
#'
#' @param date_crossing A vector of dates.
#' @param is_arrival A logical vector, the same length as \code{date_crossing}
#' specifying whether each border crossing is an arrival.
#' @param days_to_next_crossing A number vector, the same length as
#' \code{date_crossing} specifying the days span between two journeys.
#' @param res_status_before_str Character or numeric vector,
#' the same length as \code{date_crossing}, showing residence
#' status before each crossing. Optional.
#' @param res_status_after_str Character or numeric vector,
#' the same length as \code{date_crossing}, showing residence
#' status after each crossing.
#' @param date_first The start date for the travel history. Optional.
#' @param date_last The end date for the travel history. Optional.
#' @param show_dates Logical. Whether to display the dates of each border
#' crossing.
#' @param show_days Logical. Whether to display the length, in days, of each
#' spell in or out of the country.
#' @param cex 'Character expansion factor'. A number. Larger values
#' lead to larger text. Defaults to 1.
#' @param lwd Line width. A number. Larger values lead to thicker lines.
#' Defaults to 1.
#' @return Returns \code{NULL}, but as a side effect draws a graph
#' (using R's traditional graphics system).
#' @examples
#' plot_test <- function(mig_hist) {
#'   plot_mig_hist(date_crossing = as.character(mig_hist$date_crossing), 
#'                 is_arrival = mig_hist$is_arrival,
#'                 days_to_next_crossing = mig_hist$days_to_next_crossing,
#'                 show_date = FALSE, 
#'                 cex = 0.8)
#' }
#' number_of_people = 1
#' person_data <- migrbc::setup_random_test_data(number_of_people, 
#'                                               initial_date = '2001-01-01', 
#'                                               numJourneys = 3,
#'                                               min = 0, 
#'                                              max = 100)
#' cross_spaces <- migrbc::pre_process(person_data, n_groups = 1)
#' ## run in non-parallel
#' post_data <- migrbc::run_rbc(cross_spaces, 
#'                        window_size = 487, 
#'                        threshold_year = 365, 
#'                        parallel=FALSE)
## plot good result
#' old_par <- par(mfrow = c(1, 1))
#' plot_test(post_data$journeys)
#' par(old_par)
#' 
#' @export
plot_mig_hist <- function(date_crossing, 
                          is_arrival, 
                          days_to_next_crossing,
                          res_status_before_str = NULL,
                          res_status_after_str = NULL,
                          date_first = NULL, 
                          date_last = NULL,
                          show_dates = TRUE,
                          show_days = TRUE,
                          cex = 1,
                          lwd = 1) {
  date_crossing <- check_and_tidy_date_crossing(date_crossing)
  check_is_logic(is_arrival)
  check_is_logic(show_dates)
  check_is_logic(show_days)
  date_first <- check_and_tidy_date_first_last(date = date_first,
                                               date_crossing = date_crossing, 
                                               name = "date_first")
  date_last <- check_and_tidy_date_first_last(date = date_last,
                                              date_crossing = date_crossing,
                                              name = "date_last")
  check_positive_number(number = cex, name = "cex")
  check_positive_number(number = lwd, name = "lwd")
  n <- length(date_crossing)

  segment_coord_horiz <- segment_coord_horiz(date_crossing = date_crossing,
                                             is_arrival = is_arrival,
                                             date_first = date_first, 
                                             date_last = date_last)
  segment_coord_vert <- segment_coord_vert(date_crossing = date_crossing,
                                           is_arrival = is_arrival)
  x <- range(segment_coord_horiz$x0, segment_coord_horiz$x1)
  y <- range(segment_coord_horiz$y0, segment_coord_horiz$y1)
  if (!is.null(res_status_before_str)) {
    y <- 1.25 * y  ## make room for labels
  }
  graphics::plot(x = x, 
                 y = y,
                 type = "n",
                 axes = FALSE, 
                 xlab = "", 
                 ylab = "")
  graphics::segments(x0 = segment_coord_horiz$x0,
                     x1 = segment_coord_horiz$x1,
                     y0 = segment_coord_horiz$y0,
                     y1 = segment_coord_horiz$y1, 
                     lwd = lwd)
  graphics::segments(x0 = segment_coord_vert$x0,
                     x1 = segment_coord_vert$x1,
                     y0 = segment_coord_vert$y0, 
                     y1 = segment_coord_vert$y1,
                     lwd = lwd)
  graphics::abline(h = 0, lty = "dotted", lwd = lwd)
  graphics::mtext(text = "Out",
                  side = 2, 
                  las = 1, 
                  line = -1, 
                  cex = 0.8 * cex, 
                  at = 0.2, 
                  col = "grey20")
  graphics::mtext(text = "In",
                  side = 2,
                  las = 1, 
                  cex = 0.8 * cex,
                  line = -1, 
                  at = -0.2,
                  col = "grey20")
  if (!is.null(res_status_before_str)) {
    ## before
    graphics::text(x = segment_coord_vert$x0, 
                   y = segment_coord_vert$y0,
                   labels = res_status_before_str,
                   pos = ifelse(is_arrival, 3, 1),
                   cex = cex)
    ## after
    graphics::text(x = segment_coord_vert$x1, 
                   y = segment_coord_vert$y1,
                   labels = res_status_after_str, 
                   pos = ifelse(is_arrival, 1, 3), 
                   cex = cex)
  }
  if (show_dates) {
    graphics::mtext(text = date_crossing,
                    side = 1, 
                    at = date_crossing, 
                    cex = cex)
  }
  if (show_days) {
    days <- days_to_next_crossing
    at <- date_crossing[n] + 0.5 * days
    if (n > 1L) {
      days <- c(diff(date_crossing), days)
      at <- c(date_crossing[-n] + 0.5 * diff(date_crossing), at)
    }
    text <- ifelse(days > 0, sprintf("%d days", days), "")
    side <- ifelse(is_arrival, 1, 3)
    graphics::mtext(text = text, side = side, at = at, cex = cex)
  }
  invisible(NULL)
}


#' Internal function
#' 
#' @param  date_crossing date of border crossing.
#' @param  is_arrival A Boolean value.
#' @param date_first The first date occurred.
#' @param date_last The last date occurred.
#' 
#' @return NULL
#' 
segment_coord_horiz <- function(date_crossing,
                                is_arrival,
                                date_first,
                                date_last) {
  n <- length(date_crossing)
  x0 <- c(date_first, date_crossing)
  x1 <- c(date_crossing, date_last)
  y0 <- ifelse(is_arrival, 1, -1)
  y0 <- c(y0, -y0[n])
  y1 <- y0
  list(x0 = x0, x1 = x1, y0 = y0, y1 = y1)
}

#' Internal function
#' @param  date_crossing date of border crossing.
#' @param  is_arrival A Boolean value.
#' 
#' @return NULL
#' 
segment_coord_vert <- function(date_crossing, is_arrival) {
  n <- length(date_crossing)
  x0 <- date_crossing
  x1 <- date_crossing
  y0 <- rep(-1L + 2L * is_arrival, times = n)
  y1 <- rep(1L - 2L * is_arrival, times = n)
  list(x0 = x0, x1 = x1, y0 = y0, y1 = y1)
}
