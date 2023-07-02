#' Read the whiteboard data
#'
#' @param file Path to the csv file
#'
#' @return A tibble
#' @export
read <- function(file = getOption("whiteboard.data")) {
  readr::read_csv(
    file,
    col_types = "Ddcllllll",
    show_col_types = FALSE
  ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.logical),
        \(x) { class(x) <- "tick"; x }
      )
    )
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.tick <- function(x, ...) {
  y <- dplyr::if_else(x, "\u2713", "  ")
  pillar::new_pillar_shaft_simple(y, min_width = 2, align = "left")
}

new_row <- function(date,
                    make_bed,
                    clean_up,
                    exercise,
                    no_alcohol,
                    no_cigarettes,
                    eat_okay) {
  tibble::tibble_row(
    date = date,
    wday = as.character(lubridate::wday(date, label = TRUE)),
    day = as.numeric(date - as.Date("2023-06-18")),
    make_bed = make_bed,
    clean_up = clean_up,
    exercise = exercise,
    no_alcohol = no_alcohol,
    no_cigarettes = no_cigarettes,
    eat_okay = eat_okay
  ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.logical),
        \(x) { class(x) <- "tick"; x }
      )
    )
}

#' Update the whiteboard state
#'
#' @param backdate Days to backdate
#' @param make_bed Logical
#' @param clean_up Logical
#' @param exercise Logical
#' @param no_alcohol Logical
#' @param no_cigarettes Logical
#' @param eat_okay Logical
#'
#' @return Invisibly returns the whiteboard tibble
#' @export
add <- function(backdate = 0,
                make_bed = TRUE,
                clean_up = TRUE,
                exercise = TRUE,
                no_alcohol = TRUE,
                no_cigarettes = TRUE,
                eat_okay = TRUE) {
  dat <- read()
  row <- new_row(
    lubridate::today() - backdate,
    make_bed,
    clean_up,
    exercise,
    no_alcohol,
    no_cigarettes,
    eat_okay
  )
  print(row)
  cat("\n")
  val <- readline("okay to add this row? [y/n] ")
  if(val != "y") {
    cat("aborting\n")
    return(invisible(NULL))
  }
  ind <- which(dat$date == date)
  if (length(ind) > 0) dat <- dat[-ind, ]
  dat <- dat |>
    tibble::add_row(row) |>
    dplyr::arrange(dplyr::desc(date))
  write(dat)
  return(dat)
}

#' Write whiteboard data to file
#'
#' @param dat Data frame
#' @param file File location
#'
#' @export
write <- function(dat, file = getOption("whiteboard.data")) {
  readr::write_csv(dat, file)
}

