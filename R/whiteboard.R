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

yesno_as_logical <- function(x) {
  if(tolower(x) == "y") return(TRUE)
  if(tolower(x) == "n") return(FALSE)
  rlang::abort("invalid response")
}

#' Use prompts to update the whiteboard state
#'
#' @return Invisibly returns the whiteboard tibble
#' @export
prompt <- function() {
  backdate <- readline("How many days to backdate: ") |> as.numeric()
  entry_date <- lubridate::today() - backdate
  cat(
    "Prompting is for the date: ",
    as.character(entry_date),
    " (",
    as.character(lubridate::wday(entry_date, label = TRUE)),
    ")\n",
    sep = ""
  )
  make_bed <- readline("Did you make the bed? [y/n] ") |> yesno_as_logical()
  clean_up <- readline("Did you clean up? [y/n] ") |> yesno_as_logical()
  exercise <- readline("Did you exercise? [y/n] ") |> yesno_as_logical()
  no_alcohol <- readline("Did you avoid alcohol? [y/n] ") |> yesno_as_logical()
  no_cigarettes <- readline("Did you avoid cigarettes? [y/n] ") |> yesno_as_logical()
  eat_okay <- readline("Did you eat okay? [y/n] ") |> yesno_as_logical()
  add(
    backdate,
    make_bed,
    clean_up,
    exercise,
    no_alcohol,
    no_cigarettes,
    eat_okay
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
  entry_date <- lubridate::today() - backdate
  row <- new_row(
    entry_date,
    make_bed,
    clean_up,
    exercise,
    no_alcohol,
    no_cigarettes,
    eat_okay
  )
  print(row)
  cat("\n")
  val <- readline("Okay to add this row? [y/n] ")
  if(val != "y") {
    cat("aborting\n")
    return(invisible(NULL))
  }
  ind <- which(dat$date == entry_date)
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

