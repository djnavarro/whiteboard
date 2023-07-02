whiteboard_get <- function(file = getOption("whiteboard.data")) {
    readr::read_csv(
      file,
      col_types = "Dcllllll",
      show_col_types = FALSE
    ) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.logical),
          \(x) { class(x) <- "tick"; x }
        )
      )
}

pillar_shaft.tick <- function(x, ...) {
    y <- dplyr::if_else(x, "\u2713", "  ")
    pillar::new_pillar_shaft_simple(y, min_width = 2, align = "left")
}

backdate <- function(days_back = 0) {
    lubridate::today() - days_back
}

whiteboard_update <- function(date = backdate(0),
                     make_bed = TRUE,
                     clean_up = TRUE,
                     exercise = TRUE,
                     no_alcohol = TRUE,
                     no_cigarettes = TRUE,
                     eat_okay = TRUE) {
      dat <- whiteboard
      row <- tibble::tibble_row(
        date = date,
        wday = as.character(lubridate::wday(date, label = TRUE)),
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
      readr::write_csv(dat, getOption("whiteboard.data"))
      return(dat)
    }