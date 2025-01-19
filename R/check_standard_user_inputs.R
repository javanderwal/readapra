#' Check whether the publication statistics and cur_hist inputs are a valid.
#'
#' @param stat_pub character vector detailing the publication statistics to be
#' checked as valid.
#' @param cur_hist character vector detailing the cur_hist availability to be
#' checked.
#' @param call the caller environment.
#'
#' @noRd
#'
check_standard_user_inputs <- function(stat_pub,
                                       cur_hist,
                                       pub_stats_arg = rlang::caller_arg(stat_pub),
                                       cur_hist_arg = rlang::caller_arg(cur_hist),
                                       call = rlang::caller_env()) {
  check_character_length_one(stat_pub, call = call)
  check_character_length_one(cur_hist, call = call)

  unique_stat_pubs <- unique(apra_stat_pubs_details$stat_pub_acronym)

  if (!stat_pub %in% unique_stat_pubs) {
    best_match_stat_pub <-
      stringdist::amatch(
        x = stat_pub,
        table = unique_stat_pubs,
        method = "dl",
        nomatch = 0,
        maxDist = 3
      )

    cli::cli_abort(
      message = c(
        "{.arg {pub_stats_arg}} must be an acronym of a statistical publication, not {.val {stat_pub}}.",
        if (best_match_stat_pub != 0) c("i" = "Did you mean {.val {unique_stat_pubs[best_match_stat_pub]}}?"),
        "i" = "A list of valid acronyms is available in the {.code apra_stat_pubs} dataset."
      ),
      class = "readapra_error_invalid_stat_pub_input",
      call = call
    )
  }

  stat_pub_rows <-
    dplyr::filter(.data = apra_stat_pubs_details, stat_pub_acronym == stat_pub)

  if (!cur_hist %in% stat_pub_rows$cur_hist) {
    best_match_cur_hist <-
      stringdist::amatch(
        x = cur_hist,
        table = stat_pub_rows$cur_hist,
        method = "dl",
        nomatch = 0,
        maxDist = 3
      )

    cli::cli_abort(
      message = c(
        "{.arg {cur_hist_arg}} must be equal to {.or {.val {stat_pub_rows$cur_hist}}}, not {.val {cur_hist}}.",
        if (best_match_cur_hist != 0) c("i" = "Did you mean {.val {stat_pub_rows$cur_hist[best_match_cur_hist]}}?")
      ),
      class = "readapra_error_invalid_cur_hist_input",
      call = call
    )
  }

  stat_pub_selected <-
    dplyr::filter(.data = stat_pub_rows, cur_hist == {{ cur_hist }})

  if (nrow(stat_pub_selected) != 1) {
    cli::cli_abort(
      message = "Could not determine selected statistical publication based on argument inputs.",
      class = "readapra_error_could_not_select_stat_pub",
      call = call
    )
  }

  return(stat_pub_selected)
}
