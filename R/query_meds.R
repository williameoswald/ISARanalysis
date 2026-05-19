#' Query ISAR medications dataset to flag use of medication within period of time.
#'
#' Determines whether a patient was on a specified medication class within a
#' defined time window preceding (or following) an anchor date. The function
#' joins visit-level medication records against patient-level query dates,
#' constructs treatment intervals, and classifies each patient as Yes/No based
#' on whether any recorded treatment interval overlaps the search window.
#'
#' @param df A data frame of medication records, defaulting to `meds`. Must
#'   contain columns: `country`, `patient_id`, `visit_id`, `visit_date`,
#'   `med_class`, `med_yn`, `med_name`, `med_stdate`, `med_endate`,
#'   `med_dailydose_eqv`, and `med_ongoing`.
#' @param query_dates A tibble with at minimum a `patient_id` column and a date
#'   column matching `search_anchor_date_col`. One row per patient. Patients
#'   present in `query_dates` but absent from `df` (i.e. no medication records)
#'   are dropped; ensure upstream filtering accounts for this.
#' @param med_class_select Character scalar. The medication class to filter on,
#'   matched against the `med_class` column. Defaults to `"LTOCS"`. The output
#'   variable is derived from this value as `{str_to_lower(med_class_select)}_yn`
#'   (e.g. `"LTOCS"` produces `ltocs_yn`).
#' @param search_anchor_date_col Character scalar. Name of the date column in
#'   `query_dates` to use as the anchor for the search window. The window runs
#'   from `anchor - search_mos months` to `anchor` (inclusive). Defaults to
#'   `"index_date"`.
#' @param search_mos Integer. Number of months to look back from the anchor
#'   date. Negative values look forward. Month arithmetic uses
#'   [lubridate::%m-%()][lubridate::`%m-%`] to handle end-of-month edge cases.
#'   Defaults to `12`.
#' @param countryname Character vector or `NULL`. If provided, restricts
#'   records to the specified country or countries (matched against `country`).
#'   `NULL` (default) processes all countries.
#' @param keep_df Logical. If `TRUE`, assigns the full visit-level working data
#'   frame to the global environment for inspection. The assigned name is
#'   `{countryname}_{med_class_newvar}_df` if a single country is specified,
#'   otherwise `{med_class_newvar}_df`. Defaults to `FALSE`.
#'
#' @details
#' ## Search window construction
#'
#' The search window is derived from `query_dates` using the column specified
#' by `search_anchor_date_col` as the index date:
#'
#' ```
#' search_start <- anchor_date %m-% months(search_mos)
#' search_end   <- anchor_date
#' ```
#' Note - Uses [lubridate::interval] allows search_start > search_end without a problem.#'
#'
#' Month arithmetic uses [lubridate::`%m-%`] to handle end-of-month edge cases.
#' Always returns a date in the nth month after Date. If the new date would usually
#' spill over into the n + 1th month, %m+% will return the last day of the nth month.
#'
#' ## Classification algorithm
#'
#' - If both `med_stdate` and `med_endate` are present: `interval(med_stdate, med_endate)`
#' - If only `med_stdate` is present (open-ended): `interval(med_stdate, visit_date)`,
#'   using the visit date as a conservative imputed end — i.e. the treatment
#'   was known to be active at least up to this visit.
#' - Otherwise: `NA` (no interval can be constructed).
#'
#' Each visit row is then assigned a `search_result`:
#'
#' \describe{
#'   \item{`"No med recorded"`}{All visits for the patient have `med_yn == "No"`;
#'     treatment was never recorded regardless of timing.}
#'   \item{`"Yes overlap"`}{The treatment interval overlaps the search window
#'     per [lubridate::int_overlaps()].}
#'   \item{`"No overlap"`}{The treatment interval does not overlap the search window.}
#'   \item{`"No at visit"`}{`med_yn == "No"` with no constructable interval;
#'     `int_overlaps()` returns `NA` rather than `FALSE` so this branch
#'     catches those rows explicitly.}
#'   \item{`NA`}{No condition matched — typically a `med_yn == "Yes"` row
#'     where neither `med_stdate` nor `med_endate` are available, so no
#'     interval can be constructed and overlap cannot be assessed.}
#' }
#'
#' The patient-level outcome variable is then derived as:
#'
#' \describe{
#'   \item{`"Yes"`}{Any visit has `search_result == "Yes overlap"`.}
#'   \item{`"No"`}{All `search_result` values contain `"No"` (i.e. no overlap
#'     and no recorded treatment).}
#'   \item{`NA`}{Indeterminate — insufficient data to classify.}
#' }
#'
#' @return A tibble with one row per patient containing:
#' \describe{
#'   \item{`country`}{Patient country.}
#'   \item{`patient_id`}{Patient identifier.}
#'   \item{`{med_class_newvar}`}{Factor with levels `c("Yes", "No")` indicating
#'     whether the patient was on the specified medication class within the
#'     search window. `NA` indicates indeterminate status.}
#' }
#'
#' @seealso [lubridate::interval()], [lubridate::int_overlaps()], [lubridate::`%m-%`()]
#'
#' @importFrom dplyr filter select distinct inner_join mutate bind_rows
#' @importFrom lubridate interval int_overlaps `%m-%`
#' @importFrom stringr str_to_lower str_detect
#'
#' @examples
#' # Basic usage with a single anchor date column
#' query_meds(
#'   query_dates = bx_start_dates,
#'   search_anchor_date_col = "bx_start_date"
#' )
#'
#' # Look forward 6 months using a negative search_mos value
#' query_meds(
#'   query_dates = bx_start_dates,
#'   search_anchor_date_col = "bx_start_date",
#'   search_mos = -6
#' )
#'
#' # Restrict to two countries and retain the working data frame
#' query_meds(
#'   query_dates = bx_start_dates,
#'   search_anchor_date_col = "bx_start_date",
#'   countryname = c("Spain", "Italy"),
#'   keep_df = TRUE
#' )
#'
#' # Use with a vector of med_class_select values (e.g. to create baseline dataset)
#' c("LTOCS", "LAMA", "ICSLABA", "LTRA", "Theophyllines", "Macrolides") |>
#' purrr::map(
#'   \(x) {
#'     query_meds(
#'       query_dates = query_dates,
#'       med_class_select = x,
#'       search_anchor_date_col = "bx_start_date",
#'       keep_df = TRUE
#'     )
#'   },
#'   .progress = TRUE
#' ) |>
#'   purrr::reduce(left_join, by = join_by(country, patient_id)) |>
#'   dplyr::rename_with(.cols = -c("country", "patient_id"), ~ paste0(., "_bl"))
#'
#'
#' @export
query_meds <- function(
  df = meds,
  query_dates,
  med_class_select = "LTOCS",
  search_anchor_date_col = "index_date",
  search_mos = 12,
  countryname = NULL,
  keep_df = FALSE
) {
  med_class_newvar <- paste0(str_to_lower(med_class_select), "_yn")

  med_yn_df <- df |>
    filter(med_class == med_class_select) |>
    remove_empty_imputation_fields() |>
    # All records if countryname is null, and allows vector of countries
    filter(is.null(countryname) | country %in% .env$countryname) |>
    select(
      country,
      patient_id,
      visit_id,
      visit_date,
      med_class,
      med_yn,
      med_name,
      med_stdate,
      med_endate,
      med_dailydose_eqv,
      med_ongoing
    ) |>
    distinct() |>
    inner_join(
      query_dates |>
        distinct() |>
        mutate(
          # https://lubridate.tidyverse.org/reference/mplus.html
          search_start = .data[[search_anchor_date_col]] %m-%
            months(search_mos),
          search_end = .data[[search_anchor_date_col]], # query date (inclusive)
          search_interval = interval(search_start, search_end)
        ) |>
        select(patient_id, contains("search_")),
      by = join_by(patient_id),
      relationship = "many-to-one"
    ) |>
    # group_by(patient_id, med_yn, med_stdate, med_endate, med_dailydose_eqv) |>
    # Some patients have duplicated information across visits, keep the last as presumptive end date
    # (NB - this doesn't change result, but may be cleaner to avoid assessing multiple rows)
    # slice_tail(n = 1) |>
    # ungroup() |>
    # arrange(patient_id, visit_date) |>
    mutate(
      med_interval = case_when(
        !is.na(med_stdate) & !is.na(med_endate) ~ interval(
          med_stdate,
          med_endate
        ),
        # Impute end date as the visit date if it's missing. This seems reasonable as we knew treatment ongoing up to the latest visit with equivalent information.
        !is.na(med_stdate) & is.na(med_endate) ~ interval(
          med_stdate,
          visit_date
        ),
        .default = NA
      )
    ) |>
    mutate(
      search_result = case_when(
        all(as.character(med_yn) == "No") ~ "No med recorded", # If all med_yn=="No" then ltocs_yn == "No" (time-irrelevant)
        int_overlaps(search_interval, med_interval) ~ "Yes overlap",
        !int_overlaps(search_interval, med_interval) ~ "No overlap",
        med_yn == "No" ~ "No at visit",
        # Remaining missing are duplicate visit records - no new information
        .default = NA
      ),
      .by = patient_id
    ) |>
    mutate(
      "{ med_class_newvar }" := case_when(
        # all(search_result == "No med recorded") ~ "No",
        # all(search_result == "No overlap") ~ "No",
        all(str_detect(search_result, "No"), na.rm = TRUE) ~ "No",
        any(search_result == "Yes overlap") ~ "Yes",
        .default = NA
      ) |>
        factor(levels = c("Yes", "No")),
      .by = patient_id
    )

  if (keep_df) {
    assign(
      paste0(
        if (!is.null(countryname) & length(countryname) == 1) {
          paste0(str_to_lower(countryname), "_", med_class_newvar, "_df")
        } else {
          paste0(med_class_newvar, "_df")
        }
      ),
      med_yn_df,
      envir = .GlobalEnv
    )
  }

  # Pull patient-level ltocs measure
  med_yn_df |>
    distinct(country, patient_id, .data[[med_class_newvar]])

  # assign(
  #   paste0(
  #     if (!is.null(countryname) & length(countryname) == 1) {
  #       paste0(str_to_lower(countryname), "_ltocs_var")
  #     } else {
  #       "ltocs_var"
  #     }
  #   ),
  #   ltocs_var,
  #   envir = .GlobalEnv
  # )
}
