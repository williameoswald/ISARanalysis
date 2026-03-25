#' Clean and derive spirometry measures from ISAR standardised spirometry dataset.
#'
#' Applies range checks and plausibility checks (FEV1 >= FVC) to pre- and
#' post-bronchodilator spirometry measures. Where post-bronchodilator measures
#' are available at every timepoint for a patient, they are prioritised over
#' pre-bronchodilator measures. Derives single composite FEV1, FVC, FEV1/FVC ratio,
#' and percent predicted FEV1 measures, along with categorical classifications
#' for FEV1 percent predicted (<80% vs ≥80%) and FEV1/FVC ratio (<0.70 vs ≥0.70).
#'
#' @param df character. Name of labelled spirometry dataset.
#' @param fev1_range_low numeric. Lowest plausible value for FEV1 range check.
#' @param fev1_range_high numeric. Highest plausible value for FEV1 range check.
#' @param fvc_range_low numeric. Lowest plausible value for FVC range check.
#' @param fvc_range_high numeric. Highest plausible value for FVC range check.

#' @importFrom dplyr filter_out mutate across if_else case_when
#'
#' @export
clean_spirometry <- function(
  df = spirometry_labelled,
  fev1_range_low = 0.15,
  fev1_range_high = 8,
  fvc_range_low = 0.15,
  fvc_range_high = 8
) {
  df |>
    # Drop rows with missing measurement date is missing
    filter_out(is.na(spirometry_date)) |>
    # If post-bronchodilator measures are not available at all time points for a patient, use pre-bronchodilator only to be consistent over time
    mutate(
      exclude_post_fev1_measures = !all(!is.na(post_fev1)),
      exclude_post_fvc_measures = !all(!is.na(post_fvc)),
      .by = patient_id
    ) |>
    # Range checks against values specified above
    mutate(
      across(
        c(pre_fev1, post_fev1),
        \(var) {
          !is.na(var) &
            (var < fev1_range_low | var > fev1_range_high)
        },
        .names = "{.col}_rangefail"
      ),
      across(
        c(pre_fvc, post_fvc),
        \(var) {
          !is.na(var) &
            (var < fvc_range_low | var > fvc_range_high)
        },
        .names = "{.col}_rangefail"
      ),
      # FEV1 should be less than FVC
      pre_fev1_fvc_implausible = !is.na(pre_fev1) &
        !is.na(pre_fvc) &
        pre_fev1 >= pre_fvc,
      post_fev1_fvc_implausible = !is.na(post_fev1) &
        !is.na(post_fvc) &
        post_fev1 >= post_fvc,
      # Flag records to exclude based on above checks
      pre_fev1_exclude = pre_fev1_rangefail | pre_fev1_fvc_implausible,
      pre_fvc_exclude = pre_fvc_rangefail | pre_fev1_fvc_implausible,
      post_fev1_exclude = post_fev1_rangefail |
        post_fev1_fvc_implausible |
        exclude_post_fev1_measures,
      post_fvc_exclude = post_fvc_rangefail |
        post_fev1_fvc_implausible |
        exclude_post_fvc_measures
    ) |>
    mutate(
      # Create single fev1 measure for baseline and longitudinal description
      fev1 = structure(
        case_when(
          pre_fev1_exclude & post_fev1_exclude ~ NA,
          !is.na(post_fev1) & !post_fev1_exclude ~ post_fev1,
          !is.na(post_fev1) & post_fev1_exclude & !pre_fev1_exclude ~ pre_fev1,
          !is.na(pre_fev1) & !pre_fev1_exclude ~ pre_fev1,
          .default = NA
        ) |>
          as.numeric(),
        label = "FEV1 (L)"
      ),
      # Flag value used for overall measure
      fev1_flag = case_when(
        pre_fev1_exclude & post_fev1_exclude ~ "none",
        !is.na(post_fev1) & !post_fev1_exclude ~ "post",
        !is.na(post_fev1) & post_fev1_exclude & !pre_fev1_exclude ~ "pre",
        !is.na(pre_fev1) & !pre_fev1_exclude ~ "pre",
        .default = NA_character_
      ),
      # Create single fvc measure for baseline and longitudinal description
      fvc = structure(
        case_when(
          pre_fvc_exclude & post_fvc_exclude ~ NA,
          !is.na(post_fvc) & !post_fvc_exclude ~ post_fvc,
          !is.na(post_fvc) & post_fvc_exclude & !pre_fvc_exclude ~ pre_fvc,
          !is.na(pre_fvc) & !pre_fvc_exclude ~ pre_fvc,
          .default = NA
        ) |>
          as.numeric(),
        label = "FVC (L)"
      ),
      fvc_flag = case_when(
        pre_fvc_exclude & post_fvc_exclude ~ "none",
        !is.na(post_fvc) & !post_fvc_exclude ~ "post",
        !is.na(post_fvc) & post_fvc_exclude & !pre_fvc_exclude ~ "pre",
        !is.na(pre_fvc) & !pre_fvc_exclude ~ "pre",
        .default = NA_character_
      ),
      # Create single percent predicted measure for baseline and longitudinal description
      fev1_percpred = structure(
        case_when(
          pre_fev1_exclude & post_fev1_exclude ~ NA,
          !is.na(percpred_post_fev1) & !post_fev1_exclude ~ percpred_post_fev1,
          !is.na(percpred_post_fev1) &
            post_fev1_exclude &
            !pre_fev1_exclude ~ percpred_pre_fev1,
          !is.na(percpred_pre_fev1) & !pre_fev1_exclude ~ percpred_pre_fev1,
          .default = NA
        ),
        label = "percent predicted FEV1 (%)"
      ),
      fev1_percpred_flag = case_when(
        pre_fev1_exclude & post_fev1_exclude ~ "none",
        !is.na(percpred_post_fev1) & !post_fev1_exclude ~ "post",
        !is.na(percpred_post_fev1) &
          post_fev1_exclude &
          !pre_fev1_exclude ~ "pre",
        !is.na(percpred_pre_fev1) & !pre_fev1_exclude ~ "pre",
        .default = NA_character_
      ),
      # Create categorical percent predicted measure for baseline and longitudinal description
      fev1_percpred_cat = structure(
        case_when(
          fev1_percpred < 80 ~ "<80%",
          fev1_percpred >= 80 ~ "≥80%",
          .default = NA
        ) |>
          factor(levels = c("<80%", "≥80%")),
        label = "percent predicted FEV1 (%)"
      ),
      # Calculate ratio measure for baseline and longitudinal description
      fev1_fvc_ratio = structure(
        fev1 / fvc,
        label = "FEV1/FVC ratio"
      ),
      # Create categorical ratio measure for baseline and longitudinal description
      fev1_fvc_ratio_cat = structure(
        case_when(
          fev1_fvc_ratio < 0.70 ~ "<0.70",
          fev1_fvc_ratio >= 0.70 ~ "≥0.70",
          .default = NA
        ) |>
          factor(levels = c("<0.70", "≥0.70")),
        label = "FEV1/FVC ratio"
      )
    )
}
