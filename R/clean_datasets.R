#' Clean ISAR standardised research datasets
#'
#' This function further processes standardised spirometry dataset to flag fev1 and fvc measures outside specified range, create single FEV1, FVC, FEV1/FVC, and percent predicted FEV1 prioritisint post-bronchodilator measures and excluding those failing range check and creates new categorical measures for FEV1, FEV1/FVC ratio.
#' @param df character. Name of labelled spirometry dataset.
#' @param fev1_range_low numeric. Lowest plausible value for FEV1 range check.
#' @param fev1_range_high numeric. Highest plausible value for FEV1 range check.
#' @param fvc_range_low numeric. Lowest plausible value for FVC range check.
#' @param fvc_range_high numeric. Highest plausible value for FVC range check.

#' @importFrom dplyr filter pull mutate select any_of across everything cur_column if_else na_if
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
    # Range checks
    mutate(
      across(
        c(pre_fev1, post_fev1),
        \(var) {
          !is.na(var) &
            (var < fev1_range_low | var > fev1_range_high)
        },
        .names = "{.col}_exclude"
      ),
      across(
        c(pre_fvc, post_fvc),
        \(var) {
          !is.na(var) &
            (var < fvc_range_low | var > fvc_range_high)
        },
        .names = "{.col}_exclude"
      )
    ) |>
    mutate(
      fev1 = structure(
        case_when(
          pre_fev1_exclude & post_fev1_exclude ~ NA,
          !is.na(post_fev1) & !post_fev1_exclude ~ post_fev1,
          !is.na(post_fev1) & post_fev1_exclude ~ pre_fev1,
          !is.na(pre_fev1) & !pre_fev1_exclude ~ pre_fev1,
          .default = NA
        ) |>
          as.numeric(),
        label = "FEV1 (L)"
      ),
      fvc = structure(
        case_when(
          pre_fvc_exclude & post_fvc_exclude ~ NA,
          !is.na(post_fvc) & !post_fvc_exclude ~ post_fvc,
          !is.na(post_fvc) & post_fvc_exclude ~ pre_fvc,
          !is.na(pre_fvc) & !pre_fvc_exclude ~ pre_fvc,
          .default = NA
        ) |>
          as.numeric(),
        label = "FVC (L)"
      ),
      fev1_fvc_ratio = structure(
        fev1 / fvc,
        label = "FEV1/FVC ratio"
      ),
      fev1_percpred = structure(
        case_when(
          pre_fev1_exclude & post_fev1_exclude ~ NA,
          !is.na(percpred_post_fev1) & !post_fev1_exclude ~ percpred_post_fev1,
          !is.na(percpred_post_fev1) & post_fev1_exclude ~ percpred_pre_fev1,
          !is.na(percpred_pre_fev1) & !pre_fev1_exclude ~ percpred_pre_fev1,
          .default = NA
        ),
        label = "percent predicted FEV1 (%)"
      ),
      fev1_percpred_cat = structure(
        case_when(
          fev1_percpred < 80 ~ "<80%",
          fev1_percpred >= 80 ~ "≥80%",
          .default = NA
        ) |>
          factor(levels = c("<80%", "≥80%")),
        label = "percent predicted FEV1 (%)"
      ),
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
