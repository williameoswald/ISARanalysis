#' Clean and derive demographic measures from ISAR standardised demographics dataset.
#'
#' Derives asthma onset categories (<12, 12-40, >40 years), asthma duration (in years),
#' and asthma duration categories (<10, 10-<25, ≥25) from labelled demographic data.
#'
#' @param df character. Name of labelled demographics dataset.
#' @importFrom dplyr mutate case_when
#'
#' @export
clean_demographics <- function(df = demographics_labelled) {
  df |>
    mutate(
      asthma_onset_cat = structure(
        case_when(
          asthma_onset < 12 ~ "<12",
          asthma_onset <= 40 ~ "12-40",
          asthma_onset > 40 ~ ">40",
          .default = NA
        ) |>
          factor(levels = c("<12", "12-40", ">40")),
        label = "Age at asthma onset, Years"
      ),
      asthma_duration = structure(
        index_age - asthma_onset,
        label = "Asthma duration, Years"
      ),
      asthma_duration_cat = structure(
        case_when(
          asthma_duration < 10 ~ "<10",
          asthma_duration < 25 ~ "10-<25",
          asthma_duration >= 25 ~ "≥25",
          .default = NA
        ) |>
          factor(levels = c("<10", "10-<25", "≥25")),
        label = "Asthma duration, Years"
      )
    )
}

#' Clean and derive lifestyle measures from ISAR standardised lifestyle dataset.
#'
#' Applies range checks and plausibility checks height and weight measures and cleans
#' BMI measure accordingly. Derives categorical classifications for obese (BMI <30 vs. ≥30 kg/m2)
#'
#' @param df character. Name of labelled lifestyle dataset.
#' @importFrom dplyr across mutate case_when
#'
#' @export
clean_lifestyle <- function(df = lifestyle_labelled) {
  df |>
    mutate(
      # Range checks
      across(
        c(weight),
        \(var) {
          var < 20 | var > 635
        },
        .names = "{.col}_exclude"
      ),
      across(
        c(height),
        \(var) {
          var < 1.3 | var > 2.2
        },
        .names = "{.col}_exclude"
      ),
      bmi = case_when(
        height_exclude | weight_exclude ~ NA,
        is.na(height) | is.na(weight) ~ NA,
        .default = bmi
      ),
      obese = structure(
        case_when(
          bmi < 30 ~ "No",
          bmi >= 30 ~ "Yes",
          .default = NA
        ) |>
          factor(levels = c("Yes", "No")),
        label = "Obese (BMI ≥30 kg/m2)"
      )
    )
}

#' Clean and derive comorbidity measures from ISAR standardised comorbidities dataset.
#'
#' @param df character. Name of labelled comorbidity dataset.
#' @importFrom dplyr across mutate case_when
#'
#' @export
clean_comorbidities <- function(df = comorbidities_labelled) {
  df |>
    mutate(
      crs = structure(crs, label = "CRS (+/-NP)"),
      crswnp = structure(nasal_polyps, label = "CRSwNP"),
      ad = structure(eczema, label = "Eczema/Atopic dermatitis"),
      anxdep = structure(
        case_when(
          anxiety == "Ever" | depression == "Ever" ~ "Yes",
          anxiety == "Never" & depression == "Never" ~ "No",
          .default = NA
        ) |>
          factor(levels = c("Yes", "No")),
        label = "History of anxiety and/or depression"
      )
    )
}

#' Clean and derive asthma control measures from ISAR standardised asthma control dataset.
#'
#' Recodes GINA asthma control responses to numeric values to calculate score and
#' uses to derive GINA asthma control classification (Well controlled vs
#' Partly controlled vs Uncontrolled). Derives categorical classifications for
#' asthma control based on available ACQ and ACT scores. Results are prioritised
#' GINA > ACT > ACQ > AIRQ (not included yet). Italy has missing
#' responses for GINA questions 1 and 4, so prioritise ACT.
#'
#' @param df character. Name of labelled asthma control dataset.
#' @importFrom dplyr filter_out mutate across if_else case_when contains
#'
#' @export
clean_asthma_control <- function(df = asthma_control_labelled) {
  df |>
    mutate(
      # Recode gina control questions as yes = 1 and no = 0
      across(
        contains("gina"),
        ~ case_when(. == "Yes" ~ 1, . == "No" ~ 0, .default = NA)
      ),
      # Calculate gina control score
      gina_score = pick(
        gina_day_1,
        gina_activity_2,
        gina_night_3,
        gina_reliever_4
      ) |>
        rowSums(na.rm = FALSE),
      # Classify gina control score
      gina_ac = case_when(
        gina_score == 0 ~ "Well Controlled",
        gina_score <= 2 ~ "Partly Controlled",
        gina_score <= 4 ~ "Uncontrolled"
      ) |>
        factor(
          levels = c("Uncontrolled", "Partly Controlled", "Well Controlled")
        ),
      # Classify ACQ score (version of test used is unavailable)
      acq_ac = case_when(
        acq_score <= 0.75 ~ "Well Controlled",
        acq_score < 1.5 ~ "Partly Controlled",
        acq_score >= 1.5 ~ "Uncontrolled",
        .default = NA
      ) |>
        factor(
          levels = c("Uncontrolled", "Partly Controlled", "Well Controlled")
        ),
      # Classify ACT score
      act_ac = case_when(
        act_score < 16 ~ "Uncontrolled",
        act_score < 20 ~ "Partly Controlled",
        act_score >= 20 ~ "Well Controlled",
        .default = NA
      ) |>
        factor(
          levels = c("Uncontrolled", "Partly Controlled", "Well Controlled")
        )
    )
}

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

#' Clean and derive biomarker measures from ISAR standardised biomarkers dataset.
#'
#' Reshapes biomarkers dataset to long format with multiple rows per test.
#' Applies range checks (0 ≤ BEOS ≤ 5000; 0 ≤ FeNO ≤ 300; 0 ≤ IgE ≤ 100000)
#' to recorded biomarker measures. Flags cases Where multiple results are
#' recorded for a test on a single date, and allows for three approaches for
#' reducing these duplicates, either keeping minimum, mean, or maximum value.
#' Further cleaning steps to drop these duplicates and exclude measures failing
#' range checks can be specified.
#'
#' @param df character. Name of labelled biomarkers dataset.
#' @param beos_range_low numeric. Lowest plausible value for BEOS range check.
#' @param beos_range_high numeric. Highest plausible value for BEOS range check.
#' @param feno_range_low numeric. Lowest plausible value for FeNO range check.
#' @param feno_range_high numeric. Highest plausible value for FeNO range check.
#' @param ige_range_low numeric. Lowest plausible value for IgE range check.
#' @param ige_range_high numeric. Highest plausible value for Ige range check.

#' @importFrom dplyr filter filter_out select distinct mutate across if_else case_when rename_with n
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate ymd

#' @export
clean_biomarkers <- function(
  df = biomarkers_labelled,
  beos_range_low = 0,
  beos_range_high = 5000,
  feno_range_low = 0,
  feno_range_high = 300,
  ige_range_low = 0,
  ige_range_high = 100000,
  reduce_function = "max",
  drop_duplicate = TRUE,
  drop_rangefail = FALSE
) {
  if (!reduce_function %in% c("min", "max", "mean")) {
    stop('De-duplicating function must be "min", "mean", or "max"')
  }

  df <- df |>
    # remove_empty_imputation_fields() |>
    rename_with(~ str_replace(.x, "_rlt", "_cnt"), contains("feno")) |>
    pivot_longer(
      cols = -c(
        patient_id,
        visit_id,
        visit_id_imputed,
        visit_id_imputed_flag,
        result_date,
        result_date_imputed,
        result_date_imputed_flag
      ),
      names_to = c("test", ".value"),
      names_pattern = "^(.+?)_(cnt|cnt_imputed_flag|cnt_imputed)$"
    ) |>
    filter_out(is.na(cnt)) |>
    rename_with(~ str_replace(.x, "cnt", "result"), contains("cnt")) |>
    mutate(result_date = ymd(result_date)) |>
    # Range checks against values specified above
    mutate(
      exclude_rangefail = case_when(
        test == "beos" &
          (result < beos_range_low | result > beos_range_high) ~ TRUE,
        test == "feno" &
          (result < feno_range_low | result > feno_range_high) ~ TRUE,
        test == "ige" &
          (result < ige_range_low | result > ige_range_high) ~ TRUE,
        .default = FALSE
      ),
    ) |>
    mutate(
      duplicate_result = case_when(
        n() > 1 ~ TRUE,
        .default = FALSE
      ),
      keep_result = case_when(
        duplicate_result &
          reduce_function == "min" &
          result != min(result, na.rm = TRUE) ~ FALSE,
        duplicate_result &
          reduce_function == "mean" &
          result != mean(result, na.rm = TRUE) ~ FALSE,
        duplicate_result &
          reduce_function == "max" &
          result != max(result, na.rm = TRUE) ~ FALSE,
        .default = TRUE
      ),
      .by = c(patient_id, result_date, test)
    )

  if (drop_duplicate) {
    df <- df |>
      filter(keep_result) |>
      # Some patients still have identical rows - these can be dropped, keeps first
      distinct(patient_id, result_date, test, .keep_all = TRUE) |>
      select(-keep_result, -duplicate_result)
  }

  if (drop_rangefail) {
    df <- df |>
      filter_out(exclude_rangefail) |>
      select(-exclude_rangefail)
  }
}

#' Clean and derive measures from ISAR standardised biologics dataset.
#'
#' Creates additional short biologic name variable that excludes product names.
#' Creates biologic type variable that distinguishes between IL5 and IL5R based
#' on product. Renames variables to shorter prefix "bx_" in place of "bio_"
#' and expands start and end date variable names.
#'
#' @param df character. Name of labelled biologics dataset.
#' @importFrom dplyr mutate relocate case_when contains
#'
#' @export
clean_biologics <- function(df = biologics_labelled) {
  df |>
    rename_with(
      .cols = contains("bio_"),
      ~ str_replace(., "bio_", "bx_")
    ) |>
    mutate(
      bx_name_short = structure(
        factor(
          str_trim(str_remove(bx_name, " \\(.*\\)$")),
          levels = c(
            "Benralizumab",
            "Dupilumab",
            "Mepolizumab",
            "Omalizumab",
            "Reslizumab",
            "Tezepelumab"
          )
        ),
        label = "Biologic name"
      ),
      bx_type = structure(
        case_when(
          bx_name_short %in% c("Mepolizumab", "Reslizumab") ~ "Anti-IL5",
          bx_name_short %in% c("Benralizumab") ~ "Anti-IL5R",
          bx_name_short == "Omalizumab" ~ "Anti-IgE",
          bx_name_short == "Dupilumab" ~ "Anti-IL4Ralpha",
          bx_name_short == "Tezepelumab" ~ "Anti-TSLP"
        ) |>
          factor(
            levels = c(
              "Anti-IgE",
              "Anti-IL4Ralpha",
              "Anti-IL5",
              "Anti-IL5R",
              "Anti-TSLP"
            ),
            labels = c(
              "Anti-IgE",
              "Anti-IL4Ralpha",
              "Anti-IL5",
              "Anti-IL5R",
              "Anti-TSLP"
            )
          ),
        label = "Biologic class"
      )
    ) |>
    relocate(bx_name_short, bx_type, .after = bx_name) |>
    rename(bx_start_date = bx_stdate, bx_end_date = bx_endate)
}
