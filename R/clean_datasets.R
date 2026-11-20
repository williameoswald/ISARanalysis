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
      wt = as.numeric(wt),
      ht = as.numeric(ht),
      # Range checks
      across(
        c(wt),
        \(var) {
          var < 20 | var > 635
        },
        .names = "{.col}_rangefail"
      ),
      across(
        c(ht),
        \(var) {
          var < 1.3 | var > 2.2
        },
        .names = "{.col}_rangefail"
      ),
      weight = structure(if_else(!wt_rangefail, wt, NA), label = "Weight, kg"),
      height = structure(if_else(!ht_rangefail, ht, NA), label = "Height, m"),
      bmi = structure(
        case_when(
          ht_rangefail | wt_rangefail ~ NA,
          is.na(weight) | is.na(height) ~ NA,
          .default = weight / (height^2)
        ),
        label = "BMI, kg/m^2"
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
  gina_vars <- c(
    "gina_day_1",
    "gina_activity_2",
    "gina_night_3",
    "gina_reliever_4"
  )
  control_levels <- c("Uncontrolled", "Partly Controlled", "Well Controlled")
  source_labels <- c(gina_ac = "GINA", act_ac = "ACT", acq_ac = "ACQ")

  has_gina <- all(gina_vars %in% names(df))
  has_act <- "act_score" %in% names(df)
  has_acq <- "acq_score" %in% names(df)

  if (!any(has_gina, has_act, has_acq)) {
    warning(
      "Asthma control variables not found (need all four GINA items, act_score, or acq_score). Returning data unchanged.",
      call. = FALSE
    )
    return(df)
  }
  if (!"country" %in% names(df)) {
    warning(
      "Country variable not found. Returning data unchanged.",
      call. = FALSE
    )
    return(df)
  }

  # GINA: requires all four items
  if (has_gina) {
    df <- df |>
      mutate(
        # Recode gina control questions as yes = 1 and no = 0
        across(
          all_of(gina_vars),
          ~ case_when(
            . == "Yes" ~ 1,
            . == "1" ~ 1,
            . == "No" ~ 0,
            . == "2" ~ 0,
            .default = NA
          )
        ),
        # Calculate gina control score
        gina_score = structure(
          rowSums(pick(all_of(gina_vars)), na.rm = FALSE),
          label = "GINA symptom control score"
        ),
        # Range check
        gina_score_rangefail = gina_score < 0 | gina_score > 4,
        # Classify gina control score
        gina_ac = structure(
          case_when(
            gina_score_rangefail ~ NA,
            gina_score == 0 ~ "Well Controlled",
            gina_score <= 2 ~ "Partly Controlled",
            gina_score <= 4 ~ "Uncontrolled"
          ) |>
            factor(levels = control_levels),
          label = "Asthma control, GINA"
        )
      )
  } else if (any(gina_vars %in% names(df))) {
    message(
      "GINA control not derived: missing ",
      paste(setdiff(gina_vars, names(df)), collapse = ", ")
    )
  }

  # ACT
  if (has_act) {
    df <- df |>
      mutate(
        act_score = structure(
          as.numeric(act_score),
          label = "ACT score"
        ),
        # Range check
        act_score_rangefail = act_score < 5 | act_score > 25,
        act_ac = structure(
          case_when(
            act_score_rangefail ~ NA,
            act_score < 16 ~ "Uncontrolled",
            act_score < 20 ~ "Partly Controlled",
            act_score >= 20 ~ "Well Controlled",
            .default = NA
          ) |>
            factor(levels = control_levels),
          label = "Asthma control, ACT"
        )
      )
  }

  # ACQ (version of test used is unavailable)
  if (has_acq) {
    df <- df |>
      mutate(
        acq_score = structure(
          as.numeric(acq_score),
          label = "ACQ score"
        ),
        # Range check, exclude ACQ scores outside range, 0 - 6, ACQ is mean of 7 questions with 7-point scale (0 = no impairment; 6 = maximum impairment)
        acq_score_rangefail = acq_score < 0 | acq_score > 6,
        acq_ac = structure(
          case_when(
            acq_score_rangefail ~ NA,
            acq_score <= 0.75 ~ "Well Controlled",
            acq_score < 1.5 ~ "Partly Controlled",
            acq_score >= 1.5 ~ "Uncontrolled",
            .default = NA
          ) |>
            factor(levels = control_levels),
          label = "Asthma control, ACQ"
        ),
        acq_version = structure(
          case_when(
            is.na(acq_score) ~ NA,
            country == "UK" & str_detect(visit_id, "baseline") ~ "ACQ7",
            country == "UK" & str_detect(visit_id, "followup") ~ "ACQ6",
            country %in% c("Poland", "Mexico", "Brazil", "Italy") ~ "ACQ7",
            country %in% c("Belgium", "Denmark") ~ "ACQ6",
            !is.na(acq_score) ~ as.character("Unspecified ACQ")
          ),
          label = "ACQ version"
        ),
      )
  }

  # Combined control: gina_ac > act_ac > acq_ac (first non-missing, per row)
  priority <- intersect(names(source_labels), names(df))

  df <- df |>
    mutate(
      # reduce same as coalesce(coalesce(gina_ac, act_ac), acq_ac) but won't fail if column not present
      asthma_control = structure(
        reduce(pick(all_of(priority)), coalesce) |>
          factor(levels = control_levels),
        label = "Asthma control"
      ),
      # identify source of control measure selected
      asthma_control_source = pick(all_of(priority)) |>
        imap(\(x, nm) {
          if_else(is.na(x), NA_character_, source_labels[[nm]])
        }) |>
        reduce(coalesce)
    )
  source_levels <- unname(source_labels[priority])

  if (has_acq) {
    df <- df |>
      mutate(
        asthma_control_source = if_else(
          asthma_control_source == "ACQ",
          acq_version,
          asthma_control_source
        )
      )
    source_levels <- c(
      setdiff(source_levels, "ACQ"),
      c("ACQ7", "ACQ6", "Unspecified ACQ")
    )
  }

  df |>
    mutate(
      asthma_control_source = structure(
        factor(asthma_control_source, levels = source_levels),
        label = "Asthma control source"
      )
    ) |>
    select(
      patient_id,
      uidpatvis,
      visit_id,
      visit_date,
      any_of(
        c(
          "gina_day_1",
          "gina_activity_2",
          "gina_night_3",
          "gina_reliever_4",
          "gina_lf_5",
          "gina_score",
          "gina_score_rangefail",
          "gina_ac",
          "act_score",
          "act_score_rangefail",
          "act_ac",
          "acq_score",
          "acq_score_rangefail",
          "acq_version",
          "acq_ac",
          "airq_score",
          "asthma_control",
          "asthma_control_source"
        )
      ),
      everything(),
      -country
    )
}

#' Clean and derive spirometry measures from ISAR standardised spirometry dataset.
#'
#' Applies range checks and plausibility checks (FEV1 >= FVC) to pre- and
#' post-bronchodilator spirometry measures. Where post-bronchodilator measures
#' are available at every timepoint for a patient, they are prioritised over
#' pre-bronchodilator measures. Derives single composite FEV1, FVC, and FEV1/FVC ratio,
#' along with categorical classifications for FEV1/FVC ratio (<0.70 vs ≥0.70).
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
    rename_with(
      ~ str_remove(.x, "observation_|visit_|spirometry_"),
      .cols = contains("_date")
    ) |>
    # Drop rows with missing measurement date is missing
    filter_out(is.na(date)) |>
    # If post-bronchodilator measures are not available at all time points for a patient, use pre-bronchodilator only to be consistent over time
    mutate(
      exclude_post_fev1_measures = !all(!is.na(post_fev1)),
      exclude_post_fvc_measures = !all(!is.na(post_fvc)),
      .by = patient_id
    ) |>
    # Range checks against values specified above
    mutate(
      across(c(pre_fev1, post_fev1, pre_fvc, post_fvc), \(var) as.numeric(var)),
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
      # FEV1 should be less than FVC, but could be the same
      pre_fev1_fvc_implausible = !is.na(pre_fev1) &
        !is.na(pre_fvc) &
        pre_fev1 > pre_fvc,
      post_fev1_fvc_implausible = !is.na(post_fev1) &
        !is.na(post_fvc) &
        post_fev1 > post_fvc,
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
        .default = NA_character_ # Empty if no FEV1 measure
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
        .default = NA_character_ # Empty if no FVC measure
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
#' Applies range checks (0 ≤ BEOS ≤ 5000; 0 ≤ FeNO ≤ 300; 0 ≤ IgE ≤ 100000)
#' to recorded biomarker measures. Flags cases Where multiple results are
#' recorded for a test on a single date, and allows for three approaches for
#' reducing these duplicates, either keeping minimum, mean, or maximum value.
#' Further cleaning steps to drop these duplicates and exclude measures failing
#' range checks can be specified. All measures must be in cells/mcL for beos,
#' IU/mL for IgE, and ppb for FeNO.
#'
#' @param df character. Name of labelled biomarkers dataset.
#' @param beos_range_low numeric. Lowest plausible value for BEOS range check.
#' @param beos_range_high numeric. Highest plausible value for BEOS range check.
#' @param feno_range_low numeric. Lowest plausible value for FeNO range check.
#' @param feno_range_high numeric. Highest plausible value for FeNO range check.
#' @param ige_range_low numeric. Lowest plausible value for IgE range check.
#' @param ige_range_high numeric. Highest plausible value for Ige range check.
#' @param reduce_function character. Function to reduce duplicates (max, mean, min).
#' @param drop_duplicate logical. Remove flagged rows among duplicated records.
#' @param drop_rangefail logical. Remove flagged out of range rows.
#' @param keep_flags logical. Keep columns used to flag rows with errors.
#' @param flag_only logical. Use to check output, keeps all records.
#'
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
  drop_rangefail = FALSE,
  keep_flags = TRUE,
  flag_only = FALSE
) {
  if (!reduce_function %in% c("min", "max", "mean")) {
    stop('De-duplicating function must be "min", "mean", or "max"')
  }

  group_vars <- c("patient_id", "date", "test")
  is_mean <- reduce_function == "mean"
  reduce_fn <- switch(reduce_function, min = min, max = max, mean = mean)

  flag_cols <- c(
    "result_out_of_range",
    "eligible_row",
    "duplicate_result",
    "reduced_result",
    "reduced_result_out_of_range",
    "keep_result"
  )

  # Vectorised range check, usable on any value (raw result or reduced result)
  out_of_range <- function(test, x) {
    case_when(
      test == "beos" & (x < beos_range_low | x > beos_range_high) ~ TRUE,
      test == "feno" & (x < feno_range_low | x > feno_range_high) ~ TRUE,
      test == "ige" & (x < ige_range_low | x > ige_range_high) ~ TRUE,
      .default = FALSE
    )
  }

  # Missing results are dropped up front, so they never reach the flags
  df <- filter_out(df, is.na(result))

  # Flag everything, drop nothing
  #   result_out_of_range:         raw result outside the specified range
  #   eligible_row:                row may be used to choose/average the result
  #                                (out-of-range rows are not eligible if they
  #                                will be dropped)
  #   duplicate_result:            group has more than one row
  #   reduced_result:              min/max/mean of eligible results in the group
  #   reduced_result_out_of_range: reduced_result outside the specified range
  #   keep_result:                 the single row retained per group
  #                                (FALSE for every row if none are eligible)
  df <- df |>
    mutate(
      result_out_of_range = out_of_range(test, result),
      eligible_row = !(drop_rangefail & result_out_of_range)
    ) |>
    mutate(
      duplicate_result = n() > 1,
      reduced_result = if (any(eligible_row)) {
        reduce_fn(result[eligible_row])
      } else {
        NA_real_
      },
      keep_result = if (any(eligible_row)) {
        row_number() ==
          which(eligible_row & (is_mean | result == reduced_result))[1]
      } else {
        FALSE
      },
      .by = all_of(group_vars)
    ) |>
    mutate(reduced_result_out_of_range = out_of_range(test, reduced_result))

  # Drop, according to flags
  if (!flag_only) {
    if (drop_rangefail) {
      df <- filter_out(df, result_out_of_range)
    }

    if (drop_duplicate) {
      df <- filter(df, keep_result)

      if (is_mean) {
        # Swap in the group mean and carry its range flag across
        df <- df |>
          mutate(
            result = reduced_result,
            result_out_of_range = reduced_result_out_of_range
          )

        # Means that fail the range check are dropped too
        if (drop_rangefail) {
          df <- filter_out(df, result_out_of_range)
        }
      }
    }

    if (!keep_flags) {
      df <- select(df, -any_of(flag_cols))
    }
  }

  df
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
