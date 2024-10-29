#' Homogeneity of Observed Effects Extract
#'
#' The HT statistic is a test of homogeneity of observed effects (impacts).
#' The HT statistic is used to check whether impacts across subgroups are
#' statistically different.
#'
#' Greenberg, D., Meyer, R. H., & Wiseman, M. (1994). Multisite Employment
#' and Training Program Evaluations: A Tale of Three Studies. Industrial and
#' Labor Relations Review, 47(4), 679–691. https://doi.org/10.2307/2524666
#'
#' The paper is available in the QMG SharePoint site. Please reach out if there
#' are questions.
#'
#' @param .dataset     Input data, an extract with subgroups from
#' \link[mdrcAnalysis]{lm_extract}, \link[mdrcAnalysis]{hetreg_extract},
#' \link[mdrcAnalysis]{clusterreg_extract}
#'
#' @param .output_path file path to output location
#' @param .output_file output file name
#'
#' @return Tibble displaying model outputs over all combinations of
#' dependents and subgroup values
#'
#'
#' @export
ht_extract <- function(.dataset,
                       .output_path = NA_character_,
                       .output_file = NA_character_) {

  # Parameter checks
  if (!is.data.frame(.dataset)) {
    stop("ERROR: ht_extract expects an extract data frame as the first argument.")
  }

  if (length(.dataset[["SubGrpVal"]]) < 1) {
    stop("ERROR: ht_extract expects an extract with subgroup calculations.")
  }

  # Grab column names
  .StdErr <- names(dplyr::select(.dataset,
                                 tidyselect::starts_with("StdErr_")))
  .Impact <- names(dplyr::select(.dataset,
                                 tidyselect::starts_with("Impact_"),
                                -tidyselect::starts_with("Impact_CI")))
  .Stars  <- names(dplyr::select(.dataset,
                                 tidyselect::starts_with("Stars_"),
                                -Stars_ProbF))

  # calculate the sum of impacts (expressed in squared se) divided by
  # the sum of groups (expressed in squared se terms) ~ a weighted mean
  # impact across groups
  theta_den <-
    .dataset %>%
    dplyr::mutate(isesq = (.data[[.StdErr]]^2),
                  numtheta = .data[[.Impact]]/isesq,
                  dentheta = 1/isesq) %>%
    dplyr::group_by(Dependent) %>%
    dplyr::summarise(Num_Den = sum(numtheta)/sum(dentheta),
                     .groups = "drop")

  # calculate the homogeneity of Impacts as
  # the sum of each groups deviation from the weighted mean impact across groups
  # (group impact minus the weighted mean impact across groups in
  # squared standard errors) divided by the squared standard error
  HTtest <-
    .dataset %>%
    dplyr::left_join(theta_den, by = "Dependent") %>%
    dplyr::mutate(HTi = ((.data[[.Impact]]-Num_Den)^2) / (.data[[.StdErr]]^2)) %>%
    dplyr::group_by(Dependent) %>%
    dplyr::summarise(HT=sum(HTi),
                     .groups = "drop")

  # calculate the probability of the homogeneity of impacts to be random
  # p value
  ProbHTtest <-
    .dataset %>%
    dplyr::left_join(HTtest, by = "Dependent") %>%
    dplyr::group_by(Dependent,HT) %>%
    dplyr::summarise(ProbHT = 1 - stats::pchisq(unique(HT),dplyr::n()-1),
                     .groups = "drop") %>%
    dplyr::mutate(HT_Stars = create_stars(ProbHT)) %>%
    dplyr::mutate(HT_Stars = stringr::str_replace_all(HT_Stars, stringr::fixed("*"), "\u2020"))

  # stack the HT statistics onto the subgroup statistics and reorder the results
  Output <-
    .dataset %>%
    dplyr::arrange(Dependent, SubGrpVal) %>%
    dplyr::left_join(ProbHTtest, by = "Dependent") %>%
    dplyr::relocate(HT, ProbHT, HT_Stars,
                    .after = .Stars)

  # Document which functions were used
  if (stringr::str_detect(unique(.dataset["FunctionParams"]), ".hcc_method")) {
    .Functions <- "hetreg_extract + ht_extract"
  } else if (stringr::str_detect(unique(.dataset["FunctionParams"]), ".cluster_var")) {
    .Functions <- "clusterreg_extract + ht_extract"
  } else {
    .Functions <- "lm_extract + ht_extract"
  }

  Output <-
    Output %>%
    dplyr::mutate(Functions = .Functions)

  # Outputting to an excel spreadsheet
  if (!is.na(.output_path) & !is.na(.output_file)) {

    create_excel(
      .output_path = .output_path,
      .output_file = .output_file,
      .x = Output
    )
  }

  return(Output)
}
