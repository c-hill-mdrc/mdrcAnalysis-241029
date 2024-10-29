#' crosstab_extract main function
#'
#' For creating crosstabs for an entire data frame. Options include creating
#' crosstabs by subgroup, including missing values, and including chi-square
#' test results.
#'
#' @param .dataset required, data frame with .col_var and .row_var
#' @param .col_var required, one variable for columns of cross-tabulation
#' @param .row_vars optional, character vector of variables in rows of cross-
#'  tabulation. All variables are included if not provided.
#' @param .subgroup optional, character vector of variables to group the data by
#' @param .chisq optional, Boolean, include chi-square test, default is FALSE
#' @param .missing optional, Boolean, include missing values of row variable(s)
#'  in percentage calculations, default is FALSE. Missing counts of column
#'  variable and row variable(s) are always reported in the output table.
#'  Missing values of all variables will be included in chi-square results if
#'  .missing is set to TRUE and chi-square is requested.
#' @param .max_lvls optional, maximum number of levels for a variable to be
#'  included in the cross-tabulations. Default is 24.
#' @param .row_labels optional, data frame with variable labels. Data frame
#'  should have Variable and Label columns.
#' @param .inc_trail   RTU parameter to include trail in output
#' @param .output_path optional, path to output location
#' @param .output_file optional, filename for output file
#'
#' @returns A tibble of all the cross-tabulations
#'
#' @export crosstab_extract
#'
#' @examples
#' crosstab_extract(.dataset  = bif_final
#'                 ,.col_var  = "RA_CODE"
#'                 ,.row_vars = c("blmale", "bldiplomas_as")
#'                 )
#'
crosstab_extract <-
  function(.dataset
          ,.col_var
          ,.row_vars    = NA_character_
          ,.subgroup    = NA_character_
          ,.chisq       = FALSE
          ,.missing     = FALSE
          ,.max_lvls    = 24
          ,.row_labels  = NA_character_
          ,.inc_trail   = TRUE
          ,.output_path = NA_character_
          ,.output_file = NA_character_
          )
    {

    # drop unnecessary variables

    ## if no .row_vars were provided, include all variables
    if (missing(.row_vars)) {
      .row_vars <- setdiff(names(.dataset),c(.col_var, .subgroup))
    } # end if no .row_vars provided

    ## check all .row_vars for .max_levels
    ### store all requested row variables
    old_rvs <- .row_vars

    ### create list of row variables that are less than max_lvls
    .row_vars <-
      .dataset |>
      dplyr::select(tidyselect::all_of({{.row_vars}})) |>
      dplyr::select(
        tidyselect::where(
          ~dplyr::n_distinct(., na.rm = !.missing) <= .max_lvls)) |>
      names()

    ### List any removed variables and report
    rmvd_rvs <- setdiff(old_rvs, .row_vars)

    if (length(rmvd_rvs) > 0) {
      print(paste("The following requested variables exceed the maximum number"
                  ,"of levels and were removed from analysis:"
                  ,paste(rmvd_rvs, collapse = ", ")))
    }

    ## create vector of necessary variables
    cols <- c(.col_var, .row_vars, .subgroup)

    ## create vector of necessary variables available in input data frame
    keep <- intersect(cols, names(.dataset))

    ## create data frame with only necessary variables
    keepdf <-
      .dataset |>
      dplyr::select(tidyselect::all_of(keep))

    # run helpers on each combination of .subgroup and .row_var
    if (missing(.subgroup)) {

      # no subgroups, iterate over row variables
      outdf <-
        purrr::map(.row_vars, \(x) crosstab_stats(.dataset = keepdf
                                                  ,.col_var = {{.col_var}}
                                                  ,.row_var = x
                                                  ,.missing = {{.missing}}
                                                  ,.chisq   = {{.chisq}}
                                                  )
                   ) |>
        purrr::list_rbind() |>
        dplyr::select(UniqueID, Variable , Value,
                      dplyr::starts_with("Freq"),
                      dplyr::starts_with("CumFreq"),
                      dplyr::starts_with("Pct"),
                      dplyr::starts_with("CumPct"),
                      dplyr::everything()) |>
        dplyr::mutate(across(where(is.numeric), ~tidyr::replace_na(.,0)))

    } else {

      # subgroups, iterate over groups and row variables

      # first create split data frames
      groupdf <-
        keepdf |>
        dplyr::group_by(dplyr::pick({{.subgroup}}))

      # iterate over row variables and subgroups
      outdf <-
        purrr::map(.row_vars, \(y) {
          dplyr::group_map(groupdf, ~
            crosstab_stats(.dataset = .x
                           ,.col_var = {{.col_var}}
                           ,.row_var = y
                           ,.missing = {{.missing}}
                           ,.chisq   = {{.chisq}}
            )
          ) |>
            purrr::list_rbind(names_to = "Subgroup") |>
            dplyr::mutate(Subgroup = paste0(.subgroup
                                          ,"_"
                                          ,dplyr::group_keys(groupdf)[[1]][Subgroup])
           )
        }) |>
        purrr::list_rbind() |>
        dplyr::select(Subgroup, UniqueID, Variable , Value,
                      dplyr::starts_with("Freq"),
                      dplyr::starts_with("CumFreq"),
                      dplyr::starts_with("Pct"),
                      dplyr::starts_with("CumPct"),
                      dplyr::everything()) |>
        dplyr::mutate(across(where(is.numeric), ~tidyr::replace_na(.,0)))


    } # end looping over subgroups and row variables

    # add row variable labels if provided
    if (!is.na(.row_labels)) {
      outdf <-
        dplyr::left_join(outdf, {{.row_labels}}, by = "Variable") |>
        dplyr::relocate(Label, .after = Variable)
    } # end adding row labels

    # Adding the trail
    if(.inc_trail == TRUE) {
      outdf <-
        outdf |>
        dplyr::bind_cols(trail(call = match.call.defaults()))
    }

    # Outputting to an excel spreadsheet
    if (!missing(.output_path) & !missing(.output_file)) {
      create_excel(.output_path = .output_path,
                   .output_file = .output_file,
                   .x = outdf
                   )
      } # end exporting to Excel if provided

  return(outdf)
}


#' crosstab_extract helper which runs the crosstab for a pair of variables
#'
#' Takes input data frame, creates cross-tabulation, calculates desired
#' frequencies and percentages. Also runs chi-square test, if requested.
#'
#' @param .dataset required, data frame with .col_var and .row_var
#' @param .col_var required, variable in columns of cross-tabulation
#' @param .row_var required, variable in rows of cross-tabulation
#' @param .missing optional, Boolean, include missing values, default is FALSE
#' @param .chisq optional, Boolean, request chi-square test, default is FALSE
#'
#' @return tibble of single variable cross-tab with chi-square, if requested
#'
crosstab_stats <-
  function(.dataset
           ,.col_var
           ,.row_var
           ,.missing = FALSE
           ,.chisq   = FALSE
           ) {

    # Create tibble for all combinations of col_var and row_var
    # Ensure all factor levels are present, even if that level is not in dataset

    row.fct <- is.factor(.dataset[[.row_var]])
    col.fct <- is.factor(.dataset[[.col_var]])

    row.vals <- unique(.dataset[[.row_var]])
    col.vals <- unique(.dataset[[.col_var]])

    if (row.fct & col.fct){

      # Ensure factor levels AND missing values are captured
      grid <- tidyr::expand_grid(union(levels(.dataset[[.col_var]]),col.vals),
                                 union(levels(.dataset[[.row_var]]),row.vals))


    } else if (row.fct & !col.fct){

      grid <- tidyr::expand_grid(col.vals,
                                 union(levels(.dataset[[.row_var]]),row.vals))

    } else if (!row.fct & col.fct){

      grid <- tidyr::expand_grid(union(levels(.dataset[[.col_var]]),col.vals),
                                 row.vals)
    } else {

      grid <- tidyr::expand_grid(col.vals, row.vals)
    }

    names(grid) <- c({{.col_var}},{{.row_var}})


    # Get frequencies
    initial.crosstab <- .dataset |>
      dplyr::count(dplyr::pick({{.col_var}})
                  ,dplyr::pick({{.row_var}}))

    # Join frequencies to grid, fix missing values, create UniqueID column
    crosstab <- grid |>
      dplyr::left_join(initial.crosstab, by = c({{.col_var}},{{.row_var}})) |>
      dplyr::mutate(n = tidyr::replace_na(n,0)) |>
      dplyr::mutate(dplyr::across(dplyr::everything()
                                 ,~ifelse(is.na(.),"NA",.))) |>
      dplyr::mutate(UniqueID = paste({{.row_var}}
                                     ,.data[[{{.row_var}}]]
                                     ,sep = "_"))

    # Calculate percentages
    # if .missing = T, include row_var NA in calculation
    if (.missing){
      crosstab <- crosstab |>
        dplyr::group_by(dplyr::pick({{.col_var}})) |>
        dplyr::mutate(CumFreq = cumsum(n)
                     ,Pct     = ifelse(CumFreq>0,n/max(CumFreq)*100,0)
                     ,CumPct  = cumsum(Pct)) |>
        dplyr::ungroup()

    }else{

      missing.rows <- crosstab |> dplyr::filter(.data[[{{.row_var}}]] == "NA")

      crosstab <- crosstab |>
        dplyr::filter(.data[[{{.row_var}}]] != "NA") |>
        dplyr::group_by(dplyr::pick({{.col_var}})) |>
        dplyr::mutate(CumFreq = cumsum(n)
                     ,Pct     = ifelse(CumFreq>0,n/max(CumFreq)*100,0)
                     ,CumPct  = cumsum(Pct)) |>
        dplyr::ungroup() |>
        dplyr::bind_rows(missing.rows)
    }

    crosstab <- crosstab |>
      tidyr::pivot_longer(cols = {{.row_var}},
                          names_to = "Variable") |>
      dplyr::mutate(Value = as.character(.data[["value"]]), .keep = "unused") |>
      dplyr::rename(Freq = n) |>
      tidyr::pivot_wider(names_from = tidyselect::all_of(.col_var)
                        ,values_from = c(Freq, CumFreq, Pct, CumPct)
                        ,values_fill = 0)


    # If requested, add chisq
    ## if .missing = T, include NAs in the calculations
    if (.chisq) {

      if(.missing){
        .dataset <- .dataset |>
          dplyr::mutate(dplyr::across(dplyr::everything()
                                      ,~ifelse(is.na(.),"NA",.)))

        print("Missing values are included in chi-square calculations. To exclude missing values, set the .missing parameter to FALSE.")
      }

      # checking if .col_var and .row_var have at least 2 levels each
      clvls <- dplyr::n_distinct(.dataset[[.col_var]])
      rlvls <- dplyr::n_distinct(.dataset[[.row_var]])

      if (clvls >= 2 & rlvls >= 2) {
        # calculating chisquare estimation
        chisqr <-
          broom::tidy(
            suppressWarnings(
              stats::chisq.test(x = dplyr::pull(.dataset, var = {{.col_var}})
                               ,y = dplyr::pull(.dataset, var = {{.row_var}})
                               ,correct = FALSE)
            )
          )|>
          dplyr::mutate(ChiSqStars = create_stars(p.value)) |>
          dplyr::relocate(ChiSqStars, .after = "p.value") |>
          dplyr::rename(ChiSqStatistic = statistic
                       ,ChiSqPValue    = p.value
                       )


      } else {
        print(paste("Variables"
                   ,.col_var
                   ,"or"
                   ,.row_var
                   ,"have fewer than 2 levels. Cannot calculate chi-square."))

        # create empty tibble
        chisqr <-
          tibble::tibble(ChiSqStatistic = NA
                        ,ChiSqPValue    = NA
                        ,parameter      = NA
                        ,method         = NA_character_
                        )
      }

      # Combine results
      crosstab <-
        dplyr::bind_cols(crosstab, chisqr)
    }

    return(crosstab)
}

