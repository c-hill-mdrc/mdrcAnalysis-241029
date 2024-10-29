#' General linear regression extract
#'
#' Performs regression across a set of dependent variables.
#'
#' NOTE: This function does NOT provide heteroskedasticity-robust standard
#' errors. In general, staff will want to use heteroskedasticity-robust standard
#' errors, as in the \link[mdrcAnalysis]{hetreg_extract} function.
#'
#' @param .dataset      Input dataframe containing all needed variables
#' @param .dependents   Character vector of all column names for outcomes.
#' @param .treatment    Character vector of column name for
#' .treatment/independent variable
#' @param .covariates   Character vector of column names for other
#' predictors/explanatory variables
#' @param .asfactorcovs Character vector of .covariates that should be treated as
#' factors/categorical variables
#' @param .subgroup     Character vector of column name to be used to
#' segment/partition analysis, e.g. sites, cohorts, etc.
#' @param .wt_var       Character vector of column name to be used as weights
#' for linear model
#' @param .confintalpha decimal value of desired alpha level for confidence
#' intervals, i.e. 0.90, 0.95, 0.99. Default is 0.95
#'
#' @param .dep_labels Dataframe of labels for dependent variables.
#' Dataframe must have the columns `Dependent` and `Labels`
#' @param .output_path optional, path to output location
#' @param .output_file optional, filename for output file
#'
#' @param .inc_trail   RTU parameter to include trail in output
#' @param .inc_sample  RTU parameter to include sample size row in output
#'
#' @author Melvin Gutierrez
#'
#' @return data frame with one row per dependent with desired regression statistics
#'
#' @export
#'
#' @examples
#'# Functional Sample Call
#'
#' extract <-
#' lm_extract(.dataset    = sim_data_weighted,
#'            .dependents = c("employed_01", "employed_02"),
#'            .treatment  = c("treatment"),
#'            .covariates = c("eduLevel"),
#'            .wt_var     = c("genWeight")
#'            )
#'
#' extract
#'
lm_extract <- function (.dataset
                       ,.dependents
                       ,.treatment
                       ,.covariates   = NA_character_
                       ,.asfactorcovs = NA_character_
                       ,.subgroup     = NA_character_
                       ,.wt_var       = NA_character_
                       ,.confintalpha = 0.95
                       ,.dep_labels   = NA_character_
                       ,.output_path  = NA_character_
                       ,.output_file  = NA_character_
                       ,.inc_trail    = TRUE
                       ,.inc_sample   = TRUE
                       ) {

  # Parameter checks
  if (!is.data.frame(.dataset))   {stop("ERROR: lm_extract expects a data frame as the first argument.")}
  if (!is.character(.treatment))    {stop("ERROR: lm_extract expects a character vector of treatment." )}
  if (!is.character(.dependents))   {stop("ERROR: lm_extract expects a character vector of dependents.")}
  if (!is.character(.covariates))   {stop("ERROR: lm_extract expects a character vector of covariates.")}
  if (!is.character(.asfactorcovs)) {stop("ERROR: lm_extract expects a character vector of covariates to be treated as factors.")}
  if (!is.character(.wt_var))       {stop("ERROR: lm_extract expects a character vector of weights.")}
  if (length(attr(.dataset, "groups")) > 0) {stop("ERROR: data frame is grouped/rowwise. This may cause issues with calculations. Use ungroup() to remove.")}

  ##################################################################################
  # Minimum necessary data
  ##################################################################################
  # keep only needed variables
  .allvars <- setdiff(c({{.treatment}}
                        ,{{.dependents}}
                        ,{{.covariates}}
                        ,{{.subgroup}}
                        ,{{.wt_var}}), NA)

  .dataset01a <-
    .dataset %>%
    dplyr::filter(!is.na(.data[[.treatment]])) %>%
    dplyr::select(tidyselect::all_of(.allvars))

  if (nrow(.dataset) !=  nrow(.dataset01a)) {
    print(paste0("Records were removed due to ",
                 "missing values in Treatment Variable: ",
                 .treatment))
  } # End checking for missing .dependent values


  ## Missing weight variable
  if (!is.na(.wt_var)) {

    .dataset01b <-
      .dataset01a %>%
      dplyr::filter(!is.na({{.wt_var}}))

    if (nrow(.dataset01a) != nrow(.dataset01b)) {
      print(paste0("Records were removed due to ",
                   "missing values in Weight Variable: ",
                   .wt_var))
    }
  } else {
    .dataset01b <- .dataset01a
  }# End checking for missing .wt_var values

  ##################################################################################
  # 3 Converting variables to factors if requested as factors
  ##################################################################################
  # Check if covariates in asfactorcovs parameter are factors
  # If not, convert to factors.
  # Print message if converted to a factor.
  if(!any(is.na(.asfactorcovs))){

    # check which ones are factors
    are_factors <- .dataset01b %>%
      dplyr::select(all_of(.asfactorcovs)) %>%
      dplyr::summarise(across(everything(), is.factor))

    if(sum(are_factors) < length(.asfactorcovs)){

      # pulling out non factors
      nonFactorVars <- .asfactorcovs[which(are_factors == 0)]

      # print the non factors
      print(paste0("The following variables are not factors but were listed ",
                   "in the .asfactorcovs parameter.",
                   "They will be converted to factors for the analysis: ",
                   nonFactorVars))

      # convert to factors
      .dataset01c <-
        .dataset01b %>%
        dplyr::mutate(across(all_of(nonFactorVars), factor))

    } else {
      .dataset01c <- .dataset01b
    }
  } else {
    .dataset01c <- .dataset01b
  }

  ##################################################################################
  # 4 Create Data Frame of models to be run (per dependent)
  ##################################################################################
  # Pulling out the levels
  if (!missing(.subgroup)) {
    .by_values <- unique(.dataset[[.subgroup]])
  } else{
    .by_values <- NULL
  }

  # creating a list of data frames,
  # dependent variables and by values to run regressions on
  variables_list <- tidyr::expand_grid(".dataset" = list(.dataset01c),
                                       ".dependent" = .dependents,
                                       ".subgroup" = .subgroup,
                                       ".by_values" = .by_values)

  # Adding non varying additional parameters to the expanded list
  # to be supplied to the function
  variables_list$.treatment    <- .treatment
  variables_list$.covariates   <- list(.covariates)
  variables_list$.asfactorcovs <- list(.asfactorcovs)
  variables_list$.wt_var       <- .wt_var
  variables_list$.confintalpha <- .confintalpha

  models_df <- tidyr::as_tibble(variables_list)

  ##################################################################################
  # 5 Run all models
  ##################################################################################
  # Running all the models in one step with pmap and list_rbind
  # with the use of explicit functions
  OutputDS <-
    models_df %>%
    purrr::pmap(lm_helper) %>%
    purrr::list_rbind()

  # Running all the naive/NULL models
  # Create vector of identifier variables to be dropped later
  if (!is.na(.subgroup)) {
    ids <- c("Dependent", "SubGrpVal")
  } else {
    ids <- c("Dependent")
  }

  # If covariates are provided, set all to missing for naive/NULL models
  if (!any(is.na(.covariates))) {
    # Set covariate arguments to missing
    models_df$.covariates   <- NA_character_
    models_df$.asfactorcovs <- NA_character_

    # Run NULL models
    # Drop excess variables
    # Add Unadj_ prefix
    nullOutputDS <-
      models_df %>%
      purrr::pmap(lm_helper) %>%
      purrr::list_rbind() %>%
      dplyr::select(-tidyselect::contains("_CI_"),
                    -tidyselect::starts_with("EffectSize_"),
                    -ProbF,
                    -Stars_ProbF,
                    -tidyselect::starts_with("RSquare"),
                    -tidyselect::starts_with("StdDev"),
                    -tidyselect::starts_with("NObs"),
                    -tidyselect::any_of(ids)) %>%
      dplyr::rename_with(~ gsub("Adj", "", .x, fixed = TRUE), tidyselect::starts_with("AdjMean_")) %>%
      dplyr::rename_with(~paste0("Unadj_", .x),
                         c(tidyselect::everything(),
                           -UniqueId))
  } else {
    nullOutputDS <-
      OutputDS %>%
      dplyr::select(-tidyselect::contains("_CI_"),
                    -tidyselect::starts_with("EffectSize_"),
                    -ProbF,
                    -Stars_ProbF,
                    -tidyselect::starts_with("RSquare"),
                    -tidyselect::starts_with("NObs"),
                    -tidyselect::starts_with("StdDev"),
                    -tidyselect::any_of(ids)) %>%
      dplyr::rename_with(~ gsub("Adj", "", .x, fixed = TRUE), tidyselect::starts_with("AdjMean_")) %>%
      dplyr::rename_with(~paste0("Unadj_", .x),
                         c(tidyselect::everything(),
                           -UniqueId))
  }

  # Joining adjusted and unadjusted models
  # Arranging by UniqueId
  OutputDS <-
    OutputDS %>%
    dplyr::left_join(nullOutputDS, by = "UniqueId") %>%
    dplyr::arrange(UniqueId)


  ##################################################################################
  # 6 Calculating and joining summary statistics for input data
  ##################################################################################
  if (!is.na(.subgroup)) {
    UnadjStatsOverall <-
      mean_extract(
        .dataset = .dataset
        ,.subgroup = .subgroup
        ,.inc_sample = FALSE
        ,.inc_trail = FALSE
      )

    UnadjStatsOverall <-
      UnadjStatsOverall %>%
      dplyr::filter(Variable %in% .dependents) %>%
      dplyr::select(UniqueId
                    ,DepVarMean = Mean
                    ,DepVarSD   = Stddev
                    ,DepVarN    = n_total
                    ,DepVarNMiss= NMiss
      )

    OutputDS <-
      OutputDS %>%
      dplyr::left_join(UnadjStatsOverall, by = "UniqueId")

  } else {
    UnadjStatsOverall <-
      mean_extract(
        .dataset = .dataset
        ,.inc_sample = FALSE
        ,.inc_trail = FALSE
      )

    UnadjStatsOverall <-
      UnadjStatsOverall %>%
      dplyr::filter(Variable %in% .dependents) %>%
      dplyr::select(Dependent  = Variable
                    ,DepVarMean = Mean
                    ,DepVarSD   = Stddev
                    ,DepVarN    = n_total
                    ,DepVarNMiss= NMiss
      )

    OutputDS <-
      OutputDS %>%
      dplyr::left_join(UnadjStatsOverall, by = "Dependent")
  }

  ##################################################################################
  # 7 Finalize Data: Add labels, trail, sample columns, and write to excel if requested
  ##################################################################################

  # If no label dataframe provided, check for labels in data as attributes
  if (is.na(.dep_labels)) {
    if (purrr::map(.dataset, attr, "label") %>% unlist %>% length() > 0) {
      .dep_labels <-
        purrr::map(.dataset, attr, "label") %>%
        unlist() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        dplyr::rename("Dependent" = "rowname", "Label" = ".")
    } else {
      .dep_labels <- NA_character_
    }
  }

  # if the labels are provided or found, join labels on
  if(!all(is.na(.dep_labels))){
    OutputDS <-
      OutputDS %>%
      dplyr::left_join(.dep_labels, by = dplyr::join_by(Dependent)) %>%
      dplyr::relocate(Label, .after = Dependent)
  }

  # Adding the sample size function to the larger table
  if(.inc_sample == TRUE) {

    # pull out the sample counts
    sampleCounts <-
      sample_size(.dataset = .dataset,
                  .treatment = .treatment
      ) %>%
      dplyr::rename(NObs = n_total) %>%
      dplyr::rename_with(~stringr::str_replace(.x, "^n_", "NObs_" )
                         ,tidyselect::starts_with("n_"))

    # rename the sample count function first column
    # to the first column of the output tibble for join
    names(sampleCounts)[1] <- names(OutputDS)[1]

    OutputDS <-
      OutputDS %>%
      dplyr::bind_rows(sampleCounts)

  }

  # Adding the trail function
  if(.inc_trail == TRUE) {

    ## Capturing the function parameters
    call <- match.call.defaults()

    OutputDS <-
      OutputDS %>%
      dplyr::bind_cols(trail(call))
  }


  # Outputting to an excel spreadsheet
  if (!is.na(.output_path) & !is.na(.output_file)) {

    create_excel(
      .output_path = .output_path,
      .output_file = .output_file,
      .x = OutputDS
    )
  }

  return(OutputDS)

}



# Function for running single regression
## Function will take a single formula and create the output record
## Function will later be called on all formulas
lm_helper <-
  function(.dataset
          ,.dependent
          ,.treatment
          ,.covariates
          ,.asfactorcovs = NA_character_
          ,.subgroup     = NA_character_
          ,.by_values    = NA_character_
          ,.wt_var       = NA_character_
          ,.confintalpha = 0.95
          ) {

    # filtering out where dependent variable is missing.
    # By default, the lm model would take care of missing values
    # but this allows for explicit reporting of missingness

    .innervars <- setdiff(c({{.treatment}}
                          ,{{.dependent}}
                          ,{{.covariates}}
                          ,{{.subgroup}}
                          ,{{.wt_var}}), NA)

    .dataset01 <-
      .dataset %>%
      dplyr::filter(!is.na(.data[[.dependent]])) %>%
      dplyr::select(tidyselect::all_of(.innervars))

    if (nrow(.dataset) !=  nrow(.dataset01)) {
      print(paste0("Records were removed due to ",
                   "missing values in Dependent Variable: ",
                   .dependent))
    } # End checking for missing .dependent values

    # if we are not missing levels of a subgroup
    if (!missing(.by_values)) {

      # if the level is a missing level, filter the
      # data set to the missing level
      if (is.na(.by_values)) {
        .dataset02 <- .dataset01 %>%
          dplyr::filter(is.na(.data[[.subgroup]]))
      } else {
        # otherwise, filter the data set to the level
        .dataset02 <- .dataset01 %>%
          dplyr::filter(.data[[.subgroup]] == {{.by_values}})
      } # end is.na(.by_values)
    } else {
      .dataset02 <- .dataset01
    } # end !missing(.by_values)

    # Check that data has two levels of treatment
    if (length(names(table(.dataset02[.treatment]))) < 2) {
      if (!is.na(.subgroup)) {print(paste0("NOTE: Subgroup used. ",
                                           .subgroup,
                                           " = ",
                                           .by_values,
                                           "."))}


      stop(paste0(".treatment variable does not have two levels for dependent ",
                  .dependent, ". ",
                  "Please address in input data."))
    }

    # Check that data has two variation of dependent
    if (length(table(.dataset02[.dependent])) < 2) {
      print(paste("NOTE: Dependent variable ",
                  .dependent,
                  " has no variation and will not have regression performed."))
      SkipOutput <- TRUE
    } else {
      SkipOutput <- FALSE
    }

    # Check that more than 20 obs per predictor (treatment + covariates)
    if (!missing(.by_values) &
        (nrow(.dataset02) / length(c(.treatment, .covariates)) < 20)) {
      print(paste("WARNING: This function has been called to run a separate",
                  "regression for each subpopulation. As a result, the",
                  "relationship between covariates and outcomes is estimated",
                  "separately for each subpopulation. One of your subpopulations",
                  "has ", nrow(.dataset02), " total observations and ",
                  length(c(.treatment, .covariates)), " predictors. This results",
                  "in fewer than 20 observations per predictor, a rule of thumb",
                  "for sufficient sample for these analyses. Consider whether it",
                  "is more appropriate to conduct a pooled analysis, where",
                  "treatment by subgroup interaction terms are used to obtain",
                  "effect estimates for each subpopulation, and the",
                  "covariate-outcome relationship is instead estimated using the",
                  "full sample. ", "Outcome: ", .dependent, ". Subgroup: ",
                  .subgroup, ". By Level: ", .by_values, "."))
    }

    # creating formula for linear model
    modFormula.obj <- create_formula(
      .TmntVar = .treatment,
      .CoVars = unlist(.covariates),
      .DepVar = .dependent
    )

    # pulling out the formula object
    modFormula <- modFormula.obj$modFormula

    # fitting the linear model on the filtered data set
    if (!is.na(.wt_var)) {
      .reg <- as.formula(modFormula)
      environment(.reg) <- environment()
      lmfit <- stats::lm(.reg,
                         data    = .dataset02,
                         weights = .dataset02[[.wt_var]])
    } else {
      lmfit <- stats::lm(modFormula,
                         data = .dataset02)
    }

    # Calculate sample by treatment
    Obs <-
      broom::augment(lmfit) %>%
      dplyr::count(.data[[.treatment]]) %>%
      tidyr::pivot_wider(names_from = 1, names_prefix = "NObs_", values_from = n) %>%
      dplyr::mutate(NObs = sum(dplyr::c_across(tidyselect::starts_with("NObs_"))))

    # Apply emmeans to linear model
    # Change reference grid to treat only categorical variables as factors
    if (length(setdiff(.asfactorcovs, NA) > 0)) {
      covkeep <- c(.treatment, .asfactorcovs)
    } else {
      covkeep <- .treatment
    }

    # Calculating and restructuring estimates/AdjMeans
    lm_rg <- emmeans::ref_grid(lmfit, data = .dataset02, cov.keep = covkeep)
    emm <- emmeans::emmeans(lm_rg, data = .dataset02, specs = .treatment)

    Means <-
      broom::tidy(emm) %>%
      dplyr::select(all_of(.treatment), "estimate") %>%
      tidyr::pivot_wider(names_from = .treatment, names_prefix = "AdjMean_", values_from = "estimate") %>%
      dplyr::bind_cols(Dependent = .dependent)

    # Grab treatment values for naming variables
    treatsuffix <-
      sort(broom::tidy(emm)[[.treatment]], decreasing = TRUE) %>%
      paste0(collapse = "_")

    Impact_SE <-
      broom::tidy(lmfit) %>%
      dplyr::filter(stringr::str_detect(term, paste0("^", .treatment))) %>%
      dplyr::select(estimate, std.error, p.value, statistic) %>%
      dplyr::mutate(stars = create_stars(p.value)) %>%
      dplyr::relocate(statistic, .after = stars) %>%
      dplyr::rename_with(~ paste(c("Impact", "StdErr", "ProbT", "Stars", "Tstat"),
                                 treatsuffix, sep = "_"),
                         c(estimate, std.error, p.value, stars, statistic)) %>%
      dplyr::bind_cols(Dependent = .dependent)

    # Apply confint to each linear model
    ## level based on argument provided
    ## default is .95

    if (.confintalpha == 0.90) {
      low_name <- "5 %"
      high_name <- "95 %"
    }
    else if (.confintalpha == 0.95) {
      low_name <- "2.5 %"
      high_name <- "97.5 %"
    }
    else if (.confintalpha == 0.99) {
      low_name <- "0.5 %"
      high_name <- "99.5 %"
    }

    ### estimated means CIs
    ### dropping unnecessary columns
    estCIs <-
      confint(emm, parm = "estimate", level = .confintalpha) %>%
      broom::tidy() %>%
      dplyr::select(-estimate, -std.error, -df) %>%
      dplyr::rename(CI_L = conf.low,
                    CI_U = conf.high) %>%
      tidyr::pivot_wider(names_from = .treatment, values_from = tidyselect::starts_with("CI_")) %>%
      dplyr::rename_with(~paste0("AdjMean_", .x))

    ### estimated impact CIs
    impCIs <-
      confint(lmfit, level = .confintalpha) %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      dplyr::filter(stringr::str_detect(rowname, paste0("^",.treatment))) %>%
      dplyr::rename(Impact_CI_L = all_of(low_name),
                    Impact_CI_U = all_of(high_name)) %>%
      dplyr::select(tidyselect::starts_with("Impact_CI_"))

    # Calculate Effect Sizes
    ## Calculate dependent overall standard deviation
    VarStdDev <-
      broom::augment(lmfit) %>%
      dplyr::select({{.dependent}}) %>%
      dplyr::summarise(StdDev = sd(.data[[.dependent]]))

    ## Collecting dependent standard deviations by group
    VarStdDevG <-
      broom::augment(lmfit) %>%
      dplyr::select({{.dependent}}, .treatment) %>%
      dplyr::group_by(.data[[.treatment]]) %>%
      dplyr::summarise(StdDev = sd(.data[[.dependent]])) %>%
      tidyr::pivot_wider(names_from = .treatment, values_from = "StdDev", names_prefix = "StdDev_")

    ## Combine necessary data frames and calculate Effect Sizes
    treat_lvls <- broom::tidy(emm)[[.treatment]]

    ES <-
      Impact_SE %>%
      dplyr::bind_cols(Obs, VarStdDev, VarStdDevG) %>%
      dplyr::mutate(EffectSize_Overall = get(paste0("Impact_",treatsuffix)) / StdDev,
                    EffectSize_Control = get(paste0("Impact_",treatsuffix)) / get(paste0("StdDev_", treat_lvls[1])),
                    EffectSize_Pooled  = get(paste0("Impact_",treatsuffix)) /
                      sqrt((
                        ((get(paste0("NObs_",treat_lvls[1]))-1)*get(paste0("StdDev_",treat_lvls[1]))^2) +
                          ((get(paste0("NObs_",treat_lvls[2]))-1)*get(paste0("StdDev_",treat_lvls[2]))^2)) /
                          (get(paste0("NObs_",treat_lvls[1])) + get(paste0("NObs_",treat_lvls[2])) - 2)
                      ) # close sqrt function (pooled ES denominator)
      ) %>% # close mutate parenthesis
      dplyr::select(c(tidyselect::starts_with("EffectSize_"),tidyselect::starts_with("StdDev_")))

    # Calculating the F statistic
    lmsummary <- stats::summary.lm(lmfit)
    lmsumF <- lmsummary$fstatistic
    Fstat <- stats::pf(lmsumF[1], lmsumF[2], lmsumF[3], lower.tail = F)

    # Getting the RSquare
    RSquare <- lmsummary$r.squared
    AdjRSquare <- lmsummary$adj.r.squared

    Output <-
      dplyr::full_join(Means, Impact_SE, by = "Dependent") %>%
      dplyr::bind_cols(ES) %>%
      dplyr::bind_cols(estCIs) %>%
      dplyr::bind_cols(impCIs) %>%
      dplyr::bind_cols(ProbF = Fstat, RSquare = RSquare, AdjRSquare = AdjRSquare) %>%
      dplyr::mutate(Stars_ProbF = create_stars(ProbF)) %>%
      dplyr::relocate(Stars_ProbF, .after = ProbF) %>%
      dplyr::relocate(tidyselect::starts_with("StdDev_"), .after = tidyselect::starts_with("EffectSize_") ) %>%
      dplyr::bind_cols(Obs) %>%
      dplyr::relocate(Dependent)

    # Attaching by column if specified
    if (!missing(.by_values)) {

      Output <-
        Output %>%
        dplyr::mutate(SubGrpVal := {{.by_values}},
                      UniqueId = paste(SubGrpVal, Dependent, sep = "_")) %>%
        dplyr::relocate(UniqueId, SubGrpVal)
    } else {

      Output <-
        Output %>%
        dplyr::mutate(UniqueId = paste(Dependent)) %>%
        dplyr::relocate(UniqueId)
    }

    if (SkipOutput) {
     Output <-
       Output %>%
       dplyr::select(UniqueId, Dependent)
    }

    return(Output)

}
