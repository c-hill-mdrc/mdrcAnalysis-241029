# 1. crosstab_extract with no grouping
testthat::test_that("1. crosstab_extract with no grouping",{

  # load crosstab extract without by from sas
  sasctnoby <-
    readxl::read_excel(
      path = testthat::test_path("sas_tests/crosstabcomp.xlsx"),
      sheet = "noby") |>
    dplyr::select(Variable
                  ,Value
                  ,Freq_0 = Frequency_0
                  ,Freq_1 = Frequency_1
                  ,Pct_0  = ColPct_0
                  ,Pct_1  = ColPct_1
                  ,ChiSqStatistic = ChiSqValue
                  ,ChiSqPValue    = ChiSqProb
    ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")) |>
    dplyr::mutate(across(where(is.numeric), ~tidyr::replace_na(.,0))) |>
    dplyr::arrange(Variable, Value)


  # create crosstab_extract without by in R
  rctnoby <- crosstab_extract(.dataset = sim_data_robust_reg
                             ,.col_var = "treatment"
                             ,.max_lvls = 10
                             ,.chisq = TRUE
                              ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")) |>
    dplyr::mutate(Pct_0 = round(Pct_0, 3)
                 ,Pct_1 = round(Pct_1, 4)
                 ,ChiSqPValue    = round(ChiSqPValue   , 3)
                 ,ChiSqStatistic = round(ChiSqStatistic, 4)
                  ) |>
    dplyr::select(Variable
                 ,Value
                 ,tidyselect::starts_with("Freq_")
                 ,tidyselect::starts_with("Pct_")
                 ,tidyselect::starts_with("Chi")
                 ,-ChiSqStars
                 ) |>
    dplyr::arrange(Variable, Value)

  #Perform Test
  testthat::expect_equal(object    = sasctnoby,
                         expected  = rctnoby,
                         tolerance = 0.001,
                         ignore_attr = "names")
  })


# 2. crosstab_extract with grouping
testthat::test_that("2. crosstab_extract with grouping",{

  # load crosstab extract without by from sas
  sasctby <-
    readxl::read_excel(
      path = testthat::test_path("sas_tests/crosstabcomp.xlsx"),
      sheet = "by") |>
    dplyr::filter(Value != "Total") |>
    dplyr::select(Subgroup = SubGrpVal
                  ,Variable
                  ,Value
                  ,Freq_0 = Frequency_0
                  ,Freq_1 = Frequency_1
                  ,Pct_0  = ColPct_0
                  ,Pct_1  = ColPct_1
                  ,ChiSqStatistic = ChiSqValue
                  ,ChiSqPValue    = ChiSqProb
    ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")
                 ,Subgroup = paste0("cities_", Subgroup)
                 ) |>
    dplyr::mutate(across(where(is.numeric), ~tidyr::replace_na(.,0))) |>
    dplyr::arrange(Variable, Value)

  # create crosstab_extract without by in R
  rctby <- crosstab_extract(.dataset = sim_data_robust_reg
                              ,.col_var = "treatment"
                              ,.subgroup = "cities"
                              ,.max_lvls = 10
                              ,.chisq = TRUE
  ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")) |>
    dplyr::select(Subgroup
                  ,Variable
                  ,Value
                  ,tidyselect::starts_with("Freq_")
                  ,tidyselect::starts_with("Pct_")
                  ,tidyselect::starts_with("Chi")
                  ,-ChiSqStars
    ) |>
    dplyr::arrange(Variable, Value)

  #Perform Test
  testthat::expect_equal(object    = sasctby,
                         expected  = rctby,
                         tolerance = 0.001,
                         ignore_attr = "names")
})


# 3. crosstab_extract with factors
testthat::test_that("3. crosstab_extract with factors",{

  # create crosstab_extract without factors in R
  rctnofct <- crosstab_extract(.dataset = sim_data_robust_reg
                               ,.col_var = "treatment"
                               ,.subgroup = "cities"
                               ,.max_lvls = 10
                               ,.chisq = TRUE
  ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")) |>
    dplyr::select(Subgroup
                  ,Variable
                  ,Value
                  ,tidyselect::starts_with("Freq_")
                  ,tidyselect::starts_with("Pct_")
                  ,tidyselect::starts_with("Chi")
                  ,-ChiSqStars
    ) |>
    dplyr::arrange(Variable, Value)

  # create crosstab_extract with factors in R
  rctfct <- crosstab_extract(.dataset = sim_data_robust_reg |>
                               dplyr::mutate(gender = factor(gender))
                            ,.col_var = "treatment"
                            ,.subgroup = "cities"
                            ,.max_lvls = 10
                            ,.chisq = TRUE
  ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")) |>
    dplyr::select(Subgroup
                  ,Variable
                  ,Value
                  ,tidyselect::starts_with("Freq_")
                  ,tidyselect::starts_with("Pct_")
                  ,tidyselect::starts_with("Chi")
                  ,-ChiSqStars
    ) |>
    dplyr::arrange(Variable, Value)

  #Perform Test
  testthat::expect_equal(object    = rctfct,
                         expected  = rctnofct,
                         tolerance = 0.001,
                         ignore_attr = "names")
})


# 3. crosstab_extract with factors and extra levels
testthat::test_that("3. crosstab_extract with factors and extra levels",{

  # create crosstab_extract without factors in R
  rctnofct <- crosstab_extract(.dataset = sim_data_robust_reg
                               ,.col_var = "treatment"
                               ,.subgroup = "cities"
                               ,.max_lvls = 10
                               ,.chisq = TRUE
  ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")) |>
    dplyr::select(Subgroup
                  ,Variable
                  ,Value
                  ,tidyselect::starts_with("Freq_")
                  ,tidyselect::starts_with("Pct_")
                  ,tidyselect::starts_with("Chi")
                  ,-ChiSqStars
    ) |>
    dplyr::arrange(Subgroup, Variable, Value)

  # create crosstab_extract with factors in R
  rctfct <- crosstab_extract(.dataset = sim_data_robust_reg |>
                               dplyr::mutate(gender = factor(gender, levels=c(0,1,2)))
                             ,.col_var = "treatment"
                             ,.subgroup = "cities"
                             ,.max_lvls = 10
                             ,.chisq = TRUE
  ) |>
    dplyr::mutate(Value = stringr::str_trunc(Value, 10, "right", "")) |>
    dplyr::select(Subgroup
                  ,Variable
                  ,Value
                  ,tidyselect::starts_with("Freq_")
                  ,tidyselect::starts_with("Pct_")
                  ,tidyselect::starts_with("Chi")
                  ,-ChiSqStars
    ) |>
    dplyr::arrange(Subgroup, Variable, Value)|>
    dplyr::filter(!(Variable == "gender" & Value == "2"))

  #Perform Test
  testthat::expect_equal(object    = rctfct,
                         expected  = rctnofct,
                         tolerance = 0.001,
                         ignore_attr = "names")
})
