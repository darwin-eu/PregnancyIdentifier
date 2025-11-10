summariseCategory <- function(df, colName) {
  df %>%
    dplyr::group_by(.data[[colName]]) %>%
    dplyr::summarise(
      n = n()
    ) %>%
    dplyr::mutate(
      colName = colName,
      total = sum(.data$n),
      pct = .data$n / .data$total * 100
    ) %>%
    dplyr::relocate("colName")
}

summariseNumeric <- function(df, colName) {
  df %>%
    dplyr::summarise(
      colName = colName,
      min = min(.data[[colName]], na.rm = TRUE),
      Q25 = stats::quantile(.data[[colName]], probs = 0.25, na.rm = TRUE),
      median = stats::median(.data[[colName]], na.rm = TRUE),
      Q75 = stats::quantile(.data[[colName]], probs = 0.75, na.rm = TRUE),
      max = max(.data[[colName]], na.rm = TRUE),
      mean = mean(.data[[colName]], na.rm = TRUE),
      sd = stats::sd(.data[[colName]], na.rm = TRUE)
    ) %>%
    dplyr::tibble()
}

summariseColumn <- function(df, colName) {
  sampleClass <- df %>%
    head() %>%
    dplyr::pull(.data[[colName]]) %>%
    class()

  switch(
    sampleClass,
    "numeric" = {
      summariseNumeric(df, colName)
    },
    "integer" = {
      summariseNumeric(df, colName)
    },
    "character" = {
      summariseCategory(df, colName)
    },
    "logical" = {
      summariseCategory(df, colName)
    }
  )
}
