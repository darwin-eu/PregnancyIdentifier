getTblRowCount <- function(tbl) {
  tbl %>%
    dplyr::count() %>%
    dplyr::pull(.data$n)
}
