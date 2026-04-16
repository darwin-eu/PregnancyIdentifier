# 31-quality-metrics.R — Episode Quality Metrics summary table
#
# Computes quality metrics across all databases using the same data loaded
# into the Shiny app, ensuring consistency with other tabs.

qualityMetricsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Episode Quality Metrics"),
    p("Summary quality metrics across databases and alogorithm versions."),
    br(),
    gt::gt_output(ns("quality_table")) %>% withSpinner(type = 6),
    br(),
    downloadButton(ns("download_csv"), "Download table (.csv)"),
    downloadButton(ns("download_docx"), "Download table (.docx)")
  )
}

qualityMetricsServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # Build quality metrics from app data
    quality_data <- reactive({
      allDP <- rv$allDP
      if (is.null(allDP) || length(allDP) == 0) return(tibble::tibble())

      plaus <- rv$gestationalWeeksPlausibility
      over52 <- rv$gestationalWeeksOver52
      overlap <- rv$pregnancyOverlapCounts
      oc <- rv$outcomeCategoriesCount

      purrr::map_dfr(allDP, function(db) {

        # --- Total episodes: from hipps outcome categories (sum of all n) ---
        # Matches the episode frequency tab which uses episode_frequency.csv
        total_episodes <- NA_integer_
        if (!is.null(oc) && nrow(oc) > 0 &&
            "algorithm" %in% colnames(oc) && "n" %in% colnames(oc)) {
          oc_db <- oc %>%
            dplyr::filter(.data$cdm_name == db)
          algo_ep <- if ("hipps" %in% oc_db$algorithm) "hipps" else "hip"
          oc_ep <- oc_db %>% dplyr::filter(.data$algorithm == algo_ep)
          if (nrow(oc_ep) > 0) {
            total_episodes <- as.integer(sum(oc_ep$n, na.rm = TRUE))
          }
        }

        # --- GW Plausibility % (overall across outcome categories) ---
        pct_plausible <- NA_real_
        if (!is.null(plaus) && nrow(plaus) > 0) {
          plaus_db <- plaus %>%
            dplyr::filter(.data$cdm_name == db) %>%
            dplyr::group_by(.data$plausibility) %>%
            dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
            dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1))
          plaus_row <- plaus_db %>% dplyr::filter(.data$plausibility == "Plausible")
          if (nrow(plaus_row) > 0) pct_plausible <- plaus_row$pct[1]
        }

        # --- >52 weeks % (overall across outcome categories) ---
        pct_over_364 <- NA_real_
        if (!is.null(over52) && nrow(over52) > 0) {
          o52_db <- over52 %>%
            dplyr::filter(.data$cdm_name == db) %>%
            dplyr::group_by(.data$over_52_weeks) %>%
            dplyr::summarise(n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
            dplyr::mutate(pct = round(100 * .data$n / sum(.data$n, na.rm = TRUE), 1))
          o52_row <- o52_db %>% dplyr::filter(.data$over_52_weeks == ">52 weeks")
          if (nrow(o52_row) > 0) pct_over_364 <- o52_row$pct[1]
        }

        # --- Overlap % ---
        pct_overlap <- NA_real_
        if (!is.null(overlap) && nrow(overlap) > 0 && "overlap" %in% colnames(overlap)) {
          ov_db <- overlap %>%
            dplyr::filter(.data$cdm_name == db)
          if (nrow(ov_db) > 0) {
            ov_true <- ov_db %>% dplyr::filter(.data$overlap == TRUE)
            total_ov <- sum(ov_db$n, na.rm = TRUE)
            if (total_ov > 0) {
              n_overlap <- if (nrow(ov_true) > 0) sum(ov_true$n, na.rm = TRUE) else 0
              pct_overlap <- round(100 * n_overlap / total_ov, 1)
            }
          }
        }

        # --- Outcome categories: DELIV % and PREG % (hipps algorithm) ---
        pct_deliv <- NA_real_
        pct_preg <- NA_real_
        if (!is.null(oc) && nrow(oc) > 0 &&
            "algorithm" %in% colnames(oc) && "outcome_category" %in% colnames(oc)) {
          oc_db <- oc %>%
            dplyr::filter(.data$cdm_name == db)
          algo <- if ("hipps" %in% oc_db$algorithm) "hipps" else "hip"
          oc_algo <- oc_db %>% dplyr::filter(.data$algorithm == algo)

          if (nrow(oc_algo) > 0) {
            total_oc <- sum(oc_algo$n, na.rm = TRUE)
            if (total_oc > 0) {
              deliv_n <- sum(oc_algo$n[oc_algo$outcome_category == "DELIV"], na.rm = TRUE)
              preg_n <- sum(oc_algo$n[oc_algo$outcome_category == "PREG"], na.rm = TRUE)
              pct_deliv <- round(100 * deliv_n / total_oc, 1)
              pct_preg <- round(100 * preg_n / total_oc, 1)
            }
          }
        }

        pct_deliv_preg <- if (!is.na(pct_deliv) && !is.na(pct_preg)) {
          round(pct_deliv + pct_preg, 1)
        } else NA_real_

        tibble::tibble(
          database = db,
          total_episodes = total_episodes,
          pct_plausible = pct_plausible,
          pct_over_364 = pct_over_364,
          pct_overlap = pct_overlap,
          pct_deliv = pct_deliv,
          pct_preg = pct_preg,
          pct_deliv_preg = pct_deliv_preg
        )
      })
    })

    # Build gt table
    quality_gt <- reactive({
      d <- quality_data()
      validate(need(nrow(d) > 0, "No quality metrics data available."))

      tbl <- d %>%
        arrange(database) %>%
        select(
          database,
          total_episodes,
          pct_plausible,
          pct_over_364,
          pct_overlap,
          pct_preg,
          pct_deliv,
          pct_deliv_preg
        ) %>%
        gt::gt() %>%
        gt::cols_label(
          database        = "Database",
          total_episodes  = "Episodes",
          pct_plausible   = "Plausible (0\u2013308d)",
          pct_over_364    = ">52 weeks",
          pct_overlap     = "Overlapping",
          pct_preg        = "PREG",
          pct_deliv       = "DELIV",
          pct_deliv_preg  = "PREG+DELIV"
        ) %>%
        gt::fmt_integer(columns = "total_episodes") %>%
        gt::fmt_number(
          columns = c("pct_plausible", "pct_over_364",
                      "pct_overlap", "pct_deliv", "pct_preg", "pct_deliv_preg"),
          decimals = 1
        ) %>%
        gt::cols_align(align = "right",
          columns = c("total_episodes", "pct_plausible",
                      "pct_over_364", "pct_overlap", "pct_deliv", "pct_preg", "pct_deliv_preg")
        ) %>%
        gt::cols_align(align = "left", columns = "database") %>%
        gt::tab_spanner(
          label = "Percentage of Total Episodes (%)",
          columns = c("pct_plausible", "pct_over_364", "pct_overlap")
        ) %>%
        gt::tab_spanner(
          label = "Outcome (%)",
          columns = c("pct_preg", "pct_deliv", "pct_deliv_preg")
        ) %>%
        gt::tab_header(
          title = "Episode Quality Metrics",
          subtitle = "Quality metrics across databases"
        ) %>%
        gt::tab_footnote(
          footnote = "Plausible: episode duration \u2265 0 and \u2264 308 days (gestational weeks \u2264 44).",
          locations = gt::cells_column_labels(columns = "pct_plausible")
        ) %>%
        gt::tab_footnote(
          footnote = ">52 weeks: episodes with gestational age > 364 days.",
          locations = gt::cells_column_labels(columns = "pct_over_364")
        ) %>%
        gt::tab_footnote(
          footnote = "Overlap: episodes sharing any date range within the same person.",
          locations = gt::cells_column_labels(columns = "pct_overlap")
        ) %>%
        gt::tab_footnote(
          footnote = "DELIV: 'DELIV' outcome category (HIPPS algorithm). PREG: ongoing/unresolved pregnancy outcome.",
          locations = gt::cells_column_spanners(spanners = "Outcome (%)")
        ) %>%
        gt::sub_missing(missing_text = "\u2014") %>%
        gt::tab_options(
          table.font.size = gt::px(12),
          data_row.padding = gt::px(4)
        ) %>%
        # Highlight concerning values
        gt::tab_style(
          style = gt::cell_text(color = "#c0392b"),
          locations = gt::cells_body(columns = "pct_over_364", rows = d$pct_over_364 > 5)
        ) %>%
        gt::tab_style(
          style = gt::cell_text(color = "#c0392b"),
          locations = gt::cells_body(columns = "pct_overlap", rows = d$pct_overlap > 10)
        ) %>%
        gt::tab_style(
          style = gt::cell_text(color = "#c0392b"),
          locations = gt::cells_body(columns = "pct_preg", rows = d$pct_preg > 30)
        )

      tbl
    })

    output$quality_table <- gt::render_gt({ quality_gt() })

    output$download_csv <- downloadHandler(
      filename = function() "episode_quality_metrics.csv",
      content = function(file) {
        readr::write_csv(quality_data(), file)
      }
    )

    output$download_docx <- downloadHandler(
      filename = function() "episode_quality_metrics.docx",
      content = function(file) {
        gt::gtsave(quality_gt(), file)
      }
    )
  })
}
