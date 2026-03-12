# 28-version-differences.R - Explain version differences module

# ── UI ────────────────────────────────────────────────────────────────────────

versionDifferencesUI <- function(id) {
  ns <- NS(id)
  # Extract version choices at UI build time (after data loading)
  vd <- if (exists("versionDifferences") && is.data.frame(versionDifferences) && nrow(versionDifferences) > 0) {
    sort(unique(c(versionDifferences$old_version, versionDifferences$new_version)))
  } else {
    character(0)
  }
  tagList(
    div(class = "tab-help-text",
        "Compare PregnancyIdentifier package versions to understand why episode",
        "and person counts differ. Select an old and new version to see the",
        "algorithmic changes and their expected impact."),
    fluidRow(
      column(3, selectInput(ns("old_version"), "Old version", choices = vd, selected = if (length(vd) > 0) vd[1] else NULL)),
      column(3, selectInput(ns("new_version"), "New version", choices = if (length(vd) > 1) vd[-1] else character(0),
                            selected = if (length(vd) > 1) vd[2] else NULL))
    ),
    uiOutput(ns("metrics_cards")),
    hr(),
    h4("Algorithmic Changes"),
    uiOutput(ns("explanation"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

versionDifferencesServer <- function(id, versionDiffData) {
  moduleServer(id, function(input, output, session) {

    # Sorted unique versions from the CSV
    allVersions <- sort(unique(c(versionDiffData$old_version,
                                 versionDiffData$new_version)))

    # Keep new-version choices strictly > old-version
    observeEvent(input$old_version, {
      req(input$old_version)
      later <- allVersions[numeric_version(allVersions) >
                             numeric_version(input$old_version)]
      sel <- if (length(later) > 0) later[1] else NULL
      updateSelectInput(session, "new_version",
                        choices  = later,
                        selected = sel)
    })

    # ── Metrics cards ─────────────────────────────────────────────────────────

    output$metrics_cards <- renderUI({
      req(input$old_version, input$new_version)

      if (!exists("episodeFrequency", envir = .GlobalEnv) ||
          !is.data.frame(episodeFrequency) ||
          nrow(episodeFrequency) == 0) {
        return(NULL)
      }

      # Parse (base_db, pkg_version) from each cdm_name column
      dbCols <- setdiff(colnames(episodeFrequency), "name")
      if (length(dbCols) == 0) return(NULL)

      extractVersion <- function(x) {
        m <- regmatches(x, regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", x))
        if (length(m) == 0 || !nzchar(m)) NA_character_ else m
      }
      extractBase <- function(x) {
        trimws(sub("\\s*[0-9]+\\.[0-9]+\\.[0-9]+.*$", "", x))
      }

      lookup <- data.frame(
        cdm_name = dbCols,
        db_base  = vapply(dbCols, extractBase, character(1)),
        version  = vapply(dbCols, extractVersion, character(1)),
        stringsAsFactors = FALSE
      )

      oldDbs <- lookup[!is.na(lookup$version) & lookup$version == input$old_version, , drop = FALSE]
      newDbs <- lookup[!is.na(lookup$version) & lookup$version == input$new_version, , drop = FALSE]
      matched <- merge(oldDbs, newDbs, by = "db_base", suffixes = c("_old", "_new"))

      if (nrow(matched) == 0) {
        return(div(
          style = "margin-top: 10px; padding: 12px 16px; background: #FEF9C3; border: 1px solid #FDE68A; border-radius: 6px;",
          "No databases loaded with both version ",
          tags$code(input$old_version), " and version ",
          tags$code(input$new_version),
          ". Computed metrics are not available, but the algorithmic explanation below still applies."
        ))
      }

      getVal <- function(cdmName, metric) {
        row <- episodeFrequency[episodeFrequency$name == metric, , drop = FALSE]
        if (nrow(row) == 0 || !cdmName %in% colnames(row)) return(NA_real_)
        v <- suppressWarnings(as.numeric(row[[cdmName]]))
        if (length(v) == 0) return(NA_real_)
        v[1L]
      }

      formatDelta <- function(delta, pct) {
        if (length(delta) != 1 || is.na(delta)) return("--")
        sign <- if (delta >= 0) "+" else ""
        paste0(sign, format(round(delta), big.mark = ","),
               " (", sign, round(pct, 1), "%)")
      }
      fmtN <- function(x) {
        if (length(x) != 1 || is.na(x)) return("--")
        format(round(x), big.mark = ",")
      }

      cards <- lapply(seq_len(nrow(matched)), function(i) {
        dbBase <- matched$db_base[i]
        oldCdm <- matched$cdm_name_old[i]
        newCdm <- matched$cdm_name_new[i]

        oldEp <- getVal(oldCdm, "total_episodes")
        newEp <- getVal(newCdm, "total_episodes")
        oldPn <- getVal(oldCdm, "total_individuals")
        newPn <- getVal(newCdm, "total_individuals")

        dEp <- newEp - oldEp
        dPn <- newPn - oldPn
        pEp <- if (isTRUE(!is.na(oldEp) & oldEp > 0)) 100 * dEp / oldEp else NA_real_
        pPn <- if (isTRUE(!is.na(oldPn) & oldPn > 0)) 100 * dPn / oldPn else NA_real_

        colEp <- if (isTRUE(dEp >= 0)) "#16a34a" else "#dc2626"
        colPn <- if (isTRUE(dPn >= 0)) "#16a34a" else "#dc2626"

        metricBox <- function(label, value, color = "inherit") {
          tags$div(style = "text-align:center; flex:1; min-width:120px; padding:4px 8px;",
            tags$p(style = "color:#6b7280; margin:0; font-size:0.85em;", label),
            tags$h4(style = paste0("margin:4px 0; color:", color, ";"), value)
          )
        }

        div(
          style = "margin-bottom: 12px; padding: 14px; background: #f9fafb; border-radius: 8px; border: 1px solid #e5e7eb;",
          h4(style = "margin-top:0; margin-bottom:10px;", toupper(dbBase)),
          div(style = "display:flex; flex-wrap:wrap; gap:8px; justify-content:space-around;",
            metricBox(paste0("Episodes (v", input$old_version, ")"), fmtN(oldEp)),
            metricBox(paste0("Episodes (v", input$new_version, ")"), fmtN(newEp)),
            metricBox("Episode change", formatDelta(dEp, pEp), colEp),
            metricBox("Person change", formatDelta(dPn, pPn), colPn)
          )
        )
      })

      tagList(cards)
    })

    # ── Markdown explanation ──────────────────────────────────────────────────

    output$explanation <- renderUI({
      req(input$old_version, input$new_version)

      row <- versionDiffData[versionDiffData$old_version == input$old_version &
                             versionDiffData$new_version == input$new_version, ,
                             drop = FALSE]
      if (nrow(row) == 0) {
        return(div(
          style = "padding: 12px 16px; background: #EFF6FF; border: 1px solid #BFDBFE; border-radius: 6px;",
          "No pre-written explanation is available for v", input$old_version,
          " to v", input$new_version,
          ". Try selecting a consecutive version pair or a common comparison ",
          "(e.g. v2.0.1 to v3.0.5)."
        ))
      }

      mdText   <- row$difference_explanation[1]
      htmlBody <- commonmark::markdown_html(mdText, extensions = TRUE)
      div(
        style = paste("max-width:960px; padding:16px; background:white;",
                      "border-radius:8px; border:1px solid #e5e7eb;"),
        HTML(htmlBody)
      )
    })
  })
}
