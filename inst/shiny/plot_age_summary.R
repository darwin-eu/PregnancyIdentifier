#!/usr/bin/env Rscript
# Plot age_summary.csv as horizontal Plotly boxplots, one row per database.
# Usage: Rscript plot_age_summary.R [path/to/age_summary.csv]
library(plotly)
library(htmlwidgets)
library(magrittr)
# Default: export/age_summary.csv relative to repo root, or inst/shiny/data-ipci/ipci/age_summary.csv

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  csv_path <- args[1]
} else {
  wd <- getwd()
  default1 <- file.path(wd, "export", "age_summary.csv")
  default2 <- file.path(wd, "inst", "shiny", "data-ipci", "ipci", "age_summary.csv")
  default3 <- file.path(wd, "age_summary.csv")
  csv_path <- if (file.exists(default1)) default1 else if (file.exists(default2)) default2 else if (file.exists(default3)) default3 else default1
}

if (!file.exists(csv_path)) {
  stop("File not found: ", csv_path)
}

d <- read.csv(csv_path, stringsAsFactors = FALSE)

# One row per database: keep "overall" when final_outcome_category exists, else keep all (assume one row per cdm)
if ("final_outcome_category" %in% names(d)) {
  if ("overall" %in% d$final_outcome_category) {
    d <- d[d$final_outcome_category == "overall", ]
  } else {
    d <- d[!duplicated(d$cdm_name), ]
  }
}
if (!"cdm_name" %in% names(d)) {
  stop("age_summary.csv must contain a 'cdm_name' column.")
}

# Required box stats (column names may vary by export)
for (col in c("min", "Q25", "median", "Q75", "max")) {
  if (!col %in% names(d)) stop("Missing column: ", col)
  d[[col]] <- as.numeric(d[[col]])
}
# Optional for plotly box
if ("mean" %in% names(d)) d$mean <- as.numeric(d$mean)
if ("sd" %in% names(d)) d$sd <- as.numeric(d$sd)

# Order databases for consistent y-axis (e.g. alphabetically)
d <- d[order(d$cdm_name), ]
d$cdm_name <- factor(d$cdm_name, levels = unique(d$cdm_name))

# Horizontal boxplot: one row per database (y = cdm_name, box extent on x)
p <- plotly::plot_ly(data = d, y = ~ cdm_name) %>%
  plotly::add_boxplot(
    lowerfence = ~ min,
    q1 = ~ Q25,
    median = ~ median,
    q3 = ~ Q75,
    upperfence = ~ max,
    orientation = "h",
    boxpoints = FALSE
  )
if ("mean" %in% names(d)) {
  p <- p %>%
    plotly::add_trace(
      x = ~ mean,
      y = ~ cdm_name,
      type = "scatter",
      mode = "markers",
      marker = list(symbol = "diamond", size = 8, color = "red"),
      name = "mean"
    )
}
p <- p %>%
  plotly::layout(
    title = "Age summary by database",
    xaxis = list(title = "Age"),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(levels(d$cdm_name))),
    margin = list(l = max(80, max(nchar(as.character(d$cdm_name))) * 6)),
    showlegend = "mean" %in% names(d)
  )

# Save HTML and optionally display
out_dir <- dirname(csv_path)
out_html <- file.path(out_dir, "age_summary_boxplot.html")
htmlwidgets::saveWidget(
  widget = plotly::as_widget(p),
  file = out_html,
  selfcontained = TRUE
)
message("Saved: ", out_html)

# Show in viewer if interactive
if (interactive()) {
  print(p)
}
