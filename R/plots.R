# Interactive charts (plotly). Colours follow a fixed palette so a series or
# status always looks the same wherever it appears.

#' Chart palette
#'
#' @return A list of named colour vectors: `series`, `status`, `ink`.
#' @export
viz_palette <- function() {
  list(
    series = c(
      blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a",
      yellow = "#eda100", violet = "#6250d6"
    ),
    status = c(
      Breach = "#d03b3b", Fail = "#d03b3b",
      Review = "#fab219", Warn = "#fab219",
      Missing = "#ec835a",
      Pass = "#0ca30c",
      `Not assessed` = "#898781"
    ),
    ink = c(
      primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
      grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb"
    )
  )
}

plot_layout <- function(p, title = NULL, yaxis = list(), xaxis = list(),
                        ...) {
  ink <- viz_palette()$ink
  axis_base <- list(
    gridcolor = ink[["grid"]], linecolor = ink[["axis"]],
    zerolinecolor = ink[["axis"]], tickfont = list(color = ink[["muted"]]),
    titlefont = list(color = ink[["secondary"]])
  )
  plotly::layout(
    p,
    title = if (!is.null(title)) {
      list(text = title, x = 0, xanchor = "left",
           font = list(size = 15, color = ink[["primary"]]))
    },
    paper_bgcolor = ink[["surface"]],
    plot_bgcolor = ink[["surface"]],
    font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif"),
    xaxis = utils::modifyList(axis_base, xaxis),
    yaxis = utils::modifyList(axis_base, yaxis),
    legend = list(orientation = "h", x = 0, y = -0.15),
    hovermode = "x unified",
    margin = list(t = if (is.null(title)) 10 else 50, r = 10),
    ...
  ) |>
    plotly::config(displaylogo = FALSE)
}

measure_format <- function(measure) {
  switch(measure,
    Value = list(prefix = "\u20ac", tickformat = ".3s"),
    list(prefix = "", tickformat = ".3s")
  )
}

#' Plot one series with its seasonal adjustment and expected range
#'
#' Shows the reported series, the STL seasonally adjusted series, the
#' anomaly band from [timetk::anomalize()] and flagged points.
#'
#' @param flagged Output of [flag_variances()].
#' @param series_key Series to plot.
#' @param period Reporting period to highlight.
#' @return A plotly object.
#' @export
plot_series_variance <- function(flagged, series_key, period = NULL) {
  pal <- viz_palette()
  df <- dplyr::filter(flagged, .data$series_key == !!series_key)
  fmt <- measure_format(df$measure[1])
  flagged_pts <- dplyr::filter(df, .data$status %in% c("Breach", "Review"))

  hover <- paste0(
    "%{x|%b %Y}: ", fmt$prefix, "%{y:,.0f}<extra>%{fullData.name}</extra>"
  )

  p <- plotly::plot_ly(df, x = ~date) |>
    plotly::add_ribbons(
      ymin = ~recomposed_l1, ymax = ~recomposed_l2, name = "Expected range",
      line = list(width = 0), fillcolor = "rgba(42,120,214,0.12)",
      hoverinfo = "skip"
    ) |>
    plotly::add_lines(
      y = ~value, name = "Reported", line = list(color = pal$series[["blue"]], width = 2),
      hovertemplate = hover
    ) |>
    plotly::add_lines(
      y = ~seasadj, name = "Seasonally adjusted",
      line = list(color = pal$series[["orange"]], width = 2, dash = "dot"),
      hovertemplate = hover
    )

  if (nrow(flagged_pts) > 0) {
    p <- p |>
      plotly::add_markers(
        data = flagged_pts, y = ~value, name = "Flagged",
        marker = list(
          size = 10, color = unname(pal$status[as.character(flagged_pts$status)]),
          line = list(color = pal$ink[["surface"]], width = 2)
        ),
        text = ~paste0(status, ": ", reason),
        hovertemplate = "%{text}<extra></extra>"
      )
  }

  shapes <- list()
  if (!is.null(period)) {
    shapes <- list(list(
      type = "line", x0 = as.Date(period), x1 = as.Date(period),
      y0 = 0, y1 = 1, yref = "paper",
      line = list(color = pal$ink[["muted"]], dash = "dash", width = 1)
    ))
  }

  plot_layout(
    p,
    title = df$series_label[1],
    yaxis = list(title = df$measure[1], tickprefix = fmt$prefix,
                 tickformat = fmt$tickformat, rangemode = "tozero"),
    xaxis = list(title = ""),
    shapes = shapes
  )
}

#' Count of exceptions by reporting period
#'
#' @param flagged Output of [flag_variances()].
#' @return A plotly stacked bar chart.
#' @export
plot_status_history <- function(flagged) {
  pal <- viz_palette()
  counts <- flagged |>
    dplyr::filter(.data$status %in% c("Breach", "Review", "Missing")) |>
    dplyr::count(.data$date, .data$status) |>
    dplyr::mutate(status = droplevels(.data$status))

  p <- plotly::plot_ly()
  for (s in levels(counts$status)) {
    d <- dplyr::filter(counts, .data$status == s)
    p <- plotly::add_bars(
      p, data = d, x = ~date, y = ~n, name = s,
      marker = list(color = pal$status[[s]],
                    line = list(color = pal$ink[["surface"]], width = 1)),
      hovertemplate = paste0(s, ": %{y}<extra></extra>")
    )
  }
  plot_layout(
    p,
    yaxis = list(title = "Series flagged"),
    xaxis = list(title = ""),
    barmode = "stack", bargap = 0.25
  )
}

#' Waterfall bridge of a value movement
#'
#' @param bridge Output of [value_volume_bridge()].
#' @return A plotly waterfall chart.
#' @export
plot_bridge <- function(bridge) {
  pal <- viz_palette()
  lbl <- function(d) format(d, "%b %Y")
  x <- c(
    paste("Value", lbl(bridge$base_period)), "Volume effect",
    "Average-ticket effect", paste("Value", lbl(bridge$period))
  )
  y <- c(bridge$base_value, bridge$volume_effect, bridge$atv_effect, 0)

  p <- plotly::plot_ly(
    type = "waterfall",
    x = factor(x, levels = x),
    y = y,
    measure = c("absolute", "relative", "relative", "total"),
    text = scales::label_currency(prefix = "\u20ac", scale_cut = scales::cut_short_scale())(
      c(bridge$base_value, bridge$volume_effect, bridge$atv_effect,
        bridge$current_value)
    ),
    textposition = "outside",
    hovertemplate = "%{x}: \u20ac%{y:,.0f}<extra></extra>",
    connector = list(line = list(color = pal$ink[["axis"]], width = 1)),
    increasing = list(marker = list(color = pal$series[["aqua"]])),
    decreasing = list(marker = list(color = pal$series[["orange"]])),
    totals = list(marker = list(color = pal$series[["blue"]]))
  )
  plot_layout(
    p,
    yaxis = list(title = "Value (EUR)", tickprefix = "\u20ac", tickformat = ".3s"),
    xaxis = list(title = ""),
    hovermode = "closest", showlegend = FALSE
  )
}

#' Year-on-year growth by county
#'
#' @param county_df Rows of [period_variances()] output for one MPCY
#'   category and measure, counties only.
#' @return A plotly horizontal bar chart coloured by variance status.
#' @export
plot_county_growth <- function(county_df) {
  pal <- viz_palette()
  df <- county_df |>
    dplyr::filter(!is.na(.data$yoy_pct)) |>
    dplyr::arrange(.data$yoy_pct) |>
    dplyr::mutate(area = factor(.data$area, levels = .data$area))

  p <- plotly::plot_ly()
  for (s in intersect(status_levels(), as.character(unique(df$status)))) {
    d <- dplyr::filter(df, .data$status == s)
    p <- plotly::add_bars(
      p, data = d, y = ~area, x = ~yoy_pct, orientation = "h", name = s,
      marker = list(color = pal$status[[s]]),
      text = ~reason, textposition = "none",
      hovertemplate = paste0(
        "%{y}: %{x:+.1%} YoY<br>", s, ": %{text}<extra></extra>"
      )
    )
  }
  plot_layout(
    p,
    xaxis = list(title = "Year-on-year change", tickformat = ".0%"),
    yaxis = list(title = "", categoryorder = "array",
                 categoryarray = levels(df$area)),
    hovermode = "closest", bargap = 0.2,
    legend = list(orientation = "h", x = 0, y = 1.06)
  )
}

#' Validation exceptions by rule and period
#'
#' @param results Output of [run_validation()].
#' @return A plotly heatmap of Fail + Warn counts.
#' @export
plot_validation_heatmap <- function(results) {
  counts <- results |>
    dplyr::mutate(exception = .data$status %in% c("Fail", "Warn")) |>
    dplyr::group_by(.data$rule_id, .data$rule, .data$date) |>
    dplyr::summarise(n = sum(.data$exception), .groups = "drop") |>
    dplyr::mutate(label = paste(.data$rule_id, .data$rule))

  plotly::plot_ly(
    counts, x = ~date, y = ~label, z = ~n, type = "heatmap",
    colors = c("#f0efec", "#86b6ef", "#2a78d6", "#104281"),
    xgap = 2, ygap = 2,
    hovertemplate = "%{y}<br>%{x|%b %Y}: %{z} exception(s)<extra></extra>",
    colorbar = list(title = "Exceptions")
  ) |>
    plot_layout(
      yaxis = list(title = "", autorange = "reversed"),
      xaxis = list(title = ""),
      hovermode = "closest"
    )
}

#' Transactions per card in issue
#'
#' @param ppc Output of [payments_per_card()].
#' @param category Transaction category to plot.
#' @return A plotly line chart.
#' @export
plot_per_card <- function(ppc, category = "Card payments") {
  pal <- viz_palette()
  df <- dplyr::filter(ppc, .data$category == !!category)
  p <- plotly::plot_ly(
    df, x = ~date, y = ~per_card, type = "scatter", mode = "lines+markers",
    name = category,
    line = list(width = 2, color = pal$series[["blue"]]),
    marker = list(size = 8, color = pal$series[["blue"]],
                  line = list(color = pal$ink[["surface"]], width = 2)),
    hovertemplate = "%{x|%b %Y}: %{y:.2f} per card<extra></extra>"
  )
  plot_layout(
    p,
    title = paste(category, "per card in issue (monthly)"),
    yaxis = list(title = "Transactions per card", rangemode = "tozero"),
    xaxis = list(title = "")
  )
}
