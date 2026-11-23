# app.R
# -----------------------------------------------------------------------------
# Uganda Maternity Indicators — Shiny dashboard (We Care Solar branding).
# Mirrors the Claude dashboard: Region/Year slicers, six KPI tiles, six
# trend charts, a key-insights card, Power BI-style layout.
#
# Prerequisites: run data_management.R first to produce data/by_region.rds
# Run:
#   shiny::runApp("app.R")
# -----------------------------------------------------------------------------

# --- 1. Dependencies ---------------------------------------------------------

# --- 2. Load data ------------------------------------------------------------
if (!file.exists("data/by_region.rds")) {
  source("data_management.R")
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(htmltools)
})


# --- 3. Brand palette --------------------------------------------------------

brand <- list(
  orange = "#FFA914",
  olive = "#666E4A",
  dark = "#333333",
  cream = "#F5EFE6",
  canvas = "#ECE7DC",
  surface = "#FFFFFF",
  border = "#EAE6DE",
  bad = "#A53F2B"
)


series_colors <- c(
  "#FFA914",
  "#666E4A",
  "#C2831D",
  "#8A9263",
  "#333333",
  "#D49433",
  "#4A5134",
  "#A9B088"
)

# --- 4. Theme + CSS ----------------------------------------------------------

theme <- bs_theme(
  version = 5,
  bg = brand$canvas,
  fg = brand$dark,
  primary = brand$orange,
  secondary = brand$olive,
  base_font = font_google("Inter", wght = c(400, 500, 600)),
  heading_font = font_google("Lora", wght = c(400, 500))
)


app_css <- "
  body { background: #ECE7DC; }
  .wcs-header {
    display:flex; gap:12px;
    background:#fff; padding:18px 22px;
    border-radius:6px;
    box-shadow:0 1px 2px rgba(51,51,51,.04), 0 2px 8px rgba(51,51,51,.05);
    margin-bottom:16px;
  }
  .wcs-accent { width:4px; background:#FFA914; border-radius:2px; flex-shrink:0; }
  .wcs-eyebrow { color:#FFA914; font-size:.75rem; text-transform:uppercase; letter-spacing:.1em; font-weight:600; margin:0 0 4px; }
  .wcs-header h1 { margin:0 0 4px; font-size:1.5rem; font-weight:400; color:#333; }
  .wcs-takeaway { margin:0; color:#666E4A; max-width:70ch; font-size:.95rem; }
  .wcs-section-label {
    font-size:.75rem; text-transform:uppercase; letter-spacing:.1em; font-weight:600;
    color:#666E4A; margin:16px 0 8px;
    display:flex; align-items:center; gap:8px;
  }
  .wcs-section-label::before { content:''; width:14px; height:2px; background:#FFA914; display:inline-block; }
  .wcs-card {
    background:#fff; border:1px solid #EAE6DE; border-radius:6px;
    box-shadow:0 1px 2px rgba(51,51,51,.04), 0 2px 8px rgba(51,51,51,.05);
    padding:16px;
  }
  .wcs-kpi { border-left:3px solid #FFA914; }
  .wcs-kpi-label { font-size:.7rem; text-transform:uppercase; letter-spacing:.05em; font-weight:500; color:#666E4A; margin:0 0 6px; }
  .wcs-kpi-value { font-size:1.75rem; font-weight:400; color:#333; line-height:1.1; font-family:'Lora', Georgia, serif; }
  .wcs-kpi-sub { font-size:.75rem; color:#666E4A; margin-top:4px; }
  .wcs-chart-title { font-size:.95rem; font-weight:500; color:#333; margin:0 0 2px; }
  .wcs-chart-note { font-size:.75rem; color:#666E4A; margin:0 0 8px; }
  .wcs-insight { display:flex; gap:12px; padding:8px 0; border-top:1px solid #EAE6DE; }
  .wcs-insight:first-child { border-top:none; padding-top:0; }
  .wcs-insight-dot { width:10px; height:10px; border-radius:50%; flex-shrink:0; margin-top:7px; }
  .wcs-insight-dot.ok { background:#666E4A; }
  .wcs-insight-dot.warn { background:#FFA914; }
  .wcs-insight-dot.bad { background:#A53F2B; }
  .wcs-insight-head { font-size:.95rem; font-weight:500; color:#333; margin:0 0 2px; }
  .wcs-insight-body { font-size:.85rem; color:#666E4A; margin:0; line-height:1.45; }
  .bslib-sidebar-layout > .sidebar { background: #fff !important; }
"

# --- 5. Helpers --------------------------------------------------------------

fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) sprintf("%.1f%%", x * 100)
fmt_rate <- function(x) sprintf("%.1f", x)

wcs_kpi_card <- function(label, value_id, sub_id, accent = brand$orange) {
  div(
    class = "wcs-card wcs-kpi",
    style = sprintf("border-left-color:%s;", accent),
    p(class = "wcs-kpi-label", label),
    div(class = "wcs-kpi-value", textOutput(value_id, inline = TRUE)),
    div(class = "wcs-kpi-sub", textOutput(sub_id, inline = TRUE))
  )
}

wcs_chart_card <- function(title, note, plot_id) {
  div(
    class = "wcs-card",
    h3(class = "wcs-chart-title", title),
    p(class = "wcs-chart-note", note),
    plotlyOutput(plot_id, height = "260px")
  )
}

wcs_gg_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = brand$border, linewidth = .3),
      axis.title = element_blank(),
      axis.text = element_text(color = brand$olive, size = 9),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = brand$surface, color = NA),
      panel.background = element_rect(fill = brand$surface, color = NA),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = brand$olive, size = 9)
    )
}

wcs_plotly <- function(p) {
  ggplotly(p, tooltip = c("x", "y", "colour", "fill")) %>%
    config(displayModeBar = FALSE) %>%
    layout(
      hoverlabel = list(
        bgcolor = brand$surface,
        bordercolor = brand$border,
        font = list(color = brand$dark)
      ),
      legend = list(orientation = "h", y = 1.08, x = 0),
      margin = list(l = 48, r = 16, t = 20, b = 36)
    )
}

wcs_plotly_empty <- function(msg = "No data for current filter") {
  plot_ly() %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = msg,
        showarrow = FALSE,
        font = list(color = brand$olive, size = 12)
      ))
    ) %>%
    config(displayModeBar = FALSE)
}

# --- 6. UI -------------------------------------------------------------------

ui <- page_sidebar(
  theme = theme,
  fillable = FALSE,

  sidebar = sidebar(
    title = "Slicers",
    width = 260,
    open = "open",
    selectInput(
      "region",
      "Region",
      choices = c("All regions" = "", sort(unique(by_region$region))),
      selected = ""
    ),
    selectInput(
      "year",
      "Year",
      choices = c("All years" = "", "2021", "2022", "2023"),
      selected = ""
    ),
    div(
      style = "display:flex; gap:8px; flex-wrap:wrap; margin-top:8px;",
      actionButton(
        "apply",
        "Apply filters",
        class = "btn btn-primary btn-sm",
        icon = icon("check")
      ),
      actionButton("reset", "Reset", class = "btn btn-outline-secondary btn-sm")
    ),
    p(
      class = "wcs-filter-hint",
      style = "font-size:.7rem; color:#666E4A; margin:8px 0 0;",
      "Changes apply only after you click ",
      tags$b("Apply filters"),
      "."
    )
  ),

  tags$head(tags$style(HTML(app_css))),

  # Report header
  div(
    class = "wcs-header",
    div(class = "wcs-accent"),
    div(
      style = "flex:1; min-width:0;",
      p(
        class = "wcs-eyebrow",
        "Maternal & newborn health || Anonymous Associates"
      ),
      h1("TICIANA Maternity Indicators, 2021–2023"),
      p(class = "wcs-takeaway", htmlOutput("takeaway", inline = TRUE))
    )
  ),

  # Headline metrics
  p(class = "wcs-section-label", "Headline metrics"),
  layout_columns(
    col_widths = c(4, 4, 4, 4, 4, 4),
    wcs_kpi_card(
      "Total deliveries",
      "kpi_deliveries",
      "kpi_deliveries_sub",
      accent = series_colors[1]
    ),
    wcs_kpi_card(
      "Live births",
      "kpi_live_births",
      "kpi_live_births_sub",
      accent = series_colors[2]
    ),
    wcs_kpi_card(
      "Newborn deaths (0–7 days)",
      "kpi_newborn",
      "kpi_newborn_sub",
      accent = brand$bad
    ),
    wcs_kpi_card(
      "Breastfeeding initiation",
      "kpi_bf",
      "kpi_bf_sub",
      accent = series_colors[6]
    ),
    wcs_kpi_card(
      "Birth asphyxia",
      "kpi_asphyxia",
      "kpi_asphyxia_sub",
      accent = series_colors[3]
    ),
    wcs_kpi_card(
      "Stillbirth rate",
      "kpi_stillbirth",
      "kpi_stillbirth_sub",
      accent = series_colors[4]
    )
  ),

  # Trends
  p(class = "wcs-section-label", "Trends over time"),
  layout_columns(
    col_widths = c(6, 6, 6, 6, 6, 6),
    wcs_chart_card(
      "Monthly deliveries and live births",
      "Facility-reported",
      "plot_deliveries"
    ),
    wcs_chart_card(
      "Newborn deaths (0–7 days)",
      "Monthly early neonatal deaths",
      "plot_newborn"
    ),
    wcs_chart_card(
      "Stillbirth rate over time",
      "Per 1,000 total births, monthly",
      "plot_stillbirth"
    ),
    wcs_chart_card(
      "Breastfeeding initiation",
      "Share of live births, monthly",
      "plot_bf"
    ),
    wcs_chart_card(
      "Birth asphyxia vs resuscitation",
      "Monthly counts",
      "plot_asphyxia"
    ),
    wcs_chart_card(
      "Year-on-year totals",
      "Deliveries, live births, newborn deaths",
      "plot_yearly"
    )
  ),

  # Insights
  p(class = "wcs-section-label", "Insights & actions"),
  div(
    class = "wcs-card",
    p(
      style = "font-size:.75rem; color:#666E4A; font-variant-numeric:tabular-nums; margin:0 0 10px;",
      htmlOutput("insights_scope", inline = TRUE)
    ),
    uiOutput("insights_list")
  )
)

# --- 7. Server ---------------------------------------------------------------

server <- function(input, output, session) {
  # Applied filter state — only changes when the user clicks Apply or Reset.
  # Every downstream reactive (totals, plots, insights) reads this, so the
  # dashboard stays frozen while slicers are being adjusted.
  applied_filters <- reactiveVal(list(region = "", year = ""))

  observeEvent(input$apply, {
    applied_filters(list(region = input$region, year = input$year))
  })

  # Reset: clear the slicers AND apply so the view updates in one click.
  observeEvent(input$reset, {
    updateSelectInput(session, "region", selected = "")
    updateSelectInput(session, "year", selected = "")
    applied_filters(list(region = "", year = ""))
  })

  # Filtered slice (by_region rows matching the APPLIED region + year)
  slice_data <- reactive({
    f <- applied_filters()
    d <- by_region
    if (nzchar(f$region)) {
      d <- d %>% filter(region == f$region)
    }
    if (nzchar(f$year)) {
      d <- d %>% filter(format(month, "%Y") == f$year)
    }
    d
  })

  # Aggregated to monthly nationwide (or per region, if region was filtered)
  monthly_data <- reactive({
    d <- slice_data()
    if (nrow(d) == 0) {
      return(d[0, setdiff(names(d), "region"), drop = FALSE])
    }
    d %>%
      group_by(month) %>%
      summarise(
        across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      arrange(month)
  })

  totals <- reactive({
    m <- monthly_data()
    if (nrow(m) == 0) {
      return(NULL)
    }
    list(
      deliveries = sum(m$deliveries),
      live_births = sum(m$live_births),
      newborn_deaths = sum(m$newborn_deaths),
      fresh_stillbirth = sum(m$fresh_stillbirth),
      macerated_stillbirth = sum(m$macerated_stillbirth),
      breastfeeding = sum(m$breastfeeding),
      birth_asphyxia = sum(m$birth_asphyxia),
      resuscitated = sum(m$resuscitated)
    )
  })

  # ---- Header takeaway ----
  output$takeaway <- renderUI({
    t <- totals()
    m <- monthly_data()
    f <- applied_filters()
    if (is.null(t) || nrow(m) == 0) {
      return(HTML("No data for current filter."))
    }
    scope <- paste(
      ifelse(nzchar(f$region), f$region, "Nationwide"),
      "·",
      ifelse(nzchar(f$year), f$year, "Jan 2021 – Dec 2023")
    )
    HTML(sprintf(
      "%s: <b>%s</b> deliveries, <b>%s</b> live births and <b>%s</b> newborn deaths across %d months.",
      scope,
      fmt_int(t$deliveries),
      fmt_int(t$live_births),
      fmt_int(t$newborn_deaths),
      nrow(m)
    ))
  })

  # ---- KPI tiles ----
  output$kpi_deliveries <- renderText({
    t <- totals()
    if (is.null(t)) "—" else fmt_int(t$deliveries)
  })
  output$kpi_deliveries_sub <- renderText({
    sprintf("%d-month total", nrow(monthly_data()))
  })

  output$kpi_live_births <- renderText({
    t <- totals()
    if (is.null(t)) "—" else fmt_int(t$live_births)
  })
  output$kpi_live_births_sub <- renderText({
    sprintf("%d-month total", nrow(monthly_data()))
  })

  output$kpi_newborn <- renderText({
    t <- totals()
    if (is.null(t)) "—" else fmt_int(t$newborn_deaths)
  })
  output$kpi_newborn_sub <- renderText({
    t <- totals()
    if (is.null(t)) {
      return("—")
    }
    nmr <- if (t$live_births) (t$newborn_deaths / t$live_births) * 1000 else 0
    sprintf("%s per 1,000 live births", fmt_rate(nmr))
  })

  output$kpi_bf <- renderText({
    t <- totals()
    if (is.null(t)) {
      return("—")
    }
    fmt_pct(if (t$live_births) t$breastfeeding / t$live_births else 0)
  })
  output$kpi_bf_sub <- renderText({
    "Within 1st hour · share of live births"
  })

  output$kpi_asphyxia <- renderText({
    t <- totals()
    if (is.null(t)) "—" else fmt_int(t$birth_asphyxia)
  })
  output$kpi_asphyxia_sub <- renderText({
    t <- totals()
    if (is.null(t)) {
      return("—")
    }
    rate <- if (t$birth_asphyxia) t$resuscitated / t$birth_asphyxia else 0
    sprintf("%s successfully resuscitated", fmt_pct(rate))
  })

  output$kpi_stillbirth <- renderText({
    t <- totals()
    if (is.null(t)) {
      return("—")
    }
    tot <- t$live_births + t$fresh_stillbirth + t$macerated_stillbirth
    fmt_rate(
      if (tot) (t$fresh_stillbirth + t$macerated_stillbirth) / tot * 1000 else 0
    )
  })
  output$kpi_stillbirth_sub <- renderText({
    "Per 1,000 total births"
  })

  # ---- Plots ----
  output$plot_deliveries <- renderPlotly({
    m <- monthly_data()
    if (nrow(m) == 0) {
      return(wcs_plotly_empty())
    }
    d <- m %>%
      select(month, Deliveries = deliveries, `Live births` = live_births) %>%
      pivot_longer(-month, names_to = "Series", values_to = "Value")
    p <- ggplot(d, aes(month, Value, color = Series)) +
      geom_line(linewidth = .7) +
      geom_point(size = 1.6) +
      scale_color_manual(
        values = c(
          Deliveries = series_colors[1],
          `Live births` = series_colors[2]
        )
      ) +
      scale_y_continuous(labels = label_comma()) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
      wcs_gg_theme()
    wcs_plotly(p)
  })

  output$plot_newborn <- renderPlotly({
    m <- monthly_data()
    if (nrow(m) == 0) {
      return(wcs_plotly_empty())
    }
    p <- ggplot(m, aes(month, newborn_deaths)) +
      geom_col(fill = series_colors[3], width = 20) +
      scale_y_continuous(labels = label_comma()) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
      wcs_gg_theme()
    wcs_plotly(p)
  })

  output$plot_stillbirth <- renderPlotly({
    m <- monthly_data()
    if (nrow(m) == 0) {
      return(wcs_plotly_empty())
    }
    d <- m %>%
      mutate(
        total = live_births + fresh_stillbirth + macerated_stillbirth,
        rate = ifelse(
          total > 0,
          (fresh_stillbirth + macerated_stillbirth) / total * 1000,
          0
        )
      )
    p <- ggplot(d, aes(month, rate)) +
      geom_line(color = series_colors[4], linewidth = .7) +
      geom_point(color = series_colors[4], size = 1.6) +
      scale_y_continuous(labels = label_number(accuracy = 0.1)) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
      wcs_gg_theme()
    wcs_plotly(p)
  })

  output$plot_bf <- renderPlotly({
    m <- monthly_data()
    if (nrow(m) == 0) {
      return(wcs_plotly_empty())
    }
    d <- m %>%
      mutate(rate = ifelse(live_births > 0, breastfeeding / live_births, 0))
    p <- ggplot(d, aes(month, rate)) +
      geom_line(color = series_colors[5], linewidth = .7) +
      geom_point(color = series_colors[5], size = 1.6) +
      scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
      wcs_gg_theme()
    wcs_plotly(p)
  })

  output$plot_asphyxia <- renderPlotly({
    m <- monthly_data()
    if (nrow(m) == 0) {
      return(wcs_plotly_empty())
    }
    d <- m %>%
      select(month, Asphyxia = birth_asphyxia, Resuscitated = resuscitated) %>%
      pivot_longer(-month, names_to = "Series", values_to = "Value")
    p <- ggplot(d, aes(month, Value, color = Series)) +
      geom_line(linewidth = .7) +
      geom_point(size = 1.6) +
      scale_color_manual(
        values = c(Asphyxia = series_colors[6], Resuscitated = series_colors[7])
      ) +
      scale_y_continuous(labels = label_comma()) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
      wcs_gg_theme()
    wcs_plotly(p)
  })

  output$plot_yearly <- renderPlotly({
    m <- monthly_data()
    if (nrow(m) == 0) {
      return(wcs_plotly_empty())
    }
    d <- m %>%
      mutate(year = format(month, "%Y")) %>%
      group_by(year) %>%
      summarise(
        Deliveries = sum(deliveries),
        `Live births` = sum(live_births),
        `Newborn deaths` = sum(newborn_deaths),
        .groups = "drop"
      ) %>%
      pivot_longer(-year, names_to = "Metric", values_to = "Value")
    p <- ggplot(d, aes(year, Value, fill = Metric)) +
      geom_col(position = position_dodge(width = .8), width = .7) +
      scale_fill_manual(
        values = c(
          Deliveries = series_colors[1],
          `Live births` = series_colors[2],
          `Newborn deaths` = series_colors[3]
        )
      ) +
      scale_y_continuous(labels = label_comma()) +
      wcs_gg_theme()
    wcs_plotly(p)
  })

  # ---- Insights ----
  output$insights_scope <- renderUI({
    t <- totals()
    m <- monthly_data()
    f <- applied_filters()
    if (is.null(t) || nrow(m) == 0) {
      return(HTML("No data in current filter."))
    }
    region <- if (nzchar(f$region)) f$region else "All regions"
    year <- if (nzchar(f$year)) f$year else "2021–2023"
    HTML(sprintf(
      "%s · %s · %d months · %s live births in scope.",
      region,
      year,
      nrow(m),
      fmt_int(t$live_births)
    ))
  })

  output$insights_list <- renderUI({
    t <- totals()
    m <- monthly_data()
    if (is.null(t) || nrow(m) == 0) {
      return(NULL)
    }

    nmr <- if (t$live_births) (t$newborn_deaths / t$live_births) * 1000 else 0
    bf <- if (t$live_births) t$breastfeeding / t$live_births else 0
    tot_sb <- t$fresh_stillbirth + t$macerated_stillbirth
    tot_b <- t$live_births + tot_sb
    sbr <- if (tot_b) (tot_sb / tot_b) * 1000 else 0
    fresh_share <- if (tot_sb) t$fresh_stillbirth / tot_sb else 0

    ins <- list()

    # 1. Newborn mortality vs WHO SDG ceiling (12 / 1,000)
    if (nmr >= 12) {
      ins[[length(ins) + 1]] <- list(
        status = "bad",
        head = sprintf(
          "Early newborn mortality is %.1f per 1,000 live births — above the WHO SDG ceiling of 12.",
          nmr
        ),
        body = "Focus on the first 24 hours: Helping Babies Breathe refresher training, Kangaroo Mother Care for preterm and low-birth-weight babies, and timely referral for complications."
      )
    } else if (nmr >= 8) {
      ins[[length(ins) + 1]] <- list(
        status = "warn",
        head = sprintf(
          "Early newborn mortality is %.1f per 1,000 live births — approaching the WHO 12 ceiling.",
          nmr
        ),
        body = "Audit the highest-mortality facilities and strengthen postnatal observation in the first 24 hours."
      )
    } else {
      ins[[length(ins) + 1]] <- list(
        status = "ok",
        head = sprintf(
          "Early newborn mortality is %.1f per 1,000 live births — comfortably below the WHO 12 ceiling.",
          nmr
        ),
        body = "Keep an eye on individual facilities whose rate climbs above the slice average; aggregate can hide hotspots."
      )
    }

    # 2. Breastfeeding initiation vs 90% target
    if (bf >= 0.9) {
      ins[[length(ins) + 1]] <- list(
        status = "ok",
        head = sprintf(
          "Breastfeeding initiation within the first hour is %.1f%% — meets the WHO/UNICEF 90%% target.",
          bf * 100
        ),
        body = "Hold the line: keep skin-to-skin and first-hour support in every delivery pack and track facilities that slip below 90%."
      )
    } else if (bf >= 0.75) {
      ins[[length(ins) + 1]] <- list(
        status = "warn",
        head = sprintf(
          "Breastfeeding initiation is %.1f%% — short of the WHO/UNICEF 90%% target.",
          bf * 100
        ),
        body = "Reinforce immediate skin-to-skin contact and delay first bathing. Share practices of facilities that already clear 90%."
      )
    } else {
      ins[[length(ins) + 1]] <- list(
        status = "bad",
        head = sprintf(
          "Breastfeeding initiation is only %.1f%% — well below the WHO/UNICEF 90%% target.",
          bf * 100
        ),
        body = "Prioritise Baby-Friendly Hospital Initiative training; audit c-section wards where early contact is often delayed."
      )
    }

    # 3. Stillbirth rate + composition
    comp_note <- if (tot_sb == 0) {
      ""
    } else if (fresh_share >= 0.55) {
      sprintf(
        "%.1f%% are fresh (intrapartum). Review partograph use, labour monitoring and timeliness of C-section decisions.",
        fresh_share * 100
      )
    } else {
      sprintf(
        "%.1f%% are macerated (antepartum). Strengthen ANC-side early risk detection and management of hypertension / IUGR.",
        (1 - fresh_share) * 100
      )
    }

    if (sbr >= 12) {
      ins[[length(ins) + 1]] <- list(
        status = "bad",
        head = sprintf(
          "Facility stillbirth rate is %.1f per 1,000 total births — above the WHO 12 target.",
          sbr
        ),
        body = comp_note
      )
    } else if (sbr >= 8) {
      ins[[length(ins) + 1]] <- list(
        status = "warn",
        head = sprintf(
          "Facility stillbirth rate is %.1f per 1,000 — approaching the WHO 12 target.",
          sbr
        ),
        body = comp_note
      )
    } else {
      ins[[length(ins) + 1]] <- list(
        status = "ok",
        head = sprintf(
          "Facility stillbirth rate is %.1f per 1,000 — within the WHO 12 target.",
          sbr
        ),
        body = comp_note
      )
    }

    # 4. Resuscitation quality / data integrity
    if (t$birth_asphyxia > 0) {
      res_ratio <- t$resuscitated / t$birth_asphyxia
      if (res_ratio > 1.1) {
        ins[[length(ins) + 1]] <- list(
          status = "warn",
          head = sprintf(
            "%s babies resuscitated vs %s asphyxia cases (%.1f%%).",
            fmt_int(t$resuscitated),
            fmt_int(t$birth_asphyxia),
            res_ratio * 100
          ),
          body = "Resuscitations exceed recorded asphyxia — likely under-reporting of birth asphyxia at delivery. Audit DHIS2 entry and clarify the case definition with maternity staff."
        )
      } else if (res_ratio < 0.7) {
        ins[[length(ins) + 1]] <- list(
          status = "bad",
          head = sprintf(
            "Only %.1f%% of asphyxia cases were successfully resuscitated.",
            res_ratio * 100
          ),
          body = "Prioritise Helping Babies Breathe refresher training and verify every delivery room has a working bag-and-mask and suction device."
        )
      } else {
        ins[[length(ins) + 1]] <- list(
          status = "ok",
          head = sprintf(
            "%.1f%% of birth asphyxia cases were successfully resuscitated.",
            res_ratio * 100
          ),
          body = "Sustain Helping Babies Breathe practice and keep equipment checks on each shift."
        )
      }
    }

    # 5. Trend across the period
    if (nrow(m) >= 6) {
      third <- max(2, floor(nrow(m) / 3))
      head3 <- head(m, third)
      tail3 <- tail(m, third)
      a <- sum(head3$newborn_deaths) / max(1, sum(head3$live_births)) * 1000
      b <- sum(tail3$newborn_deaths) / max(1, sum(tail3$live_births)) * 1000
      delta <- b - a
      if (delta >= 0.5) {
        ins[[length(ins) + 1]] <- list(
          status = "warn",
          head = sprintf(
            "Newborn mortality rose from %.1f to %.1f per 1,000 across the period.",
            a,
            b
          ),
          body = "Trend is adverse. Compare the facilities whose rate climbed most against those that stayed flat; look for differences in staffing, equipment, or caseload."
        )
      } else if (delta <= -0.5) {
        ins[[length(ins) + 1]] <- list(
          status = "ok",
          head = sprintf(
            "Newborn mortality fell from %.1f to %.1f per 1,000 across the period.",
            a,
            b
          ),
          body = "Trend is favourable. Identify the facilities driving the improvement and codify what changed so it spreads."
        )
      } else {
        ins[[length(ins) + 1]] <- list(
          status = "warn",
          head = sprintf(
            "Newborn mortality held roughly steady (%.1f → %.1f per 1,000).",
            a,
            b
          ),
          body = "No systematic shift. If the rate is still above target, treat it as a signal that current interventions are not yet moving the dial."
        )
      }
    }

    tagList(lapply(ins, function(x) {
      div(
        class = "wcs-insight",
        span(class = sprintf("wcs-insight-dot %s", x$status)),
        div(
          p(class = "wcs-insight-head", x$head),
          p(class = "wcs-insight-body", x$body)
        )
      )
    }))
  })
}

# --- 8. Run ------------------------------------------------------------------

shinyApp(ui, server)
