# AFRICA HIV OBSERVATORY -- UPDATED APP

required_packages <- c("shiny", "bslib", "bsicons", "readxl", "scales", "leaflet",
                       "plotly", "sf", "rnaturalearth", "rnaturalearthdata", "dplyr", "DT")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install missing packages in the Console: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))", call. = FALSE)
}
library(shiny)
library(bslib)

data_file <- Sys.getenv("HIV_DATA_FILE", "Africa_HIV_Data_1990_2025.xlsx")
if (!file.exists(data_file)) {
  stop("Put ", data_file, " beside app.R. Current working folder: ", getwd(), call. = FALSE)
}
hiv <- as.data.frame(readxl::read_excel(data_file, sheet = "Shiny_Data"))
metric_keys <- c("plhiv_all_ages", "hiv_prevalence_15_49_pct", "new_infections_all_ages",
                 "aids_deaths_all_ages", "art_coverage_all_ages_pct")
needed_columns <- c("country", "iso3", "subregion", "year", metric_keys,
                    paste0(metric_keys, "_lower"), paste0(metric_keys, "_upper"))
if (length(setdiff(needed_columns, names(hiv)))) {
  stop("Missing workbook columns: ", paste(setdiff(needed_columns, names(hiv)), collapse = ", "))
}
if (anyDuplicated(hiv[c("iso3", "year")])) stop("Duplicate country-year records in workbook.")
numeric_columns <- c("year", metric_keys, paste0(metric_keys, "_lower"), paste0(metric_keys, "_upper"))
if (!all(vapply(hiv[numeric_columns], is.numeric, logical(1)))) stop("Year and estimate columns must be numeric.")
if (anyNA(hiv[c("country", "iso3", "subregion", "year")])) stop("Missing geographic identifiers or years.")
hiv$year <- as.integer(hiv$year)
country_list <- unique(hiv[c("country", "iso3", "subregion")])
country_list <- country_list[order(country_list$country), ]
country_choices <- stats::setNames(country_list$iso3, country_list$country)
region_names <- sort(unique(hiv$subregion))
count_columns <- metric_keys[c(1, 3, 4)]
baseline_year <- 2010L


hv_indicators <- data.frame(
  key = c("plhiv_all_ages", "hiv_prevalence_15_49_pct",
          "new_infections_all_ages", "aids_deaths_all_ages",
          "art_coverage_all_ages_pct"),
  label = c("People living with HIV (all ages)", "HIV prevalence among adults aged 15-49 (%)",
            "New HIV infections (all ages)", "AIDS-related deaths (all ages)",
            "ART coverage among people living with HIV (%)"),
  percent = c(FALSE, TRUE, FALSE, FALSE, TRUE),
  colour = c("#73C7B8", "#C69EB8", "#86A9D9", "#B0A0D0", "#D4B273"),
  bg1 = c("#263C37", "#40313B", "#2B3748", "#3A3348", "#3A3428"),
  bg2 = c("#22332F", "#3B2F38", "#27313F", "#342E40", "#322D24"),
  icon = c("people-fill", "percent", "plus-circle-fill", "activity", "shield-check"),
  stringsAsFactors = FALSE
)

hv_meta <- function(key) {
  i <- match(key, hv_indicators$key)
  if (is.na(i)) stop("Unknown indicator: ", key)
  hv_indicators[i, , drop = FALSE]
}

hv_format <- function(x, key) {
  m <- hv_meta(key)
  accuracy <- if (key == "hiv_prevalence_15_49_pct") 0.0001 else
    if (m$percent) 0.1 else 1
  answer <- scales::number(x, accuracy = accuracy, big.mark = ",")
  if (m$percent) answer <- paste0(answer, "%")
  answer[!is.finite(x)] <- "Unavailable"
  answer
}

hv_headline <- function(x, key) {
  if (length(x) != 1L || !is.finite(x)) return("Unavailable")
  if (hv_meta(key)$percent) return(hv_format(x, key))
  if (x >= 1e6) return(paste0(scales::number(x / 1e6, accuracy = .01), "M"))
  if (x >= 1e3) return(paste0(scales::number(x / 1e3, accuracy = .1), "K"))
  hv_format(x, key)
}

hv_metric_data <- function(d, key) {
  data.frame(country = d$country, iso3 = d$iso3, year = as.integer(d$year),
             value = d[[key]], lower = d[[paste0(key, "_lower")]],
             upper = d[[paste0(key, "_upper")]], stringsAsFactors = FALSE)
}

hv_bounds <- function(lower, upper, key) {
  ok <- is.finite(lower) & is.finite(upper) & lower <= upper
  text <- paste0(hv_format(lower, key), " to ", hv_format(upper, key))
  text[!ok] <- "Not supplied"
  text
}

hv_hover <- function(d, key) {
  paste0("<b>", htmltools::htmlEscape(d$country), "</b> | ", d$year,
         "<br>", hv_meta(key)$label, ": <b>", hv_format(d$value, key),
         "</b><br>Uncertainty bounds: ", hv_bounds(d$lower, d$upper, key))
}

hv_chart_style <- function(p, x_title = "Year", y_title = "People",
                           percent = FALSE, year_axis = TRUE) {
  p <- plotly::layout(
    p, paper_bgcolor = "#202226", plot_bgcolor = "#202226",
    font = list(family = "Arial, sans-serif", color = "#D8D9DA", size = 12),
    margin = list(l = 70, r = 25, t = 54, b = 75),
    xaxis = list(title = list(text = x_title), showgrid = FALSE,
                 tickformat = if (year_axis) "d" else "~s",
                 automargin = TRUE, zeroline = FALSE),
    yaxis = list(title = list(text = y_title), automargin = TRUE,
                 gridcolor = "rgba(216,217,218,0.08)", rangemode = "tozero",
                 zerolinecolor = "#AAB0B8",
                 tickformat = if (percent) ".2f" else "~s",
                 ticksuffix = if (percent) "%" else ""),
    # BAR OVERLAY FIX: controls stay in the reserved top margin, with no
    # opaque background. This keeps image export and zoom controls available.
    modebar = list(bgcolor = "rgba(0,0,0,0)", color = "#AAB0B8",
                   activecolor = "#73C7B8"),
    hovermode = "closest",
    hoverlabel = list(bgcolor = "#2C3137", bordercolor = "#343A43",
                      font = list(color = "#D8D9DA")),
    legend = list(orientation = "h", x = 0, y = -0.18,
                  font = list(color = "#D8D9DA"))
  )
  plotly::config(p, displaylogo = FALSE, responsive = TRUE,
                 toImageButtonOptions = list(format = "png", scale = 2,
                                             filename = "Africa_HIV_chart"))
}

hv_add_series <- function(p, d, key, bands = TRUE, colour = NULL,
                          name = NULL, legend = FALSE) {
  m <- hv_meta(key)
  if (is.null(colour)) colour <- m$colour
  if (is.null(name)) name <- m$label
  d <- d[order(d$year), , drop = FALSE]
  d$hover <- hv_hover(d, key)
  
  # Separate ribbon segments so a missing year never gets bridged.
  if (bands) {
    b <- d[is.finite(d$value) & is.finite(d$lower) & is.finite(d$upper) &
             d$lower <= d$upper, , drop = FALSE]
    if (nrow(b)) {
      b$segment <- cumsum(c(TRUE, diff(b$year) != 1L))
      rgb <- grDevices::col2rgb(colour)
      shade <- sprintf("rgba(%d,%d,%d,0.18)", rgb[1], rgb[2], rgb[3])
      for (part in split(b, b$segment)) {
        if (nrow(part) < 2L) next
        p <- plotly::add_ribbons(
          p, data = part, x = ~year, ymin = ~lower, ymax = ~upper,
          fillcolor = shade, line = list(color = "transparent", width = 0),
          hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
        )
      }
    }
  }
  plotly::add_trace(
    p, data = d, x = ~year, y = ~value,
    type = "scatter", mode = "lines+markers", connectgaps = FALSE,
    line = list(color = colour, width = 3),
    marker = list(color = colour, size = 5),
    text = ~hover, hoverinfo = "text", name = name,
    showlegend = legend, inherit = FALSE
  )
}

hv_annual_change <- function(d) {
  d <- d[order(d$year), , drop = FALSE]
  if (!nrow(d)) { d$change <- numeric(0); return(d) }
  previous <- c(NA_real_, head(d$value, -1))
  consecutive <- c(FALSE, diff(d$year) == 1L)
  d$change <- d$value - previous
  d$change[!consecutive | !is.finite(previous) | !is.finite(d$value)] <- NA_real_
  d
}

.hv_map_cache <- new.env(parent = emptyenv())
hv_africa_map <- function(hiv) {
  ids <- sort(unique(hiv$iso3))
  cache_key <- paste(ids, collapse = "_")
  if (exists(cache_key, envir = .hv_map_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .hv_map_cache, inherits = FALSE))
  }
  # Load the whole world before filtering by ISO. Natural Earth classifies
  # Mauritius and Seychelles as ocean islands, so continent-only filtering
  # would incorrectly drop them.
  world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  world$iso3 <- world$iso_a3
  world$iso3[world$admin == "Somaliland"] <- "SOM"
  world <- world[world$iso3 %in% ids, "iso3", drop = FALSE]
  world <- sf::st_make_valid(world)
  world <- dplyr::summarise(dplyr::group_by(world, iso3), .groups = "drop")
  world <- sf::st_transform(world, 4326)
  missing <- setdiff(ids, world$iso3)
  if (length(missing)) stop("Missing map boundaries for: ", paste(missing, collapse = ", "))
  labels <- unique(hiv[c("iso3", "country")])
  world$country <- labels$country[match(world$iso3, labels$iso3)]
  assign(cache_key, world, envir = .hv_map_cache)
  world
}



# =========================================================
# VISUAL INVENTORY AND SHARED CALCULATIONS
# =========================================================
visual_catalog <- data.frame(
  number = 1:40,
  tab = rep(c("Africa overview", "Country profile", "Country comparisons",
              "Progress since 2010", "Subregional patterns", "Data explorer"),
            c(8, 8, 7, 6, 5, 6)),
  title = c("People living with HIV (all ages)",
            "New HIV infections during the year (all ages)",
            "AIDS-related deaths during the year (all ages)",
            "People living with HIV receiving ART (%)",
            "Countries with at least one HIV count estimate",
            "Selected HIV indicator by country",
            "Countries ranked by the selected HIV indicator",
            "Selected HIV indicator over time",
            "HIV burden and treatment coverage in the selected year",
            "People living with HIV over time (all ages)",
            "Adults aged 15-49 living with HIV over time (%)",
            "Annual new HIV infections over time (all ages)",
            "Annual AIDS-related deaths over time (all ages)",
            "People living with HIV receiving ART over time (%)",
            "Annual new HIV infections and AIDS-related deaths over time",
            "Year-to-year change in the selected HIV indicator",
            "Selected HIV indicator over time by country",
            "Selected HIV indicator by comparison country",
            "Selected HIV indicator by country and year",
            "Change in the selected HIV count relative to 2010",
            "Adult HIV prevalence (ages 15-49) versus ART coverage",
            "Population-relative HIV burden by country: adults aged 15-49 (%)",
            "Country estimates and changes since 2010 for the selected HIV indicator",
            "Percentage change in annual new HIV infections since 2010",
            "Percentage change in annual AIDS-related deaths since 2010",
            "Percentage change in people living with HIV since 2010",
            "ART coverage by country: 2010 versus selected year (%)",
            "Adult HIV prevalence by country: 2010 versus selected year (%)",
            "Country estimates and progress since 2010 for the selected HIV indicator",
            "People living with HIV over time by subregion (all ages)",
            "Annual new HIV infections over time by subregion (all ages)",
            "Annual AIDS-related deaths over time by subregion (all ages)",
            "Selected HIV count over time, stacked by subregion",
            "Country HIV indicator values grouped by subregion",
            "Filtered country-year HIV estimates and source uncertainty bounds",
            "Availability of the selected HIV indicator by country and year",
            "Number of countries with estimates for each HIV indicator",
            "Latest available reporting year for each country and HIV indicator",
            "Definition, units and interpretation of the selected HIV indicator",
            "Data sources, aggregation rules and interpretation"),
  stringsAsFactors = FALSE
)
region_colours <- stats::setNames(c("#73C7B8", "#86A9D9", "#D4B273", "#B0A0D0", "#C69EB8"), region_names)

h40_sum <- function(x) if (any(is.finite(x))) sum(x[is.finite(x)]) else NA_real_
# EDITS 6-7: Internal IDs never become visible numbering. Plot headers are
# reactive; KPI labels use the same plain-language title catalogue.
h40_title <- function(n) visual_catalog$title[n]
h40_header <- function(n) card_header(textOutput(paste0("chart_title_", n)))
h40_unit <- function(key) if (hv_meta(key)$percent) "Percent" else if (key == "aids_deaths_all_ages") "Deaths" else "People"
# EDIT 4: Aggregate matched denominators, never add national percentages.
# Missing and impossible values are excluded, and coverage is disclosed.
# National ART coverage is retained even if its PLHIV weight is missing.
h40_art_summary <- function(d, national = FALSE) {
  coverage <- d$art_coverage_all_ages_pct
  weights <- d$plhiv_all_ages
  ok <- is.finite(coverage) & coverage >= 0 & coverage <= 100 &
    is.finite(weights) & weights >= 0
  denominator <- sum(weights[ok])
  treated <- if (any(ok)) sum(weights[ok] * coverage[ok] / 100) else NA_real_
  value <- if (denominator > 0) 100 * treated / denominator else NA_real_
  if (national && nrow(d) == 1L && is.finite(coverage) &&
      coverage >= 0 && coverage <= 100) value <- coverage
  list(value = value, treated = treated, plhiv = denominator,
       n = sum(ok), total = nrow(d))
}

h40_art_note <- function(d, national = FALSE) {
  a <- h40_art_summary(d, national)
  if (!is.finite(a$value)) return("No matched ART coverage and PLHIV estimates in this selection")
  if (national) return("People living with HIV receiving ART | National source estimate")
  paste0("PLHIV-weighted estimate | ", a$n, " / ", a$total,
         " countries | Estimated ", scales::comma(a$treated, accuracy = 1),
         " receiving ART / ", scales::comma(a$plhiv, accuracy = 1), " people living with HIV")
}

# Available-country totals for each year match the headline for that year.
# They are not cumulative sums across time. No aggregate bounds are invented.
h40_scope_history <- function(pool, key, years, label, national = FALSE) {
  if (national) return(hv_metric_data(pool[pool$year %in% years, ], key))
  if (key == "hiv_prevalence_15_49_pct") return(NULL)
  rows <- lapply(years, function(y) {
    part <- pool[pool$year == y, ]
    if (key == "art_coverage_all_ages_pct") {
      a <- h40_art_summary(part)
      value <- a$value; n <- a$n
    } else {
      value <- h40_sum(part[[key]]); n <- sum(is.finite(part[[key]]))
    }
    data.frame(country = paste0(label, " (", n, " countries)"), iso3 = "ALL",
               year = as.integer(y), value = value, lower = NA_real_,
               upper = NA_real_, n = n)
  })
  do.call(rbind, rows)
}

# Adult prevalence cannot be combined without age-matched population weights.
# Show separate country lines for that metric in aggregate history views.
h40_scope_plot <- function(pool, key, years, label, national, country_colours) {
  d <- h40_scope_history(pool, key, years, label, national)
  country_lines <- is.null(d)
  if (!is.null(d)) {
    validate(need(any(is.finite(d$value)), "No estimates available in this period."))
    p <- hv_add_series(plotly::plot_ly(), d, key, bands = national)
  } else {
    d <- hv_metric_data(pool[pool$year %in% years, ], key)
    validate(need(any(is.finite(d$value)), "No adult prevalence estimates in this selection."))
    p <- plotly::plot_ly()
    for (part in split(d, d$iso3)) {
      if (!any(is.finite(part$value))) next
      p <- hv_add_series(p, part, key, bands = FALSE,
                         colour = country_colours[[part$iso3[1]]],
                         name = part$country[1], legend = TRUE)
    }
  }
  p <- h40_chart(p, y = h40_unit(key), percent = hv_meta(key)$percent)
  if (country_lines) {
    p <- plotly::layout(p, margin = list(r = 150, b = 65),
                         legend = list(orientation = "v", x = 1.02, y = 1,
                                       xanchor = "left", yanchor = "top"))
  }
  p
}

h40_changes <- function(pool, end_year, key, baseline = 2010L) {
  ids <- unique(pool[c("country", "iso3", "subregion")])
  old <- pool[pool$year == baseline, ]
  now <- pool[pool$year == end_year, ]
  ids$baseline <- old[[key]][match(ids$iso3, old$iso3)]
  ids$current <- now[[key]][match(ids$iso3, now$iso3)]
  ids$lower <- now[[paste0(key, "_lower")]][match(ids$iso3, now$iso3)]
  ids$upper <- now[[paste0(key, "_upper")]][match(ids$iso3, now$iso3)]
  ids$delta <- ids$current - ids$baseline
  ids$percent_change <- ifelse(is.finite(ids$baseline) & ids$baseline > 0,
                               100 * ids$delta / ids$baseline, NA_real_)
  ids$status <- ifelse(!is.finite(ids$current), "Selected-year estimate missing",
                       ifelse(!is.finite(ids$baseline), "2010 estimate missing",
                              ifelse(!hv_meta(key)$percent & ids$baseline == 0,
                                     "Zero baseline: percent change undefined", "Available")))
  if (end_year < baseline) {
    ids$delta <- NA_real_; ids$percent_change <- NA_real_
    ids$status <- "Choose a reporting year of 2010 or later"
  }
  ids
}

h40_cohort <- function(pool, key, years) {
  d <- hv_metric_data(pool[pool$year %in% years, ], key)
  groups <- split(d, d$iso3)
  keep <- names(Filter(function(x) nrow(x) == length(years) &&
                         all(is.finite(x$value)) && identical(sort(x$year), as.integer(years)), groups))
  if (!length(keep)) return(NULL)
  d <- d[d$iso3 %in% keep, ]
  data.frame(country = paste0("Fixed cohort: ", length(keep), " countries"), iso3 = "ALL",
             year = years, value = vapply(years, function(y) sum(d$value[d$year == y]), numeric(1)),
             lower = NA_real_, upper = NA_real_, n = length(keep))
}

h40_heat_data <- function(pool, key, years, availability = FALSE) {
  countries <- unique(pool[c("country", "iso3")])
  countries <- countries[order(countries$country), ]
  z <- matrix(NA_real_, nrow(countries), length(years))
  hover <- matrix("", nrow(countries), length(years))
  for (i in seq_len(nrow(countries))) {
    rows <- pool[pool$iso3 == countries$iso3[i], ]
    values <- rows[[key]][match(years, rows$year)]
    z[i, ] <- if (availability) as.numeric(is.finite(values)) else values
    hover[i, ] <- paste0("<b>", htmltools::htmlEscape(countries$country[i]), "</b> | ", years,
                         "<br>", hv_meta(key)$label, ": ", hv_format(values, key))
  }
  list(z = z, hover = hover, years = years, countries = countries$country)
}

h40_index <- function(pool, key, years, baseline = 2010L) {
  d <- hv_metric_data(pool[pool$year %in% years, ], key)
  old <- pool[pool$year == baseline, ]
  base <- old[[key]][match(d$iso3, old$iso3)]
  d$index <- ifelse(is.finite(base) & base > 0, 100 * d$value / base, NA_real_)
  d
}

h40_chart <- function(p, x = "Year", y = "People", percent = FALSE, year = TRUE) {
  hv_chart_style(p, x_title = x, y_title = y, percent = percent, year_axis = year)
}
h40_table <- function(d, page_length = 12) {
  DT::datatable(d, rownames = FALSE, filter = "top", extensions = "Buttons",
                class = "stripe hover compact", style = "default", selection = "none",
                options = list(dom = "Bfrtip", buttons = c("copy", "csv"), pageLength = page_length,
                               scrollX = TRUE, autoWidth = TRUE, order = list()))
}
h40_card <- function(n, output_id, height = "360px", scroll = FALSE, note = NULL) {
  body <- if (scroll) div(class = "h40-scroll", uiOutput(paste0(output_id, "_box"))) else
    plotly::plotlyOutput(output_id, height = height)
  card(h40_header(n), if (!is.null(note)) p(note, class = "h40-note"), body)
}
h40_table_card <- function(n, id) card(h40_header(n), DT::DTOutput(id))
h40_grid <- function(...) div(class = "h40-grid", ...)
h40_kpi <- function(title, id, note_id, icon, bg1, bg2) {
  value_box(title, textOutput(id), div(class = "h40-kpi-note", textOutput(note_id)),
            showcase = bsicons::bs_icon(icon), showcase_layout = "top right",
            theme = value_box_theme(bg = bg1, fg = "#D8D9DA"), min_height = "220px",
            class = "h40-kpi", style = paste0("background:linear-gradient(135deg,", bg1, ",", bg2, ")!important;"))
}

# =========================================================
# EDIT 1: SOFT CHARCOAL USER INTERFACE; EDITS 6-7: DESCRIPTIVE, UNNUMBERED HEADERS
# =========================================================
ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#181B1F", fg = "#D8D9DA", primary = "#73C7B8"),
  tags$head(tags$title("Evidence Across Africa: The HIV Pulse"), tags$style(HTML("
    body{padding:28px;background:#181B1F;color:#D8D9DA;color-scheme:dark;}
    .h40-eyebrow{color:#73C7B8;letter-spacing:3px;font-size:12px;font-weight:750;margin-bottom:12px;}
    .h40-hero h1{font-size:clamp(30px,3vw,44px);font-weight:500;letter-spacing:-1px;margin-bottom:10px;}
    .h40-hero h1 strong{font-weight:750;}
    .h40-subtitle{color:#AAB0B8;margin-bottom:26px;}
    .h40-filters{display:flex;flex-wrap:wrap;gap:18px;align-items:flex-start;padding:20px;
      border:1px solid #343A43;background:#202226;border-radius:20px;margin-bottom:22px;}
    .h40-filters .shiny-input-container{margin-bottom:0;}
    .h40-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:20px;margin:20px 0;}
    .h40-grid>.card{margin-bottom:0;min-width:0;}
    .card{background:#202226;border:1px solid #343A43;border-radius:20px;box-shadow:0 12px 28px rgba(0,0,0,.12);margin:18px 0;}
    .card-header{background:transparent;border-bottom:1px solid #343A43;color:#D8D9DA;padding:16px 20px;font-size:14px;font-weight:650;}
    .h40-note{font-size:12px;line-height:1.7;color:#AAB0B8;padding:0 10px;}
    .h40-scroll{max-height:570px;overflow-y:auto;}
    .h40-kpi{margin:0;border-radius:22px;border:1px solid rgba(216,217,218,.10);transition:transform .18s ease;}
    .h40-kpi:hover{transform:translateY(-3px);}
    .h40-kpi .value-box-title{font-size:14px;font-weight:650;}
    .h40-kpi .value-box-value{font-size:2.5rem;font-weight:800;letter-spacing:-1px;line-height:1.15;}
    .h40-kpi-note{font-size:11px;line-height:1.6;opacity:.9;margin-top:10px;}
    .nav-tabs{border-bottom:1px solid #343A43;gap:4px;flex-wrap:wrap;}
    .nav-tabs .nav-link{color:#AAB0B8;padding:12px 15px;border-radius:12px 12px 0 0;}
    .nav-tabs .nav-link.active{color:#D8D9DA;background:#202226;border-color:#343A43;border-top:3px solid #73C7B8;}
    .leaflet-container{background:#181B1F!important;border-radius:12px;}
    .leaflet-tooltip,.leaflet-control.legend{background:#2C3137;color:#D8D9DA;border:1px solid #343A43;border-radius:10px;}
    .leaflet-tooltip{padding:12px;}
    .shiny-output-error-validation{padding:24px;color:#C3C7CD;}
    .dataTables_wrapper{color:#C3C7CD;font-size:12px;padding:8px;}
    table.dataTable tbody tr,table.dataTable tbody tr.odd,table.dataTable tbody tr.even{background:#202226!important;color:#D8D9DA;}
    table.dataTable thead th{background:#2C3137;color:#D8D9DA;border-color:#343A43;}
    table.dataTable tbody td{border-color:#343A43!important;}
    .dataTables_wrapper input,.dataTables_wrapper select{background:#181B1F!important;color:#D8D9DA!important;border:1px solid #343A43!important;}
    .dt-button{background:#2C3137!important;color:#D8D9DA!important;border:1px solid #343A43!important;border-radius:8px!important;}
    .dataTables_wrapper .dataTables_paginate .paginate_button{color:#C3C7CD!important;}
    .selectize-input,.selectize-dropdown{background:#202226!important;color:#D8D9DA!important;border-color:#343A43!important;}
    .selectize-dropdown .active{background:#2C3137!important;color:#D8D9DA!important;}
    .selectize-input .item{background:#2C3137!important;color:#D8D9DA!important;}
    .nav-tabs .nav-link:hover{color:#73C7B8;background:#2C3137;}
    /* BAR OVERLAY FIX: no opaque toolbar/group rectangle over chart data. */
    .js-plotly-plot .plotly .modebar,
    .js-plotly-plot .plotly .modebar-group,
    .js-plotly-plot .plotly .modebar-container{background:transparent!important;}
    .js-plotly-plot .plotly .modebar-btn path{fill:#AAB0B8!important;}
    .js-plotly-plot .plotly .modebar-btn:hover path,
    .js-plotly-plot .plotly .modebar-btn.active path{fill:#73C7B8!important;}
    .js-plotly-plot .plotly .modebar{top:4px!important;right:4px!important;}
    .form-control,.form-select{background-color:#202226;color:#D8D9DA;border-color:#343A43;}
    .leaflet-bar a,.leaflet-control-attribution{background:#2C3137!important;color:#C3C7CD!important;}
    .leaflet-control-attribution a{color:#73C7B8!important;}
    .h40-scroll{scrollbar-color:#4C5159 #202226;}
    .h40-scroll::-webkit-scrollbar{width:9px;height:9px;}
    .h40-scroll::-webkit-scrollbar-track{background:#202226;}
    .h40-scroll::-webkit-scrollbar-thumb{background:#4C5159;border:2px solid #202226;border-radius:8px;}
    table.dataTable tbody tr:hover td{background:#181B1F!important;}
    .h40-panel{padding:20px;line-height:1.8;color:#C3C7CD;}
    @media(max-width:1100px){.h40-grid{grid-template-columns:1fr;}}
    @media(max-width:600px){body{padding:14px;}}
    @media(prefers-reduced-motion:reduce){.h40-kpi{transition:none;}.h40-kpi:hover{transform:none;}}
  "))),
  div(class = "h40-hero", p("AFRICA HIV OBSERVATORY", class = "h40-eyebrow"),
      h1(strong("Evidence Across Africa:"), " The HIV Pulse"), div(class = "h40-subtitle", textOutput("scope_subtitle"))),
  div(class = "h40-filters",
      selectInput("year", "Year", sort(unique(hiv$year), decreasing = TRUE), max(hiv$year), width = "155px"),
      selectInput("subregion", "Subregion", c("All subregions" = "ALL", stats::setNames(region_names, region_names)), "ALL", width = "205px"),
      selectInput("country", "Country", c("All countries" = "ALL", country_choices), "ALL", width = "270px"),
      selectInput("indicator", "Indicator", stats::setNames(hv_indicators$key, hv_indicators$label), "plhiv_all_ages", width = "290px"),
      sliderInput("start_year", "Trend start year", min(hiv$year), max(hiv$year), min(hiv$year), step = 1, sep = "", width = "205px")
  ),
  navset_tab(id = "main_tabs",
             nav_panel("Africa overview",
                       p("Counts combine available countries in the selected year. Combined ART coverage is weighted by people living with HIV; contributing countries are shown beneath it.", class = "h40-note"),
                       layout_column_wrap(width = "240px", fill = FALSE, gap = "18px",
                                          h40_kpi(h40_title(1), "v01", "v01_note", "people-fill", "#263C37", "#22332F"),
                                          h40_kpi(h40_title(2), "v02", "v02_note", "plus-circle-fill", "#2B3748", "#27313F"),
                                          h40_kpi(h40_title(3), "v03", "v03_note", "activity", "#3A3348", "#342E40"),
                                          h40_kpi(h40_title(4), "v04", "v04_note", "shield-check", "#3A3428", "#322D24"),
                                          h40_kpi(h40_title(5), "v05", "v05_note", "globe", "#30343B", "#292D33")
                       ),
                       h40_grid(card(h40_header(6), leaflet::leafletOutput("hv_map", height = "550px")),
                                # Chart-only control: placed in its own card so
                                # it cannot be mistaken for a global data filter.
                                card(h40_header(7),
                                     selectInput("rank_n", "Countries shown (this chart only)",
                                                 c("Top 10" = 10, "Top 15" = 15,
                                                   "Top 25" = 25, "All countries" = 54),
                                                 15, width = "270px"),
                                     p("This control changes only the ranking. Use Year, Subregion and Country above to filter the dashboard.", class = "h40-note"),
                                     div(class = "h40-scroll", uiOutput("v07_box")))),
                       # EDIT 2: These two cards sit side by side on desktop,
                       # and stack on narrow screens, using the shared grid.
                       h40_grid(
                         card(h40_header(8), p(class = "h40-note", textOutput("v08_note")),
                              plotly::plotlyOutput("v08", height = "390px")),
                         card(card_header(textOutput("burden_title")),
                              p("Each value is the number living with HIV per 100 adults aged 15-49. Countries are ranked by prevalence, not total cases.", class = "h40-note"),
                              div(class = "h40-scroll", uiOutput("burden_overview_box")))
                       )
             ),
             nav_panel("Country profile",
                       p(class = "h40-note", textOutput("profile_heading")),
                       conditionalPanel("input.country",
                                        card(h40_header(9), uiOutput("v09")),
                                        p("Cards use the reporting year. All-country views combine counts and ART coverage; adult prevalence remains country-specific. Bands show source uncertainty only for a single country.", class = "h40-note"),
                                        h40_grid(h40_card(10, "v10"), h40_card(11, "v11"), h40_card(12, "v12"),
                                                 h40_card(13, "v13"), h40_card(14, "v14"), h40_card(15, "v15")),
                                        h40_card(16, "v16", note = "Uses the selected indicator. Percentage indicators change in percentage points. Colours indicate direction only.")
                       )
             ),
             nav_panel("Country comparisons",
                       p("Global year, subregion and country filters apply. With all countries selected, the optional comparison list narrows this tab; clear it to compare every country in scope.", class = "h40-note"),
                       selectizeInput("compare_countries", "Comparison countries", country_choices,
                                      selected = character(0), multiple = TRUE,
                                      width = "100%", options = list(plugins = list("remove_button"), placeholder = "All countries in scope (or choose a subset)")),
                       h40_grid(h40_card(17, "v17"), h40_card(18, "v18", scroll = TRUE)),
                       h40_card(19, "v19", scroll = TRUE, note = "Blank cells mean the estimate is unavailable."),
                       h40_card(20, "v20", note = "Count indicators only. A positive, available 2010 estimate is required for each country."),
                       h40_grid(h40_card(21, "v21"),
                                h40_card(22, "v22", scroll = TRUE,
                                         note = "Population-relative burden: people living with HIV per 100 adults aged 15-49. This is an age-matched percentage, not an all-age case total.")),
                       h40_table_card(23, "v23")
             ),
             nav_panel("Progress since 2010",
                       p("Choose 2010 or later. Counts use percent change; prevalence and ART coverage use percentage-point change. Direction alone does not establish improvement or deterioration.", class = "h40-note"),
                       h40_grid(h40_card(24, "v24", scroll = TRUE), h40_card(25, "v25", scroll = TRUE)),
                       h40_card(26, "v26", scroll = TRUE),
                       h40_grid(h40_card(27, "v27", scroll = TRUE), h40_card(28, "v28", scroll = TRUE)),
                       h40_table_card(29, "v29")
             ),
             nav_panel("Subregional patterns",
                       p("Subregional count trends use fixed cohorts with complete estimates across the history window. Percentages are compared as country values, not averaged.", class = "h40-note"),
                       h40_grid(h40_card(30, "v30"), h40_card(31, "v31")),
                       h40_grid(h40_card(32, "v32"), h40_card(33, "v33")),
                       selectInput("dot_indicator", "Indicator", stats::setNames(metric_keys[c(2, 5)], hv_indicators$label[c(2, 5)]),
                                   "art_coverage_all_ages_pct", width = "320px"),
                       h40_card(34, "v34")
             ),
             nav_panel("Data explorer",
                       div(class = "h40-filters",
                           selectInput("data_layout", "Table format", c("Country-year: all five indicators" = "wide", "Selected indicator" = "long"), "wide", width = "300px"),
                           downloadButton("download_data", "Download filtered CSV")),
                       h40_table_card(35, "v35"),
                       h40_card(36, "v36", scroll = TRUE),
                       h40_grid(h40_card(37, "v37"), h40_table_card(38, "v38")),
                       h40_grid(card(h40_header(39), uiOutput("v39")),
                                card(h40_header(40), div(class = "h40-panel",
                                                                     p(strong("Source: "), "UNAIDS AIDSinfo Estimates 2026 package. Observation years: 1990-2025."),
                                                                     p(tags$a("UNAIDS dataset and guidance", href = "https://aidsinfo.unaids.org/dataset", target = "_blank")),
                                                                     p("Scope: 54 African UN member states, including Northern Africa. Country boundaries: Natural Earth. Subregions: UN M49."),
                                                                     p("These are modelled estimates and source programme indicators. Blank values mean unavailable, not zero. No missing estimates are imputed."),
                                                                     p("Overview and profile counts sum available countries separately in each year, matching the headline totals. ART trends use matched country inputs in each year. Subregional trends use fixed complete-data cohorts. Coverage can change between years."),
                                                                     p("Combined ART coverage uses sum(PLHIV x national ART coverage) / sum(PLHIV) for countries with both inputs. Adult HIV prevalence is shown by country because matching adult population denominators are not provided."),
                                                                     p("Uncertainty bands use supplied lower and upper bounds. Bounds are not summed across countries. They are not labelled as a particular confidence level."),
                                                                     p("Progress uses exact 2010 and selected-year estimates. Percent changes with zero or missing baselines remain unavailable. Percentage indicators use percentage-point change."),
                                                                     p("A rise in people living with HIV may reflect improved survival and other factors. Change colours indicate direction, not a performance rating."),
                                                                     p("Latest-year tables use the selected history window through the reporting year. Downloads retain unrounded numbers and blank missing values."),
                                                                     tags$details(tags$summary("Visual guide"), tableOutput("visual_inventory"))
                                )))
             )
  ),
  p("Source: UNAIDS AIDSinfo. Prevalence: ages 15-49. Other core indicators: all ages.", class = "h40-note")
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  regional_pool <- reactive({
    req(input$subregion)
    if (input$subregion == "ALL") return(hiv)
    hiv[hiv$subregion == input$subregion, ]
  })
  scope_pool <- reactive({
    req(input$country)
    d <- regional_pool()
    if (input$country != "ALL") d <- d[d$iso3 == input$country, ]
    d
  })
  scope_year <- reactive({ req(input$year); d <- scope_pool(); d[d$year == as.integer(input$year), ] })
  chosen_key <- reactive({ req(input$indicator); hv_meta(input$indicator); input$indicator })
  scope_metric <- reactive(hv_metric_data(scope_year(), chosen_key()))
  hist_years <- reactive({
    req(input$year, input$start_year)
    seq.int(min(as.integer(input$start_year), as.integer(input$year)), as.integer(input$year))
  })
  history_pool <- reactive({ d <- scope_pool(); d[d$year %in% hist_years(), ] })
  country_history <- reactive({
    # EDIT 4: Both a single country and ALL are valid profile scopes.
    d <- history_pool(); d[order(d$year), ]
  })
  comparison_pool <- reactive({
    # EDIT 4: Never bypass the global country filter. A single country wins;
    # an optional comparison list further filters the all-country scope.
    d <- scope_pool()
    if (input$country == "ALL" && length(input$compare_countries))
      d <- d[d$iso3 %in% input$compare_countries, ]
    d
  })
  comparison_year <- reactive({ d <- comparison_pool(); d[d$year == as.integer(input$year), ] })
  comparison_metric <- reactive(hv_metric_data(comparison_year(), chosen_key()))
  scope_label <- reactive({
    req(input$country, input$subregion)
    if (input$country != "ALL") return(country_list$country[match(input$country, country_list$iso3)])
    if (input$subregion != "ALL") return(input$subregion)
    "All African countries"
  })
  observeEvent(input$subregion, {
    countries <- unique(regional_pool()[c("country", "iso3")])
    countries <- countries[order(countries$country), ]
    choices <- stats::setNames(countries$iso3, countries$country)
    selected <- isolate(input$country)
    if (is.null(selected) || !selected %in% c("ALL", countries$iso3)) selected <- "ALL"
    updateSelectInput(session, "country", choices = c("All countries" = "ALL", choices), selected = selected)
    selected_compare <- intersect(isolate(input$compare_countries), countries$iso3)
    updateSelectizeInput(session, "compare_countries", choices = choices, selected = selected_compare)
  }, ignoreInit = FALSE)
  observeEvent(input$year, {
    start <- isolate(input$start_year)
    if (is.null(start)) start <- min(hiv$year)
    updateSliderInput(session, "start_year", max = as.integer(input$year),
                      value = min(start, as.integer(input$year)))
  }, ignoreInit = FALSE)
  output$scope_subtitle <- renderText(paste0(scope_label(), " | Year: ", input$year,
                                             " | Trend period: ", min(hist_years()), "-", max(hist_years()), " | ", hv_meta(chosen_key())$label))
  
  country_colours <- stats::setNames(grDevices::hcl.colors(nrow(country_list), "Dark 3"), country_list$iso3)

  # EDITS 6-7: Headers update with the filters. Fixed-indicator charts
  chart_title <- function(n) {
    key <- chosen_key(); metric <- hv_meta(key)$label
    scope <- scope_label(); year <- as.integer(input$year)
    window <- paste0(min(hist_years()), "-", max(hist_years()))
    comparison <- if (input$country != "ALL") scope else
      paste0(length(unique(comparison_pool()$iso3)), " countries in ", scope)
    title <- switch(as.character(n),
      "6" = paste0(metric, " by country | ", scope, " | ", year),
      "7" = paste0("Countries ranked by ", metric, " | ", scope, " | ", year),
      "8" = paste0(metric, if (key == "hiv_prevalence_15_49_pct" && input$country == "ALL") " by country" else "", " over time | ", scope, " | ", window),
      "16" = paste0("Year-to-year change in ", metric, " | ", scope, " | ", window),
      "17" = paste0(metric, " over time | ", comparison, " | ", window),
      "18" = paste0(metric, " ranked by country | ", comparison, " | ", year),
      "19" = paste0(metric, " by country and year | ", comparison, " | ", window),
      "20" = paste0(metric, " relative to 2010 (2010 = 100) | ", comparison, " | ", window),
      "21" = paste0("Adult HIV prevalence (ages 15-49) versus ART coverage | ", comparison, " | ", year),
      "22" = paste0("Adults aged 15-49 living with HIV (%) by country | ", comparison, " | ", year),
      "23" = paste0(metric, ": country estimates and changes since 2010 | ", comparison, " | ", year),
      "29" = paste0(metric, ": country estimates and changes since 2010 | ", scope, " | ", year),
      "33" = paste0(metric, " over time, stacked by subregion | ", scope, " | ", window),
      "34" = paste0(hv_meta(input$dot_indicator)$label, " by country, grouped by subregion | ", scope, " | ", year),
      "35" = paste0(if (input$data_layout == "long") metric else "All five HIV indicators",
                     ": country-year estimates and uncertainty bounds | ", scope, " | ", window),
      "36" = paste0("Availability of ", metric, " by country and year | ", scope, " | ", window),
      "38" = paste0("Latest available year for each HIV indicator within ", window, " | ", scope),
      "39" = paste0(metric, ": definition, units and interpretation"),
      NULL)
    if (!is.null(title)) return(title)
    base <- h40_title(n)
    if (n %in% c(10:15, 30:32, 35)) return(paste0(base, " | ", scope, " | ", window))
    if (n %in% c(9, 24:28, 37)) return(paste0(base, " | ", scope, " | ", year))
    base
  }
  for (i in seq_len(nrow(visual_catalog))) local({
    n <- i
    output[[paste0("chart_title_", n)]] <- renderText(chart_title(n))
  })
  output$burden_title <- renderText(paste0("Adults aged 15-49 living with HIV (%) by country | ", scope_label(), " | ", input$year))

  # 01-05: Overview KPIs. Do not turn an entirely missing metric into zero.
  for (j in 1:3) local({
    n <- j; key <- count_columns[n]
    output[[sprintf("v%02d", n)]] <- renderText(hv_headline(h40_sum(scope_year()[[key]]), key))
    output[[sprintf("v%02d_note", n)]] <- renderText({
      d <- scope_year(); x <- d[[key]]; available <- sum(is.finite(x))
      if (!available) return("No estimates available in this selection")
      paste0(hv_format(h40_sum(x), key), " ", tolower(h40_unit(key)), " | ",
             available, " / ", nrow(d), " countries")
    })
  })
  # EDIT 4: No-country/ALL now displays the combined coverage, not a dash.
  output$v04 <- renderText(hv_headline(
    h40_art_summary(scope_year(), national = input$country != "ALL")$value,
    "art_coverage_all_ages_pct"))
  output$v04_note <- renderText(h40_art_note(scope_year(), national = input$country != "ALL"))
  output$v05 <- renderText({
    d <- scope_year()
    paste0(sum(rowSums(is.finite(as.matrix(d[count_columns]))) > 0), " / ", nrow(d))
  })
  output$v05_note <- renderText("At least one of the three count indicators is available")
  
  # 06: Reused, complete-country Leaflet map and map-to-profile interaction.
  output$hv_map <- leaflet::renderLeaflet({
    d <- scope_metric()
    key <- chosen_key()
    m <- hv_meta(key)
    geo <- hv_africa_map(hiv)
    j <- match(geo$iso3, d$iso3)
    geo$value <- d$value[j]
    geo$lower <- d$lower[j]
    geo$upper <- d$upper[j]
    geo$year <- as.integer(input$year)
    geo$active <- geo$iso3 %in% d$iso3
    tip <- hv_hover(sf::st_drop_geometry(geo), key)
    tip[!geo$active] <- paste0("<b>", htmltools::htmlEscape(geo$country[!geo$active]),
                               "</b><br>Outside the selected scope")
    geo$tip <- tip
    finite_values <- geo$value[is.finite(geo$value)]
    domain <- if (length(finite_values)) range(c(0, finite_values)) else c(0, 1)
    if (diff(domain) == 0) domain <- c(0, 1)
    pal <- leaflet::colorNumeric(c("#28383A", "#497F7B", m$colour),
                                 domain = domain, na.color = "#343A43")
    map <- leaflet::leaflet(geo, options = leaflet::leafletOptions(minZoom = 2))
    map <- leaflet::addPolygons(
      map, layerId = ~iso3, fillColor = ~pal(value), fillOpacity = .92,
      color = "#727980", weight = .7,
      label = lapply(geo$tip, htmltools::HTML),
      labelOptions = leaflet::labelOptions(direction = "auto", opacity = 1),
      highlightOptions = leaflet::highlightOptions(color = "#181B1F", weight = 2,
                                                   fillOpacity = 1, bringToFront = TRUE)
    )
    # Make the five small island states easier to hover and click.
    small <- geo[geo$iso3 %in% c("CPV", "COM", "STP", "MUS", "SYC"), ]
    pts <- sf::st_transform(sf::st_point_on_surface(sf::st_transform(small, 3857)), 4326)
    xy <- sf::st_coordinates(pts)
    pts$lng <- xy[, 1]; pts$lat <- xy[, 2]
    map <- leaflet::addCircleMarkers(
      map, data = pts, lng = ~lng, lat = ~lat, radius = 5,
      layerId = ~iso3, color = "#C3C7CD", weight = 1,
      fillColor = ~pal(value), fillOpacity = 1,
      label = lapply(pts$tip, htmltools::HTML)
    )
    map <- leaflet::fitBounds(map, -26, -36, 58, 38)
    if (length(unique(finite_values)) > 1L) {
      map <- leaflet::addLegend(map, pal = pal, values = finite_values,
                                title = m$label, opacity = 1, position = "bottomleft",
                                na.label = "Unavailable / outside selection")
    } else if (length(finite_values)) {
      map <- leaflet::addLegend(map, colors = pal(finite_values[1]),
                                labels = hv_format(finite_values[1], key),
                                title = m$label, position = "bottomleft")
    }
    leaflet::addControl(map, "Boundaries: Natural Earth", position = "bottomright")
  })
  
  # Map interaction uses the same global country control as the KPI cards.
  open_country <- function(click) {
    if (is.null(click$id) || !click$id %in% regional_pool()$iso3) return(invisible(NULL))
    shiny::updateSelectInput(session, "country", selected = click$id)
    bslib::nav_select("main_tabs", selected = "Country profile", session = session)
  }
  shiny::observeEvent(input$hv_map_shape_click, open_country(input$hv_map_shape_click))
  shiny::observeEvent(input$hv_map_marker_click, open_country(input$hv_map_marker_click))
  
  
  
  # Shared sizing keeps long rankings and 54-country heatmaps readable.
  set_plot_height <- function(id, rows) {
    force(id); force(rows)
    output[[paste0(id, "_box")]] <- renderUI(
      plotly::plotlyOutput(id, height = paste0(max(420, 25 * rows() + 110), "px")))
  }
  rank_data <- reactive({
    req(input$rank_n)
    d <- scope_metric(); d <- d[is.finite(d$value), ]
    head(d[order(-d$value, d$country), ], as.integer(input$rank_n))
  })
  set_plot_height("v07", function() nrow(rank_data()))
  # EDIT 3: Explicit hovertext and textposition prevent Plotly from printing
  # tooltip HTML/values inside bars. All bar constructors follow this pattern.
  rank_plot <- function(d, key) {
    validate(need(nrow(d) > 0, "No estimates available for this selection."))
    m <- hv_meta(key); d <- d[order(-d$value, d$country), ]
    d$hover <- hv_hover(d, key)
    colours <- rev(grDevices::colorRampPalette(c("#4E8D80", m$colour))(nrow(d)))
    p <- plotly::plot_ly(d, x = ~value, y = ~country, type = "bar", orientation = "h",
                         marker = list(color = colours), hovertext = ~hover, hoverinfo = "text", textposition = "none", showlegend = FALSE)
    p <- h40_chart(p, x = m$label, y = "", year = FALSE)
    plotly::layout(p, margin = list(l = 180, r = 30, t = 54, b = 70),
                   xaxis = list(tickformat = if (m$percent) ".2f" else "~s",
                                ticksuffix = if (m$percent) "%" else "", rangemode = "tozero"),
                   yaxis = list(type = "category", categoryorder = "array", categoryarray = rev(d$country),
                                showgrid = FALSE, tickformat = NULL))
  }
  output$v07 <- plotly::renderPlotly(rank_plot(rank_data(), chosen_key()))
  
  # EDIT 2: Population-relative burden in the reporting year, next to history.
  burden_metric <- reactive(hv_metric_data(scope_year(), "hiv_prevalence_15_49_pct"))
  set_plot_height("burden_overview", function() nrow(burden_metric()))
  output$burden_overview <- plotly::renderPlotly({
    d <- burden_metric()
    rank_plot(d[is.finite(d$value), ], "hiv_prevalence_15_49_pct")
  })

  # EDIT 4: ART has an aggregate trend. Adult prevalence remains separate
  # country lines because an adult population denominator is not supplied.
  output$v08_note <- renderText({
    if (input$country != "ALL") return(paste0(scope_label(), " | Shading: source uncertainty bounds where available."))
    key <- chosen_key()
    if (key == "hiv_prevalence_15_49_pct") return("Country-specific adult prevalence. Each line is a country; no unmatched population weights or simple percentage averages are used.")
    if (key == "art_coverage_all_ages_pct") return("Combined ART coverage is weighted by people living with HIV in each year. Only matched coverage and PLHIV estimates contribute; the country set can change between years.")
    "Counts sum countries with estimates in each year, matching the headline totals. The country set can change between years; no aggregate uncertainty band is constructed."
  })
  output$v08 <- plotly::renderPlotly(h40_scope_plot(
    scope_pool(), chosen_key(), hist_years(), scope_label(),
    input$country != "ALL", country_colours))

  # 09-16: Country profile, including the five source uncertainty-band trends.
  output$profile_heading <- renderText({
    paste0(scope_label(), " | Year: ", input$year,
           if (input$country == "ALL") " | Combined counts and ART; adult prevalence is shown by country." else "")
  })
  output$v09 <- renderUI({
    country_history()
    cards <- lapply(seq_len(nrow(hv_indicators)), function(i) {
      m <- hv_indicators[i, ]
      h40_kpi(m$label, paste0("profile_kpi_", i), paste0("profile_note_", i), m$icon, m$bg1, m$bg2)
    })
    do.call(layout_column_wrap, c(list(width = "240px", fill = FALSE, gap = "18px"), cards))
  })
  for (i in seq_len(nrow(hv_indicators))) local({
    index <- i; key <- hv_indicators$key[index]
    output[[paste0("profile_kpi_", index)]] <- renderText({
      d <- scope_year(); national <- input$country != "ALL"
      if (key == "art_coverage_all_ages_pct") return(hv_headline(h40_art_summary(d, national)$value, key))
      if (!national && hv_meta(key)$percent) return("By country")
      hv_headline(h40_sum(d[[key]]), key)
    })
    output[[paste0("profile_note_", index)]] <- renderText({
      d <- scope_year(); national <- input$country != "ALL"
      if (key == "art_coverage_all_ages_pct") return(h40_art_note(d, national))
      if (!national && hv_meta(key)$percent) return("See the adult-prevalence country lines below; matching adult population weights are not supplied.")
      if (!national) return(paste0(sum(is.finite(d[[key]])), " / ", nrow(d), " countries with estimates"))
      if (nrow(d) != 1L || !is.finite(d[[key]])) return("No estimate for this country-year")
      paste0("Bounds: ", hv_bounds(d[[paste0(key, "_lower")]], d[[paste0(key, "_upper")]], key))
    })
    output[[sprintf("v%02d", index + 9L)]] <- plotly::renderPlotly(
      h40_scope_plot(scope_pool(), key, hist_years(), scope_label(),
                     input$country != "ALL", country_colours))
  })
  output$v15 <- plotly::renderPlotly({
    pool <- country_history()
    a <- h40_scope_history(pool, "new_infections_all_ages", hist_years(), scope_label(), input$country != "ALL")
    b <- h40_scope_history(pool, "aids_deaths_all_ages", hist_years(), scope_label(), input$country != "ALL")
    validate(need(any(is.finite(a$value)) || any(is.finite(b$value)), "No infection or death estimates in this period."))
    p <- hv_add_series(plotly::plot_ly(), a, "new_infections_all_ages", bands = FALSE, colour = "#86A9D9", name = "New infections", legend = TRUE)
    p <- hv_add_series(p, b, "aids_deaths_all_ages", bands = FALSE, colour = "#B0A0D0", name = "AIDS-related deaths", legend = TRUE)
    h40_chart(p, y = "Annual number of people")
  })
  output$v16 <- plotly::renderPlotly({
    key <- chosen_key(); m <- hv_meta(key)
    pool <- country_history()
    history <- h40_scope_history(pool, key, hist_years(), scope_label(), input$country != "ALL")
    if (is.null(history)) {
      # Adult prevalence stays country-specific when no matching weights exist.
      parts <- split(hv_metric_data(pool, key), pool$iso3)
      history <- do.call(rbind, lapply(parts, hv_annual_change))
      d <- history
    } else d <- hv_annual_change(history)
    d <- d[is.finite(d$change), ]
    validate(need(nrow(d) > 0, "Two consecutive years with estimates are needed."))
    accuracy <- if (key == "hiv_prevalence_15_49_pct") .0001 else if (m$percent) .01 else 1
    suffix <- if (m$percent) " percentage points" else paste0(" ", tolower(h40_unit(key)))
    d$hover <- paste0(d$country, " | ", d$year, "<br>", m$label, "<br>Annual change: ",
                      ifelse(d$change > 0, "+", ""), scales::number(d$change, accuracy = accuracy, big.mark = ","), suffix)
    p <- plotly::plot_ly(d, x = ~year, y = ~change, type = "bar",
                         marker = list(color = ifelse(d$change >= 0, "#86A9D9", "#B0A0D0")), hovertext = ~hover, hoverinfo = "text", textposition = "none", showlegend = FALSE)
    p <- h40_chart(p, y = if (m$percent) "Change (percentage points)" else paste0("Change (", tolower(h40_unit(key)), ")"), percent = FALSE)
    if (input$country == "ALL" && key == "hiv_prevalence_15_49_pct") {
      p <- plotly::plot_ly(d, x = ~year, y = ~change, color = ~country,
                           colors = unname(country_colours[unique(d$iso3)]),
                           type = "bar", hovertext = ~hover, hoverinfo = "text",
                           textposition = "none")
      p <- h40_chart(p, y = "Annual change (percentage points)")
    }
    plotly::layout(p, barmode = "group", yaxis = list(zeroline = TRUE,
                   zerolinewidth = 1.5, ticksuffix = if (m$percent) " pp" else ""))
  })
  
  # 17-23: Country comparisons. Named colours stay stable across selections.
  output$v17 <- plotly::renderPlotly({
    key <- chosen_key(); pool <- comparison_pool(); pool <- pool[pool$year %in% hist_years(), ]
    d <- hv_metric_data(pool, key)
    validate(need(any(is.finite(d$value)), "No estimates for the selected countries and period."))
    p <- plotly::plot_ly()
    for (part in split(d, d$iso3)) {
      if (!any(is.finite(part$value))) next
      p <- hv_add_series(p, part, key, bands = FALSE, colour = country_colours[[part$iso3[1]]], name = part$country[1], legend = TRUE)
    }
    p <- h40_chart(p, y = h40_unit(key), percent = hv_meta(key)$percent)
    plotly::layout(p, margin = list(r = 165, b = 65),
                   legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left", yanchor = "top"))
  })
  set_plot_height("v18", function() nrow(comparison_metric()))
  output$v18 <- plotly::renderPlotly({ d <- comparison_metric(); rank_plot(d[is.finite(d$value), ], chosen_key()) })
  
  heat_plot <- function(d, key, availability = FALSE) {
    validate(need(length(d$countries) > 0 && length(d$years) > 0, "No countries or years selected."))
    colours <- if (availability) list(list(0, "#343A43"), list(.499, "#343A43"), list(.5, "#73C7B8"), list(1, "#73C7B8")) else
      list(list(0, "#293438"), list(.5, "#497F7B"), list(1, "#73C7B8"))
    scale <- if (availability) list(tickvals = c(0, 1), ticktext = c("Missing", "Available"), tickfont = list(color = "#D8D9DA")) else
      list(tickformat = if (hv_meta(key)$percent) ".2f" else "~s", ticksuffix = if (hv_meta(key)$percent) "%" else "", tickfont = list(color = "#D8D9DA"))
    p <- plotly::plot_ly(x = d$years, y = d$countries, z = d$z, type = "heatmap",
                         text = d$hover, hoverinfo = "text", hoverongaps = TRUE, colorscale = colours,
                         colorbar = scale, zmin = if (availability) 0 else NULL, zmax = if (availability) 1 else NULL,
                         xgap = 1, ygap = 1)
    p <- h40_chart(p, y = "")
    plotly::layout(p, margin = list(l = 185, r = 90, t = 54, b = 60),
                   yaxis = list(type = "category", autorange = "reversed", showgrid = FALSE, tickformat = NULL))
  }
  set_plot_height("v19", function() length(unique(comparison_pool()$iso3)))
  output$v19 <- plotly::renderPlotly(heat_plot(h40_heat_data(comparison_pool(), chosen_key(), hist_years()), chosen_key()))
  output$v20 <- plotly::renderPlotly({
    key <- chosen_key()
    validate(need(!hv_meta(key)$percent, "Choose a count indicator for indexed trends. Percentage indicators are compared using percentage points."))
    d <- h40_index(comparison_pool(), key, hist_years())
    validate(need(any(is.finite(d$index)), "No countries have both an available positive 2010 baseline and estimates in this period."))
    p <- plotly::plot_ly()
    for (part in split(d, d$iso3)) {
      if (!any(is.finite(part$index))) next
      part <- part[order(part$year), ]
      part$hover <- paste0(part$country, " | ", part$year, "<br>Index: ", scales::number(part$index, accuracy = .1), " (2010 = 100)<br>Estimate: ", hv_format(part$value, key))
      p <- plotly::add_trace(p, data = part, x = ~year, y = ~index, type = "scatter", mode = "lines+markers", connectgaps = FALSE,
                             line = list(color = country_colours[[part$iso3[1]]], width = 2.5), marker = list(size = 4),
                             text = ~hover, hoverinfo = "text", name = part$country[1], inherit = FALSE)
    }
    p <- h40_chart(p, y = "Index (2010 = 100)")
    plotly::layout(p, margin = list(r = 165, b = 65),
                   legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
                   shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 100, y1 = 100,
                                      line = list(color = "#AAB0B8", dash = "dot"))))
  })
  scatter_plot <- function() {
    d <- comparison_year()
    valid <- is.finite(d$hiv_prevalence_15_49_pct) & is.finite(d$art_coverage_all_ages_pct)
    d <- d[valid, ]
    validate(need(nrow(d) > 0, "No countries have all required estimates for this chart."))
    d$size <- 11
    d$hover <- paste0("<b>", htmltools::htmlEscape(d$country), "</b> | ", input$year,
                      "<br>Prevalence, ages 15-49: ", hv_format(d$hiv_prevalence_15_49_pct, metric_keys[2]),
                      "<br>ART coverage: ", hv_format(d$art_coverage_all_ages_pct, metric_keys[5]))
    p <- plotly::plot_ly()
    for (part in split(d, d$subregion)) {
      p <- plotly::add_trace(p, data = part, x = ~hiv_prevalence_15_49_pct, y = ~art_coverage_all_ages_pct,
                             type = "scatter", mode = "markers", name = part$subregion[1],
                             marker = list(color = region_colours[[part$subregion[1]]], size = part$size, opacity = .85,
                                           line = list(color = "#D8D9DA", width = .6)), text = ~hover, hoverinfo = "text", inherit = FALSE)
    }
    p <- h40_chart(p, x = "HIV prevalence, ages 15-49 (%)", y = "ART coverage (%)", percent = TRUE, year = FALSE)
    plotly::layout(p, xaxis = list(tickformat = ".2f", ticksuffix = "%"))
  }
  output$v21 <- plotly::renderPlotly(scatter_plot())
  set_plot_height("v22", function() nrow(comparison_year()))
  output$v22 <- plotly::renderPlotly({
    d <- hv_metric_data(comparison_year(), "hiv_prevalence_15_49_pct")
    rank_plot(d[is.finite(d$value), ], "hiv_prevalence_15_49_pct")
  })
  
  progress_table <- function(pool, key) {
    d <- h40_changes(pool, as.integer(input$year), key)
    data.frame(Country = d$country, ISO3 = d$iso3, Year = as.integer(input$year),
               Indicator = hv_meta(key)$label, Unit = h40_unit(key), Baseline_2010 = d$baseline,
               Estimate = d$current, Lower = d$lower, Upper = d$upper,
               Change_since_2010 = if (hv_meta(key)$percent) d$delta else d$percent_change,
               Change_unit = if (hv_meta(key)$percent) "percentage points" else "percent",
               Status = d$status, check.names = FALSE)
  }
  output$v23 <- DT::renderDT({
    d <- progress_table(comparison_pool(), chosen_key())
    DT::formatRound(h40_table(d), c("Baseline_2010", "Estimate", "Lower", "Upper", "Change_since_2010"),
                    digits = if (hv_meta(chosen_key())$percent) 4 else 2)
  }, server = FALSE)
  
  # 24-29: Exact 2010-to-selected-year comparisons.
  progress_for <- function(key) {
    validate(need(as.integer(input$year) >= baseline_year, "Select a reporting year of 2010 or later."))
    h40_changes(scope_pool(), as.integer(input$year), key)
  }
  progress_plot <- function(key) {
    d <- progress_for(key); d <- d[is.finite(d$percent_change), ]
    validate(need(nrow(d) > 0, "No country has an available positive baseline and selected-year estimate."))
    d <- d[order(-d$percent_change, d$country), ]
    d$hover <- paste0(d$country, "<br>2010: ", hv_format(d$baseline, key), "<br>", input$year, ": ", hv_format(d$current, key),
                      "<br>Change: ", scales::number(d$percent_change, accuracy = .1), "%")
    p <- plotly::plot_ly(d, x = ~percent_change, y = ~country, type = "bar", orientation = "h",
                         marker = list(color = ifelse(d$percent_change < 0, "#73C7B8", "#B0A0D0")), hovertext = ~hover, hoverinfo = "text", textposition = "none", showlegend = FALSE)
    p <- h40_chart(p, x = "Change since 2010 (%)", y = "", year = FALSE)
    plotly::layout(p, margin = list(l = 180, r = 30, t = 54, b = 65),
                   xaxis = list(ticksuffix = "%", tickformat = ".1f", zeroline = TRUE, zerolinewidth = 1.5),
                   yaxis = list(type = "category", categoryorder = "array", categoryarray = rev(d$country), tickformat = NULL, showgrid = FALSE))
  }
  for (j in 1:3) local({
    id <- paste0("v", j + 23L); key <- metric_keys[c(3, 4, 1)][j]
    set_plot_height(id, function() nrow(h40_changes(scope_pool(), as.integer(input$year), key)))
    output[[id]] <- plotly::renderPlotly(progress_plot(key))
  })
  dumbbell <- function(key) {
    d <- progress_for(key); d <- d[is.finite(d$baseline) & is.finite(d$current), ]
    validate(need(nrow(d) > 0, "No countries have both baseline and selected-year estimates."))
    d <- d[order(-d$delta, d$country), ]
    p <- plotly::plot_ly()
    p <- plotly::add_segments(p, data = d, x = ~baseline, xend = ~current, y = ~country, yend = ~country,
                              line = list(color = "#AAB0B8", width = 2), hoverinfo = "skip", showlegend = FALSE)
    d$old_hover <- paste0(d$country, "<br>2010: ", hv_format(d$baseline, key))
    d$new_hover <- paste0(d$country, "<br>", input$year, ": ", hv_format(d$current, key),
                          "<br>Change: ", scales::number(d$delta, accuracy = .01), " percentage points")
    p <- plotly::add_markers(p, data = d, x = ~baseline, y = ~country, text = ~old_hover, hoverinfo = "text",
                             marker = list(color = "#AAB0B8", size = 8), name = "Baseline: 2010", inherit = FALSE)
    p <- plotly::add_markers(p, data = d, x = ~current, y = ~country, text = ~new_hover, hoverinfo = "text",
                             marker = list(color = hv_meta(key)$colour, size = 9), name = paste0("Selected: ", input$year), inherit = FALSE)
    p <- h40_chart(p, x = hv_meta(key)$label, y = "", year = FALSE)
    plotly::layout(p, margin = list(l = 180, r = 30, t = 54, b = 85),
                   xaxis = list(ticksuffix = "%", tickformat = ".2f"),
                   yaxis = list(type = "category", categoryorder = "array", categoryarray = rev(d$country), tickformat = NULL, showgrid = FALSE))
  }
  for (j in 1:2) local({
    id <- paste0("v", j + 26L); key <- metric_keys[c(5, 2)][j]
    set_plot_height(id, function() nrow(h40_changes(scope_pool(), as.integer(input$year), key)))
    output[[id]] <- plotly::renderPlotly(dumbbell(key))
  })
  output$v29 <- DT::renderDT({
    validate(need(as.integer(input$year) >= 2010L, "Select a reporting year of 2010 or later."))
    d <- progress_table(scope_pool(), chosen_key())
    DT::formatRound(h40_table(d), c("Baseline_2010", "Estimate", "Lower", "Upper", "Change_since_2010"), digits = if (hv_meta(chosen_key())$percent) 4 else 2)
  }, server = FALSE)
  
  # 30-34: Subregional patterns from fixed complete-data cohorts.
  regional_series <- function(key) {
    pool <- scope_pool(); pieces <- list()
    for (region in sort(unique(pool$subregion))) {
      part <- h40_cohort(pool[pool$subregion == region, ], key, hist_years())
      if (is.null(part)) next
      part$subregion <- region; part$country <- paste0(region, " (", part$n, " countries)")
      pieces[[region]] <- part
    }
    if (!length(pieces)) return(data.frame())
    do.call(rbind, pieces)
  }
  for (j in 1:3) local({
    id <- paste0("v", j + 29L); key <- count_columns[j]
    output[[id]] <- plotly::renderPlotly({
      d <- regional_series(key)
      validate(need(nrow(d) > 0, "No complete-data subregional cohorts for this period."))
      p <- plotly::plot_ly()
      for (part in split(d, d$subregion)) {
        p <- hv_add_series(p, part, key, bands = FALSE, colour = region_colours[[part$subregion[1]]],
                           name = part$subregion[1], legend = TRUE)
      }
      h40_chart(p, y = h40_unit(key))
    })
  })
  output$v33 <- plotly::renderPlotly({
    key <- chosen_key()
    validate(need(!hv_meta(key)$percent, "Choose a count indicator for the stacked subregional totals."))
    d <- regional_series(key)
    validate(need(nrow(d) > 0, "No complete-data cohorts for this history window."))
    p <- plotly::plot_ly()
    for (part in split(d, d$subregion)) {
      part <- part[order(part$year), ]; part$hover <- hv_hover(part, key)
      p <- plotly::add_trace(p, data = part, x = ~year, y = ~value, type = "scatter", mode = "lines",
                             stackgroup = "one", line = list(color = region_colours[[part$subregion[1]]], width = .6),
                             fillcolor = region_colours[[part$subregion[1]]], text = ~hover, hoverinfo = "text", name = part$subregion[1], inherit = FALSE)
    }
    h40_chart(p, y = h40_unit(key))
  })
  output$v34 <- plotly::renderPlotly({
    req(input$dot_indicator); key <- input$dot_indicator
    d <- scope_year(); d <- d[is.finite(d[[key]]), ]
    validate(need(nrow(d) > 0, "No country percentages for this selection."))
    regions <- sort(unique(d$subregion)); p <- plotly::plot_ly()
    for (region in regions) {
      part <- d[d$subregion == region, ]; part <- part[order(part$country), ]
      jitter <- if (nrow(part) > 1) seq(-.22, .22, length.out = nrow(part)) else 0
      part$y_position <- match(region, regions) + jitter
      part$value <- part[[key]]
      part$hover <- paste0(part$country, " | ", input$year, "<br>", hv_meta(key)$label, ": ", hv_format(part$value, key))
      p <- plotly::add_trace(p, data = part, x = ~value, y = ~y_position, type = "scatter", mode = "markers",
                             marker = list(color = region_colours[[region]], size = 10, line = list(color = "#D8D9DA", width = .5)),
                             text = ~hover, hoverinfo = "text", name = region, showlegend = FALSE, inherit = FALSE)
    }
    p <- h40_chart(p, x = hv_meta(key)$label, y = "", year = FALSE)
    plotly::layout(p, margin = list(l = 160, r = 30, t = 54, b = 65),
                   xaxis = list(ticksuffix = "%", tickformat = ".2f"),
                   yaxis = list(tickvals = seq_along(regions), ticktext = regions, tickformat = NULL, zeroline = FALSE))
  })
  
  # 35-40: Explorer, coverage, definitions and sources.
  explorer_data <- reactive({
    d <- history_pool(); d <- d[order(d$country, d$year), ]
    req(input$data_layout)
    if (input$data_layout == "wide") return(d)
    key <- chosen_key(); out <- hv_metric_data(d, key)
    out$indicator <- hv_meta(key)$label; out$unit <- h40_unit(key)
    out
  })
  output$v35 <- DT::renderDT(h40_table(explorer_data(), 15), server = FALSE)
  output$download_data <- downloadHandler(
    filename = function() paste0("Africa_HIV_", input$data_layout, "_", input$year, ".csv"),
    content = function(file) {
      d <- explorer_data(); rows <- input$v35_rows_all
      if (!is.null(rows)) d <- d[rows[rows >= 1 & rows <= nrow(d)], , drop = FALSE]
      utils::write.csv(d, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )
  set_plot_height("v36", function() length(unique(scope_pool()$iso3)))
  output$v36 <- plotly::renderPlotly(heat_plot(h40_heat_data(scope_pool(), chosen_key(), hist_years(), TRUE), chosen_key(), TRUE))
  output$v37 <- plotly::renderPlotly({
    d <- scope_year()
    counts <- data.frame(indicator = hv_indicators$label,
                         n = vapply(metric_keys, function(key) sum(is.finite(d[[key]])), integer(1)))
    counts$hover <- paste0(counts$indicator, "<br>", counts$n, " / ", nrow(d), " countries | ", input$year)
    p <- plotly::plot_ly(counts, x = ~n, y = ~indicator, type = "bar", orientation = "h",
                         marker = list(color = hv_indicators$colour), hovertext = ~hover, hoverinfo = "text", textposition = "none", showlegend = FALSE)
    p <- h40_chart(p, x = "Countries with an estimate", y = "", year = FALSE)
    plotly::layout(p, margin = list(l = 200, r = 25, t = 54, b = 65),
                   xaxis = list(tickformat = "d", range = c(0, max(1, nrow(d)))),
                   yaxis = list(type = "category", categoryorder = "array", categoryarray = rev(counts$indicator), tickformat = NULL, showgrid = FALSE))
  })
  output$v38 <- DT::renderDT({
    pool <- history_pool()
    ids <- unique(pool[c("country", "iso3", "subregion")]); ids <- ids[order(ids$country), ]
    out <- ids
    for (j in seq_along(metric_keys)) {
      key <- metric_keys[j]
      out[[hv_indicators$label[j]]] <- vapply(ids$iso3, function(iso) {
        valid <- pool$iso3 == iso & is.finite(pool[[key]])
        if (!any(valid)) return(NA_integer_)
        as.integer(max(pool$year[valid]))
      }, integer(1))
    }
    names(out)[1:3] <- c("Country", "ISO3", "Subregion")
    h40_table(out)
  }, server = FALSE)
  output$v39 <- renderUI({
    key <- chosen_key(); m <- hv_meta(key)
    meaning <- c(
      "Estimated people living with HIV in a country-year, including people receiving treatment. This is a population stock, not cumulative infections.",
      "People aged 15-49 living with HIV divided by the total population aged 15-49, multiplied by 100.",
      "Estimated people who newly acquired HIV during the year. This is an annual flow.",
      "Estimated deaths from AIDS-related causes during the year. This does not include all deaths among people living with HIV.",
      "People living with HIV receiving antiretroviral therapy divided by all people living with HIV, multiplied by 100.")
    index <- match(key, metric_keys)
    div(class = "h40-panel", h4(m$label),
        p(meaning[index]),
        tags$dl(tags$dt("Population"), tags$dd(if (key == metric_keys[2]) "Adults aged 15-49, both sexes" else "All ages, both sexes"),
                tags$dt("Unit"), tags$dd(if (m$percent) "Percentage on a 0-100 scale" else h40_unit(key)),
                tags$dt("Lower and upper"), tags$dd("Source uncertainty bounds, where available. They are not additional counts."),
                tags$dt("Missing observations"), tags$dd("Unavailable values remain blank. A numeric zero remains zero."),
                tags$dt("Aggregation"), tags$dd(if (key == "art_coverage_all_ages_pct") "Combined coverage = 100 x sum(PLHIV x national coverage / 100) / sum(PLHIV), using countries with both inputs. Country coverage and estimated treated people are disclosed." else if (m$percent) "Country adult prevalence is shown separately; matching adult population denominators are required to combine it." else "Counts may be summed across countries for a single year, with coverage disclosed.")))
  })
  output$visual_inventory <- renderTable({
    guide <- visual_catalog[c("tab", "title")]
    rbind(guide, data.frame(tab = "Africa overview",
                           title = "Adults aged 15-49 living with HIV (%) by country"))
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui = ui, server = server)

