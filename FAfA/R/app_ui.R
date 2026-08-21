#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @noRd

# ---- Helper: sidebar group label (dark background safe) ----
.sidebar_label <- function(icon_name, label) {
  div(
    class = "fafa-sidebar-label mt-4 mb-1",
    bs_icon(icon_name), " ", label
  )
}

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    div(
      id = "fafa-language-gate",
      style = paste(
        "position:fixed; inset:0; z-index:10000;",
        "display:flex; align-items:center; justify-content:center;",
        "background:linear-gradient(135deg,#0f172a,#1d4ed8); padding:24px;"
      ),
      div(
        style = paste(
          "width:min(460px,100%); background:#fff; border-radius:16px;",
          "padding:34px; text-align:center; box-shadow:0 24px 70px rgba(0,0,0,.3);"
        ),
        tags$img(
          src = "www/logo.png", alt = "FAfA Logo",
          style = "width:104px; height:auto; margin-bottom:18px;"
        ),
        h2("FAfA", style = "color:#1d4ed8; margin-bottom:8px;"),
        p(
          "L\u00fctfen aray\u00fcz dilini se\u00e7in / Please select the interface language",
          style = "color:#475569; margin-bottom:24px;"
        ),
        div(
          class = "d-grid gap-3",
          tags$button(
            type = "button", id = "fafa-start-turkish",
            class = "btn btn-primary btn-lg", "T\u00fcrk\u00e7e"
          ),
          tags$button(
            type = "button", id = "fafa-start-english",
            class = "btn btn-outline-secondary btn-lg", "English"
          )
        )
      )
    ),

    page_fillable(
      title = "FAfA: Factor Analysis for All",
      theme = bs_theme(
        version      = 5,
        base_font    = "'Segoe UI', 'FAfA Inter', Arial, sans-serif",
        heading_font = "'Segoe UI', 'FAfA Inter', Arial, sans-serif",
        primary      = "#2563EB",
        secondary    = "#64748B",
        success      = "#16a34a",
        info         = "#0891B2",
        warning      = "#D97706",
        danger       = "#DC2626",
        "enable-rounded"    = TRUE,
        "font-size-base"    = "0.875rem",
        "body-bg"           = "#f1f5f9",
        "body-color"        = "#1e293b",
        "card-border-color" = "#e2e8f0",
        "card-cap-bg"       = "#f8fafc",
        "card-cap-color"    = "#334155",
        "border-radius"     = "0.5rem"
      ) |>
        bs_add_rules("

@font-face {
  font-family: 'FAfA Inter';
  src: url('www/fonts/InterVariable.woff2') format('woff2');
  font-style: normal;
  font-weight: 100 900;
  font-display: swap;
}

@font-face {
  font-family: 'FAfA Inter';
  src: url('www/fonts/InterVariable-Italic.woff2') format('woff2');
  font-style: italic;
  font-weight: 100 900;
  font-display: swap;
}

/* ================================================================
   FAfA Modern Design System
   ================================================================ */

/* ---- Sidebar logo ---- */
.fafa-logo {
  display: flex;
  justify-content: center;
  padding: 16px 8px 4px 8px;
}
.fafa-logo img {
  width: 108px;
  height: auto;
  filter: drop-shadow(0 2px 6px rgba(0,0,0,0.35));
  border-radius: 4px;
}

/* ---- Sidebar: dark panel ---- */
.row > .col-sm-3:has(#tabs) {
  background: #0f172a;
  border-radius: 14px;
  padding: 14px 10px 20px 10px !important;
  min-height: calc(100vh - 80px);
}

#tabs .nav-link {
  color: #94a3b8;
  border-radius: 8px;
  font-size: 0.815rem;
  padding: 7px 11px;
  margin-bottom: 1px;
  transition: background 0.14s ease, color 0.14s ease;
  font-weight: 400;
  line-height: 1.4;
}

#tabs .nav-link:hover {
  background: rgba(255,255,255,0.07);
  color: #e2e8f0;
}

#tabs .nav-link.active {
  background: linear-gradient(135deg,#1d4ed8,#3b82f6);
  color: #fff !important;
  font-weight: 500;
  box-shadow: 0 2px 10px rgba(37,99,235,0.38);
}

/* Sidebar group labels */
.fafa-sidebar-label {
  color: #4b5563;
  font-size: 0.67rem;
  font-weight: 700;
  letter-spacing: 0.09em;
  text-transform: uppercase;
  padding: 12px 11px 3px 11px;
  display: block;
}

/* ---- Cards ---- */
.card {
  border: 1px solid #e2e8f0 !important;
  border-radius: 12px !important;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05),0 1px 2px rgba(0,0,0,0.04) !important;
}

.card-header {
  border-radius: 11px 11px 0 0 !important;
  padding: 11px 16px;
  font-size: 0.84rem;
  font-weight: 600;
  letter-spacing: 0.01em;
  border-bottom: 1px solid rgba(0,0,0,0.08) !important;
}

.card-header.bg-primary { background: linear-gradient(135deg,#1d4ed8,#2563eb) !important; color:#fff !important; }
.card-header.bg-info    { background: linear-gradient(135deg,#0369a1,#0891b2) !important; color:#fff !important; }
.card-header.bg-success { background: linear-gradient(135deg,#15803d,#16a34a) !important; color:#fff !important; }
.card-header.bg-danger  { background: linear-gradient(135deg,#b91c1c,#dc2626) !important; color:#fff !important; }
.card-header.bg-warning { background: linear-gradient(135deg,#b45309,#d97706) !important; color:#fff !important; }
.card-header.bg-dark    { background: linear-gradient(135deg,#0f172a,#1e293b) !important; color:#fff !important; }

.card-header:not([class*='bg-']) {
  background: #f8fafc !important;
  color: #334155 !important;
  border-bottom: 1px solid #e2e8f0 !important;
}

.card-body { padding: 16px; }

/* ---- Buttons ---- */
.btn {
  border-radius: 8px;
  font-size: 0.84rem;
  font-weight: 500;
  letter-spacing: 0.01em;
  padding: 7px 15px;
  transition: all 0.14s ease;
}

.btn-primary { box-shadow: 0 1px 5px rgba(37,99,235,0.32); }
.btn-success { box-shadow: 0 1px 5px rgba(22,163,74,0.32);  }
.btn-danger  { box-shadow: 0 1px 5px rgba(220,38,38,0.32);  }
.btn-info    { box-shadow: 0 1px 5px rgba(8,145,178,0.32);  }

.btn:hover:not(:disabled) { transform: translateY(-1px); filter: brightness(1.07); }
.btn:active { transform: translateY(0) !important; }

.btn-outline-secondary {
  color: #475569 !important;
  border-color: #94a3b8 !important;
  background: transparent !important;
}
.btn-outline-secondary:hover {
  background: #64748b !important;
  border-color: #64748b !important;
  color: #fff !important;
}

/* ---- Tables ---- */
.table { font-size: 0.825rem; }

.table > thead > tr > th {
  background: #f1f5f9;
  color: #475569;
  font-weight: 600;
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  border-bottom: 2px solid #e2e8f0 !important;
  padding: 9px 13px;
  white-space: nowrap;
}

.table > tbody > tr > td {
  padding: 8px 13px;
  vertical-align: middle;
  border-color: #f1f5f9;
}

.table-striped > tbody > tr:nth-of-type(odd) > td { background: #f8fafc; }
.table-bordered { border-color: #e2e8f0 !important; }

/* ---- Form controls ---- */
.form-control, .form-select {
  border-radius: 8px;
  border-color: #e2e8f0;
  font-size: 0.84rem;
  background: #fff;
}

.form-control:focus, .form-select:focus {
  border-color: #3b82f6;
  box-shadow: 0 0 0 3px rgba(59,130,246,0.12);
}

.form-label {
  font-weight: 500;
  font-size: 0.81rem;
  color: #374151;
  margin-bottom: 4px;
}

/* ---- Select menus ---- */
.factor-retention-card,
.efa-configuration-card,
.ega-network-card,
.factor-retention-card .card-body,
.efa-configuration-card .card-body,
.ega-network-card .card-body {
  overflow: visible !important;
}

.factor-retention-card:has(.selectize-control.dropdown-active),
.efa-configuration-card:has(.selectize-control.dropdown-active),
.ega-network-card:has(.selectize-control.dropdown-active) {
  position: relative;
  z-index: 2000;
}

.factor-retention-card .selectize-control.dropdown-active,
.efa-configuration-card .selectize-control.dropdown-active,
.ega-network-card .selectize-control.dropdown-active {
  z-index: 2100;
}

.factor-retention-card .selectize-dropdown,
.efa-configuration-card .selectize-dropdown,
.ega-network-card .selectize-dropdown {
  z-index: 2200 !important;
}

.factor-retention-card .selectize-dropdown-content,
.efa-configuration-card .selectize-dropdown-content,
.ega-network-card .selectize-dropdown-content {
  max-height: 65vh !important;
}

/* ---- Shiny notifications ---- */
.shiny-notification {
  border-radius: 10px !important;
  border: none !important;
  border-left: 4px solid #3b82f6 !important;
  box-shadow: 0 8px 24px rgba(0,0,0,0.13) !important;
  font-size: 0.83rem;
  background: #fff !important;
  color: #1e293b !important;
}

.shiny-notification-error   { border-left-color: #dc2626 !important; }
.shiny-notification-warning { border-left-color: #d97706 !important; }
.shiny-notification-message { border-left-color: #16a34a !important; }

/* ---- Value boxes ---- */
.bslib-value-box {
  border-radius: 12px !important;
  border: none !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.07) !important;
}

/* ---- Scrollbars ---- */
::-webkit-scrollbar           { width: 5px; height: 5px; }
::-webkit-scrollbar-track     { background: transparent; }
::-webkit-scrollbar-thumb     { background: #cbd5e1; border-radius: 4px; }
::-webkit-scrollbar-thumb:hover { background: #94a3b8; }

/* ---- Misc ---- */
hr           { border-color: #e2e8f0; opacity: 0.7; }
.text-muted  { color: #94a3b8 !important; }
small,.small { font-size: 0.78rem; }
code         { font-size: 0.8rem; background: #f1f5f9; border-radius: 4px; padding: 1px 5px; }

        "),

      # ---- Navigation ----
      navset_pill_list(
        id     = "tabs",
        widths = c(3, 9),

        # Logo
        nav_item(
          div(
            class = "fafa-logo",
            tags$img(src = "www/logo.png", alt = "FAfA Logo")
          )
        ),

        # Data & Wrangling
        nav_item(.sidebar_label("database-gear", "Data & Wrangling")),
        nav_panel("Select Data",       value = "data",        icon = bs_icon("file-earmark-arrow-up"), data_selection_ui("data_selection")),
        nav_panel("Project & Reports", value = "project",     icon = bs_icon("folder"),                 project_ui("project")),
        nav_panel("Exclude Variables", value = "ex_var",      icon = bs_icon("x-circle"),              wrangling_ui_ex_var("wrangling_ex_var")),
        nav_panel("Recode Variables",  value = "recode_var",  icon = bs_icon("pencil-square"),          wrangling_ui_recode("wrangling_recode")),
        nav_panel("Missing Values",    value = "missing_val", icon = bs_icon("patch-question"),         mod_missing_ui("missing_val")),
        nav_panel("Split Dataset",     value = "split_data",  icon = bs_icon("scissors"),              wrangling_ui_split("wrangling_split")),
        nav_panel("Manage Outliers",   value = "outliers",    icon = bs_icon("exclamation-triangle"),   wrangling_ui_outliers("wrangling_outliers")),

        # Assumption Checks
        nav_item(.sidebar_label("clipboard-check", "Assumption Checks")),
        nav_panel("Assumptions", value = "assumptions", icon = bs_icon("check2-square"), assumptions_ui("assumptions")),

        # Exploratory Factor Analysis
        nav_item(.sidebar_label("search", "Exploratory Factor Analysis")),
        nav_panel("Factor Retention",     value = "fac_ret",    icon = bs_icon("graph-up"),   efa_ui_fac_ret("efa_fac_ret")),
        nav_panel("EFA Setup & Analysis", value = "efa",        icon = bs_icon("sliders"),    efa_ui_analysis("efa_analysis")),
        nav_panel("Item Drop Out",        value = "item_rest",  icon = bs_icon("list-check"), mod_itemrest_ui("item_rest")),
        nav_panel("EFA Reporting",        value = "report_efa", icon = bs_icon("file-text"),  efa_ui_report("efa_report")),

        # Advanced Analysis
        nav_item(.sidebar_label("cpu", "Advanced Analysis")),
        nav_panel("Exploratory Graph Analysis",   value = "ega",         icon = bs_icon("diagram-3"),        ega_ui("ega")),
        nav_panel("Confirmatory Factor Analysis", value = "cfa",         icon = bs_icon("diagram-2"),        cfa_ui("cfa")),
        nav_panel("Measurement Invariance",       value = "inv",         icon = bs_icon("arrow-left-right"), inv_ui("inv")),
        nav_panel("Reliability Analysis",         value = "reliability", icon = bs_icon("award"),            reliability_ui("reliability")),
        nav_panel("Item Weighting",               value = "item_weight", icon = bs_icon("calculator"),       item_weighting_ui("item_weighting")),

        # Info
        nav_item(.sidebar_label("info-circle", "Info")),
        nav_panel("About", value = "about", icon = bs_icon("question-circle"), about_ui("about"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "FAfA")
  )
}
