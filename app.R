# app.R

library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(tibble)

# ──────────────────────────────────────────────────────────────────────────────
# Theme & CSS
# ──────────────────────────────────────────────────────────────────────────────
spotify_theme <- bs_theme(
  version   = 5,
  bg        = "#121212",
  fg        = "#FFFFFF",
  primary   = "#1DB954",
  secondary = "#535353",
  base_font = font_google("Inter")
)

custom_css <- HTML("
  /* Top taskbar */
  #topbar {
    position: fixed;
    top: 0; left: 0; right: 0;
    height: 60px;
    display: flex;
    align-items: center;
    padding: 0 240px;
    background: linear-gradient(90deg,#006400,#1DB954);
    z-index: 1030;
  }
  .nav-btn {
    margin-right: 16px;
    font-weight: 700;
    background: transparent;
    border: none;
    color: #e0e0e0;
    font-size: 1rem;
  }
  .nav-btn.active, .nav-btn:hover {
    color: #fff;
    border-bottom: 3px solid #fff;
  }

  /* Left sidebar */
  #sidebar-left {
    position: fixed;
    top: 60px; left: 0; bottom: 0;
    width: 220px;
    background-color: #000;
    padding: 24px 18px;
    overflow-y: auto;
  }
  #sidebar-left h4 { color:#1DB954; margin:0 0 18px; font-weight:800; }
  #sidebar-left label { color:#b3b3b3; font-weight:600; margin-top:16px; }
  #sidebar-left .selectize-input { background:#181818; border:none; }

  /* Right sidebar */
  #sidebar-right {
    position: fixed;
    top: 60px; right: 0; bottom: 0;
    width: 300px;
    background-color: #000;
    padding: 24px;
    overflow-y: auto;
  }
  #sidebar-right img { width:100%; border-radius:8px; }
  #sidebar-right h4 { color:#1DB954; margin-top:18px; font-weight:800; }
  #sidebar-right p { text-align:justify; line-height:1.45; }

  /* Main content */
  #main {
    margin-top: 80px;
    margin-left: 240px;
    margin-right: 320px;
    padding: 0 32px 32px 32px;
  }
")

# ──────────────────────────────────────────────────────────────────────────────
# JS to toggle .active on your buttons
# ──────────────────────────────────────────────────────────────────────────────
custom_js <- HTML("
  Shiny.addCustomMessageHandler('highlightTab', function(btnId) {
    $('.nav-btn').removeClass('active');
    $('#' + btnId).addClass('active');
  });
")

# ──────────────────────────────────────────────────────────────────────────────
# UI
# ──────────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = spotify_theme,
  
  tags$head(
    tags$style(custom_css),
    tags$script(custom_js)
  ),
  
  # Top taskbar
  tags$div(
    id = "topbar",
    actionButton("tab_home",    "HOME",          class = "nav-btn"),
    actionButton("tab_summary", "Summary Page",  class = "nav-btn"),
    actionButton("tab_sailor",  "Sailor Shift",  class = "nav-btn"),
    actionButton("tab_oceanus", "Oceanus Folk",  class = "nav-btn active"),
    actionButton("tab_other",   "Other Artists", class = "nav-btn")
  ),
  
  # Left sidebar
  tags$div(
    id = "sidebar-left",
    h4("Plot Controls"),
    selectInput("sailor_plot",  "Sailor Shift",  choices = c("Select a Plot")),
    selectInput("oceanus_plot", "Oceanus Folk",  choices = c("Select a Plot")),
    selectInput("artist_plot",  "Top Artists",   choices = c("Select a Plot")),
    actionButton("submit_plots","Submit",         class = "btn btn-success", width = "100%")
  ),
  
  # Right sidebar
  tags$div(
    id = "sidebar-right",
    img(src = "sailorshift2.png", alt = "Artist"),
    h4("About the Artist"),
    htmlOutput("artist_title"),
    htmlOutput("artist_bio")
  ),
  
  # Main content (Shiny’s built-in nav will appear under your CSS’d bar)
  tags$div(
    id = "main",
    tabsetPanel(
      id       = "tabs",
      selected = "oceanus",
      
      tabPanel(
        title = "Home", value = "home",
        fluidRow(column(12, h2("Welcome to the Home Page")))
      ),
      
      tabPanel(
        title = "Summary Page", value = "summary",
        fluidRow(column(12, h2("Summary Overview")))
      ),
      
      tabPanel(
        title = "Sailor Shift", value = "sailor",
        fluidRow(column(12, h2("Plots for Sailor Shift")))
      ),
      
      tabPanel(
        title = "Oceanus Folk", value = "oceanus",
        fluidRow(column(12, h2("Global Oceanus Folk Influence"))),
        fluidRow(column(12, plotlyOutput("oceanus_plot")))
      ),
      
      tabPanel(
        title = "Other Artists", value = "other",
        fluidRow(column(12, h2("Plots for Other Artists")))
      )
    )
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# Server
# ──────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Artist info
  output$artist_title <- renderUI({
    tags$h5(strong("Sailor Shift"), style = "color:#fff; margin-top:8px;")
  })
  output$artist_bio <- renderUI({
    tags$p("Sailor Shift is an Oceanus-born artist whose journey from local folk singer to global superstar epitomises both personal drive and cultural impact…")
  })
  
  # Button → tab logic
  for (nm in c("home","summary","sailor","oceanus","other")) {
    local({
      tab <- nm
      btn <- paste0("tab_", tab)
      observeEvent(input[[btn]], {
        updateTabsetPanel(session, "tabs", selected = tab)
        session$sendCustomMessage("highlightTab", btn)
      })
    })
  }
  
  # Oceanus Folk data + plot
  oceanus_data <- reactive({
    kg        <- fromJSON("data/MC1_graph.json")
    nodes_tbl <- as_tibble(kg$nodes)
    links_tbl <- as_tibble(kg$links)
    
    nodes_clean <- nodes_tbl %>%
      mutate(
        date_raw    = coalesce(release_date, written_date, notoriety_date),
        date_parsed = parse_date_time(date_raw,
                                      orders = c("Ymd","Y-m-d","Y"),
                                      quiet  = TRUE),
        year        = year(date_parsed),
        rowid       = row_number()
      ) %>%
      select(rowid, genre, year)
    
    edges_clean <- links_tbl %>%
      rename(source_id = source, target_id = target) %>%
      left_join(nodes_clean %>% select(rowid), by = c("source_id" = "rowid")) %>% rename(from = rowid) %>%
      left_join(nodes_clean %>% select(rowid), by = c("target_id" = "rowid")) %>% rename(to   = rowid) %>%
      filter(!is.na(from) & !is.na(to))
    
    yearly_raw <- edges_clean %>%
      inner_join(
        nodes_clean %>% filter(genre == "Oceanus Folk") %>% select(rowid),
        by = c("to" = "rowid")
      ) %>%
      left_join(nodes_clean %>% select(rowid, year), by = c("to" = "rowid")) %>%
      filter(!is.na(year)) %>%
      count(target_year = year, name = "new_influences")
    
    yearly_raw %>%
      complete(target_year = seq(min(target_year), 2040),
               fill        = list(new_influences = 0)) %>%
      arrange(target_year) %>%
      mutate(cumulative = cumsum(new_influences))
  })
  
  output$oceanus_plot <- renderPlotly({
    yearly <- oceanus_data()
    plot_ly(yearly, x=~target_year, y=~new_influences, type="bar", name="Annual") %>%
      add_trace(y=~cumulative, type="scatter", mode="lines+markers", name="Cumulative") %>%
      layout(
        title="Global Oceanus Folk Influence (through 2040)",
        xaxis=list(title="Year", dtick=5), yaxis=list(title="Count"),
        legend=list(orientation="h", x=0.5, xanchor="center", y=-0.1)
      )
  })
}

# ──────────────────────────────────────────────────────────────────────────────
# Run the app
# ──────────────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
