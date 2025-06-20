# ─────────────────────────────────────────────────────────────────────────────
#  Spotify-style Shiny app  +  Sailor-Shift visNetwork
#  • builds network tables from JSON (or loads cache)
#  • audio auto-advances & icon always reflects play/pause state
# ─────────────────────────────────────────────────────────────────────────────

# ---- libraries -------------------------------------------------------------
library(shiny)
library(bslib)
library(plotly)
library(visNetwork)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

# ---- paths / filenames -----------------------------------------------------
cache_rds <- "data/network_tables.rds"
json_path <- "data/MC1_graph.json"
tracks    <- c("song1.mp3", "song2.mp3", "song3.mp3")   # all in ./www/

# ---- colour maps -----------------------------------------------------------
all_node_type_colors <- c(
  Song="#e15759", Album="#4e79a7", Person="#f28e2c",
  MusicalGroup="#59a14f", RecordLabel="#edc949", Unknown="#bab0ac")
all_edge_type_colors <- c(
  PerformerOf="#e15759", ComposerOf="#4e79a7", ProducerOf="#f28e2c",
  LyricistOf="#59a14f", RecordedBy="#edc949", InterpolatesFrom="#af7aa1",
  InStyleOf="#76b7b2", LyricalReferenceTo="#ff9da7", CoverOf="#9c755f",
  DirectlySamples="#bab0ac", DistributedBy="#d37295", MemberOf="#8cd17d",
  default="#999999")
desired_edge_types <- names(all_edge_type_colors)[names(all_edge_type_colors)!="default"]

# ════════════════════════════════════════════════════════════════════════════
# 1. LOAD  or  BUILD  NETWORK TABLES
# ════════════════════════════════════════════════════════════════════════════
required_objs <- c(
  "nodes_inward_vis","edges_inward_final",
  "nodes_outward_vis","edges_outward_final",
  "nodes_all_vis","edges_all_final")

if (file.exists(cache_rds)) {
  list2env(readRDS(cache_rds), .GlobalEnv)
} else {
  if (!file.exists(json_path))
    stop("Cannot find ", json_path, ". Please place MC1_graph.json in /data")
  
  kg <- fromJSON(json_path)
  
  nodes_tbl <- as_tibble(kg$nodes) %>%
    mutate(
      id   = tolower(trimws(as.character(id))),
      name = trimws(name),
      release_date_parsed = suppressWarnings(parse_date_time(
        release_date, orders=c("ymd","Ymd","Y-m-d","Y/m/d","Y"))),
      release_year = coalesce(year(release_date_parsed),
                              suppressWarnings(as.integer(str_extract(release_date,"\\d{4}")))),
      `Node Type` = trimws(coalesce(`Node Type`, "Unknown"))
    ) %>%
    distinct(id,.keep_all=TRUE)
  
  for (col in c("stage_name","genre","notable")) {
    if (!col %in% names(nodes_tbl))
      nodes_tbl[[col]] <- NA
  }
  
  edges_tbl <- as_tibble(kg$links) %>%
    rename(from=source,to=target) %>%
    mutate(across(c(from,to),~tolower(trimws(as.character(.))))) %>%
    filter(`Edge Type` %in% desired_edge_types)
  
  sailor_id <- nodes_tbl %>% filter(tolower(name)=="sailor shift") %>% pull(id) %>% first()
  stopifnot(!is.na(sailor_id))
  
  inward_ids <- unique(c(
    sailor_id,
    edges_tbl %>% filter(to==sailor_id) %>% pull(from),
    edges_tbl %>% filter(to %in% (edges_tbl %>% filter(to==sailor_id) %>% pull(from))) %>% pull(from)
  ))
  outward_ids <- unique(c(
    sailor_id,
    edges_tbl %>% filter(from==sailor_id) %>% pull(to),
    edges_tbl %>% filter(from %in% (edges_tbl %>% filter(from==sailor_id) %>% pull(to))) %>% pull(to)
  ))
  
  make_nodes <- function(ids){
    nodes_tbl %>%
      filter(id %in% ids) %>%
      mutate(
        label            = name,
        group            = `Node Type`,
        color.background = coalesce(all_node_type_colors[`Node Type`],
                                    all_node_type_colors["Unknown"]),
        size             = ifelse(id==sailor_id,60,25)
      )
  }
  make_edges <- function(ids){
    edges_tbl %>%
      filter(from %in% ids & to %in% ids) %>%
      mutate(label=`Edge Type`, title=`Edge Type`, arrows="to",
             color=all_edge_type_colors[`Edge Type`])
  }
  
  nodes_inward_vis   <- make_nodes(inward_ids)
  edges_inward_final <- make_edges(inward_ids)
  nodes_outward_vis  <- make_nodes(outward_ids)
  edges_outward_final<- make_edges(outward_ids)
  nodes_all_vis      <- bind_rows(nodes_inward_vis,nodes_outward_vis) %>% distinct(id,.keep_all=TRUE)
  edges_all_final    <- bind_rows(edges_inward_final,edges_outward_final) %>% distinct(from,to,label,.keep_all=TRUE)
  
  saveRDS(list(
    nodes_inward_vis=nodes_inward_vis,   edges_inward_final=edges_inward_final,
    nodes_outward_vis=nodes_outward_vis, edges_outward_final=edges_outward_final,
    nodes_all_vis=nodes_all_vis,         edges_all_final=edges_all_final),
    cache_rds)
}

# ════════════════════════════════════════════════════════════════════════════
# 2. UI
# ════════════════════════════════════════════════════════════════════════════
spotify <- bs_theme(5, bg="#121212", fg="#FFFFFF",
                    primary="#1DB954", base_font=font_google("Inter"))

ui <- fluidPage(
  theme = spotify,
  tags$head(
    tags$style(HTML("
      #topbar{position:fixed;top:0;left:0;right:0;height:60px;display:flex;z-index:1000;}
      #topbar-left{display:flex;align-items:center;background:#1A1A1A;padding:0 24px;min-width:260px;}
      #topbar-main{flex:1;display:flex;align-items:center;padding:0 60px;
                   background:linear-gradient(90deg,#006400,#1DB954);}
      .nav-icon,.nav-btn{background:transparent;border:none;color:#e0e0e0;margin-right:16px;}
      .nav-icon{font-size:1.5rem;} .nav-btn{font-weight:700;font-size:1rem;}
      .nav-btn.active,.nav-btn:hover,.nav-icon:hover{color:#fff;border-bottom:3px solid #fff;}
      #btn_play.nav-icon{color:#1DB954;} #btn_play.nav-icon:hover{color:#1ED760;}
      #sidebar-left{position:fixed;top:60px;left:0;bottom:0;width:240px;background:#000;
                    padding:20px;overflow-y:auto;}
      #sidebar-left h4{color:#1DB954;margin:0 0 8px;font-weight:800;}
      #sidebar-left select{background:#181818;color:#fff;border:1px solid #333;
                           margin-bottom:16px;width:100%;}
      #main{margin-top:80px;margin-left:260px;margin-right:360px;padding:20px;}
      #sidebar-right{position:fixed;top:60px;right:0;bottom:0;width:300px;background:#000;
                     padding:24px;overflow-y:auto;}
      #sidebar-right .cover-img{width:100%;border-radius:8px;margin-bottom:16px;}
      #sidebar-right h4{color:#FFFFFF;margin:16px 0 8px;font-weight:800;}
      .vis-network canvas{background:#ffffff !important;}
    ")),
tags$script(HTML(sprintf("
  document.addEventListener('DOMContentLoaded',function(){
    var tracks=%s, idx=0,
        p=document.getElementById('songPlayer'),
        playBtn=document.getElementById('btn_play'),
        nextBtn=document.getElementById('btn_next'),
        ico=playBtn.querySelector('i');
    p.src=tracks[idx];

    function setIcon(isPlaying){
      if(isPlaying){
        ico.classList.remove('fa-play-circle');
        ico.classList.add   ('fa-pause-circle');
      }else{
        ico.classList.remove('fa-pause-circle');
        ico.classList.add   ('fa-play-circle');
      }
    }

    /* ---- Play / Pause --------------------------------------------------*/
    playBtn.onclick=function(){
      if(p.paused){ p.play().then(()=>setIcon(true)).catch(()=>setIcon(false)); }
      else        { p.pause();              setIcon(false); }
    };

    /* ---- NEXT  (EDIT #1) ----------------------------------------------*/
    nextBtn.onclick=function(){
      idx=(idx+1)%%tracks.length;
      p.src=tracks[idx];
      /* attempt to auto-play; if browser blocks, leave icon as “play” */
      p.play().then(()=>setIcon(true)).catch(()=>setIcon(false));   // ← changed
    };

    /* ---- Auto-advance on ended (EDIT #2) ------------------------------*/
    p.addEventListener('ended',function(){
      idx=(idx+1)%%tracks.length;
      p.src=tracks[idx];
      p.play().then(()=>setIcon(true)).catch(()=>setIcon(false));   // ← changed
    });

    /* ---- Nav highlight (unchanged) -----------------------------------*/
    var nav=['nav_summary','nav_sailor','nav_oceanus','nav_other'];
    nav.forEach(function(id){
      document.getElementById(id).onclick=function(){
        nav.forEach(function(x){document.getElementById(x).classList.remove('active');});
        this.classList.add('active');
        Shiny.setInputValue('activeTab',id,{priority:'event'});
      };
    });
  });
", toJSON(tracks, auto_unbox = TRUE))))

  ),
  
  tags$audio(id="songPlayer", controls=NA,
             style="position:absolute;left:-9999px;"),
  
  tags$div(id="topbar",
           tags$div(id="topbar-left",
                    actionButton("btn_home", NULL, icon=icon("home"),        class="nav-icon"),
                    actionButton("btn_play", NULL, icon=icon("play-circle"), class="nav-icon"),
                    actionButton("btn_next", NULL, icon=icon("forward"),     class="nav-icon")
           ),
           tags$div(id="topbar-main",
                    actionButton("nav_summary","Summary",      class="nav-btn"),
                    actionButton("nav_sailor", "Sailor Shift", class="nav-btn"),
                    actionButton("nav_oceanus","Oceanus Folk", class="nav-btn active"),
                    actionButton("nav_other",  "Other Artists",class="nav-btn")
           )
  ),
  
  uiOutput("left_sidebar"),
  uiOutput("main_panel"),
  tags$div(id="sidebar-right",
           h3("Summary", style="color:#FFF;margin-top:0;margin-bottom:12px;"),
           selectInput("summary", NULL,
                       choices=c("Sailor Shift","Oceanus Folk","Other Artists"),
                       selected="Sailor Shift", width="100%"),
           uiOutput("cover_img"),
           uiOutput("about_header"),
           htmlOutput("genre_description")
  )
)

# ════════════════════════════════════════════════════════════════════════════
# 3. SERVER
# ════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session){
  
  activeTab <- reactive(input$activeTab %||% "nav_oceanus")
  
  output$left_sidebar <- renderUI({
    switch(activeTab(),
           "nav_summary"=NULL,
           "nav_sailor" = div(id="sidebar-left",
                              h4("Sailor Shift"),
                              selectInput("direction_sailor","Show interactions:",
                                          choices=c("All","Inward","Outward"), selected="All")),
           "nav_oceanus"=div(id="sidebar-left",
                             h4("Oceanus Folk"),
                             selectInput("direction","Outward Influences",
                                         choices=c("Outward Influences","Inward Influences")),
                             h4("Plot 1"),selectInput("plot1","Network - Outward",
                                                      choices=c("Network - Outward","Network - Inward")),
                             h4("Select by ID"),selectInput("select_id","Oceanus Folk",
                                                            choices=c("Oceanus Folk","Artist A","Artist B")),
                             h4("Plot 2"),selectInput("plot2","Plotly - Outward",
                                                      choices=c("Plotly - Outward","Plotly - Inward")),
                             h4("Genre"),selectInput("genre","Genre",
                                                     choices=c('Americana','Indie Folk','Indie Rock','Blues Rock','Celtic Folk'),
                                                     selected='Americana',multiple=TRUE,selectize=FALSE,size=6)),
           "nav_other"  = div(id="sidebar-left",
                              h4("Other Artists"),
                              selectInput("other_artist","Choose artist",
                                          choices=c("Orla Seabloom","Beatrice Albright","Daniel O’Connell")))
    )
  })
  
  output$main_panel <- renderUI({
    switch(activeTab(),
           "nav_summary" = div(id="main", plotOutput("summaryPlot",height="400px")),
           "nav_sailor"  = div(id="main", visNetworkOutput("sailor_network",height="700px")),
           "nav_oceanus" = div(id="main",
                               fluidRow(
                                 column(6, plotOutput("network_plot",height="400px")),
                                 column(6, plotlyOutput("influence_plot",height="400px")),
                                 column(12, tags$h4("General Overview"), htmlOutput("general_overview"))
                               )),
           "nav_other"   = div(id="main", plotOutput("otherPlot",height="400px"))
    )
  })
  
  sailor_data <- reactive({
    switch(input$direction_sailor %||% "All",
           "Inward"  = list(nodes=nodes_inward_vis,  edges=edges_inward_final),
           "Outward" = list(nodes=nodes_outward_vis, edges=edges_outward_final),
           "All"     = list(nodes=nodes_all_vis,     edges=edges_all_final))
  })
  
  output$sailor_network <- renderVisNetwork({
    dat <- sailor_data(); req(dat)
    visNetwork(dat$nodes, dat$edges) %>%
      visNodes(color=list(background=~color.background,border="black",
                          highlight=list(background="red",border="darkred")),
               shape="dot",shadow=TRUE,font=list(size=12)) %>%
      visEdges(arrows="to",label=~label,
               color=list(color=~color,highlight=~color),
               smooth=list(enabled=TRUE,type="continuous",roundness=0.5),width=1,
               font=list(size=10,align="middle")) %>%
      visOptions(highlightNearest=list(enabled=TRUE,degree=2,hover=TRUE),
                 nodesIdSelection=TRUE,selectedBy="group") %>%
      visInteraction(navigationButtons=TRUE,keyboard=TRUE) %>%
      visIgraphLayout(layout="layout_with_fr",randomSeed=1234) %>%
      visPhysics(enabled=FALSE)
  })
  
  output$cover_img <- renderUI({
    tags$img(src=switch(input$summary,
                        "Sailor Shift" ="sailorshift2.png",
                        "Other Artists"="otherartists.png",
                        "Oceanus Folk" ="OceanusFolk.png"), class="cover-img")
  })
  output$about_header <- renderUI({
    tags$h4(switch(input$summary,
                   "Oceanus Folk" ="About the Genre",
                   "Sailor Shift" ="About Sailor Shift",
                   "Other Artists"="About Other Artists"),
            style="color:#FFF;margin:16px 0 8px;font-weight:800;")
  })
  output$genre_description <- renderUI({
    HTML(switch(input$summary,
                "Sailor Shift" ="… Sailor description …",
                "Oceanus Folk" ="… Oceanus description …",
                "Other Artists"="… Other Artists description …"))
  })
  
  output$summaryPlot <- renderPlot({ hist(rnorm(100)) })
  output$network_plot <- renderPlot({ plot(1:10,1:10) })
  output$influence_plot <- renderPlotly({
    plot_ly(x=1:10,y=1:10,type="scatter",mode="lines+markers") })
  output$general_overview <- renderUI({
    HTML("<p>Oceanus Folk’s stylistic reach...</p>") })
  output$otherPlot <- renderPlot({ boxplot(matrix(rnorm(100), ncol=5)) })
}

shinyApp(ui, server)
