# ─────────────────────────────────────────────────────────────────────────────
#  Spotify-style Shiny app  +  Sailor-Shift visNetwork
#  • builds network tables from JSON (or loads cache)
#  • audio auto-advances & icon always reflects play/pause state
# ─────────────────────────────────────────────────────────────────────────────

# ---- libraries -------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(dplyr)
library(bslib)
library(plotly)
library(visNetwork)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(rlang)
library(igraph)
library(gridlayout)
library(gridExtra)
library(tibble)
library(forcats)
library(stringr)
library(tidyr)

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

# ─── Data prep (Andre's code) ───────────────────────────────────────────────────
kg_raw      <- fromJSON("data/MC1_graph.json")
nodes_tbl   <- as_tibble(kg_raw$nodes) %>% mutate(idx = row_number())
links_tbl   <- as_tibble(kg_raw$links)

id_map    <- nodes_tbl %>% select(id, idx)
edges_idx <- links_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = idx) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to   = idx) %>%
  filter(!is.na(from) & !is.na(to))

person_idxs   <- which(nodes_tbl$`Node Type` == "Person")
genre_choices <- nodes_tbl %>%
  filter(`Node Type` == "Song") %>%
  pull(genre) %>% unique() %>% sort()

group_choices <- edges_idx %>%
  filter(`Edge Type` == "MemberOf") %>%
  pull(to) %>% unique() %>%
  { nodes_tbl$name[.] } %>% sort()

# ════════════════════════════════════════════════════════════════════════════
# 1. LOAD  or  BUILD  NETWORK TABLES
# ════════════════════════════════════════════════════════════════════════════
# required_objs <- c(
#   "nodes_inward_vis","edges_inward_final",
#   "nodes_outward_vis","edges_outward_final",
#   "nodes_all_vis","edges_all_final")
# 
# if (file.exists(cache_rds)) {
#   list2env(readRDS(cache_rds), .GlobalEnv)
# } else {
#   if (!file.exists(json_path))
#     stop("Cannot find ", json_path, ". Please place MC1_graph.json in /data")
#   
#   kg <- fromJSON(json_path)
#   
#   nodes_tbl <- as_tibble(kg$nodes) %>%
#     mutate(
#       id   = tolower(trimws(as.character(id))),
#       name = trimws(name),
#       release_date_parsed = suppressWarnings(parse_date_time(
#         release_date, orders=c("ymd","Ymd","Y-m-d","Y/m/d","Y"))),
#       release_year = coalesce(year(release_date_parsed),
#                               suppressWarnings(as.integer(str_extract(release_date,"\\d{4}")))),
#       `Node Type` = trimws(coalesce(`Node Type`, "Unknown"))
#     ) %>%
#     distinct(id,.keep_all=TRUE)
#   
#   for (col in c("stage_name","genre","notable")) {
#     if (!col %in% names(nodes_tbl))
#       nodes_tbl[[col]] <- NA
#   }
#   
#   edges_tbl <- as_tibble(kg$links) %>%
#     rename(from=source,to=target) %>%
#     mutate(across(c(from,to),~tolower(trimws(as.character(.))))) %>%
#     filter(`Edge Type` %in% desired_edge_types)
#   
#   sailor_id <- nodes_tbl %>% filter(tolower(name)=="sailor shift") %>% pull(id) %>% first()
#   stopifnot(!is.na(sailor_id))
#   
#   inward_ids <- unique(c(
#     sailor_id,
#     edges_tbl %>% filter(to==sailor_id) %>% pull(from),
#     edges_tbl %>% filter(to %in% (edges_tbl %>% filter(to==sailor_id) %>% pull(from))) %>% pull(from)
#   ))
#   outward_ids <- unique(c(
#     sailor_id,
#     edges_tbl %>% filter(from==sailor_id) %>% pull(to),
#     edges_tbl %>% filter(from %in% (edges_tbl %>% filter(from==sailor_id) %>% pull(to))) %>% pull(to)
#   ))
#   
#   make_nodes <- function(ids){
#     nodes_tbl %>%
#       filter(id %in% ids) %>%
#       mutate(
#         label            = name,
#         group            = `Node Type`,
#         color.background = coalesce(all_node_type_colors[`Node Type`],
#                                     all_node_type_colors["Unknown"]),
#         size             = ifelse(id==sailor_id,60,25)
#       )
#   }
#   make_edges <- function(ids){
#     edges_tbl %>%
#       filter(from %in% ids & to %in% ids) %>%
#       mutate(label=`Edge Type`, title=`Edge Type`, arrows="to",
#              color=all_edge_type_colors[`Edge Type`])
#   }
#   
#   nodes_inward_vis   <- make_nodes(inward_ids)
#   edges_inward_final <- make_edges(inward_ids)
#   nodes_outward_vis  <- make_nodes(outward_ids)
#   edges_outward_final<- make_edges(outward_ids)
#   nodes_all_vis      <- bind_rows(nodes_inward_vis,nodes_outward_vis) %>% distinct(id,.keep_all=TRUE)
#   edges_all_final    <- bind_rows(edges_inward_final,edges_outward_final) %>% distinct(from,to,label,.keep_all=TRUE)
#   
#   saveRDS(list(
#     nodes_inward_vis=nodes_inward_vis,   edges_inward_final=edges_inward_final,
#     nodes_outward_vis=nodes_outward_vis, edges_outward_final=edges_outward_final,
#     nodes_all_vis=nodes_all_vis,         edges_all_final=edges_all_final),
#     cache_rds)
# }

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
      #topbar-left img {height: 3rem !important;width: auto;}
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

        /* ---- NEXT (EDIT #1) ----------------------------------------------*/
        nextBtn.onclick=function(){
          idx=(idx+1)%%tracks.length;
          p.src=tracks[idx];
          p.play().then(()=>setIcon(true)).catch(()=>setIcon(false));
        };

        /* ---- Auto-advance on ended (EDIT #2) ------------------------------*/
        p.addEventListener('ended',function(){
          idx=(idx+1)%%tracks.length;
          p.src=tracks[idx];
          p.play().then(()=>setIcon(true)).catch(()=>setIcon(false));
        });

         /* ---- Logo button opens external homepage --------------------------*/
         document.getElementById('btn_home').onclick = function(){
           window.open('https://oceanustv.netlify.app','_blank');
         };

        /* ---- Nav highlight (unchanged) --------------------------------------*/
        var nav=['nav_home','nav_sailor','nav_oceanus','nav_other','nav_group','nav_rising'];
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
  
  tags$audio(id="songPlayer", controls=NA, style="position:absolute;left:-9999px;"),
  
  tags$div(id="topbar",
           tags$div(id="topbar-left",
                    actionButton(inputId = "btn_home",
                                 label   = tags$img(src="otv2.png", alt="Home"),
                                 class   = "nav-icon"),
                    actionButton("btn_play", NULL, icon = icon("play-circle"), class = "nav-icon"),
                    actionButton("btn_next", NULL, icon = icon("forward"), class = "nav-icon")
           ),
           tags$div(id="topbar-main",
                    actionButton("nav_home", "Homepage", class="nav-btn active"),
                    actionButton("nav_sailor", "Influence Explorer", class = "nav-btn"),
                    actionButton("nav_oceanus", "Oceanus Folk",               class = "nav-btn"),
                    actionButton("nav_other",  "Artist Spotlight",      class = "nav-btn"),
                    actionButton("nav_group",  "Group Spotlight",            class = "nav-btn"),
                    actionButton("nav_rising","Rising Stars",             class = "nav-btn")
           )
  )
  ,
  
  uiOutput("left_sidebar"),
  uiOutput("main_panel"),
  
  # ─────────────────────────────────────────────────────────────────────────────
  # Right sidebar UI
  # ─────────────────────────────────────────────────────────────────────────────
  tags$div(id="sidebar-right",
           h3("Summary", style="color:#FFF;margin-top:0;margin-bottom:12px;"),
           selectInput(
             inputId = "summary",
             label   = NULL,
             choices = c("Sailor Shift","Oceanus Folk","Top Artist","Rising Stars"),
             selected = "Sailor Shift",
             width    = "100%"
           ),
           uiOutput("cover_img"),
           uiOutput("about_header"),
           htmlOutput("genre_description")
  )
)

# ════════════════════════════════════════════════════════════════════════════
# 3. SERVER
# ════════════════════════════════════════════════════════════════════════════

rescale <- function(x, to = c(15, 150)) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(mean(to), length(x)))
  (x - rng[1]) / diff(rng) * diff(to) + to[1]
}

server <- function(input, output, session){
  
  activeTab <- reactive(input$activeTab %||% "nav_home")
  
  output$left_sidebar <- renderUI({
    switch(activeTab(),
           "nav_home"    = NULL,
           "nav_sailor"  = div(id="sidebar-left",
                               shinyWidgets::pickerInput(
                                 inputId = "target_name",
                                 label = "Target Artist Name:",
                                 choices = NULL,
                                 selected = NULL,
                                 options = list(
                                   `live-search` = TRUE,
                                   `size` = 10
                                 )
                               ),
                               selectInput(
                                 "direction_sailor",
                                 "Show Interactions:",
                                 choices = c("All", "Inward", "Outward"),
                                 selected = "All"
                               ),
                               shinyWidgets::pickerInput(
                                 inputId = "genre_filter",
                                 label = "Filter by Genre:",
                                 choices = NULL,
                                 selected = NULL,
                                 options = list(
                                   `live-search` = TRUE,
                                   `actions-box` = TRUE,
                                   `size` = 10
                                 ),
                                 multiple = TRUE
                               ),
                               checkboxGroupInput(
                                 "node_types",
                                 "Show node types:",
                                 choices = c("Person","MusicalGroup","Song","Album","RecordLabel"),
                                 selected = c("Person","MusicalGroup","Song","Album","RecordLabel")
                               )
           ),
           "nav_oceanus" = div(id="sidebar-left",
                               helpText("Explore the influence of Oceanus Folk across genres and time."),
                               selectInput(
                                 "influence_direction",
                                 "Select Influence Direction:",
                                 choices = c("Outward", "Inward"),
                                 selected = "Outward"
                               ),
                               uiOutput("genre_filter_ui"),
                               uiOutput("year_slider_ui"),
           ),
           "nav_other"   = div(id="sidebar-left",
                               h4("Artist Spotlight"),
                               uiOutput("settings_ui")
           ),
           "nav_group"   = div(id="sidebar-left",
                               h4("Group Spotlight"),
                               uiOutput("dist_settings_ui")
           ),
           "nav_rising"= div(id="sidebar-left",
                             h4("Rising Stars"),
                             selectInput(
                               "star_genre", "Select Genre for Emerging Artists:",
                               choices  = genre_choices,
                               selected = "Oceanus Folk",
                               width    = "100%"
                             )
           )
    )
  })

  # MAIN PANEL  
  
  output$main_panel <- renderUI({
    switch(activeTab(),
           "nav_home"    = div(
             id="main",
             tags$h2(
               style="text-align:center;",
               "Step into the vibrant world of Oceanus TV, where musical journeys come alive! Navigate artist influence maps, uncover artist and group spotlights, and get a sneak peek at rising stars. Ready to explore? Check out our ",
               tags$a("User Guide",
                      href="https://oceanustv.netlify.app/userguide/userguide",
                      target="_blank"),
               " and let the adventure begin!"
             )
           ),
           "nav_sailor"  = div(id="main",
                               plotOutput("degree_bar", height = "300px"),
                               visNetworkOutput("network", height = "700px"),
                               uiOutput("legend_panel")
           ),
           "nav_oceanus" = div(id="main",
                               conditionalPanel(
                                 condition = "input.influence_direction == 'Outward'",
                                 plotlyOutput("timelinePlot", width="100%", height = "400px"),
                                 visNetworkOutput("expandedNet", height = "500px")
                               ),
                               conditionalPanel(
                                 condition = "input.influence_direction == 'Inward'",
                                 plotlyOutput("inwardTimelinePlot", width="100%", height = "400px"),
                                 visNetworkOutput("genreBandNet", height = "500px")
                               )
           ),
           "nav_other"   = div(id="main",
                               plotOutput("linePlots", height = "600px")
           ),
           "nav_group"   = div(id="main",
                               plotOutput("distPlots", height = "600px")
           ),
           "nav_rising"= div(id="main",
                             plotlyOutput("risingPlot", width = "100%", height = "600px")
           )
    )
  })
 
  # ─────────────────────────────────────────────────
  # 1. Artist Spotlight pagination & UI logic
  
  currentPage <- reactiveVal(1)
  observeEvent(input$nextPage, { currentPage(if(currentPage()==1) 2 else 1) })
  observeEvent(input$prevPage, { currentPage(if(currentPage()==1) 2 else 1) })
  
  output$settings_ui <- renderUI({
    if (currentPage()==1) {
      tagList(
        selectInput(
          "genre_selector","Select Genres:",
          choices  = genre_choices,
          selected = genre_choices[2:3],
          multiple = TRUE, width="100%"
        ),
        selectInput(
          "top_n_selector","Top N Artists in Genre:",
          choices  = as.character(1:10),
          selected = "5", width="100%"
        ),
        fluidRow(
          column(6, actionButton("prevPage","← Previous", width="100%")),
          column(6, actionButton("nextPage","Next →",    width="100%"))
        )
      )
    } else {
      req(input$genre_selector)
      top_artists <- edges_idx %>%
        filter(`Edge Type` %in% c("ComposerOf","PerformerOf"),
               from %in% person_idxs) %>%
        inner_join(
          nodes_tbl %>% filter(`Node Type`=="Song",
                               genre %in% input$genre_selector,
                               notable) %>% select(idx,genre),
          by=c("to"="idx")
        ) %>%
        mutate(Person=nodes_tbl$name[from]) %>%
        count(genre,Person,name="NotableSongs") %>%
        group_by(genre) %>%
        slice_max(NotableSongs,n=10,with_ties=FALSE) %>%
        ungroup() %>%
        pull(Person) %>% unique() %>% sort()
      
      tagList(
        selectInput(
          "person_selector","Select Artist(s):",
          choices  = top_artists,
          selected = head(top_artists,3),
          multiple = TRUE, width="100%"
        ),
        fluidRow(
          column(6, actionButton("prevPage","← Previous", width="100%")),
          column(6, actionButton("nextPage","Next →",    width="100%"))
        )
      )
    }
  })
  
  # Render A–E  
  output$linePlots <- renderPlot({
    req(input$genre_selector)
    top_n <- as.numeric(input$top_n_selector)
    selected_persons <- input$person_selector
    
    genre_songs <- nodes_tbl %>%
      filter(`Node Type`=="Song", genre %in% input$genre_selector) %>%
      select(idx,genre,notable)
    notable_edges <- edges_idx %>%
      filter(`Edge Type`%in%c("ComposerOf","PerformerOf"),
             from%in%person_idxs) %>%
      inner_join(genre_songs, by=c("to"="idx")) %>%
      mutate(Person=nodes_tbl$name[from])
    
    if (currentPage()==1) {
      # Plot B
      dfB <- genre_songs %>%
        group_by(genre,notable) %>%
        summarise(SongCount=n(),.groups="drop") %>%
        mutate(Status=ifelse(notable,"Notable","Non-notable"))
      pB <- ggplot(dfB,aes(
        x=fct_reorder(genre,SongCount,.fun=sum,.desc=TRUE),
        y=SongCount, fill=Status
      ))+
        geom_col()+coord_flip()+
        labs(title="Song Counts by Notability per Genre",
             x="Genre",y="Number of Songs",fill="")+
        theme_minimal(base_size=12)
      # Plot A
      dfA <- notable_edges %>%
        filter(notable) %>%
        count(genre,Person,name="NotableSongs") %>%
        group_by(genre) %>%
        slice_max(NotableSongs,n=top_n,with_ties=FALSE) %>%
        ungroup()
      pA <- ggplot(dfA,aes(
        x=fct_reorder(Person,NotableSongs),y=NotableSongs
      ))+
        geom_col(fill="darkgreen")+coord_flip()+
        facet_wrap(~genre,scales="free_y",ncol=2)+
        labs(title=paste0("Top ",top_n," Artists by Notable Songs"),
             x=NULL,y="Count of Notable Songs")+
        theme_minimal(base_size=12)+
        theme(strip.text=element_text(face="bold"))
      grid.arrange(pB,pA,ncol=2)
      
    } else {
      # Plot C
      dfC <- notable_edges %>%
        filter(notable,Person%in%selected_persons) %>%
        count(Person,name="NotableSongs")
      pC <- ggplot(dfC,aes(
        x=fct_reorder(Person,NotableSongs),y=NotableSongs
      ))+
        geom_col(fill="pink")+coord_flip()+
        labs(title="Notable Songs by Selected Artist(s)",
             x=NULL,y="Count of Notable Songs")+
        theme_minimal(base_size=12)
      # Plot D
      dfD <- edges_idx %>%
        filter(`Edge Type`%in%c("ComposerOf","PerformerOf"),
               from%in%nodes_tbl$idx[nodes_tbl$name%in%selected_persons]) %>%
        left_join(nodes_tbl%>%select(idx,notable,release_date,notoriety_date),
                  by=c("to"="idx")) %>%
        mutate(
          Person=nodes_tbl$name[from],
          release_year=as.integer(str_extract(release_date,"\\d{4}")),
          notoriety_year=as.integer(str_extract(notoriety_date,"\\d{4}"))
        )
      ny <- dfD %>%
        filter(notable,!is.na(release_year)) %>%
        count(Person,Year=release_year,name="Notable")
      cy <- dfD %>%
        filter(!is.na(notoriety_year)) %>%
        count(Person,Year=notoriety_year,name="Charted")
      comb <- full_join(ny,cy,by=c("Person","Year")) %>%
        replace_na(list(Notable=0,Charted=0)) %>%
        arrange(Person,Year)
      pD <- ggplot(comb,aes(x=Year))+
        geom_col(aes(y=Notable,fill="Notable"),alpha=0.5,width=0.8)+
        geom_line(aes(y=Charted,color="Charted",group=Person),size=1)+
        geom_point(aes(y=Charted,color="Charted"),size=2)+
        facet_wrap(~Person,scales="free_y")+
        scale_fill_manual(NULL,values=c(Notable="#1f78b4"))+
        scale_color_manual(NULL,values=c(Charted="#e31a1c"))+
        labs(title="Notable vs. Charted Songs by Year",x="Year",y="Song Count")+
        theme_minimal(base_size=12)
      # Plot E
      pies <- lapply(selected_persons,function(person){
        idx <- which(nodes_tbl$name==person)
        tbl <- edges_idx %>%
          filter(`Edge Type`%in%c("ComposerOf","PerformerOf"),from==idx) %>%
          left_join(nodes_tbl%>%filter(`Node Type`=="Song")%>%select(idx,notable),
                    by=c("to"="idx")) %>%
          mutate(Notable=ifelse(notable,"Yes","No")) %>%
          count(Notable)%>%arrange(Notable)%>%
          mutate(
            percentage=round(n/sum(n)*100,1),
            label=paste0(Notable,"\n",n," (",percentage,"% )")
          )
        ggplot(tbl,aes(x="",y=n,fill=Notable))+
          geom_col(width=1)+coord_polar("y",start=0)+
          scale_fill_manual(values=c(Yes="#33a02c",No="#ff7f00"))+
          geom_text(aes(label=label),position=position_stack(vjust=0.5),size=3)+
          labs(title=person,x=NULL,y=NULL)+theme_void()+
          theme(plot.title=element_text(size=10))
      })
      pE <- arrangeGrob(grobs=pies,ncol=length(pies))
      grid.arrange(grobs=list(pC,pD,pE),layout_matrix=rbind(c(1,2),c(3,2)))
    }
  })
  
  # ─────────────────────────────────────────────────
  # 2. Group Spotlight pagination & UI logic
  
  distPage <- reactiveVal(1)
  observeEvent(input$nextDistPage, { distPage(if(distPage()==1) 2 else 1) })
  observeEvent(input$prevDistPage, { distPage(if(distPage()==1) 2 else 1) })
  
  output$dist_settings_ui <- renderUI({
    if (distPage()==1) {
      tagList(
        selectInput(
          "dist_genre_selector","Select Genres:",
          choices=genre_choices,
          selected=genre_choices[2:3],
          multiple=TRUE, width="100%"
        ),
        selectInput(
          "dist_top_n","Top N (artists or groups):",
          choices=as.character(1:10),
          selected="5", width="100%"
        ),
        fluidRow(
          column(6, actionButton("prevDistPage","← Previous",width="100%")),
          column(6, actionButton("nextDistPage","Next →",   width="100%"))
        )
      )
    } else {
      tagList(
        selectInput(
          "dist_group_selector","Select Musical Group:",
          choices=group_choices,
          selected=group_choices[1],
          multiple=FALSE, width="100%"
        ),
        fluidRow(
          column(6, actionButton("prevDistPage","← Previous",width="100%")),
          column(6, actionButton("nextDistPage","Next →",   width="100%"))
        )
      )
    }
  })

  # Group Spotlight plots
    
  output$distPlots <- renderPlot({
    req(input$dist_genre_selector)
    top_n <- as.numeric(input$dist_top_n)
    
    if (distPage()==1) {
      # Plot B
      dfB <- nodes_tbl %>%
        filter(`Node Type`=="Song",genre%in%input$dist_genre_selector) %>%
        select(idx,genre,notable) %>%
        group_by(genre,notable) %>%
        summarise(SongCount=n(),.groups="drop") %>%
        mutate(Status=ifelse(notable,"Notable","Non-notable"))
      pB <- ggplot(dfB,aes(
        x=fct_reorder(genre,SongCount,.fun=sum,.desc=TRUE),
        y=SongCount,fill=Status
      ))+geom_col()+coord_flip()+
        labs(title="Song Counts by Notability per Genre",
             x="Genre",y="Number of Songs",fill="")+
        theme_minimal(base_size=12)
      
      # Plot F
      sg <- edges_idx %>%
        filter(`Edge Type`%in%c("ComposerOf","PerformerOf")) %>%
        inner_join(
          nodes_tbl%>%filter(`Node Type`=="Song",
                             genre%in%input$dist_genre_selector)%>%
            select(idx,genre,notable),
          by=c("to"="idx")
        ) %>%
        filter(notable) %>%
        inner_join(
          edges_idx%>%filter(`Edge Type`=="MemberOf"),
          by="from",suffix=c(".song",".group")
        ) %>%
        mutate(Group=nodes_tbl$name[to.group])
      dfF <- sg %>%
        count(genre,Group,name="GroupSongs") %>%
        group_by(genre) %>%
        slice_max(GroupSongs,n=top_n,with_ties=FALSE) %>%
        ungroup()
      pF <- ggplot(dfF,aes(
        x=fct_reorder(Group,GroupSongs),y=GroupSongs
      ))+geom_col(fill="steelblue")+coord_flip()+
        facet_wrap(~genre,scales="free_y",ncol=2)+
        labs(title=paste0("Top ",top_n,
                          " Musical Groups by Notable Songs"),
             x=NULL,y="Count of Notable Songs")+
        theme_minimal(base_size=12)+theme(strip.text=element_text(face="bold"))
      grid.arrange(pB,pF,ncol=2)
      
    } else {
      # Plot G
      ivy_id_map <- nodes_tbl%>%mutate(ivy_row=row_number())%>%select(id,ivy_row)
      ivy_edges  <- links_tbl%>%
        left_join(ivy_id_map,by=c("source"="id"))%>%rename(from=ivy_row)%>%
        left_join(ivy_id_map,by=c("target"="id"))%>%rename(to=ivy_row)%>%
        filter(!is.na(from)&!is.na(to))
      band_idx    <- which(nodes_tbl$name==input$dist_group_selector)
      member_idxs <- ivy_edges%>%
        filter(`Edge Type`=="MemberOf",to==band_idx)%>%pull(from)
      member_names<-nodes_tbl$name[member_idxs]
      release_edges<-ivy_edges%>%
        filter(`Edge Type`%in%c("ComposerOf","PerformerOf","LyricistOf",
                                "RecordedBy","ProducerOf","DistributedBy"),
               from%in%member_idxs)
      release_data<-release_edges%>%
        mutate(Artist=nodes_tbl$name[from],
               Year=as.integer(nodes_tbl$release_date[to]))%>%
        filter(!is.na(Year))
      raw_counts <- release_data%>%count(Artist,Year)
      year_span  <- seq(min(raw_counts$Year),max(raw_counts$Year))
      timeline_tbl<-raw_counts%>%
        complete(Artist=member_names,Year=year_span,fill=list(n=0))
      ggplot(timeline_tbl%>%mutate(Year_f=factor(Year)),
             aes(x=Year_f,y=n,color=Artist,group=Artist))+
        geom_line(position=position_dodge(width=0.5),size=1)+
        geom_point(position=position_dodge(width=0.5),size=3)+
        scale_x_discrete(drop=FALSE)+
        labs(title=paste0("Annual Output by ",
                          input$dist_group_selector,
                          " Members (Dodged)"),
             x="Year",y="Number of Works",color="Artist")+
        theme_minimal(base_size=12)+
        theme(axis.text.x=element_text(angle=45,hjust=1))
    }
  })
  
  # ─────────────────────────────────────────────────
  # 3. Rising Stars: Plot H
  
  output$risingPlot <- renderPlotly({
    req(input$star_genre)
    
    ivy_id_map <- nodes_tbl%>%mutate(ivy_row=row_number())%>%select(id,ivy_row)
    ivy_edges  <- links_tbl%>%
      left_join(ivy_id_map,by=c("source"="id"))%>%rename(from=ivy_row)%>%
      left_join(ivy_id_map,by=c("target"="id"))%>%rename(to=ivy_row)%>%
      filter(!is.na(from)&!is.na(to))
    songs_full<-nodes_tbl%>%
      filter(`Node Type`=="Song",genre==input$star_genre)%>%
      mutate(
        release_year=suppressWarnings(as.integer(str_extract(release_date,"\\d{4}"))),
        written_year=suppressWarnings(as.integer(str_extract(written_date,"\\d{4}"))),
        song_year   =pmin(release_year,written_year,na.rm=TRUE),
        is_notable  =!is.na(notoriety_date)
      )%>%
      filter(!is.na(song_year))%>%
      select(song_idx=idx,song_year,is_notable)
    pe<-ivy_edges%>%
      filter(
        `Edge Type`%in%c("ComposerOf","PerformerOf"),
        from%in%person_idxs,to%in%songs_full$song_idx
      )%>%
      select(person_idx=from,song_idx=to)
    p_first<-pe%>%
      left_join(songs_full,by="song_idx")%>%
      group_by(person_idx)%>%
      summarise(first_year=min(song_year,na.rm=TRUE),.groups="drop")%>%
      filter(first_year>=2025,first_year<=2035)%>%
      left_join(nodes_tbl%>%select(person_idx=idx,PersonName=name),by="person_idx")
    ans<-ivy_edges%>%
      filter(
        `Edge Type`%in%c("ComposerOf","PerformerOf"),
        from%in%p_first$person_idx,to%in%songs_full$song_idx
      )%>%
      left_join(songs_full,by=c("to"="song_idx"))%>%
      left_join(p_first,by=c("from"="person_idx"))%>%
      filter(is_notable,song_year>=2025,song_year<=2035)%>%
      select(PersonName,year=song_year)
    counts<-ans%>%count(PersonName,year,name="NotableCount")
    all_grid<-expand_grid(PersonName=unique(p_first$PersonName),year=2025:2035)
    trends<-all_grid%>%left_join(counts,by=c("PersonName","year"))%>%
      replace_na(list(NotableCount=0))
    static_gg<-ggplot(trends,
                      aes(x=year,y=NotableCount,color=PersonName,group=PersonName))+
      geom_line(position=position_dodge(width=1.5),size=1)+
      geom_point(position=position_dodge(width=1.5),size=2)+
      scale_x_continuous(breaks=seq(2025,2035,2))+
      labs(
        title=paste0("Notable ",input$star_genre,
                     " Songs by Emerging Artists (2025–2035)"),
        subtitle="Click legend to isolate",
        x="Year",y="Number of Notable Songs",color="Artist"
      )+
      theme_minimal(base_size=10)+
      theme(
        legend.text=element_text(size=8),
        legend.title=element_text(size=9),
        plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=10),
        axis.text.x=element_text(angle=45,hjust=1)
      )
    ggplotly(static_gg,tooltip=c("x","y","colour"))%>%
      layout(legend=list(itemclick="toggleothers",itemdoubleclick="toggle"))
  })
  
  
  # -------------For Influence Network------------------  
  observe({
    if (activeTab() == "nav_sailor") {
      # --- Data Loading and Preparation ---
      kg <- fromJSON("data/MC1_graph.json")
      nodes_tbl <- as_tibble(kg$nodes)
      edges_tbl <- as_tibble(kg$links)
      
      all_node_type_colors <- c(
        "Song" = "#FF5733",
        "Album" = "lightblue",
        "Person" = "deepskyblue",
        "MusicalGroup" = "purple",
        "RecordLabel" = "lightgreen",
        "Unknown" = "gray"
      )
      all_edge_type_colors <- c(
        "PerformerOf" = "#FF5733", "ComposerOf" = "#33FF57", "ProducerOf" = "#3357FF",
        "LyricistOf" = "#FF33F5", "RecordedBy" = "#FFCC33", "InterpolatesFrom" = "#8A2BE2",
        "InStyleOf" = "#DAA520", "LyricalReferenceTo" = "#5F9EA0", "CoverOf" = "#DC143C",
        "DirectlySamples" = "#6A5ACD", "DistributedBy" = "#20B2AA", "MemberOf" = "#8B4513",
        "default" = "#CCCCCC"
      )
      
      get_html_detail <- function(value, label) {
        if (!is.null(value) && !is.na(value) && as.character(value) != "") {
          return(paste0("<b>", label, ":</b> ", as.character(value), "<br>"))
        }
        return("")
      }
      
      # Clean nodes
      nodes_tbl_cleaned <- nodes_tbl %>%
        mutate(id = as.character(id)) %>%
        mutate(id = trimws(id)) %>%
        mutate(id = tolower(id)) %>%
        filter(!is.na(id) & id != "") %>%
        mutate(`Node Type` = trimws(ifelse(is.na(`Node Type`), "Unknown", as.character(`Node Type`)))) %>%
        distinct(id, .keep_all = TRUE)
      
      # Add missing columns as NA
      if (!"stage_name" %in% names(nodes_tbl_cleaned)) nodes_tbl_cleaned$stage_name <- NA_character_
      if (!"release_date" %in% names(nodes_tbl_cleaned)) nodes_tbl_cleaned$release_date <- NA_character_
      if (!"genre" %in% names(nodes_tbl_cleaned)) nodes_tbl_cleaned$genre <- NA_character_
      if (!"notable" %in% names(nodes_tbl_cleaned)) nodes_tbl_cleaned$notable <- NA
      
      # Parse release dates and years
      nodes_tbl_cleaned <- nodes_tbl_cleaned %>%
        mutate(
          release_date_parsed = suppressWarnings(parse_date_time(release_date, orders = c("ymd", "Ymd", "Y-m-d", "Y/m/d", "Y"))),
          release_year = ifelse(
            !is.na(release_date_parsed),
            year(release_date_parsed),
            suppressWarnings(as.integer(str_extract(as.character(release_date), "\\d{4}")))
          )
        )
      
      # Notoriety date/year: Only use if present, else NA
      if (!"notoriety_date" %in% names(nodes_tbl_cleaned)) {
        nodes_tbl_cleaned$notoriety_date <- NA
        nodes_tbl_cleaned$notoriety_year <- NA_integer_
      } else {
        nodes_tbl_cleaned <- nodes_tbl_cleaned %>%
          mutate(notoriety_year = suppressWarnings(as.integer(str_extract(as.character(notoriety_date), "\\d{4}"))))
      }
      
      # Prepare edges
      edges_igraph_df <- edges_tbl %>%
        rename(from = source, to = target) %>%
        mutate(across(c(from, to), as.character)) %>%
        mutate(across(c(from, to), trimws)) %>%
        mutate(across(c(from, to), tolower))
      
      valid_node_ids_set <- unique(nodes_tbl_cleaned$id)
      edges_igraph_df <- edges_igraph_df %>%
        filter(from %in% valid_node_ids_set & to %in% valid_node_ids_set)
      
      # Inferred genres
      genre_inference_edge_types <- c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf", "RecordedBy")
      artiste_song_genre_links <- edges_igraph_df %>%
        filter(`Edge Type` %in% genre_inference_edge_types) %>%
        inner_join(nodes_tbl_cleaned %>% select(id, `Node Type`, genre),
                   by = c("to" = "id")) %>%
        filter(`Node Type` == "Song") %>%
        select(artiste_id = from, song_genre = genre) %>%
        filter(!is.na(song_genre) & song_genre != "")
      inferred_genres_for_artistes <- artiste_song_genre_links %>%
        group_by(artiste_id) %>%
        summarise(
          inferred_genre = paste(sort(unique(song_genre)), collapse = ", "),
          .groups = 'drop'
        )
      
      # Node type color legend
      groups_for_visnetwork_combined <- tibble(
        id = names(all_node_type_colors),
        color = all_node_type_colors
      )
      
      desired_edge_types_to_display <- c(
        "PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf", "RecordedBy",
        "InterpolatesFrom", "InStyleOf", "LyricalReferenceTo", "CoverOf",
        "DirectlySamples", "DistributedBy", "MemberOf"
      )
      
      # Color palettes
      song_palette_func <- colorRampPalette(brewer.pal(9, "YlOrRd"))
      album_palette_func <- colorRampPalette(brewer.pal(9, "PuBuGn"))
      
      get_color_by_year <- function(year, min_year, max_year, palette_func) {
        if (is.na(year)) return(NA)
        if (min_year == max_year) return(palette_func(1)[1])
        idx <- round((year - min_year) / (max_year - min_year) * (9 - 1)) + 1
        palette_func(9)[min(max(idx, 1), 9)]
      }
      
      rescale <- function(x, to = c(15, 150)) {
        rng <- range(x, na.rm = TRUE)
        if (diff(rng) == 0) return(rep(mean(to), length(x)))
        (x - rng[1]) / diff(rng) * diff(to) + to[1]
      }
      
      # Only allow Person or MusicalGroup as target names
      artist_choices <- nodes_tbl_cleaned %>%
        filter(`Node Type` %in% c("Person", "MusicalGroup")) %>%
        arrange(name) %>%
        pull(name) %>%
        unique()
      
      observe({
        updatePickerInput(
          session,
          "target_name",
          choices = artist_choices,
          selected = if ("Sailor Shift" %in% artist_choices) "Sailor Shift" else artist_choices[1]
        )
      })
      
      # --- Genre filter choices ---
      observe({
        # Get genres from Person/MusicalGroup (inferred) and Song/Album (genre)
        person_group_genres <- inferred_genres_for_artistes$inferred_genre %>%
          str_split(",") %>%
          unlist() %>%
          str_trim() %>%
          discard(~ .x == "" | is.na(.x))
        song_album_genres <- nodes_tbl_cleaned %>%
          filter(`Node Type` %in% c("Song", "Album")) %>%
          pull(genre) %>%
          str_split(",") %>%
          unlist() %>%
          str_trim() %>%
          discard(~ .x == "" | is.na(.x))
        genre_choices <- union(person_group_genres, song_album_genres) %>%
          unique() %>%
          sort()
        updatePickerInput(
          session,
          "genre_filter",
          choices = genre_choices,
          selected = genre_choices
        )
      })
      
      # --- Shiny Reactivity ---
      filtered_data <- reactive({
        req(input$target_name)
        sailor_node <- nodes_tbl_cleaned %>% filter(tolower(name) == tolower(input$target_name))
        if (nrow(sailor_node) == 0) {
          validate(sprintf("No node named '%s' found.", as.character(input$target_name)))
        }
        sailor_id <- sailor_node$id[1]
        
        # Inward
        sailor_songs <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 from == sailor_id | to == sailor_id) %>%
          mutate(target_id = ifelse(from == sailor_id, to, from)) %>%
          inner_join(nodes_tbl_cleaned, by = c("target_id" = "id")) %>%
          filter(`Node Type` == "Song") %>%
          pull(target_id)
        sailor_albums <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 from == sailor_id | to == sailor_id) %>%
          mutate(target_id = ifelse(from == sailor_id, to, from)) %>%
          inner_join(nodes_tbl_cleaned, by = c("target_id" = "id")) %>%
          filter(`Node Type` == "Album") %>%
          pull(target_id)
        linked_in_targets <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 to %in% union(sailor_songs, sailor_albums)) %>%
          pull(from)
        all_in_targets <- union(union(sailor_songs, sailor_albums), linked_in_targets)
        contributors_to_targets <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 to %in% all_in_targets)
        edges_to_sailor <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 to == sailor_id | to %in% union(sailor_songs, sailor_albums))
        edges_inward_filtered <- bind_rows(edges_to_sailor, contributors_to_targets) %>%
          distinct(from, to, `Edge Type`, key)
        inward_ids <- unique(c(edges_inward_filtered$from, edges_inward_filtered$to, sailor_id))
        
        # Outward
        linked_out_targets <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 from %in% union(sailor_songs, sailor_albums)) %>%
          pull(to)
        all_out_targets <- union(union(sailor_songs, sailor_albums), linked_out_targets)
        contributors_to_targets_out <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 to %in% all_out_targets)
        edges_from_sailor <- edges_igraph_df %>%
          filter(`Edge Type` %in% desired_edge_types_to_display,
                 from == sailor_id | from %in% union(sailor_songs, sailor_albums))
        edges_outward_filtered <- bind_rows(edges_from_sailor, contributors_to_targets_out) %>%
          distinct(from, to, `Edge Type`, key)
        outward_ids <- unique(c(edges_outward_filtered$from, edges_outward_filtered$to, sailor_id))
        
        # Color ranges
        min_song_year_in <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Song" & id %in% inward_ids) %>%
          pull(release_year) %>% min(na.rm = TRUE)
        max_song_year_in <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Song" & id %in% inward_ids) %>%
          pull(release_year) %>% max(na.rm = TRUE)
        min_album_year_in <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Album" & id %in% inward_ids) %>%
          pull(release_year) %>% min(na.rm = TRUE)
        max_album_year_in <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Album" & id %in% inward_ids) %>%
          pull(release_year) %>% max(na.rm = TRUE)
        
        min_song_year_out <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Song" & id %in% outward_ids) %>%
          pull(release_year) %>% min(na.rm = TRUE)
        max_song_year_out <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Song" & id %in% outward_ids) %>%
          pull(release_year) %>% max(na.rm = TRUE)
        min_album_year_out <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Album" & id %in% outward_ids) %>%
          pull(release_year) %>% min(na.rm = TRUE)
        max_album_year_out <- nodes_tbl_cleaned %>%
          filter(`Node Type` == "Album" & id %in% outward_ids) %>%
          pull(release_year) %>% max(na.rm = TRUE)
        
        # Inward nodes/edges
        nodes_inward_vis <- nodes_tbl_cleaned %>%
          filter(id %in% inward_ids) %>%
          left_join(inferred_genres_for_artistes, by = c("id" = "artiste_id")) %>%
          rowwise() %>%
          mutate(
            label = name,
            title = paste0(
              "<b>Name:</b> ", name, "<br>",
              "<b>Node Type:</b> ", `Node Type`, "<br>",
              get_html_detail(stage_name, "Stage Name"),
              get_html_detail(release_date, "Release Date"),
              get_html_detail(genre, "Genre (Node)"),
              get_html_detail(inferred_genre, "Genre (Inferred)"),
              get_html_detail(notable, "Notable")
            ),
            group = `Node Type`,
            shape = case_when(
              (`Node Type` %in% c("Song", "Album")) & !is.na(notable) & notable ~ "star",
              TRUE ~ "dot"
            ),
            color.background = case_when(
              id == sailor_id ~ "#FF69B4",
              `Node Type` == "Song" & !is.na(release_year) ~ get_color_by_year(release_year, min_song_year_in, max_song_year_in, song_palette_func),
              `Node Type` == "Album" & !is.na(release_year) ~ get_color_by_year(release_year, min_album_year_in, max_album_year_in, album_palette_func),
              TRUE ~ unname(all_node_type_colors[as.character(`Node Type`)])
            ),
            color.background = ifelse(is.na(color.background), all_node_type_colors["Unknown"], color.background),
            color.border = "black",
            color.highlight.background = "red",
            color.highlight.border = "darkred",
            color.hover.background = "lightgray",
            color.hover.border = "darkgray"
          ) %>%
          ungroup()
        edges_inward_final <- edges_inward_filtered %>%
          filter(from %in% nodes_inward_vis$id & to %in% nodes_inward_vis$id) %>%
          group_by(from, to) %>%
          summarise(
            aggregated_label = paste(sort(unique(`Edge Type`)), collapse = ", "),
            color = all_edge_type_colors[first(`Edge Type`)],
            arrows = "to",
            .groups = 'drop'
          ) %>%
          mutate(
            label = aggregated_label,
            title = aggregated_label,
            color = ifelse(is.na(color), all_edge_type_colors["default"], color)
          )
        degree_df_in <- edges_inward_final %>%
          select(from, to) %>%
          pivot_longer(cols = c(from, to), values_to = "id") %>%
          count(id, name = "degree")
        degree_df_in$degree_scaled <- rescale(degree_df_in$degree)
        nodes_inward_vis <- nodes_inward_vis %>%
          left_join(degree_df_in, by = "id") %>%
          mutate(size = ifelse(id == sailor_id, 60, degree_scaled))
        
        # Outward nodes/edges
        nodes_outward_vis <- nodes_tbl_cleaned %>%
          filter(id %in% outward_ids) %>%
          left_join(inferred_genres_for_artistes, by = c("id" = "artiste_id")) %>%
          rowwise() %>%
          mutate(
            label = name,
            title = paste0(
              "<b>Name:</b> ", name, "<br>",
              "<b>Node Type:</b> ", `Node Type`, "<br>",
              get_html_detail(stage_name, "Stage Name"),
              get_html_detail(release_date, "Release Date"),
              get_html_detail(genre, "Genre (Node)"),
              get_html_detail(inferred_genre, "Genre (Inferred)"),
              get_html_detail(notable, "Notable")
            ),
            group = `Node Type`,
            shape = case_when(
              (`Node Type` %in% c("Song", "Album")) & !is.na(notable) & notable ~ "star",
              TRUE ~ "dot"
            ),
            color.background = case_when(
              id == sailor_id ~ "#FF69B4",
              `Node Type` == "Song" & !is.na(release_year) ~ get_color_by_year(release_year, min_song_year_out, max_song_year_out, song_palette_func),
              `Node Type` == "Album" & !is.na(release_year) ~ get_color_by_year(release_year, min_album_year_out, max_album_year_out, album_palette_func),
              TRUE ~ unname(all_node_type_colors[as.character(`Node Type`)])
            ),
            color.background = ifelse(is.na(color.background), all_node_type_colors["Unknown"], color.background),
            color.border = "black",
            color.highlight.background = "red",
            color.highlight.border = "darkred",
            color.hover.background = "lightgray",
            color.hover.border = "darkgray"
          ) %>%
          ungroup()
        edges_outward_final <- edges_outward_filtered %>%
          filter(from %in% nodes_outward_vis$id & to %in% nodes_outward_vis$id) %>%
          group_by(from, to) %>%
          summarise(
            aggregated_label = paste(sort(unique(`Edge Type`)), collapse = ", "),
            color = all_edge_type_colors[first(`Edge Type`)],
            arrows = "to",
            .groups = 'drop'
          ) %>%
          mutate(
            label = aggregated_label,
            title = aggregated_label,
            color = ifelse(is.na(color), all_edge_type_colors["default"], color)
          )
        degree_df_out <- edges_outward_final %>%
          select(from, to) %>%
          pivot_longer(cols = c(from, to), values_to = "id") %>%
          count(id, name = "degree")
        degree_df_out$degree_scaled <- rescale(degree_df_out$degree)
        nodes_outward_vis <- nodes_outward_vis %>%
          left_join(degree_df_out, by = "id") %>%
          mutate(size = ifelse(id == sailor_id, 60, degree_scaled))
        
        # All
        nodes_all_vis <- bind_rows(nodes_inward_vis, nodes_outward_vis) %>%
          distinct(id, .keep_all = TRUE)
        edges_all_final <- bind_rows(edges_inward_final, edges_outward_final) %>%
          distinct(from, to, label, .keep_all = TRUE)
        
        # Filter by node type
        if (input$direction_sailor == "All") {
          nodes <- nodes_all_vis
          edges <- edges_all_final
        } else if (input$direction_sailor == "Inward") {
          nodes <- nodes_inward_vis
          edges <- edges_inward_final
        } else {
          nodes <- nodes_outward_vis
          edges <- edges_outward_final
        }
        nodes <- nodes %>% filter(`Node Type` %in% input$node_types)
        
        # --- Filter by genre, always include target node ---
        if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
          nodes <- nodes %>%
            filter(
              id == sailor_id | (
                case_when(
                  `Node Type` %in% c("Person", "MusicalGroup") ~
                    map_lgl(
                      inferred_genre,
                      ~ any(str_trim(unlist(str_split(as.character(.), ","))) %in% input$genre_filter)
                    ),
                  `Node Type` %in% c("Song", "Album") ~
                    map_lgl(
                      genre,
                      ~ any(str_trim(unlist(str_split(as.character(.), ","))) %in% input$genre_filter)
                    ),
                  TRUE ~ TRUE
                )
              )
            )
          edges <- edges %>% filter(from %in% nodes$id & to %in% nodes$id)
        }
        
        edges <- edges %>% filter(from %in% nodes$id & to %in% nodes$id)
        
        # For legend
        list(
          nodes = nodes,
          edges = edges,
          min_song_year_in = min_song_year_in,
          max_song_year_in = max_song_year_in,
          min_album_year_in = min_album_year_in,
          max_album_year_in = max_album_year_in,
          song_palette_func = song_palette_func,
          album_palette_func = album_palette_func
        )
      })
      
      output$legend_panel <- renderUI({
        fd <- filtered_data()
        song_gradient <- paste(fd$song_palette_func(10), collapse = ",")
        album_gradient <- paste(fd$album_palette_func(10), collapse = ",")
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: flex-start; width: 50%; margin-bottom:8px;",
          tags$div(
            style = "margin-bottom:8px; font-size:12px; line-height:1.2;",
            tags$b("Node Types:", style = "font-size:13px;"),
            tags$ul(style = "list-style:none;padding-left:0;margin-bottom:4px;",
                    tags$li(
                      tags$span(style = "display:inline-block;width:12px;height:12px;background:#FF5733;border-radius:50%;border:1px solid #888;margin-right:4px;vertical-align:middle;"),
                      "Song - not notable"
                    ),
                    tags$li(
                      tags$span(
                        HTML('<svg width="12" height="14" viewBox="0 0 18 18" style="vertical-align:middle;margin-right:4px;"><polygon points="9,1 11,7 17,7 12,11 14,17 9,13 4,17 6,11 1,7 7,7" fill="#FF5733" stroke="#888" stroke-width="1"/></svg>')
                      ),
                      "Song - notable"
                    ),
                    tags$li(
                      tags$span(style = "display:inline-block;width:12px;height:12px;background:lightblue;border-radius:50%;border:1px solid #888;margin-right:4px;vertical-align:middle;"),
                      "Album - not notable"
                    ),
                    tags$li(
                      tags$span(
                        HTML('<svg width="12" height="14" viewBox="0 0 18 18" style="vertical-align:middle;margin-right:4px;"><polygon points="9,1 11,7 17,7 12,11 14,17 9,13 4,17 6,11 1,7 7,7" fill="lightblue" stroke="#888" stroke-width="1"/></svg>')
                      ),
                      "Album - notable"
                    ),
                    tags$li(
                      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;background:%s;border-radius:50%%;border:1px solid #888;margin-right:4px;vertical-align:middle;", all_node_type_colors["Person"])),
                      "Person"
                    ),
                    tags$li(
                      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;background:%s;border-radius:50%%;border:1px solid #888;margin-right:4px;vertical-align:middle;", all_node_type_colors["MusicalGroup"])),
                      "Musical Group"
                    ),
                    tags$li(
                      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;background:%s;border-radius:50%%;border:1px solid #888;margin-right:4px;vertical-align:middle;", all_node_type_colors["RecordLabel"])),
                      "Record Label"
                    ),
                    tags$li(
                      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;background:%s;border-radius:50%%;border:1px solid #888;margin-right:4px;vertical-align:middle;", all_node_type_colors["Unknown"])),
                      "Unknown"
                    ),
                    tags$li(
                      tags$span(style = "display:inline-block;width:12px;height:12px;background:#FF69B4;border-radius:50%;border:1px solid #888;margin-right:4px;vertical-align:middle;"),
                      input$target_name
                    )
            )
          ),
          tags$div(
            style = "flex: 0 0 auto; min-width: 130px; margin-top: 2px",
            tags$div(
              style = "margin-bottom:4px; font-size:12px;",
              tags$b("Song Year:", style = "font-size:12px;"),
              tags$div(
                style = sprintf("height:10px;width:120px;background:linear-gradient(to right, %s);border:1px solid #888;margin:2px 0;", song_gradient)
              ),
              tags$span(fd$min_song_year_in, style = "font-size:11px;"),
              tags$span(fd$max_song_year_in, style = "margin-left:60px;font-size:11px;")
            ),
            tags$div(
              style = "margin-bottom:4px; font-size:12px;",
              tags$b("Album Year:", style = "font-size:12px;"),
              tags$div(
                style = sprintf("height:10px;width:120px;background:linear-gradient(to right, %s);border:1px solid #888;margin:2px 0;", album_gradient)
              ),
              tags$span(fd$min_album_year_in, style = "font-size:11px;"),
              tags$span(fd$max_album_year_in, style = "margin-left:60px;font-size:11px;")
            )
          )
        )
      })
      
      output$network <- renderVisNetwork({
        fd <- filtered_data()
        visNetwork(fd$nodes, fd$edges) %>%
          visNodes(
            color = list(
              hover = list(background = "lightgray", border = "darkgray")
            ),
            shadow = TRUE,
            font = list(size = 12),
            shape = "dot"
          ) %>%
          visEdges(
            arrows = "to",
            label = ~label,
            font = list(size = 10, align = "middle"),
            title = ~title,
            color = list(color = ~color, highlight = ~color),
            smooth = list(enabled = TRUE, type = "continuous", roundness = 0.5),
            width = 1
          ) %>%
          visOptions(
            highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE, algorithm = "hierarchical"),
            nodesIdSelection = TRUE,
            selectedBy = "group"
          ) %>%
          visInteraction(navigationButtons = TRUE, keyboard = TRUE) %>%
          visIgraphLayout(layout = "layout_with_fr", randomSeed = 1234)
      })
      
      output$degree_bar <- renderPlot({
        fd <- filtered_data()
        nodes <- fd$nodes
        
        nodes_bar <- nodes %>%
          filter(`Node Type` %in% c("Person", "MusicalGroup")) %>%
          filter(name != input$target_name) %>%
          select(name, `Node Type`, degree) %>%
          mutate(degree = round(degree, 1)) %>%
          arrange(desc(degree)) %>%
          slice_head(n = 5)
        
        # Dynamic title
        plot_title <- switch(
          input$direction_sailor,
          "All" = sprintf("Top 5 Person/ MusicalGroup Overall Influences for %s (Degree)", input$target_name),
          "Outward" = sprintf("Top 5 Person/ MusicalGroup Who Influenced %s (Degree)", input$target_name),
          "Inward" = sprintf("Top 5 Person/ MusicalGroup Who %s Influenced (Degree)", input$target_name),
          ""
        )
        
        if (nrow(nodes_bar) == 0) {
          plot.new()
          text(0.5, 0.5, "No Person or MusicalGroup nodes in current view", cex = 1.2)
        } else {
          ggplot(nodes_bar, aes(x = reorder(name, degree), y = degree, fill = `Node Type`)) +
            geom_col(show.legend = TRUE) +
            geom_text(aes(label = degree), hjust = -0.1, size = 4) +
            coord_flip(clip = "off") +
            labs(
              x = "Name",
              y = "Degree",
              title = plot_title
            ) +
            scale_fill_manual(values = c("Person" = "deepskyblue", "MusicalGroup" = "purple")) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 14, face = "bold"),
              axis.text.y = element_text(size = 11),
              legend.position = "bottom",
              plot.margin = margin(5, 40, 5, 5)
            )
        }
      })
    }
  })
  
  #-------------End for Influence Network-----------------
  
  #-------------For Oceanus Folk-------------------------  
  observe({
    if (activeTab() == "nav_oceanus") {
      # ---- Oceanus Folk Tab Logic ----
      # ---- 1. Preprocess and cache data once ----
      kg_data <- fromJSON("data/MC1_graph.json")
      nodes_tbl <- as_tibble(kg_data$nodes)
      links_tbl <- as_tibble(kg_data$links)
      
      nodes_clean <- nodes_tbl %>%
        mutate(
          date_raw    = coalesce(release_date, written_date, notoriety_date),
          date_parsed = parse_date_time(date_raw, orders = c("Ymd","Y-m-d","Y"), quiet = TRUE),
          year        = year(date_parsed),
          rowid       = row_number()
        ) %>%
        select(rowid, id, name, `Node Type`, genre, year)
      
      edges_clean <- links_tbl %>%
        rename(source_id = source, target_id = target) %>%
        left_join(nodes_clean %>% select(id, rowid), by = c("source_id" = "id")) %>% rename(from = rowid) %>%
        left_join(nodes_clean %>% select(id, rowid), by = c("target_id" = "id")) %>% rename(to = rowid) %>%
        filter(!is.na(from) & !is.na(to))
      
      # ui <- fluidPage(
      #   titlePanel("Oceanus Folk Influence Explorer"),
      #   sidebarLayout(
      #     sidebarPanel(
      #       helpText("Explore the influence of Oceanus Folk across genres and time."),
      #       selectInput(
      #         "influence_direction",
      #         "Select Influence Direction:",
      #         choices = c("Outward", "Inward"),
      #         selected = "Outward"
      #       ),
      #       uiOutput("genre_filter_ui"),
      #       uiOutput("year_slider_ui")
      #       )
      #     ),
      #     mainPanel(
      #       conditionalPanel(
      #         condition = "input.influence_direction == 'Outward'",
      #         plotlyOutput("timelinePlot", height = "400px"),
      #         visNetworkOutput("expandedNet", height = "500px")
      #       ),
      #       conditionalPanel(
      #         condition = "input.influence_direction == 'Inward'",
      #         plotlyOutput("inwardTimelinePlot", height = "400px"),
      #         visNetworkOutput("genreBandNet", height = "500px")
      #       )
      #     )
      #   )
      
      
      # OUTWARD timeline data (using cached data)
      timeline_data <- reactive({
        # Use cached nodes_clean and edges_clean
        of_edges     <- c("PerformerOf","ComposerOf","ProducerOf","LyricistOf")
        of_media <- nodes_clean %>%
          filter(genre == "Oceanus Folk",
                 `Node Type` %in% c("Song","Album")) %>% 
          pull(rowid)
        of_artists <- edges_clean %>%
          filter(`Edge Type` %in% of_edges, to %in% of_media) %>%
          pull(from) %>% unique()
        hop2_all <- edges_clean %>%
          filter(from %in% of_artists) %>%
          transmute(target_media = to,
                    edge_type    = `Edge Type`)
        hop2_all <- hop2_all %>%
          left_join(
            nodes_clean %>% select(rowid, genre, year),
            by = c("target_media" = "rowid")
          ) %>%
          rename(target_genre = genre,
                 target_year  = year) %>%
          filter(!is.na(target_genre),
                 target_genre != "Oceanus Folk",
                 !is.na(target_year))
        yearly <- hop2_all %>%
          count(target_genre, target_year, name = "new_links") %>%
          arrange(target_genre, target_year) %>%
          group_by(target_genre) %>%
          mutate(cumulative = cumsum(new_links)) %>%
          ungroup()
        all_genres <- unique(yearly$target_genre)
        start_year <- min(yearly$target_year[yearly$cumulative > 0], na.rm = TRUE)
        all_years  <- seq(start_year, 2035)
        yearly_ext <- yearly %>%
          complete(target_genre = all_genres,
                   target_year  = all_years,
                   fill         = list(new_links = 0)) %>%
          group_by(target_genre) %>%
          arrange(target_year) %>%
          mutate(cumulative = cumsum(new_links)) %>%
          ungroup()
        pal <- if (length(all_genres) <= 8) {
          RColorBrewer::brewer.pal(length(all_genres), "Set2")
        } else {
          colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(all_genres))
        }
        names(pal) <- all_genres
        list(
          yearly_ext = yearly_ext,
          pal = pal,
          all_genres = all_genres,
          start_year = start_year,
          nodes_clean = nodes_clean,
          edges_clean = edges_clean
        )
      })
      
      # INWARD timeline data (using cached data)
      inward_timeline_data <- reactive({
        id_map <- nodes_clean %>% select(original_id = id, rowid)
        edges_tbl <- links_tbl # Use cached links_tbl for original IDs
        edges_clean2 <- edges_tbl %>%
          left_join(id_map, by = c("source" = "original_id")) %>% rename(from = rowid) %>%
          left_join(id_map, by = c("target" = "original_id")) %>% rename(to   = rowid) %>%
          filter(!is.na(from), !is.na(to))
        influence_types <- c(
          "InStyleOf","InterpolatesFrom","CoverOf","LyricalReferenceTo","DirectlySamples",
          "PerformerOf","ComposerOf","ProducerOf","LyricistOf",
          "RecordedBy","DistributedBy","CollaboratedWith","WrittenBy","MemberOf","Notoriety"
        )
        of_media <- nodes_clean %>%
          filter(genre == "Oceanus Folk",
                 `Node Type` %in% c("Song","Album")) %>%
          pull(rowid)
        missing_ids <- edges_clean2 %>%
          filter(`Edge Type` %in% influence_types, to %in% of_media) %>%
          left_join(nodes_clean %>% select(rowid, declared = genre),
                    by = c("from" = "rowid")) %>%
          filter(is.na(declared)) %>% pull(from) %>% unique()
        raw <- edges_clean2 %>%
          filter(`Edge Type` %in% influence_types, to %in% of_media) %>%
          left_join(nodes_clean %>% select(rowid, declared = genre),
                    by = c("from" = "rowid")) %>%
          left_join(
            edges_clean2 %>%
              filter(from %in% missing_ids) %>%
              left_join(nodes_clean %>% select(rowid, media_genre = genre),
                        by = c("to" = "rowid")) %>%
              filter(!is.na(media_genre)) %>%
              count(from, media_genre) %>%
              group_by(from) %>% slice_max(n, with_ties=FALSE) %>%
              ungroup() %>%
              transmute(from, inferred = media_genre),
            by = "from"
          ) %>%
          mutate(source_genre = coalesce(declared, inferred)) %>%
          filter(!is.na(source_genre), source_genre != "Oceanus Folk") %>%
          left_join(nodes_clean %>% select(rowid, media_year = year),
                    by = c("to" = "rowid")) %>%
          filter(!is.na(media_year))
        genre_yearly_raw <- raw %>%
          count(source_genre, media_year, name = "new_links")
        all_genres <- sort(unique(genre_yearly_raw$source_genre))
        years      <- seq(min(genre_yearly_raw$media_year), 2040)
        genre_yearly <- expand_grid(
          source_genre = all_genres,
          media_year   = years
        ) %>%
          left_join(genre_yearly_raw, by = c("source_genre","media_year")) %>%
          replace_na(list(new_links=0)) %>%
          group_by(source_genre) %>%
          arrange(media_year) %>%
          mutate(cumulative = cumsum(new_links)) %>%
          ungroup()
        pal <- if (length(all_genres) <= 8) {
          brewer.pal(length(all_genres), "Set2")
        } else {
          colorRampPalette(brewer.pal(8, "Set2"))(length(all_genres))
        }
        names(pal) <- all_genres
        first_year <- min(genre_yearly$media_year)
        list(
          genre_yearly = genre_yearly,
          pal = pal,
          all_genres = all_genres,
          first_year = first_year
        )
      })
      
      # Dynamically generate genre filter UI as checkboxes
      output$genre_filter_ui <- renderUI({
        dat <- if (input$influence_direction == "Outward") timeline_data() else inward_timeline_data()
        shinyWidgets::pickerInput(
          inputId = "genre_filter",
          label = "Select Genres to Display on Timeline:",
          choices = sort(dat$all_genres),
          selected = character(0),   # No genres selected by default
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `size` = 8
          )
        )
      })
      
      # Observe select all/deselect all buttons
      observeEvent(input$select_all, {
        dat <- if (input$influence_direction == "Outward") timeline_data() else inward_timeline_data()
        updateCheckboxGroupInput(session, "genre_filter",
                                 selected = dat$all_genres)
      })
      observeEvent(input$deselect_all, {
        updateCheckboxGroupInput(session, "genre_filter",
                                 selected = character(0))
      })
      
      # Year slider UI
      output$year_slider_ui <- renderUI({
        dat <- if (input$influence_direction == "Outward") timeline_data() else inward_timeline_data()
        min_year <- if (input$influence_direction == "Outward") min(dat$yearly_ext$target_year, na.rm = TRUE) else min(dat$genre_yearly$media_year, na.rm = TRUE)
        max_year <- if (input$influence_direction == "Outward") max(dat$yearly_ext$target_year, na.rm = TRUE) else max(dat$genre_yearly$media_year, na.rm = TRUE)
        sliderInput(
          inputId = "year_range",
          label = "Select Year Range:",
          min = min_year,
          max = max_year,
          value = c(min_year, max_year),
          sep = "",
          step = 1
        )
      })
      
      # OUTWARD filtered data for timeline
      filtered_yearly <- reactive({
        req(input$genre_filter, input$year_range)
        dat <- timeline_data()
        dat$yearly_ext %>%
          filter(
            target_genre %in% input$genre_filter,
            target_year >= input$year_range[1],
            target_year <= input$year_range[2]
          )
      })
      
      # INWARD filtered data for timeline
      filtered_inward_yearly <- reactive({
        req(input$genre_filter, input$year_range)
        dat <- inward_timeline_data()
        dat$genre_yearly %>%
          filter(
            source_genre %in% input$genre_filter,
            media_year >= input$year_range[1],
            media_year <= input$year_range[2]
          )
      })
      
      # OUTWARD timeline plot
      output$timelinePlot <- renderPlotly({
        req(input$influence_direction == "Outward")
        dat <- timeline_data()
        filtered <- filtered_yearly()
        pal_sel <- dat$pal[names(dat$pal) %in% unique(filtered$target_genre)]
        min_year <- min(filtered$target_year, na.rm = TRUE)
        max_year <- max(filtered$target_year, na.rm = TRUE)
        max_cum  <- max(filtered$cumulative, na.rm = TRUE)
        plot_ly(
          data  = filtered,
          x     = ~target_year,
          y     = ~cumulative,
          color = ~factor(target_genre, levels = names(pal_sel)),
          colors= pal_sel,
          type  = 'scatter',
          mode  = 'lines+markers',
          hoverinfo = 'text',
          hovertext = ~paste0(
            "Genre: ", target_genre,
            "<br>Year: ", target_year,
            "<br>Cumulative: ", cumulative
          )
        ) %>%
          layout(
            title = "Oceanus Folk Influence on Other Genres",
            xaxis = list(
              title     = "Year",
              tickmode  = "linear",
              dtick     = 5,
              tickangle = 0,
              range     = c(min_year - 1, max_year + 1)
            ),
            yaxis = list(
              title = "Cumulative Influence",
              range = c(0, max_cum * 1.15)
            ),
            legend = list(x = 1.02, y = 0.5, orientation = "v"),
            shapes = list(
              list(
                type = "line",
                x0 = 2028, x1 = 2028,
                y0 = 0,    y1 = max_cum * 1.15,
                line = list(dash="dash", color="firebrick")
              )
            ),
            annotations = list(
              list(
                x = 2028, 
                y = max_cum * 1.13,
                text = "2028: Sailor’s Breakthrough",
                xref = "x", yref = "y",
                xanchor = "right",
                showarrow = TRUE, arrowhead = 2,
                ax = -30, ay = -30,
                font = list(color="firebrick", size=12)
              )
            ),
            margin = list(l = 60, r = 120, b = 60, t = 80, pad = 10)
          )
      })
      
      # INWARD timeline plot
      output$inwardTimelinePlot <- renderPlotly({
        req(input$influence_direction == "Inward")
        dat <- inward_timeline_data()
        filtered <- filtered_inward_yearly()
        pal_sel <- dat$pal[names(dat$pal) %in% unique(filtered$source_genre)]
        min_year <- min(filtered$media_year, na.rm = TRUE)
        max_year <- max(filtered$media_year, na.rm = TRUE)
        max_cum  <- max(filtered$cumulative, na.rm = TRUE)
        plot_ly(
          data  = filtered,
          x     = ~media_year,
          y     = ~cumulative,
          color = ~factor(source_genre, levels = names(pal_sel)),
          colors= pal_sel,
          type  = 'scatter',
          mode  = 'lines+markers',
          hoverinfo = 'text',
          hovertext = ~paste0(
            "Genre: ", source_genre,
            "<br>Year: ", media_year,
            "<br>Cumulative: ", cumulative
          )
        ) %>%
          layout(
            title  = "Genres Influencing Oceanus Folk",
            xaxis  = list(
              title    = "Year",
              range    = c(min_year - 1, max_year + 1),
              tickmode = "linear",
              dtick    = 5
            ),
            yaxis = list(
              title = "Cumulative Influence",
              range = c(0, max_cum * 1.15)
            ),
            shapes = list(
              list(type="line", x0=2023, x1=2023, y0=0, y1=1,
                   xref="x", yref="paper", line=list(dash="dash", color="black")),
              list(type="line", x0=2026, x1=2026, y0=0, y1=1,
                   xref="x", yref="paper", line=list(dash="dash", color="black")),
              list(type="line", x0=2028, x1=2028, y0=0, y1=1,
                   xref="x", yref="paper", line=list(dash="dash", color="firebrick"))
            ),
            annotations = list(
              list(x=2023, y=0.97 * max_cum * 1.15, xref="x", yref="y", text="Joined Ivy Echoes",
                   showarrow=FALSE, font=list(size=11)),
              list(x=2026, y=0.90 * max_cum * 1.15, xref="x", yref="y", text="Disbanded",
                   showarrow=FALSE, font=list(size=11)),
              list(x=2028, y=max_cum * 1.13,
                   xref="x", yref="y", text="Breakthrough",
                   showarrow=TRUE, arrowhead=2, ax=30, ay=-30,
                   font=list(color="firebrick", size=12))
            ),
            legend = list(xanchor="left", x=1.02, y=0.5, orientation="v"),
            margin = list(l = 60, r = 120, b = 60, t = 80, pad = 10)
          )
      })
      
      # ...existing expanded_network_data, output$expandedNet, genre_band_network_data, output$genreBandNet code...
      # (No changes needed to these, as they are already referenced in the correct conditional panels)
      expanded_network_data <- reactive({
        req(input$genre_filter, input$year_range)
        dat <- timeline_data()
        nodes_clean <- dat$nodes_clean %>%
          filter(
            (`Node Type` %in% c("Song", "Album") & !is.na(year) & year >= input$year_range[1] & year <= input$year_range[2]) |
              (!`Node Type` %in% c("Song", "Album"))
          )
        edges_clean <- dat$edges_clean %>%
          filter(from %in% nodes_clean$rowid & to %in% nodes_clean$rowid)
        
        # 1. Enrich edges with source genre and performer/media types
        edges2 <- edges_clean %>%
          left_join(nodes_clean %>% select(rowid, source_genre = genre),
                    by = c("from" = "rowid")) %>%
          left_join(nodes_clean %>% select(rowid, performer_type = `Node Type`),
                    by = c("from" = "rowid")) %>%
          left_join(nodes_clean %>% select(rowid, media_type = `Node Type`),
                    by = c("to"   = "rowid"))
        
        # 2. Identify all Oceanus Folk media IDs
        folk_media <- nodes_clean %>%
          filter(`Node Type` %in% c("Song","Album") & genre == "Oceanus Folk") %>%
          pull(rowid)
        
        # 3. Count how many Oceanus Folk media each band performed
        folk_cover_counts <- edges2 %>%
          filter(`Edge Type` == "PerformerOf",
                 to %in% folk_media,
                 performer_type == "MusicalGroup") %>%
          left_join(nodes_clean %>% select(rowid, name),
                    by = c("from" = "rowid")) %>%
          count(name, name = "n_folk") %>%
          rename(artist = name)
        
        # 4. Gather all media (Song/Album) each band performed and count
        band_media_edges <- edges2 %>%
          filter(`Edge Type` == "PerformerOf",
                 performer_type == "MusicalGroup",
                 media_type %in% c("Song","Album")) %>%
          left_join(nodes_clean %>% select(rowid, band = name),
                    by = c("from" = "rowid")) %>%
          left_join(nodes_clean %>% select(rowid, media = name),
                    by = c("to"   = "rowid")) %>%
          distinct(band, media)
        
        media_counts <- band_media_edges %>%
          count(band, name = "n_media") %>%
          rename(artist = band)
        
        # 5. Compute Oceanus Folk → genre influence edges (filtered by selected genres)
        infl_edges <- edges2 %>%
          filter(source_genre == "Oceanus Folk",
                 `Edge Type` %in% c("InStyleOf","InterpolatesFrom","CoverOf",
                                    "LyricalReferenceTo","DirectlySamples",
                                    "PerformerOf","ComposerOf","ProducerOf",
                                    "LyricistOf","RecordedBy","DistributedBy"))
        
        genre_counts <- infl_edges %>%
          left_join(nodes_clean %>% select(rowid, genre, year), by = c("to" = "rowid")) %>%
          filter(!is.na(year), year >= input$year_range[1], year <= input$year_range[2]) %>%
          count(genre, name = "n_influences") %>%
          filter(genre != "Oceanus Folk", genre %in% input$genre_filter)
        
        # 6. For each influenced genre, count distinct artists
        media_by_genre <- nodes_clean %>%
          filter(`Node Type` %in% c("Song","Album") & genre %in% genre_counts$genre) %>%
          select(media_rowid = rowid, genre)
        
        genre_band_edges <- edges2 %>%
          filter(to %in% media_by_genre$media_rowid) %>%
          left_join(media_by_genre, by = c("to" = "media_rowid")) %>%
          left_join(nodes_clean %>% select(rowid, band_name = name, `Node Type`),
                    by = c("from" = "rowid")) %>%
          filter(`Node Type` == "MusicalGroup") %>%
          distinct(genre, band_name)
        
        genre_band_counts <- genre_band_edges %>%
          count(genre, name = "n_bands")
        
        # 7. Build nodes_vis
        root_node <- tibble(
          id    = "Oceanus Folk",
          label = "Oceanus Folk",
          group = "Oceanus Folk",
          title = paste0(
            "Genres influenced: ", sum(genre_counts$n_influences),
            "<br>Total Artists: ", nrow(media_counts)
          )
        )
        
        genre_nodes <- genre_counts %>%
          left_join(genre_band_counts, by = "genre") %>%
          transmute(
            id    = genre,
            label = genre,
            group = "Genre",
            title = paste0("Influences: ", n_influences, "<br>Artists: ", n_bands)
          )
        
        artist_nodes <- full_join(folk_cover_counts, media_counts, by = "artist") %>%
          replace_na(list(n_folk = 0, n_media = 0)) %>%
          transmute(
            id    = artist,
            label = artist,
            group = "Artist",
            title = paste0("Folk covers: ", n_folk,
                           "<br>Total songs/albums: ", n_media)
          )
        
        nodes_vis <- bind_rows(root_node, genre_nodes, artist_nodes)
        
        # 8. Build edges_vis
        e1 <- genre_counts %>%
          left_join(genre_band_counts, by = "genre") %>%
          transmute(
            from  = "Oceanus Folk",
            to    = genre,
            title = paste0("Influences: ", n_influences, "<br>Bands: ", n_bands)
          )
        
        e2 <- folk_cover_counts %>%
          transmute(
            from  = "Oceanus Folk",
            to    = artist,
            title = paste0("Folk covers: ", n_folk)
          )
        
        e3 <- genre_band_edges %>%
          transmute(
            from  = genre,
            to    = band_name,
            title = paste0("Genre performer: ", band_name)
          )
        
        edges_vis <- bind_rows(e1, e2, e3)
        
        # 9. Add Band → Media edges
        media_nodes <- band_media_edges %>%
          transmute(
            id    = media,
            label = media,
            group = "Song/Album",
            title = paste0("Performed by: ", band)
          ) %>%
          distinct(id, .keep_all = TRUE)
        
        media_edges <- band_media_edges %>%
          transmute(
            from  = band,
            to    = media,
            title = paste0("Performed by: ", band)
          ) %>%
          distinct()
        
        nodes_vis <- bind_rows(nodes_vis, media_nodes)
        edges_vis <- bind_rows(edges_vis, media_edges)
        
        # 10. De-duplicate & size nodes
        nodes_vis <- nodes_vis %>% distinct(id, .keep_all = TRUE)
        edges_vis <- edges_vis %>% distinct(from, to, .keep_all = TRUE)
        
        nodes_vis <- nodes_vis %>%
          left_join(genre_counts %>% select(id = genre, n_influences), by = "id") %>%
          left_join(folk_cover_counts %>% select(id = artist, n_folk), by = "id") %>%
          mutate(
            size = case_when(
              id == "Oceanus Folk" ~ 60,
              group == "Genre"     ~ rescale(n_influences, to = c(20, 50)),
              group == "Artist"    ~ rescale(n_folk, to = c(20, 50)),
              TRUE                 ~ 20
            )
          ) %>%
          select(-n_influences, -n_folk)
        
        # 11. Prune to only artists directly linked from root or genres
        valid_bands <- unique(c(e2$to, e3$to))
        valid_media <- media_edges %>%
          filter(from %in% valid_bands) %>%
          pull(to)
        valid_ids <- c("Oceanus Folk", genre_nodes$id, valid_bands, valid_media)
        nodes_vis <- nodes_vis %>%
          filter(id %in% valid_ids)
        
        # Return for visNetwork
        list(nodes = nodes_vis, edges = edges_vis)
      })
      
      output$expandedNet <- renderVisNetwork({
        net <- expanded_network_data()
        visNetwork(net$nodes, net$edges, width = "100%", height = "500px") %>%
          visNodes(shape = "dot", font = list(size = 14, face = "bold")) %>%
          visEdges(smooth = FALSE, arrows = "to") %>%
          visGroups(groupname = "Oceanus Folk",
                    color = list(background = "#87CEFA", border = "black")) %>%
          visGroups(groupname = "Genre",
                    color = list(background = "#FFA500", border = "#FFA500")) %>%
          visGroups(groupname = "Artist",
                    color = list(background = "#00C2A1", border = "#00C2A1")) %>%
          visGroups(groupname = "Song/Album",
                    color = list(background = "#FFB6C1", border = "#FFB6C1")) %>%
          visOptions(
            highlightNearest   = list(enabled = TRUE, degree = 1, hover = TRUE),
            nodesIdSelection   = list(enabled = TRUE, useLabels = TRUE)
          ) %>%
          visLegend(useGroups = TRUE, position = "left") %>%
          visLayout(randomSeed = 42) %>%
          visPhysics(solver = "forceAtlas2Based", stabilization = TRUE)
      })
      
      # # Genre-Band Network
      genre_band_network_data <- reactive({
        req(input$genre_filter, input$year_range)
        dat <- timeline_data()
        nodes_clean <- dat$nodes_clean %>%
          filter(
            # Only filter Songs/Albums by year, keep all others
            (`Node Type` %in% c("Song", "Album") & !is.na(year) & year >= input$year_range[1] & year <= input$year_range[2]) |
              (!`Node Type` %in% c("Song", "Album"))
          )
        edges_clean <- dat$edges_clean %>%
          filter(from %in% nodes_clean$rowid & to %in% nodes_clean$rowid)
        
        # 1) Oceanus Folk media rowids
        folk_media <- nodes_clean %>%
          filter(`Node Type` %in% c("Song","Album") & genre == "Oceanus Folk") %>%
          pull(rowid)
        
        # 2) Enrich edges
        edges2 <- edges_clean %>%
          left_join(nodes_clean %>% select(rowid, source_genre = genre),
                    by = c("from" = "rowid")) %>%
          left_join(nodes_clean %>% select(rowid, media_type = `Node Type`),
                    by = c("to"   = "rowid"))
        
        # ── Hop 1: genre → Oceanus Folk
        hop1 <- edges2 %>%
          filter(
            `Edge Type` %in% c("InStyleOf","CoverOf","DirectlySamples","PerformerOf",
                               "ComposerOf","ProducerOf","LyricistOf","RecordedBy",
                               "DistributedBy","LyricalReferenceTo","InterpolatesFrom"),
            media_type %in% c("Song","Album"),
            to %in% folk_media,
            !is.na(source_genre),
            source_genre != "Oceanus Folk",
            source_genre %in% input$genre_filter
          ) %>%
          count(source_genre, name = "weight") %>%
          transmute(from = source_genre, to = "Oceanus Folk", weight)
        
        # ── Hop 2: genre → MusicalGroup
        media_by_genre <- nodes_clean %>%
          filter(`Node Type` %in% c("Song","Album"),
                 genre %in% hop1$from) %>%
          transmute(media_rowid = rowid, genre)
        
        hop2 <- edges2 %>%
          filter(`Edge Type` == "PerformerOf",
                 to %in% media_by_genre$media_rowid) %>%
          left_join(media_by_genre, by = c("to" = "media_rowid")) %>%
          left_join(nodes_clean %>% select(rowid, band = name, `Node Type`),
                    by = c("from" = "rowid")) %>%
          filter(`Node Type` == "MusicalGroup") %>%
          distinct(genre, band) %>%
          count(genre, band, name = "weight") %>%
          transmute(from = genre, to = band, weight)
        
        # ── Assemble edges_vis (only Hop1 + Hop2)
        edges_vis <- bind_rows(hop1, hop2) %>%
          transmute(
            from, to,
            width  = 1,
            arrows = "to",
            title  = paste0("Links: ", weight),
            color  = "black"
          )
        
        # ── Assemble nodes_vis (only Center, Genres, Bands)
        nodes_vis <- bind_rows(
          # center
          tibble(
            id    = "Oceanus Folk",
            label = "Oceanus Folk",
            group = "Center",
            value = ifelse(nrow(hop1) > 0, max(hop1$weight, na.rm = TRUE) * 1.2, 10),
            title = "<b>Oceanus Folk</b><br>Genre hub"
          ),
          # genres
          hop1 %>% transmute(
            id    = from,
            label = from,
            group = "Genre",
            value = weight,
            title = paste0("<b>", from, "</b><br>Links→OF: ", weight)
          ),
          # bands
          hop2 %>% transmute(
            id    = to,
            label = to,
            group = "Band",
            value = weight,
            title = paste0("<b>", to, "</b><br>Genres played: ", weight)
          )
        ) %>%
          distinct(id, .keep_all = TRUE) %>%
          filter(id %in% unique(c(edges_vis$from, edges_vis$to)))
        
        list(nodes = nodes_vis, edges = edges_vis)
      })
      
      output$genreBandNet <- renderVisNetwork({
        net <- genre_band_network_data()
        visNetwork(net$nodes, net$edges, height = "600px", width = "100%") %>%
          visGroups(groupname = "Center", color = list(background = "#87CEFA", border = "black")) %>%   # Light blue
          visGroups(groupname = "Genre",  color = list(background = "#FFA500", border = "#FFA500")) %>%   # Yellow
          visGroups(groupname = "Band",   color = list(background = "#00C2A1", border = "#00C2A1")) %>%   # Red
          visNodes(
            shape = "dot",
            # color = list(
            #   background = c(Center = "#9E00E3", Genre = "#FFA500", Band = "#00C2A1")[net$nodes$group],
            #   border     = "black"
            # ),
            scaling = list(min = 10, max = 40),
            font    = list(size = 14)
          ) %>%
          visEdges(
            color   = list(color = "black"),
            smooth  = FALSE,
            arrows  = list(to = list(enabled = TRUE, scaleFactor = 1))
          ) %>%
          visOptions(
            highlightNearest  = list(enabled = TRUE, degree = 1),
            nodesIdSelection  = TRUE
          ) %>%
          visLegend(
            useGroups = FALSE,
            addNodes  = data.frame(
              label = c("Oceanus Folk", "Genre", "Band"),
              # shape = "dot",
              color = c("#87CEFA", "#FFA500", "#00C2A1")
            )
          ) %>%
          visPhysics(
            solver        = "forceAtlas2Based",
            stabilization = list(enabled = TRUE, iterations = 200)
          )
      })
      
      # end of genre band network code
      
      
    }
  })
  
  #-------------End for Oceanus Folk-------------------------
  
  output$cover_img <- renderUI({
    tags$img(
      src = switch(input$summary,
                   "Sailor Shift"   = "sailorshift.png",
                   "Oceanus Folk"   = "OceanusFolk.png",
                   "Top Artist"     = "topartist.png",
                   "Rising Stars"  = "risingact.png"
      ),
      class = "cover-img"
    )
  })
  
  output$about_header <- renderUI({
    tags$h4(
      switch(input$summary,
             "Sailor Shift"   = "About Sailor Shift",
             "Oceanus Folk"   = "About the Genre",
             "Top Artist"     = "About Top Artist",
             "Rising Stars"  = "About Rising Stars"
      ),
      style="color:#FFF;margin:16px 0 8px;font-weight:800;"
    )
  })
  
  output$genre_description <- renderUI({
    HTML(switch(input$summary,
                "Sailor Shift"   = "… Sailor description …",
                "Oceanus Folk"   = "… Oceanus description …",
                "Top Artist"     = "… Top Artist description …",
                "Rising Stars"  = "… Rising Stars description …"
    ))
  })
  
  # Existing plot/render functions for non-network tabs
  output$otherPlot      <- renderPlot({ boxplot(matrix(rnorm(100), ncol=5)) })
  output$network_plot   <- renderPlot({ plot(1:10,1:10) })
  output$influence_plot <- renderPlotly({ plot_ly(x=1:10,y=1:10,type="scatter",mode="lines+markers") })
  output$general_overview <- renderUI({ HTML("<p>Oceanus Folk’s stylistic reach...</p>") })
}

shinyApp(ui, server)
