
# ********************************
# Zurich Real Estate App
# ********************************

# Librarys ----
# pacman::p_load(, , rgdal,xlsx, forcats, raster, cleangeo, hexbin,  , , RColorBrewer, , glue, , , , , , ggpubr, gridExtra, )

#setwd("L:/STAT/03_AS/02_Datengrundlagen/MGr_Handaenderungen/Internettool/ImmoApp/01_zhre_APP/")

#source("quickXLSX.R")

devtools::install_github("statistikZH/statR") 

#library(svglite)
library(tidyverse)
library(distrr)
library(magrittr)
library(statR)
#library(gridExtra)
library(scales)
library(RColorBrewer)

library(shiny)
library(shinythemes)
library(shinyjs)
#library(shinyWidgets)

library(sp)
library(tmap)
library(leaflet)
library(plotly)


# Zusatzfunktion-----
my_sum <- function(df, var, ...) {
  group_by <- quos(...)
  var <- enquo(var)
  mean_name <- paste0("mean_", quo_name(var))
  sd_name <- paste0("sd_", quo_name(var))
  q25_name <- paste0("q25_", quo_name(var))
  med_name <- paste0("med_", quo_name(var))
  q75_name <- paste0("q75_", quo_name(var))
  
  df %>%
    group_by(!!!group_by) %>%
    summarise(
      n_obs=n(),
      !!mean_name := ifelse(n_obs<=3,NA,mean(!!var, na.rm = T)),
      !!sd_name := ifelse(n_obs<=5,NA,sd(!!var, na.rm = T)),
      !!q25_name := ifelse(n_obs<=5,NA,quantile(!!var, probs=0.25, na.rm = T)),
      !!med_name := ifelse(n_obs<=3,NA,median(!!var, probs=0.5, na.rm = T)),
      !!q75_name := ifelse(n_obs<=5,NA,quantile(!!var, probs=0.75, na.rm = T))
    ) %>% 
    ungroup()
}

#swissgridneu<-CRS("+init=epsg:2056")



#******************************

## ui.R
# *****************************************
# Layout mit Sidebar und reaktiver Kopfzeile
# Mainpanel mit mehreren Tabs
# Optionen in den Bars sind teilweise Konditional
# *****************************************

hex_geb<-read.csv("01_data/hex_geb.csv", fileEncoding = "UTF-8")
load(file = "01_data/hexbin_orig.RData")
load(file = "01_data/ZHshape.RData")
hexbin_zh <- hexbin_orig
t_gemeinde<-read.csv("01_data/t_gemeinde.csv", fileEncoding = "UTF-8")

#Auswahllisten erstellen----

#Auswahlliste Lageklasse, Räume, Baujahr, Typ, Regionale Ebene
lage <- list("irrelevant" = 9, "unknown" = 0,"very moderate" =1, "moderate" =2, "intermediate" =3, "decent" =4, "good" = 5, "very good" = 6 )
rooms <- list("irrelevant" = -1, "unknown" = 0, "1 room" = 1, "2 rooms" = 2, "3 rooms" = 3, "4 rooms" = 4, "5 rooms" = 5, "6 rooms" = 6, "7+ rooms" = 7) 
con_year <-list("all" = -1, "before 1900" = 0, "after 1900" = 1, "unknown" = 9) 
property <- list("Detached House" = "EFH", "condominium" = "STW", "Land" = "Land")
reg_level <- list("Canton of Zurich" = 'kanton', "Planning region" = 'region', "District" = 'bezirk', "Municipality" = 'bfs_akt')

#Auswahlliste für Gemeinden
t_gem <- t_gemeinde %>% dplyr::select(gde_name, gde_bfsnr) %>% unique() %>% arrange(gde_name)
choices = data.frame(
  var = t_gem$gde_name,
  num = t_gem$gde_bfsnr)
gde_list <- as.list(choices$num)
names(gde_list) <- choices$var

#Auswahlliste für Regionen
t_reg <- t_gemeinde %>% dplyr::select(region, raumplanreg_code) %>% unique() %>% arrange(region)
choices = data.frame(
  var = t_reg$region,
  num = t_reg$raumplanreg_code)
reg_list <- as.list(choices$num)
names(reg_list) <- choices$var

#Auswahlliste für Bezirke
t_bez <- t_gemeinde %>% dplyr::select(bezirk, bezirk_code) %>% unique() %>% arrange(bezirk)
choices = data.frame(
  var = t_bez$bezirk,
  num = t_bez$bezirk_code)
bez_list <- as.list(choices$num)
names(bez_list) <- choices$var
rm(t_bez, t_reg, choices)

#letzte Datenarbeiten ----
hex_geb <- hex_geb %>%
  mutate(lage=ifelse(is.na(lage),0,lage),
         n_zimmer=ifelse(n_zimmer>7,7,
                         ifelse(is.na(n_zimmer),0,n_zimmer)),
         seedist=ifelse(seedist>=5000, 5000, seedist)) %>% 
  left_join(., t_gemeinde %>% dplyr::select(gde_bfsnr, region, bezirk, gde_name), by=c("bfs_akt"="gde_bfsnr")) %>% 
  dplyr::rename(year=s_j_erhjahr, price=n_preis)


# UI ----
ui <- function(request) {
  fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$title("Zurich RealEstateApp"),
    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
  ),
  
  titlePanel(HTML(' <h1><img id="logo" src="logo.png" alt="logo" height="75">Zurich RealEstateApp</h1>'))
  
  ,
  # Layout SidebarPanel----
  sidebarLayout(
    sidebarPanel(

      shinyjs::useShinyjs(),
      id = "side-panel",
      uiOutput('resetable_input'),
      actionButton("reset_input", "Reset inputs"), 
      bookmarkButton(label="Bookmark this APP state"),

      
      tags$hr(),
      
      HTML('<h2>Land Characteristics</h2>'),
      splitLayout(
        selectInput("lage_1", label = HTML('<h5>Location Quality (min)</h5>'),
                    choices = lage, selected = 1, width='150px'),
        selectInput("lage_2",label = HTML('<h5>Location Quality (max)</h5>'),
                    choices = lage, selected = 6, width='150px')
      ),
      sliderInput("distZH", label = HTML('<h5>Distance to Zurich city centre (in km)</h5>'),
                  min = 0, max = 35, step = 1, value = c(0,35), sep = "'", ticks=F),
      sliderInput("distSee", label = HTML('<h5>Distance to Lake Zurich (in m)</h5>'),
                  min = 0, max = 5000, step = 100, value = c(0,5000),
                  sep = "'", ticks=F),
      conditionalPanel(condition = "input.typ != 'STW'",
                       sliderInput("parzf", label = HTML('<h5>Land area (in sqm)</h5>'),
                                   min = 0, max = 5000, step = 25, value = c(50,5000),
                                   sep = "'", ticks=F)),
      conditionalPanel(condition ="input.typ != 'Land'",
                       tags$hr(),
                       HTML('<h2>House or Apartment Characteristics</h2>'),
                       splitLayout(
                         selectInput("zimmer_1", label = HTML('<h5>Number of rooms (min)</h5>'),
                                     choices = rooms , 
                                     selected = 1, width='150px'),
                         selectInput("zimmer_2",label = HTML('<h5>Number of Rooms (max)</h5>'),
                                     choices = rooms , 
                                     selected = 7, width='150px')),
                       sliderInput("wohnfl", label = HTML('<h5>Living space in sqm</h5>'),
                                   min = 0, max = 500, value = c(0,500), sep = "", ticks=F),
                       splitLayout(cellWidths = c("35%", "65%"),
                                   selectInput("after1900", label = HTML('<h5>Construction year</h5>'),
                                               choices = con_year, selected = 1),
                                   conditionalPanel(condition = "input.after1900 == 1",
                                                    sliderInput("baujahr", label = HTML('<div id="myEmptyConstructionYearDiv"></div>'), min = 1900, max = 2017, value = c(1900, 2017),sep = "", ticks=F))
                       )
      ), width=3),
    # Layout MainPanel----    
    mainPanel(
      shinyjs::useShinyjs(),
      id = "main-panel",
      fluidRow(class = "mainPanelTopBar",
               column(width = 3,
                      sliderInput("jahr", label = HTML('<h5>Year of sale</h5>'),
                                  min = 2007, max = 2017, value = c(2007, 2017),
                                  sep = "", ticks=F)),
               column(width = 2,
                      selectInput("typ", label = HTML('<h5>Type of Property</h5>'),
                                  choices = property, selected = "EFH", width='150px')
               ),
               column(width = 2,
                      selectInput("Raum", label = HTML('<h5>Regional level</h5>'),
                                  choices = reg_level, selected = 0, width='150px')
               ),
               column(width = 3,
                      conditionalPanel(condition = "input.Raum == 'region'",
                                       selectInput('reg_code', label = HTML('<h5>Planning region</h5>'), choices = reg_list, multiple = TRUE, selected = 102, width='500px')),
                      conditionalPanel(condition = "input.Raum == 'bezirk'",
                                       selectInput('bez_code', label = HTML('<h5>District</h5>'), choices = bez_list, multiple = TRUE, selected = 110, width='500px')),
                      conditionalPanel(condition = "input.Raum == 'bfs_akt'",
                                       selectInput("gem_code", label = HTML('<h5>Municipality</h5>'),
                                                   choices= gde_list, multiple = TRUE, selected = 261, width='500px'))),
               column(width = 2)
      ),
      fluidRow(),
      # Layout ContentWindow ----      
      fluidRow(
        tabsetPanel(type = "tabs",
                    tabPanel(HTML('<h4>Data<h4>'),
                             h3(textOutput("datatab", container = span)),
                             tableOutput("datatable"),
                             htmlOutput("text1"),
                             downloadButton("downloadData", "Download"),
                             style = "width:50%"),
                    tabPanel(HTML('<h4>Map</h4>'),
                             h3(textOutput("maptab", container = span)),
                             leafletOutput("map", height='600px'),
                             htmlOutput("text2"),
                             #downloadButton('downloadMap', 'Download Map'),
                             style = "width:75%"),
                    tabPanel(HTML('<h4>Graphics</h4>'),
                             h3(textOutput("plottab", container = span)),
                             plotOutput("plot1", height='300px'),
                             plotOutput("plot2", height='300px'),
                             htmlOutput("text3"),
                             downloadButton("downloadGraph1", "Download Prices"),
                             downloadButton("downloadGraph2", "Download Sales"),
                             style = "width:75%"),
                    tabPanel(HTML('<h4>Igraphics</h4>'),
                             #h3(textOutput("plottab", container = span)),
                             plotlyOutput("plotly1", height='300px'),
                             plotlyOutput("plotly2", height='300px'),
                             #htmlOutput("text3"),
                             style = "width:75%"),
                    tabPanel(HTML('<h4>Explanations</h4>'),
                             h3(textOutput("exptab", container = span)),
                             htmlOutput("explan"),
                             style = "width:75%")
        )
      )
    )))
}

# Server.R----
# ******************

server <- function(input, output, session) {
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::reset("main-panel")
  })
  
  # Datensätze erstellen ----
  # ********************************************
  # Auswertungsdatensatz erstellen ----
  data_1 <- reactive({
    hex_geb  %>%
      mutate(kanton=0) %>% 
      dplyr::filter(typ==input$typ) %>%
      dplyr::filter(as.numeric(as.character(year))>=input$jahr[1] & as.numeric(as.character(year))<=input$jahr[2]) %>%
      dplyr::filter(if(input$Raum == "region") (raumplanreg_code %in% c(input$reg_code)) else TRUE) %>% 
      dplyr::filter(if(input$Raum == "bezirk") (bezirk_code %in% c(input$bez_code)) else TRUE) %>% 
      dplyr::filter(if(input$Raum == "bfs_akt") (bfs_akt %in% c(input$gem_code)) else TRUE) 
  })
  
  data_2 <- reactive({
    data_1() %>%
      dplyr::filter(lage >= input$lage_1 & lage <= input$lage_2) %>%
      dplyr::filter(seedist>=input$distSee[1] & seedist<=input$distSee[2]) %>%
      dplyr::filter(distparade>=input$distZH[1] & distparade<=input$distZH[2]) %>%
      dplyr::filter(if(input$typ != 'Land' & input$after1900 == 1) (as.numeric(as.character(gbauj))>=input$baujahr[1] & 
                                                                      as.numeric(as.character(gbauj))<=input$baujahr[2]) else 
                                                                        if(input$typ != 'Land' & input$after1900 == 0) (as.numeric(as.character(gbauj))< 1900) else 
                                                                          if(input$typ != 'Land' & input$after1900 ==9) (is.na(as.numeric(as.character(gbauj)))==T) else TRUE) %>%
      dplyr::filter(if(input$typ != 'Land') (n_zimmer>=input$zimmer_1 & n_zimmer<=input$zimmer_2) else TRUE)%>%
      dplyr::filter(if(input$typ == 'EFH') (as.numeric(as.character(n_qmwhg))>=input$wohnfl[1] & 
                                              as.numeric(as.character(n_qmwhg))<=input$wohnfl[2]) else TRUE)%>%
      dplyr::filter(if(input$typ != 'STW') (as.numeric(as.character(n_qmareal))>=input$parzf[1] & 
                                              as.numeric(as.character(n_qmareal))<=input$parzf[2])  else TRUE)
  })
  

  data <-reactive({
    if(nrow(data_2())<=3){
      showModal(modalDialog(
        title = "Too many restrictions",
        "You've made too many restrictions. Since the number of sales in your selection is less than 4, all object-specific filters are ignored. Please adjust your selection accordingly.",
        easyClose = TRUE,
        footer = modalButton("Dismiss"),
        size = c("s")
      ))
      data_1()
    }else{
      data_2()
    }
  })

  

  
  # Basistabelle ----
  TabSum_1 <- reactive({
    dcc6(data(),.variables = c("year", input$Raum), .funs_list = list(
      n_obs= ~n(),
      mean_price= ~mean(price, na.rm = T),
      sd_price = ~sd(price, na.rm = T),
      q25_price = ~quantile(price, probs=0.25, na.rm = T),
      med_price = ~median(price, na.rm = T),
      q75_price = ~quantile(price, probs=0.75, na.rm = T)),
      .total = "TOTAL") %>%
      mutate_at(vars(mean_price, sd_price, q25_price, med_price, q75_price),funs(ifelse(n_obs<=3,NA,.))) %>%
      mutate_at(vars(sd_price, q25_price, q75_price),funs(ifelse(n_obs<=5,NA,.))) %>% 
      mutate_at(.vars = vars(mean_price, sd_price, q25_price, med_price, q75_price),funs(
        if(input$typ != "Land"){round(as.numeric(.)/1000, 0)*1000}else{as.numeric(.)}))
  })
  
  TabSum1 <- reactive({
    TabSum_1() %>% 
      mutate_at(vars(mean_price, sd_price, q25_price, med_price, q75_price, n_obs),funs(formatC(round(as.numeric(.),0),format="f", digits=0, big.mark="'")))
  })
  

  
  # Zusatztabelle (wird fuer Datentabelle benoetigt) ----
  TabSum <- reactive({
    subset(TabSum1(),TabSum1()[[2]]=="TOTAL") %>% 
      dplyr::select(Year=year, 'Number of Observations'=n_obs, 'Price (Mean)' = mean_price, 'Price (sd)' = sd_price, 'Price (Q1)' = q25_price, 'Price (Median)' = med_price, 'Price (Q3)' = q75_price)
  })
  


    TabSum_dl <- reactive({
    subset(TabSum_1(),TabSum1()[[2]]=="TOTAL") %>% 
      dplyr::select(Year=year, 'Number of Observations'=n_obs, 'Price (Mean)' = mean_price, 'Price (sd)' = sd_price, 'Price (Q1)' = q25_price, 'Price (Median)' = med_price, 'Price (Q3)' = q75_price) 
  })
  
  # Data for the map----
  data_map <- reactive({
    data()  %>%
      group_by(id_grid) %>%
      summarise(anz = n(),
                med_price=median(price)) %>%
      mutate(med_price = ifelse(anz<3, NA, med_price)) %>% 
      arrange(id_grid) %>%
      ungroup()
  })
  
  # colors for the map
  blured <- c(zhpal$zhblue, rev(zhpal$zhred))[c(2,4,6,8,10,12)]
  
  # Breaks for the map
  breaks.df <- hex_geb %>% 
    group_by(id_grid, typ) %>% 
    summarize(med_preis=median(price))
  brekks <- reactive({
    pretty_breaks(6)(if(input$typ == 'EFH'){subset(breaks.df, typ=="EFH")%$%med_preis} 
                     else if(input$typ == 'STW'){subset(breaks.df, typ=="STW")%$%med_preis}
                     else {subset(breaks.df, typ=="Land")%$%med_preis})
  })
  
  # Datensatz für die Grafiken erstellen ----
  graph_tab <- reactive({
    my_sum(data(), price, year, input$Raum)
  })
  
  #Rendern der einzelnen Tabs----
  
  # Tab 1: Daten Tabelle----
  output$datatab <- renderText(if(input$typ=="Land"){"Number of sales and prices in CHF/sqm"}else{"Number of sales and prices in CHF"})
  output$datatable <- renderTable({TabSum()},  hover = TRUE, spacing = 'xs',  
                                  align = 'lrrrrrr',  
                                  digits = 0, na = '-')
  
  

  
  
  output$downloadData <- downloadHandler(
    filename = function() {"zhre_data.csv"
    },
    content = function(file) {
      write.csv(TabSum_dl(), file, row.names = FALSE)
    }
  )
  
  # Tab 2: Karte ----
  anzeige <- "Price in CHF (Median)"
  
  output$maptab <- renderText({"Map of the Canton of Zurich"})
  output$map <- renderLeaflet({
    hexbin_zh@data <- left_join(hexbin_zh@data, data_map(), by = c("id_grid" = "id_grid"))
    mapdat <- if(input$typ=="EFH"){subset(hexbin_zh, is.na(hexbin_zh@data$EFH1) & hexbin_zh@data$anz>0)} else 
      if (input$typ=="STW"){hexbin_zh <- subset(hexbin_zh, is.na(hexbin_zh@data$MFH1) & hexbin_zh@data$anz>0)}else
      {hexbin_zh <- subset(hexbin_zh, hexbin_zh@data$anz>0)}
    mapdat <- spTransform(mapdat, CRS("+init=epsg:4326"))
    pal <- colorBin(blured, domain = mapdat@data$med_price, bins = brekks())
    
    labels <- sprintf(
      "<strong>%s</strong><br/>price: %s CHF <br/>sold object(s): %g ",
      mapdat@data$GEMEINDENA, formatC(round(mapdat@data$med_price,0), format="f", big.mark="'", digits=0), mapdat@data$anz) %>%
      lapply(htmltools::HTML)
    
    leaflet(ZHshape)  %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE, minZoom = 8, maxZoom = 13))%>%
      setView(lng = 8.65, lat = 47.4, zoom = 9) %>%
      addPolygons(data=mapdat,
                  fillColor = ~pal(mapdat@data$med_price),
                  weight = 1,
                  opacity = .7,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addLegend(pal = pal, values = ~mapdat@data$med_price, na.label = "NA", opacity = 0.7, title = anzeige, position = "bottomright")
  })
  
  # Tab 3 bis 5: Grafiken ----
  ttitle <- "Canton of Zurich"
  
  graph_dat  <- reactive({
    graph_tab() %>% mutate(q25_price=ifelse(is.na(q25_price), med_price, q25_price), 
                           q75_price=ifelse(is.na(q75_price), med_price, q75_price))
  })
  
  g1  <- reactive({
    ggplot(graph_dat())+
      geom_line(aes(x=year, y = med_price), size=1.3, color="#386A87") +
      geom_point(aes(x=year, y = med_price), size=3, shape=19, color="#70AED3") +
      geom_line(aes(x=year, y = q25_price), size = 1, linetype="dashed", color="#70AED3") +
      geom_line(aes(x=year, y = q75_price), size = 1, linetype="dashed", color="#70AED3") +
      ylab("Price in CHF (Median)\n ") +
      xlab ("") +
      theme_stat() +
      theme(legend.position = "none")+
      scale_x_continuous(breaks=c(as.numeric(input$jahr[1]):as.numeric(input$jahr[2])))
  })
  
  g2 <- reactive({ 
    ggplot(graph_dat(), aes(x=year, y = n_obs))+
      geom_bar(stat = "identity", width = 0.2, fill="#549FCC") +
      ylab("Number of sales\n ") +
      xlab ("") +
      theme_stat() +
      theme(legend.position = "none")+
      scale_x_continuous(breaks=c(as.numeric(input$jahr[1]):as.numeric(input$jahr[2])))
  })
  
  # Rendern der Grafiken
  output$plotly1 <- renderPlotly({
    print(ggplotly(g1()))
  })
  
  output$plotly2 <- renderPlotly({
    print(ggplotly(g2()))
  })
  
  output$plot1 <- renderPlot({
    print(g1())
  })
  
  output$plot2 <- renderPlot({
    print(g2())
  })
  
  # Download für Grafiken
  output$downloadGraph1 <- downloadHandler(
    filename = function() {"zhre_graph_price.png"
    },
    content = function(file) {
      ggsave(file,g1())
    }
  )
  
  output$downloadGraph2 <- downloadHandler(
    filename = function() {"zhre_graph_sales.png"
    },
    content = function(file) {
      ggsave(file,g2())
    }
  )
  

  # Tab 5: Erklaerung----
  output$explan <- renderUI({
    tags$div(
      HTML('<p class="caption">
           <h4>General information </h4>
           This APP is designed for internal support in the statistical office of the Canton of Zurich. It is based on the sales of single-family houses, condominiums and building land in the Canton of Zurich between 2007 and 2017. eRum 2018 has slightly modified the current version for demonstration purposes. In particular, APP was translated into English and the prices were distorted using a random variable.<br>
           
           <h4>Provisional data</h4>
           
           The data for the last three years are provisional. The reason for this is that if a property or a new building is demolished on land with non-residential or ancillary buildings, a transaction is retrospectively categorised as a sale of building land within three years of sale. <br>
           
           <h4>How to use this APP</h4>
           
           The APP calculates the number of sales as well as the statistical price information based on the filter criteria met. Therefore, please select the corresponding filter settings directly in the APP for your evaluation. All results can be downloaded and saved as csv file. The static graphics can also be downloaded. To save, please use the corresponding download button.<br>
           
           As soon as your selection is too small (< 4 sales), the APP automatically displays evaluations without the object filters. In this case, please adjust your selection filters. <br>
           
           <h4>Feedback</h4>
           Feedback on APP is very welcome at max.gruetter@gmail.com.</p>')
      )   
  })
  
  # Captions----
  output$text1 <- renderUI({ 
    tags$div(HTML('<p class="caption"></p>'))
  })
  
  output$text2 <- renderUI({ 
    tags$div(HTML('<p class="caption"></p>'))
  })
  
  output$text3 <- renderUI({ 
    tags$div(HTML('<p class="caption"></p>'))
  })
  }
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)


