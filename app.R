library(shiny)
library(leaflet)
library(dplyr)
library(readxl)
library(sf)
library(RCzechia)
library(spdep)
library(shinydashboard)
library(shinycssloaders)
library(tidyr)
library(bslib)
library(paletteer)
library(memoise)
library(rmapshaper)
library(rsconnect)


vysledky_komplet <- read_xlsx("Vysledky_posl_snemovna.xlsx")
vysledky_komplet_k <- read_xlsx("Vysledky_posl_snemovna_kandidati.xlsx")
vysledky_komplet_obce <- read_xlsx("Vysledky_obce.xlsx")

obce <- ms_simplify(RCzechia::obce_polygony(), keep = 0.05)

data2 <- vysledky_komplet_k %>%
  left_join(vysledky_komplet, by = c("OBEC_PREZ" = "OBEC_PREZ", "ROK" = "ROK"))

geo_data <- obce %>%
  left_join(data2, by = c("KOD_OBEC" = "OBEC_PREZ"))

strany_data <- obce %>%
  left_join(vysledky_komplet_k, by = c("KOD_OBEC" = "OBEC_PREZ"))

vysledky_komplet_select <- vysledky_komplet %>%
  select("OBEC_PREZ", "Volebni_ucast", "ROK")

vysledky_komplet_wide <- vysledky_komplet_select  %>%
  pivot_wider(names_from = ROK, values_from = Volebni_ucast)

cz_shapes_data <- obce %>%
  left_join(vysledky_komplet_wide, by = c("KOD_OBEC" = "OBEC_PREZ"))

vysledky_komplet_wide_obce <- vysledky_komplet_obce  %>%
  select("KODZASTUP", "Volebni_ucast", "ROK")   %>%
  pivot_wider(names_from = ROK, values_from = Volebni_ucast)%>%
  mutate(KODZASTUP = as.character(KODZASTUP))

vysledky_komplet_wide_obce_prep <- obce %>%
  left_join(vysledky_komplet_wide_obce, by = c("KOD_OBEC" = "KODZASTUP"))

calculate_lisa <- memoise(function(year) {
  # Načtení polygonů obcí z balíčku RCzechia a spojení s volebními daty
  obce_sf <- obce %>%
    left_join(vysledky_komplet %>% mutate(OBEC_PREZ = as.character(OBEC_PREZ)),
              by = c("KOD_OBEC" = "OBEC_PREZ")) %>%
    select(KOD_OBEC, NAZ_OBEC, ROK, Volebni_ucast, geometry) %>% # Výběr relevantních sloupců
    filter(ROK == year) %>% # Filtrování podle zadaného roku
    st_transform(crs = 5514) # Transformace souřadnicového systému na S-JTSK (EPSG:5514)
  
  # Výpočet souřadnic centroidů obcí
  coords <- st_coordinates(st_centroid(obce_sf))  
  
  # Určení sousedských vztahů na základě nejbližších 5 sousedů
  neigh <- knn2nb(knearneigh(coords, k = 5))  
  
  # Váhová matice sousedství s rovnoměrnými vahami
  listw <- nb2listw(neigh, style = "W")  
  
  # Local Moran's I
  lisa <- localmoran(obce_sf$Volebni_ucast, listw)
  
  # Uložení hodnot Local Moran's I a příslušných p-hodnot
  obce_sf$lisa_i <- lisa[, "Ii"]  
  obce_sf$lisa_p <- lisa[, "Pr(z != E(Ii))"]  
  
  # Kategorizace obcí podle výsledků Local Moran's I
  obce_sf$lisa_cat <- case_when(
    obce_sf$lisa_i > 0 & obce_sf$lisa_p < 0.05 ~ "Positive autocorrelation", # Kladné hodnoty s významností
    obce_sf$lisa_i > 0 & obce_sf$lisa_p >= 0.05 ~ "Positive - Not Significant", # Kladné hodnoty, ale nevýznamné
    obce_sf$lisa_i < 0 & obce_sf$lisa_p >= 0.05 ~ "Negative - Not Significant", # Záporné hodnoty, ale nevýznamné
    obce_sf$lisa_i < 0 & obce_sf$lisa_p < 0.05 ~ "Negative autocorrelation", # Záporné hodnoty s významností
    TRUE ~ "Not Significant" # Ostatní případy
  )
  # Převod souřadnicového systému zpět na WGS84 (EPSG:4326)
  obce_sf <- st_transform(obce_sf, crs = 4326)
  
  return(obce_sf)
})


calculate_lisa_obce <- memoise(function(year_lisa_obce) {
  # Načtení polygonů obcí z balíčku RCzechia a spojení s volebními daty
  obce_sf <- obce %>%
    left_join(vysledky_komplet_obce %>% mutate(KODZASTUP = as.character(KODZASTUP)),
              by = c("KOD_OBEC" = "KODZASTUP")) %>%
    select(KOD_OBEC, NAZEVZAST, ROK, Volebni_ucast, geometry) %>%
    filter(ROK == as.numeric(year_lisa_obce)) %>%
    st_transform(crs = 5514)
  obce_sf <- obce_sf[!is.na(obce_sf$NAZEVZAST), ] 
  obce_sf <- obce_sf[!is.na(obce_sf$Volebni_ucast), ]
  # Výpočet souřadnic centroidů obcí
  coords <- st_coordinates(st_centroid(obce_sf))  
  # Určení sousedských vztahů na základě nejbližších 5 sousedů
  neigh <- knn2nb(knearneigh(coords, k = 5))  
  listw <- nb2listw(neigh, style = "W")  
  # Local Moran's I
  lisa <- localmoran(obce_sf$Volebni_ucast, listw)
  # Uložení hodnot Local Moran's I a příslušných p-hodnot
  obce_sf$lisa_i <- lisa[, "Ii"]  
  obce_sf$lisa_p <- lisa[, "Pr(z != E(Ii))"]  
  # Kategorizace obcí podle výsledků Local Moran's I
  obce_sf$lisa_cat <- case_when(
    obce_sf$lisa_i > 0 & obce_sf$lisa_p < 0.05 ~ "Positive autocorrelation", # Kladné hodnoty s významností
    obce_sf$lisa_i > 0 & obce_sf$lisa_p >= 0.05 ~ "Positive - Not Significant", # Kladné hodnoty, ale nevýznamné
    obce_sf$lisa_i < 0 & obce_sf$lisa_p >= 0.05 ~ "Negative - Not Significant", # Záporné hodnoty, ale nevýznamné
    obce_sf$lisa_i < 0 & obce_sf$lisa_p < 0.05 ~ "Negative autocorrelation", # záporné hodnoty s významností
    TRUE ~ "Not Significant"
  )
  obce_sf <- st_transform(obce_sf, crs = 4326)
  obce_sf
})


#vytvoření rozložení dashboardu
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapy", tabName = "mapy", icon = icon("map")),
      menuItem("Lokální Moran I", tabName = "lisa", icon = icon("chart-area")),
      menuItem("Lokální Moran I visibilita", tabName = "lisa_visibilita", icon = icon("chart-area")),
      menuItem("Strany", tabName = "strany", icon = icon("chart-area")),
      menuItem("Moran strany", tabName = "mapy_strany", icon = icon("chart-area")),
      menuItem("Rozdíly volební účast", tabName = "map_turnout_diff", icon = icon("chart-area")),
      menuItem("Volební účast regrese", tabName = "map_turnout_diff_reg", icon = icon("chart-area")),
      menuItem("Mapy obce", tabName = "mapy_obce", icon = icon("map")),
      menuItem("Lokální Moran I obce", tabName = "lisa_obce", icon = icon("chart-area")),
      menuItem("Lokální Moran I obce visibilita", tabName = "lisa_obce_visibilita", icon = icon("chart-area")),
      menuItem("Rozdíly volební účast obce", tabName = "map_turnout_diff_obce", icon = icon("chart-area")),
      menuItem("Volební účast regrese obce", tabName = "map_turnout_diff_reg_obce", icon = icon("chart-area"))
    )
  ),
    dashboardBody(
    tags$head(
      tags$style(HTML("
      h2 {
        font-family: Arial, sans-serif;
        font-size: 24px;
        font-weight: bold;
        color: #333;
        margin-top: 2px;
      }
      body {
          font-family: Arial, sans-serif;
      }
      .selectize-control {
        z-index: 9999 !important;
      }
    "))
    ),
    fluidPage(
      tabItems(
        tabItem(tabName = "mapy",
                titlePanel("Volební účast ve volbách do Poslanecké sněmovny"),
                selectInput("year", "Rok:", choices = unique(vysledky_komplet$ROK), selected = unique(vysledky_komplet$ROK)[1]),
                withSpinner(leafletOutput("map_actual", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "lisa",
                titlePanel("LISA Analýza"),
                selectInput("year_lisa", "Rok:", choices = unique(vysledky_komplet$ROK), selected = unique(vysledky_komplet$ROK)[1]),
                withSpinner(leafletOutput("map_lisa", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "lisa_visibilita",
                titlePanel("LISA Analýza a volební účast"),
                selectInput("year_lisa_visibilita", "Rok:", choices = unique(vysledky_komplet$ROK), selected = unique(vysledky_komplet$ROK)[1]),
                withSpinner(leafletOutput("map_lisa_visibilita", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "strany",
                titlePanel("Podpora jednotlivých stran ve volbách do Poslanecké sněmovny"),
                selectInput("rok_strany", "Vyberte rok:", choices = unique(na.omit(strany_data$ROK))),
                selectInput("strana_strany", "Vyberte stranu:", choices = NULL),
                withSpinner(leafletOutput("strany", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "mapy_strany",
                titlePanel("LISA Analýza podpory jednotlivých stran ve volbách do Poslanecké sněmovny"),
                selectInput("rok", "Vyberte rok:", choices = unique(na.omit(geo_data$ROK))),
                selectInput("strana", "Vyberte stranu:", choices = NULL),
                withSpinner(leafletOutput("mapa_strany", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "map_turnout_diff",
                titlePanel("Rozdíly volební účasti ve volbách do Poslanecké sněmovny"),
                selectInput("year1", "Vybraný rok 1:", choices = colnames(vysledky_komplet_wide)[-1], selected = (colnames(vysledky_komplet_wide)[-1])[1]),
                selectInput("year2", "Vybraný rok 2:", choices = colnames(vysledky_komplet_wide)[-1], selected = (colnames(vysledky_komplet_wide)[-1])[-1]),
                withSpinner(leafletOutput("map_turnout_diff", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "map_turnout_diff_reg",
                titlePanel("Změny volební účasti ve volbách do Poslanecké sněmovny pomocí regrese"),
                withSpinner(leafletOutput("map_turnout_diff_reg", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "mapy_obce",
                titlePanel("Volební účast ve volbách do zastupitelstev obcí"),
                selectInput("year_obce", "Rok:", choices = unique(vysledky_komplet_obce$ROK), selected = unique(vysledky_komplet_obce$ROK)[1]),
                withSpinner(leafletOutput("map_actual_obce", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "lisa_obce",
                titlePanel("LISA Analýza volební účasti ve volbách do zastupitestev obcí"),
                selectInput("year_lisa_obce", "Rok:", choices = unique(vysledky_komplet_obce$ROK), selected = unique(vysledky_komplet_obce$ROK)[1]),
                withSpinner(leafletOutput("map_lisa_obce", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "lisa_obce_visibilita",
                titlePanel("LISA Analýza volební účasti ve volbách do zastupitestev obcí"),
                selectInput("year_lisa_obce_visibilita", "Rok:", choices = unique(vysledky_komplet_obce$ROK), selected = unique(vysledky_komplet_obce$ROK)[1]),
                withSpinner(leafletOutput("map_lisa_obce_visibilita", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "map_turnout_diff_obce",
                titlePanel("Rozdíly volební účasti do zastupitelstev obcí"),
                selectInput("year1_obce", "Vybraný rok 1:", choices = unique(vysledky_komplet_obce$ROK), selected = "2002"),
                selectInput("year2_obce", "Vybraný rok 2:", choices = unique(vysledky_komplet_obce$ROK), selected = "2022"),
                withSpinner(leafletOutput("map_turnout_diff_obce", height = "90vh"), type = 6)
        ),
        tabItem(tabName = "map_turnout_diff_reg_obce",
                titlePanel("Změny volební účasti ve volbách do zastupitelstev obcí pomocí regrese"),
                withSpinner(leafletOutput("map_turnout_diff_reg_obce", height = "90vh"), type = 6)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #vytvoření mapy s volební účastí v obcích 
  output$map_actual <- renderLeaflet({
    req(input$year)
    obce <- obce %>%
      left_join(vysledky_komplet %>% mutate(OBEC_PREZ = as.character(OBEC_PREZ)),
                by = c("KOD_OBEC" = "OBEC_PREZ")) %>%
      filter(ROK == input$year)
    
    if (nrow(obce) == 0) {
      return(NULL)
    }
    
    pal <- leaflet::colorBin(palette = "plasma", domain = obce$Volebni_ucast, bins = 7)
    
    leaflet(data = obce) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::addPolygons(fillColor = ~pal(obce$Volebni_ucast),
                           stroke = NA,
                           fillOpacity = 2/3,
                           label = ~sprintf("%s: %s%%", NAZ_OBEC, round(Volebni_ucast, 2)),
                           labelOptions = leaflet::labelOptions(direction = "auto")) %>%
      leaflet::addLegend(pal = pal,
                         values = obce$Volebni_ucast,
                         title = "Volební účast (%)",
                         position = "topright",
                         labFormat = leaflet::labelFormat(suffix = "%"))
  })
  
  #vytvoření mapy s výsledky lokálního Moranova I
  output$map_lisa <- renderLeaflet({
    req(input$year_lisa)
    obce_sf <- calculate_lisa(input$year_lisa)
    
    pal <- colorFactor(palette = c("firebrick3", "slateblue4", "indianred1", "slateblue1", "gray"),
                       levels = c("Positive autocorrelation", "Negative autocorrelation", "Positive - Not Significant", "Negative - Not Significant", "Not Significant"))
    
    leaflet(data = obce_sf) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(lisa_cat),
                  fillOpacity = 0.7,
                  color = NA,
                  weight = 0.5,
                  popup = ~paste("Obec:", NAZ_OBEC,
                                 "<br>Moran's I:", round(lisa_i, 3),
                                 "<br>P-hodnota:", round(lisa_p, 3),
                                 "<br>Kategorie:", lisa_cat)) %>%
      addLegend("topright",
                pal = pal,
                values = ~lisa_cat,
                title = "LISA Kategorie",
                opacity = 1)
  })
  
  #vytvoření mapy s výsledky lokálního Moranova I a volební účastí
  output$map_lisa_visibilita <- renderLeaflet({
    req(input$year_lisa_visibilita)
    obce_sf <- calculate_lisa(input$year_lisa_visibilita)
    
    # Definování barevné palety pro volební účast
    pal <- colorNumeric(palette = "YlGnBu", domain = obce_sf$Volebni_ucast)
    
    # Zajištění správných hodnot pro LISA kategorie
    unique_lisa <- unique(obce_sf$lisa_cat)
    message("LISA categories: ", paste(unique_lisa, collapse = ", "))
    
    # Viditelnost podle LISA kategorií s výchozí hodnotou
    obce_sf$visibility <- ifelse(obce_sf$lisa_cat %in% c("Positive autocorrelation", "Negative autocorrelation"), 1,
                                 ifelse(!is.na(obce_sf$lisa_cat), 0.3, 0.1))
    
    leaflet(data = obce_sf) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(Volebni_ucast),
                  fillOpacity = ~visibility,
                  color = NA,
                  weight = 0.5,
                  popup = ~paste("Obec:", NAZ_OBEC,
                                 "<br>Volební účast:", round(Volebni_ucast, 1), "%",
                                 "<br>Kategorie LISA:", lisa_cat)) %>%
      addLegend("topright",
                pal = pal,
                values = ~Volebni_ucast,
                title = "Volební účast (%)",
                opacity = 1,
                labFormat = leaflet::labelFormat(suffix = "%"))
  })
  
  #vytvoření mapy volební účasti ve volbách do zastupitelstev obcí
  output$map_actual_obce <- renderLeaflet({
    req(input$year_obce)
    
    obce_data <- obce %>%
      left_join(vysledky_komplet_obce %>% mutate(KODZASTUP = as.character(KODZASTUP)),
                by = c("KOD_OBEC" = "KODZASTUP")) %>%
      filter(ROK == input$year_obce)
    
    if (nrow(obce_data) == 0) {
      return(NULL)  
    }
    
    pal <- leaflet::colorBin(palette = "plasma", domain = obce_data$Volebni_ucast, bins = 7)
    
    leaflet(data = obce_data) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::addPolygons(fillColor = ~pal(obce_data$Volebni_ucast),
                           stroke = NA,
                           fillOpacity = 2/3,
                           label = ~sprintf("%s: %s%%", NAZ_OBEC, round(Volebni_ucast, 2)),
                           labelOptions = leaflet::labelOptions(direction = "auto")) %>%
      leaflet::addLegend(pal = pal,
                         values = obce_data$Volebni_ucast,
                         title = "Volební účast (%)",
                         position = "topright",
                         labFormat = leaflet::labelFormat(suffix = "%"))
  })
  
  #vytvoření mapy s výsledky lokálního Moranova I
  output$map_lisa_obce <- renderLeaflet({
    req(input$year_lisa_obce)
    obce_sf <- calculate_lisa_obce(input$year_lisa_obce)
    
    pal <- colorFactor(palette = c("firebrick3", "slateblue4", "indianred1", "slateblue1", "gray"),
                       levels = c("Positive autocorrelation", "Negative autocorrelation", "Positive - Not Significant", "Negative - Not Significant", "Not Significant"))
    
    leaflet(data = obce_sf) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(lisa_cat),
                  fillOpacity = 0.7,
                  color = NA,
                  weight = 0.5,
                  popup = ~paste("Obec:", NAZEVZAST,
                                 "<br>Moran's I:", round(lisa_i, 3),
                                 "<br>P-hodnota:", round(lisa_p, 3),
                                 "<br>Kategorie:", lisa_cat)) %>%
      addLegend("topright",
                pal = pal,
                values = ~lisa_cat,
                title = "LISA Kategorie",
                opacity = 1)
    
    
  })
  
  #vytvoření mapy s výsledky lokálního Moranova I a volební účastí
  output$map_lisa_obce_visibilita <- renderLeaflet({
    req(input$year_lisa_obce_visibilita)
    obce_sf <- calculate_lisa_obce(input$year_lisa_obce_visibilita)
    
    pal <- colorNumeric(palette = "YlGnBu", domain = obce_sf$Volebni_ucast)
    
    unique_lisa <- unique(obce_sf$lisa_cat)
    
    obce_sf$visibility <- ifelse(obce_sf$lisa_cat %in% c("Positive autocorrelation", "negative autocorrelation"), 1,
                                 ifelse(!is.na(obce_sf$lisa_cat), 0.3, 0.1))
    
    leaflet(data = obce_sf) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(Volebni_ucast),
                  fillOpacity = ~visibility,
                  color = NA,
                  weight = 0.5,
                  popup = ~paste("Obec:", NAZEVZAST,
                                 "<br>Volební účast:", round(Volebni_ucast, 1), "%",
                                 "<br>Kategorie LISA:", lisa_cat)) %>%
      addLegend("topright",
                pal = pal,
                values = ~Volebni_ucast,
                title = "Volební účast (%)",
                opacity = 1,
                labFormat = leaflet::labelFormat(suffix = "%"))
    
    
  })
  
  observeEvent(input$rok, {
    # Filtruje dostupné strany na základě vybraného roku a počtu mandátů
    dostupne_strany <- geo_data %>%
      filter(ROK == input$rok, POCMANDCR > 0) %>%
      pull(NAZEVCELK) %>%
      unique()
    
    # Pokud nejsou dostupné žádné strany, nastaví výchozí hodnotu
    if (length(dostupne_strany) == 0) {
      dostupne_strany <- "Žádné dostupné strany"  
    }
    
    # Aktualizuje vstupní select box s dostupnými stranami
    updateSelectInput(session, "strana", choices = dostupne_strany)
  })
  
  filtered_data <- reactive({
    # Kontroluje, zda jsou dostupné požadované vstupy
    req(input$rok, input$strana)
    
    # Filtruje data na základě vybraného roku a strany
    data <- geo_data %>%
      filter(ROK == input$rok, NAZEVCELK == input$strana, POCMANDCR > 0)
    
    # Validuje, zda existují data pro zvolené parametry
    validate(
      need(nrow(data) > 0, "Pro zvolený rok a stranu nejsou dostupná žádná data.")
    )
    
    data
  })
  
  moran_data <- reactive({
    # Převádí filtrovaná data na sf objekt pro prostorovou analýzu
    data_sf <- filtered_data() %>% st_as_sf()  
    
    # Vypočítá souřadnice centroidů pro prostorovou analýzu
    coords <- st_coordinates(st_centroid(data_sf))  
    
    # Definuje prostorové sousedství na základě nejbližších sousedů
    neigh <- knn2nb(knearneigh(coords, k = 5))  
    
    # Vytvoří váhovou matici pro výpočty Moranova indexu
    listw <- nb2listw(neigh, style = "W")  
    
    # Vypočítá lokální Moranův index prostorové autokorelace
    moran <- localmoran(data_sf$Procento_hlasu, listw)
    
    # Přidává výsledky výpočtu do datasetu
    data_sf$LocalMoranI <- moran[, 1]  
    data_sf$PValue <- moran[, 5]    
    data_sf
  })
  
  observeEvent(input$rok_strany, {
    # Filtruje dostupné strany podle zvoleného roku
    dostupne_strany <- strany_data %>%
      filter(ROK == input$rok_strany, POCMANDCR > 0) %>%
      pull(NAZEVCELK) %>%
      unique()
    
    # Pokud nejsou dostupné žádné strany, nastaví výchozí hodnotu
    if (length(dostupne_strany) == 0) {
      dostupne_strany <- "Žádné dostupné strany"  
    }
    
    # Aktualizuje vstupní select box pro strany
    updateSelectInput(session, "strana_strany", choices = dostupne_strany)
  })
  
  filtered_data_strany <- reactive({
    # Kontroluje, zda jsou dostupné požadované vstupy
    req(input$rok_strany, input$strana_strany)
    
    # Filtruje data na základě vybraného roku a strany
    data <- strany_data %>%
      filter(ROK == input$rok_strany, NAZEVCELK == input$strana_strany, POCMANDCR > 0)
    
    # Validuje, zda existují data pro zvolené parametry
    validate(
      need(nrow(data) > 0, "Pro zvolený rok a stranu nejsou dostupná žádná data.")
    )
    
    data
  })
  
  output$strany <- renderLeaflet({
    # Zajišťuje, že vstupní hodnoty nejsou prázdné
    req(input$rok_strany, input$strana_strany)
    
    # Získává filtrovaná data pro vykreslení mapy
    obce <- filtered_data_strany()
    
    # Pokud nejsou dostupná žádná data, nevrací nic
    if (nrow(obce) == 0) {
      return(NULL)
    }
    
    # Vytváří barevnou škálu na základě kvantilů hodnot procenta hlasů
    pal <- leaflet::colorQuantile(palette = "plasma", domain = obce$Procento_hlasu, n = 10)
    
    # Vykresluje interaktivní mapu s polygonovými oblastmi
    leaflet(data = obce) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::addPolygons(fillColor = ~pal(Procento_hlasu),
                           stroke = NA,
                           fillOpacity = 2/3,
                           label = ~sprintf("%s: %s%%", NAZ_OBEC, round(Procento_hlasu, 2)),
                           labelOptions = leaflet::labelOptions(direction = "auto")) %>%
      leaflet::addLegend(pal = pal,
                         values = obce$Procento_hlasu,
                         title = "Procento hlasů",
                         position = "topright",
                         labFormat = leaflet::labelFormat(suffix = "%"))
  })
  
  #mapa shluků podpory politických stran 
  output$mapa_strany <- renderLeaflet({
    req(input$rok, input$strana)
    
    
    data_sf <- moran_data()
    
    data_sf <- data_sf %>%
      mutate(
        lisa_cat = case_when(
          LocalMoranI > 0 & PValue < 0.05 ~ "Positive autocorrelation",
          LocalMoranI < 0 & PValue < 0.05 ~ "Negative autocorrelation",
          LocalMoranI > 0 & PValue >= 0.05 ~ "Positive - Not Significant",
          LocalMoranI < 0 & PValue >= 0.05 ~ "Negative - Not Significant",
          TRUE ~ "Not Significant"
        )
      )
    
    pal <- colorFactor(
      palette = c("firebrick3", "slateblue4", "indianred1", "slateblue1", "gray"),
      levels = c("Positive autocorrelation", "Negative autocorrelation", "Positive - Not Significant", "Negative - Not Significant", "Not Significant")
    )
    
    leaflet(data = data_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(lisa_cat),
        fillOpacity = 0.7,
        color = NA,
        weight = 0.5,
        popup = ~paste(
          "<strong>Obec:</strong>", NAZ_OBEC, "<br>",
          "<strong>Moran's I:</strong>", round(LocalMoranI, 3), "<br>",
          "<strong>P-hodnota:</strong>", round(PValue, 5), "<br>",
          "<strong>Kategorie:</strong>", lisa_cat
        )
      ) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = ~lisa_cat,
        title = "LISA Kategorie",
        opacity = 1
      )
  })
  
  diff_data <- reactive({
    year1 <- input$year1
    year2 <- input$year2
    
    cz_shapes_data %>%
      mutate(difference = ifelse(is.na(.[[year2]]) | is.na(.[[year1]]), 0, .[[year2]] - .[[year1]]))
  })
  #mapa rozdílů mezi jednotlivými roky
  output$map_turnout_diff <- renderLeaflet({
    data <- diff_data()
    
    cassatt_palette <- as.character(paletteer_d("MetBrewer::Cassatt2"))
    
    # Vytvoření palety 
    color_pal <- colorNumeric(
      palette = cassatt_palette,
      domain = data$difference
    )
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~color_pal(difference),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~paste("Obec:", NAZ_OBEC, "<br>",
                       "Rozdíl:", round(difference, 2), "%"),
        highlightOptions = highlightOptions(weight = 5, color = "white", fillOpacity = 0.7)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = color_pal,
        values = data$difference,
        title = "Rozdíl ve Volební účasti (%)",
        labFormat = leaflet::labelFormat(suffix = "%")
      )
  })
  
  #mapa sklonu regresní přímky 
  output$map_turnout_diff_reg <- renderLeaflet({
    vysledky_long <- vysledky_komplet %>%
      select("OBEC_PREZ", "ROK", "Volebni_ucast")
    
    # Výpočet sklonu ve volební účasti pro každou obec
    celkove_rozdily <- vysledky_long %>%
      group_by(OBEC_PREZ) %>%
      summarise(
        slope = lm(Volebni_ucast ~ ROK)$coefficients["ROK"], # Sklon regresní přímky
        #celkovy_rozdil = slope #* (max(ROK) - min(ROK))    
      )
    
    # Načtení polygonů obcí
    obce <- RCzechia::obce_polygony()
    
    # Převod kódů obcí na znakový formát pro správné spojení dat
    celkove_rozdily <- celkove_rozdily %>%
      mutate(OBEC_PREZ = as.character(OBEC_PREZ))
    
    obce <- obce %>%
      mutate(KOD_OBEC = as.character(KOD_OBEC))  
    
    # Spojení dat o volební účasti s geografickými daty obcí
    obce_data <- obce %>%
      left_join(celkove_rozdily, by = c("KOD_OBEC" = "OBEC_PREZ"))
    
    # Stanovení mezí pro normalizaci hodnot celkového rozdílu
    lower_bound <- quantile(obce_data$slope, 0.05, na.rm = TRUE)
    upper_bound <- quantile(obce_data$slope, 0.95, na.rm = TRUE)
    
    # Ořezání hodnot na 5. a 95. percentil
    obce_data$rozdil <- pmin(pmax(obce_data$slope, lower_bound), upper_bound)
    
    # Normalizace rozdílů na škálu 0-1
    obce_data$scaled_rozdil <- (obce_data$rozdil - min(obce_data$rozdil, na.rm = TRUE)) /
      (max(obce_data$rozdil, na.rm = TRUE) - min(obce_data$rozdil, na.rm = TRUE))
    
    # Definice barevné palety pro vizualizaci
    pal <- colorNumeric("RdYlGn", domain = obce_data$scaled_rozdil)
    
    # Vytvoření Leaflet mapy
    leaflet(obce_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(scaled_rozdil),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        popup = ~paste("Obec: ", NAZ_OBEC,  
                       "<br>Sklon: ", round(slope, 2),
                       "<br>Normalizovaná data: ", round(obce_data$scaled_rozdil, 2)
        ),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = obce_data$scaled_rozdil,
        title = "Sklon",
        position = "bottomright"
      )
  })
  
  #mapa rozdílů volební účasti mezi jednotlivými roky 
  diff_data_obce <- reactive({
    # Získání hodnot z uživatelského vstupu
    year1 <- input$year1_obce
    year2 <- input$year2_obce
    
    
    # Výpočet rozdílu mezi dvěma roky
    vysledky_komplet_wide_obce_prep %>%
      mutate(difference = ifelse(is.na(.[[year2]]) | is.na(.[[year1]]), 0, .[[year2]] - .[[year1]]))
  })
  
  output$map_turnout_diff_obce <- renderLeaflet({
    data <- diff_data_obce()
    
    cassatt_palette <- as.character(paletteer_d("MetBrewer::Cassatt2"))
    
    # Vytvoření palety 
    color_pal <- colorNumeric(
      palette = cassatt_palette,
      domain = data$difference
    )
    
    # Vytvoření Leaflet mapy
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%  
      addPolygons(
        fillColor = ~color_pal(difference),
        color = "black",
        weight = 1,
        fillOpacity = 0.9,
        popup = ~paste("Obec:", NAZ_OBEC, "<br>",
                       "Rozdíl:", round(difference, 2), "%"),
        highlightOptions = highlightOptions(weight = 5, color = "white", fillOpacity = 0.9)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = color_pal,
        values = data$difference,
        title = "Rozdíl ve Volební účasti (%)",
        labFormat = leaflet::labelFormat(suffix = "%")
      )
  })
  
  #mpa sklonu regresní přímky
  output$map_turnout_diff_reg_obce <- renderLeaflet({
    vysledky_long_obce <- vysledky_komplet_obce %>%
      select("KODZASTUP", "ROK", "Volebni_ucast")
    
    # Výpočet sklonu ve volební účasti pro každou obec
    celkove_rozdily <- vysledky_long_obce %>%
      group_by(KODZASTUP) %>%
      summarise(
        slope = lm(Volebni_ucast ~ ROK)$coefficients["ROK"], # Sklon regresní přímky
        #celkovy_rozdil = slope #* (max(ROK) - min(ROK))    
      )
    
    # Načtení polygonů obcí
    obce <- RCzechia::obce_polygony()
    
    # Převod kódů obcí na znakový formát pro správné spojení dat
    celkove_rozdily <- celkove_rozdily %>%
      mutate(KODZASTUP = as.character(KODZASTUP))
    
    obce <- obce %>%
      mutate(KOD_OBEC = as.character(KOD_OBEC))  
    
    # Spojení dat o volební účasti s geografickými daty obcí
    obce_data <- obce %>%
      left_join(celkove_rozdily, by = c("KOD_OBEC" = "KODZASTUP"))
    
    # Stanovení mezí pro normalizaci hodnot celkového rozdílu
    lower_bound <- quantile(obce_data$slope, 0.05, na.rm = TRUE)
    upper_bound <- quantile(obce_data$slope, 0.95, na.rm = TRUE)
    
    # Ořezání hodnot na 5. a 95. percentil
    obce_data$rozdil <- pmin(pmax(obce_data$slope, lower_bound), upper_bound)
    
    # Normalizace rozdílů na škálu 0-1
    obce_data$scaled_rozdil <- (obce_data$rozdil - min(obce_data$rozdil, na.rm = TRUE)) /
      (max(obce_data$rozdil, na.rm = TRUE) - min(obce_data$rozdil, na.rm = TRUE))
    
    # Definice barevné palety pro vizualizaci
    pal <- colorNumeric("RdYlGn", domain = obce_data$scaled_rozdil)
    
    # Vytvoření Leaflet mapy
    leaflet(obce_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(scaled_rozdil),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        popup = ~paste("Obec: ", NAZ_OBEC,  
                       "<br>Sklon: ", round(slope, 2),
                       "<br>Normalizovaná data: ", round(obce_data$scaled_rozdil, 2)
        ),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = obce_data$scaled_rozdil,
        title = "Sklon",
        position = "bottomright"
      )
  })
  
}

shinyApp(ui = ui, server = server)

