# devtools::install_github('jtrecenti/sptrans')
# install.packages("ggmap")
library(sptrans)
library(shinydashboard)
library(leaflet)
library(stringr)
library(dplyr)

draw_bus2 <- function (.data, map = NULL) {
  if (is.null(map)) {
    map <- leaflet::leaflet() %>% leaflet::addTiles()
  }
  trips <- unique(.data$trip_id)
  for (i in seq_along(trips)) {
    d <- dplyr::filter(.data, trip_id == trips[i])
    map <- map %>% leaflet::addMarkers(~px, ~py, data = d)
  }
  map
}



# UI ------------------------------#
ui <- dashboardPage(
  dashboardHeader(title = "Mapinha do SPTrans"),
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem("Banco de dados", tabName = "banco_de_dados", icon = icon("database")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map-marker"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("banco_de_dados",
              box(width = 12,
                  dataTableOutput("bd"))
      ),
      tabItem("mapa",
              box(width = 2, numericInput("n_ps", "Número de ônibus", 1, 1, 15),
                  actionButton("atualizar", "Atualizar")),
              leafletOutput("mapa_leaflet", height = 800)
      )
    )
  )
)

# Server ----------------------------#
server <- function(input, output) {
  
  dados <- reactive({
    input$atualizar
    invalidateLater(1000 * 10)
    trips <- readRDS('www/trips.rds')
    olhovivo_pat <- '233f343e2ad2a3bf483eae00c316cfdd516c3bbbd21b6a3e916645877e137b6f'
    trips %>% 
      dplyr::filter(str_detect(trip_id, '477P'), direction_id == 0) %>% 
      collect_bus(trip_id, 'trip') 
  })
  
  # Banco de dados ---------------------------------#
  output$bd <- renderDataTable({
    dados()
  })
  
  # Mapa -------------------------------------------#
  output$mapa_leaflet <- renderLeaflet({
    ps <- dados()$p %>% head(input$n_ps)
    dados() %>%
      dplyr::filter(p == ps) %>%
      draw_bus2()
  })
}

shinyApp(ui, server)