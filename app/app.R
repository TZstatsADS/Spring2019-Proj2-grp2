# Check and install additional packages
packages.used <- c("shiny", "dplyr","leaflet")
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(dplyr)
library(leaflet)

#setwd("/Users/james/Desktop/Lectures/GR5243_Applied_Data_Science/Spring2019-Proj2-grp2/doc")
school <- read.csv("../data/college_data.csv")
state <- unique(school$STABBR)
index_major <- read.csv("../data/index_major.csv")

# major for unviersity
major.filter <- function(x){
  return(list(major[x[58:95] > 0]))
}
school$MAJOR <- apply(school, 1, major.filter)



# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
      "Smart Choice!",
      
      # the introduction
      #tabPanel(
      #  "Introduction"
      #),
      
      # the map
      tabPanel(
        "map",
        div(
          class = "outer",
          tags$head(
            includeCSS("styles.css"),
            includeScript("gomap.js")
          ),
          leafletOutput("map", width = "100%", height = "100%"),
          absolutePanel(NULL, class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = 20, right = "auto",
                        top = 60, bottom = "auto", width = 200, height = "auto", cursor = "move",
                        bsCollapse(id="choice", open = c("Major", "Tuition"), multiple = TRUE,
                                   bsCollapsePanel("Major", style = "primary",
                                                   selectInput("major",NULL, choices = index_major$major, selectize = TRUE, multiple = TRUE)),
                                   bsCollapsePanel("Tuition", style = "primary",
                                                   sliderInput("tuition", NULL, min = 20000, max = 50000, value = c(20000,50000))))
          ),
          absolutePanel(NULL, class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = "auto", right = 20,
                        top = 60, bottom = "auto", width = 200, height = "auto", cursor = "move",
                        bsCollapsePanel("Information", style = "info",
                                        uiOutput("uni1"), textOutput("uni1_info"), 
                                        uiOutput("uni2"), textOutput("uni2_info")))
          )
      ),
      
      # the comparison
      tabPanel(
        "Comparison"
      ),
      
      # about us
      tabPanel(
        "About us"
        
      )
    )
)

# Define server
server <- function(input, output){
  
  # basic map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # data filter
  school.select <-  eventReactive(c(input$tuition, input$major), {
    school %>% 
      filter(TUITION >= input$tuition[1] 
             & TUITION <= input$tuition[2]
             & any(input$major %in% MAJOR[[1]][[1]])) %>% 
      select(NAME, LONGITUDE, LATITUDE, TUITION, RANK) %>%
      arrange(RANK)
  })
  
  # information, adaptable by filtering conditions
  output$uni1 <- renderUI({
    selectInput("uni_choice1","University1", school.select()$NAME)
  })
  output$uni1_info <- renderPrint(input$uni_choice1)
  output$uni2 <- renderUI({
    selectInput("uni_choice2","University2", school.select()$NAME)
  })
  output$uni2_info <- renderPrint(input$uni_choice2)
  
  # change the filter conditions
  observe({
    leafletProxy("map", data = school.select()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = school.select()$LONGITUDE, lat = school.select()$LATITUDE, 
                 popup = as.character(school.select()$NAME), radius = 2)
  })
}


# Run the application
shinyApp(ui = ui, server = server)

