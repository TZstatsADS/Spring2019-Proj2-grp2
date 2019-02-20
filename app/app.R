# Check and install additional packages
packages.used <- c("shiny", "dplyr","leaflet", 'shinyBS')
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(shinyBS)
library(dplyr)
library(leaflet)

setwd('/Users/tianchenwang/Git/Spring2019-Proj2-grp2')
school <- read.csv("data/new_college_data.csv")
state <- sort(as.character(unique(school$STABBR)))
index_major <- read.csv("data/index_major.csv")

# major for unviersity
major.filter <- function(x){
  return(list(index_major$major[x[58:95] > 0]))
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
          
          # path change!!!!!
          
          includeCSS("app/styles.css"),
          includeScript("app/gomap.js")
        ),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(NULL, class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = 20, right = "auto",
                      top = 60, bottom = "auto", width = 200, height = "auto", cursor = "move",
                      bsCollapse(id="choice", open = c("Major", "Tuition", "State"), multiple = TRUE,
                                 bsCollapsePanel("Major", style = "primary",
                                                 selectInput("major",NULL, choices = c("Multiple Choices" = "", as.character(index_major$major)),
                                                             selectize = TRUE, multiple = TRUE)),
                                 bsCollapsePanel("Tuition", style = "primary",
                                                 sliderInput("tuition", NULL, min = 20000, max = 50000, value = c(20000,50000))),
                                 bsCollapsePanel("State",style = "primary",
                                                 selectInput("state", NULL, choices = c("Multiple Choices" = "", as.character(state)),
                                                             selectize = TRUE, multiple = TRUE)))
        ),
        absolutePanel(NULL, class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = "auto", right = 20,
                      top = 60, bottom = "auto", width = 200, height = "auto", cursor = "move",
                      bsCollapse(id="university", open = c("University"),multiple = TRUE,
                                 bsCollapsePanel("University", style = "primary",
                                                 uiOutput("uni"))
                                 
                      )
        )
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
  school.select <-  eventReactive(c(input$tuition, input$major, input$state), {
    school %>% 
      filter(TUITION >= input$tuition[1] 
             & TUITION <= input$tuition[2]
             & any(input$major %in% MAJOR[[1]][[1]])
             & STABBR %in% input$state) %>% 
      select(NAME, LONGITUDE, LATITUDE, TUITION, RANK) %>%
      arrange(RANK)
  },
  ignoreNULL = TRUE)
  
  # information, adaptable by filtering conditions
  output$uni <- renderUI({
    selectInput("uni","University", 
                choice = c(Choose = "", as.character(school.select()$NAME)), selectize = TRUE)
  })
  
  # change the filter conditions
  observeEvent(c(input$tuition, input$major, input$state), {
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(lng = school.select()$LONGITUDE, lat = school.select()$LATITUDE, 
                       popup = as.character(school.select()$NAME), radius = 4)
    
  })
  
  observeEvent(input$uni,{
    loc <- school.select() %>% filter(NAME == input$uni) %>% select(LONGITUDE, LATITUDE)
    leafletProxy("map") %>%
      
      flyTo(as.numeric(loc[1]), as.numeric(loc[2]), zoom = 8)
  })
  
  #observe({
  #  uni.choose <- school.select() %>% filter(NAME == input$uni)
  #  leafletProxy("map") %>%
  #    setView(lng = uni.choose$LONGITUDE, lat = uni.choose$LATITUDE, zoom = 6)
  #})
}


# Run the application
shinyApp(ui = ui, server = server)

