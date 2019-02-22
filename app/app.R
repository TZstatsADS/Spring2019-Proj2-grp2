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
school <- read_csv("data/new_college_data.csv")
state <- sort(as.character(unique(school$STABBR)))
index_major <- read_csv("data/index_major.csv")

############ safety 
safe <- c('All', 'Fair', 'Good' , "Safe")

# major for unviersity
major.filter <- function(x){
  return(list(index_major$major[x[58:95] > 0]))
}
safemode <- function(x){
  id <- which(colnames(school) == 'CRIMERATE')
  if(x[id] < 2.1){
    return('Safe')
  } 
  else if(x[id] < 6){
    return('Good')
  }
  else{
    return('Fair')
  }
}

school$MAJOR <- apply(school, 1, major.filter)
school$SAFE <- apply(school, 1, safemode)

collegeIcon <- makeIcon(
  iconUrl = 'app/collegeicon3.png',
  iconWidth = 38, iconHeight = 40)

#### define pop up func ######
popUp <- function(){
  
}


# Define UI #################################################
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
                      bsCollapse(id="choice", open = c("Major", "Tuition", "State", "Safety"), multiple = TRUE,
                                 bsCollapsePanel("Major", style = "primary",
                                                 selectInput("major",NULL, choices = c("Multiple Choices" = "", as.character(index_major$major)),
                                                             selectize = TRUE, multiple = TRUE)),
                                 bsCollapsePanel("Tuition", style = "primary",
                                                 sliderInput("tuition", NULL, min = 20000, max = 80000, value = c(20000,80000))),
                                 bsCollapsePanel("State",style = "primary",
                                                 selectInput("state", NULL, choices = c("Multiple Choices" = "", as.character(state)),
                                                             selectize = TRUE, multiple = TRUE)),
                                 bsCollapsePanel("Safety", style = 'info',
                                                 selectInput('safety', NULL, safe, selectize=FALSE)))
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
  school.select <-  eventReactive(c(input$tuition, input$major, input$state, input$safety), {
    df <- school %>% 
      filter(TUITION >= input$tuition[1] 
             & TUITION <= input$tuition[2]
             & any(input$major %in% MAJOR[[1]][[1]])
             & STABBR %in% input$state) %>% 
      # select(NAME, LONGITUDE, LATITUDE, TUITION, RANK, SAFE) %>%
      arrange(RANK)

    
    if(input$safety != 'All'){
      df <- df %>%
        filter(SAFE == input$safety)
    }
    df
    
  },
  ignoreNULL = TRUE)
  
  # information, adaptable by filtering conditions
  output$uni <- renderUI({
    selectInput("uni","University", 
                choice = c(Choose = "", as.character(school.select()$NAME)), selectize = TRUE)
  })
  
  # change the filter conditions
  observeEvent(c(input$tuition, input$major, input$state, input$safety), {
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      # addCircleMarkers(lng = school.select()$LONGITUDE, lat = school.select()$LATITUDE,
                       # popup = as.character(school.select()$NAME), radius = 4)
      addMarkers(lng = school.select()$LONGITUDE,
                 lat = school.select()$LATITUDE,
                 icon = collegeIcon,
                 popup = paste0(
                               "<b><a href='http://",
                               as.character(school.select()$INSTURL),
                               "'>",
                               as.character(school.select()$NAME),
                               "</a></b>",
                               "<br/>",
                               "Rank: ",
                               as.character(school.select()$RANK),
                               "<br/>",
                               paste(
                                as.character(school.select()$CITY), 
                                as.character(school.select()$STABBR), 
                                as.character(school.select()$ZIP)
                                )
                 ))
    
  })
  
  observeEvent(input$uni,{
    loc <- school.select() %>% filter(NAME == input$uni) %>% select(LONGITUDE, LATITUDE)
    leafletProxy("map") %>%
      
      flyTo(as.numeric(loc[1]), as.numeric(loc[2]), zoom = 12)
  })
  
  #observe({
  #  uni.choose <- school.select() %>% filter(NAME == input$uni)
  #  leafletProxy("map") %>%
  #    setView(lng = uni.choose$LONGITUDE, lat = uni.choose$LATITUDE, zoom = 6)
  #})
}


# Run the application
shinyApp(ui = ui, server = server)

