# Check and install additional packages
packages.used <- c("shiny", "dplyr","leaflet", 'shinyBS', 'fmsb')
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(shinyBS)
library(dplyr)
library(tidyverse)
library(leaflet)
library(fmsb)
library(shinyjs)

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
    strong("Smart Choice!"),
    
    
    
    # the map
    tabPanel(
      "map",
      useShinyjs(),
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
        
        absolutePanel(NULL, class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = "auto", right = 10,
                      top = 60, bottom = "auto", width = 400, height = "auto", cursor = "move",
                      bsCollapse(id="university", open = c("University"),multiple = TRUE,
                                 bsCollapsePanel("University", style = "primary",
                                                 uiOutput("uni"))
                                 
                                ),
                      conditionalPanel(width = 400,
                                       condition = "input.uni != '' ",
                                       uiOutput("info1")
                                       
                      )

        ),
        
        hidden(
          div(id = "conditionalPanel", 
              #fluidRow(
                absolutePanel(id = "controls", class = "panel panel-default", style="overflow-y:scroll; margin-top: 20px; margin-bottom: 0px;", fixed = TRUE,
                                                    draggable = TRUE, top = 300, right=0, bottom = 0,
                                                    width = 400,
                              h2("Detailed Information"),
                              plotOutput("radar", width = "400px")

                )
              #)
              )
        )
        # hidden(
        #   div(id = "hiddenPanel",
        #       fluidRow(
        #         absolutePanel(id = "controls", class = "panel panel-default", style="overflow-y:scroll; margin-top: 20px; margin-bottom: 0px;", fixed = TRUE,
        #                       draggable = TRUE, top = 300, right=0, bottom = 0,
        #                       width = 400,
        # 
        #                       uiOutput("info1"),
        #                       plotOutput("radar", width = "400px", height = "400px")
        #                       )
        #                )
        #       )
        #     )
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

# Define server (multiple makers code...)
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
  
  # add radar plot
  output$radar <- renderPlot({
    if(is.null(input$uni)){
        return()
      }
    else if(input$uni == ""){
      return()
    }
        college <- school.select() %>% 
          filter(NAME == input$uni) %>% 
          as.data.frame()
        
        df <- data.frame(as.numeric(college$ADM_RATE)*100,
                         as.numeric(college$SATVRMID)/8,
                         as.numeric(college$SATMTMID)/8,
                         as.numeric(college$SATWRMID)/8,
                         as.numeric(college$SAT_AVG)/16, 
                         (500 - college$RANK)/5, 
                         college$TUITION/500, 
                         college$MN_EARN_WNE_P6/900)
        
        colnames(df) <- c("Admission Rate", "SAT English" , "SAT Math", "SAT Writing" ,"SAT",
                          "Rank", "Tuition", "Earning")
        df <- rbind(rep(100, 8), rep(0, 8), df)
        
        radarchart(df , axistype=1 , 
                   #custom polygon
                   pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                   #custom labels
                   vlcex=0.8
        )
        
  })
  
  output$info1 <- renderUI({
    if(!is.null(input$uni)){
      college <- school.select() %>% 
        filter(NAME == input$uni) %>% 
        as.data.frame()
      
      head <- h3(as.character(college$NAME),
         align = "center")
      # college
      rank <- paste("<p style='font-size:15px'>", 
                    "<strong>Ranking: </strong>", as.character(college$RANK), "</p>")
      
      degree <- paste("<p style='font-size:15px'>", 
                    "<strong>Degree: </strong>", as.character(college$HIGHDEG), "</p>")
      
      locale <- paste("<p style='font-size:15px'>", 
                    "<strong>Locale: </strong>", as.character(college$LOCALE), "</p>")
      
      type <- paste("<p style='font-size:15px'>", 
                      "<strong>Type: </strong>", as.character(college$TYPE), "</p>")
    
      international <- paste("<p style='font-size:15px'>", 
                   "<strong>International Student: </strong>", as.character(college$PERCENTAGEOFINTERNATIONAL), "</p>")
      
      undergrads <- paste("<p style='font-size:15px'>", 
                          "<strong>Undergrads: </strong>", as.character(college$NUMBEROFUNDERGRAD), "</p>")
      
      table <- paste(
        "<table>", "<tr>", 
        "<td>", rank, "</td>",
        "<td>", degree, "</td>",
        "</tr>",
        "<tr>",
        "<td>", type, "</td>",
        "<td>", locale, "</td>",
        "</tr>", 
        "<tr>",
        "<td>", international,"</td>",
        "<td>", undergrads,"</td>",
        "</tr>",
        "</table>"
      )
      
      HTML(paste(head, table))
    }
  })
  
  
  outputOptions(output, "radar", suspendWhenHidden = FALSE)
  
  # change the filter conditions
  observeEvent(c(input$tuition, input$major, input$state, input$safety), {
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      # generate makers 
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
                               paste(
                                as.character(school.select()$CITY), 
                                as.character(school.select()$STABBR)
                                ),
                               '<br/>',
                               as.character(school.select()$ZIP)
                 ))
    
  })
  
  observeEvent(input$uni,{
    college <- school.select()%>% filter(NAME == input$uni)
    loc <- college %>% select(LONGITUDE, LATITUDE)
    
    
    leafletProxy("map") %>%
      
      flyTo(loc[1][[1]], loc[2][[1]], zoom = 12) %>%
      
      addPopups(loc[1][[1]], loc[2][[1]],
        paste0(
          "<b><a href='http://",
          as.character(college$INSTURL),
          "'>",
          as.character(college$NAME),
          "</a></b>",
          "<br/>",
          paste(
            as.character(college$CITY), 
            as.character(college$STABBR)
          ),
          '<br/>',
          as.character(college$ZIP)
        ),
        options = popupOptions(closeButton = FALSE)
                
      )
    
    shinyjs::show(id = "conditionalPanel")
  })

  observeEvent(input$map_click, {

    shinyjs::hide(id = "conditionalPanel")
  })

}


# Run the application
shinyApp(ui = ui, server = server)

