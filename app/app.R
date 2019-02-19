# Check and install additional packages
packages.used <- c("shiny", "dplyr","leaflet")
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(shinyBS)
library(dplyr)
library(leaflet)

#setwd("/Users/james/Desktop/Lectures/GR5243_Applied_Data_Science/Spring2019-Proj2-grp2/doc")
school <- read.csv("../data/college_data.csv")
state <- unique(school$STABBR)
index_major <- read.csv("../data/index_major.csv")

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
        "Comparison",
        wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                  tags$hr(style="border-color: #6088B1;"),
                  h1("Side-by-Side Two School Comparison",align= "center",style = "color: #333333; font-family: Times; 
                     font-size:30pt"),
                  tags$hr(style="border-color: #6088B1;"),
                  
                  fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                      uiOutput("ui.1"),
                                                      uiOutput("ui.2")
                  )
                  ),
                  br(),br(),
                  fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                      imageOutput("logo1",height = "400", width = "400"),imageOutput("logo2",height = "400", width = "400")
                  )),
                  
                  
                  br(),
                  
                  
                  
                  # ==== Title in Orange
                  fluidRow(align="center",
                           style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                           column(6,offset=3,
                                  br(),hr(style="color:#808080"),
                                  helpText( strong("Basic Information" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                  hr(style="color:#808080")
                           )),
                  
                  # === Some text to explain the Figure:
                  
                  br(),
                  # === display instnm
                  fluidRow(align = "center",splitLayout(cellWidths = c("50%","50%"),
                                                        fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm1")),
                                                        fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm2")))
                  ),br(),
                  # === display city
                  fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                         fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city1")),
                                                         fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city2")))
                  ),br(),
                  
                  # === display clevel
                  fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                         fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel1")),
                                                         fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel2")))
                  ),br(),
                  # === display control
                  
                  fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control1")),
                                                       fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control2")))
                  ),br(),
                  # === display highest degree
                  
                  fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg1")),
                                                       fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg2")))
                  ),br(),
                  # === display locale
                  
                  fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale1")),
                                                       fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale2")))
                  ),br(),
                  # === display admission rate
                  
                  fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate1")),
                                                       fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate2")))
                  ),br(),
                  # === display in-state tuition
                  
                  fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                       fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in1")),
                                                       fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in2")))
                  ),br(),
                  # === display out-of-state tuition
                  
                  fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                       fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out1")),
                                                       fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out2")))
                  ),br(),
                  # === display percentage of federal loans
                  
                  fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                                                      fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                               textOutput("pctfloan1")),
                                                      fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                               textOutput("pctfloan2")))
                  ),br(),
                  # === display total Undergraduates Seeking Degrees
                  
                  fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                                                      fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                               textOutput("ugds1")),
                                                      fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                               textOutput("ugds2")))
                  ),
                  br(),
                  br(),
                  fluidRow(align="center",
                           style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                           column(6,offset=3,
                                  br(),hr(style="color:#808080"),
                                  helpText( strong("Calculate Median Debt by Family Income" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                  hr(style="color:#808080")
                           )),
                  br(),
                  
                  # === display sliderInput for family income
                  fluidRow(align="center",sliderInput("fincome","Family Income: ",
                                                      min=0,max=200000,value=0,width = 600)
                           
                  ),br(),
                  # === display median debt based on family income input 
                  fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                       fluidRow(column(width=7,offset=1,textOutput("school1")),
                                                                strong(column(width=7,offset = 1,"Median Debt based on Family Income : ")),br(),
                                                                textOutput("debt1")),
                                                       fluidRow(column(width=7,offset=1,textOutput("school2")),
                                                                strong(column(width=7,offset=1,"Median Debt based on Family Income: ")),br(),
                                                                textOutput("debt2")))
                  ),
                  br(),
                  br(),
                  fluidRow(align="center",
                           style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                           column(6,offset=3,
                                  br(),hr(style="color:#808080"),
                                  helpText( strong("SAT & ACT Scores" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                  helpText( strong("25th-75th Percentile" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )),
                                  hr(style="color:#808080")
                           )),
                  br(),
                  
                  fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                      textOutput("school1.2"),
                                                      textOutput("school2.2"))
                           
                  ),
                  
                  fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                      tableOutput("sat1"),
                                                      tableOutput("sat2"))
                           
                  ),br(),
                  
                  fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                      tableOutput("act1"),
                                                      tableOutput("act2"))
                           
                  ),
                  br(),
                  br(),
                  fluidRow(align="center",
                           style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                           column(6,offset=3,
                                  br(),
                                  hr(style="color:#808080"),
                                  helpText( strong("Demographics of Students" , style="color: #6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                  hr(style="color:#808080")
                           )),
                  br(),
                  # === Bar with corresponding widget
                  fluidRow(align="center",column(4,h2("Major Diversity",
                                                      style="color:#4C4C4C ; font-family: Times"),
                                                 tags$hr(style="border-color: #6088B1;")),br()),
                  fluidRow(align="center",
                           splitLayout(cellWidths = c("50%","50%"),
                                       plotlyOutput("my_barplot1" , height = "500px"),
                                       plotlyOutput("my_barplot2" , height = "500px")
                           )
                           
                  ),br(),br(),
                  # === pie chart of ethnicity
                  fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Ethnicity",
                                                      style="color:#4C4C4C ; font-family: Times"),
                                                 tags$hr(style="border-color: #6088B1;")),br()),
                  fluidRow(align="center",
                           splitLayout(cellWidths = c("50%","50%"),
                                       plotlyOutput("demographics1",height="550"),
                                       plotlyOutput("demographics2",height="550"))
                           
                  ),br(),
                  fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Gender",
                                                      style="color:#4C4C4C ; font-family: Times"),
                                                 tags$hr(style="border-color: #6088B1;")),br()),
                  fluidRow(align="center",
                           splitLayout(cellWidths = c("50%","50%"),
                                       plotlyOutput("female1",height="450"),
                                       plotlyOutput("female2",height="450")
                                       
                           ))
                  ############################################TEAM 2 IMPLEMENTATION ENDS############################################################
                  )
      ),#Comparison ends here),
      
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

