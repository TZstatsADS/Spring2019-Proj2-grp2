# Check and install additional packages
packages.used <- c("shiny", "dplyr","leaflet","shinyBS","shinyjs","ggplot2","plotly","readr","fmsb","shinythemes")
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}
options(warn = -1)


library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(leaflet)
library(ggplot2)
library(fmsb)
library(readr)
library(plotly)

# setwd("/Users/james/Desktop/Lectures/GR5243_Applied_Data_Science/Spring2019-Proj2-grp2/app")
setwd("~/Git/Spring2019-Proj2-grp2")
school <- read_csv("data/final_college_data.csv")


from_fac_to_number <- function(x){
  return(as.numeric(x))
}

for (i in 36:60){
  school[[i]] <- as.numeric(school[[i]])
}

state <- sort(unique(school$STABBR))
index_major <- read_csv("data/index_major.csv")
# index_major <- read_csv("index_major.csv")


# major for unviersity
major.filter <- function(x){
  return(list(index_major$major[x[60:97] > 0]))
}
school$MAJOR <- apply(school, 1, major.filter)



# Define UI for application that draws a histogram
ui <- fluidPage(
  # begin navbarPage
  theme = shinytheme("darkly"),
  includeCSS("app/navbar.css"),
  
  navbarPage(
    tags$strong("Smart Choice!"),
      

      # map
      tabPanel(
        "map",
        div(
          class = "outer",
          includeCSS("app/styles.css"),
          includeScript("app/gomap.js"),
          br(),
          leafletOutput("map", width = "100%", height = "100%"),
          absolutePanel(NULL, id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = 20, right = "auto",
                        top = 60, bottom = "auto", width = 200, height = "auto", cursor = "move",
                        uiOutput("reset"),
                        actionButton("reset_input","Reset")
            ),
          absolutePanel(NULL, id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = "auto", right = 20,
              top = 60, bottom = "auto", width = 200, height = "auto", cursor = "move",
              uiOutput("uni"),
              verbatimTextOutput("uni_num")
          ),
          
          useShinyjs(),
          hidden(
            div(id = "conditionalPanel",
                absolutePanel(id = "information", class = "panel panel-default", style="margin-top: 20px; margin-bottom: 0px;", fixed = TRUE,
                              draggable = F, top = 30, right=0, bottom = 0,
                              width = 400, height = "auto", cursor = "move",
                              # h2("Detailed Information"),
                              uiOutput("info1"),
                              br(),
                              plotlyOutput("radar", width = "350px")

                  )
              )
            )
          ) # end div
      ), # end map
      
      # comparison
      tabPanel(
        "Comparison",
        fluidPage(
          sidebarLayout(
            sidebarPanel(width = 2,
                         #bsCollapse(id = "Uni_Choice",
                         #           bsCollapsePanel("University", style = "primary",
                         #                           uiOutput("uni_choice"))),
                         wellPanel(
                          id = "tPanel",style = "overflow-y:scroll; height: 450px; background: '#343332'",
                          uiOutput("uni_reset", inline = TRUE),
                          div(
                              actionButton("reset_input2", "Reset")
                              )

                         ) # end wellpanel
            ),
            mainPanel(
              width = 10,
              tabsetPanel(type = "tabs",
                          tabPanel("Basic Infomation", radioButtons("basic_info", NULL,
                                                                    choices = c("Tuition",
                                                                                "UnderGraduate",
                                                                                "International",
                                                                                "Population",
                                                                                "CrimeRate",
                                                                                "AdmissionRate",
                                                                                "Earning"), inline = TRUE),
                                  
                                   plotlyOutput("basicPlot"),
                                   dataTableOutput("datatable1")),
                          tabPanel("SAT&ACT", radioButtons("satact", NULL,
                                                           choices = c("SAT",
                                                                       "ACT"), inline = TRUE),
                              
                                   plotlyOutput("satactPlot"),
                                   dataTableOutput("datatable2")))
              
              
            ),
            position = "right"
          )
        )
      ),


      # about us
      tabPanel(
        "About us",
        
        div(
          h1("What's Our Goal?"),
          p("The project aims to help student better finding their dream colleges and universities.",
            style="font-size:20px"),
          h1("Who Are We?"),
          p("We are a group of graduates students at Columbia University.",
            style="font-size:20px"),
          p("Fei Zheng, fz2277@columbia.edu",
            style="font-size:20px"),
          p("HyunBin Yoo, hy2506@columbia.edu",
            style='font-size:20px'),
          p("Tianchen Wang, tw2665@columbia.edu",
            style='font-size:20px'),
          p("Yiwei Li, yl3950@columbia.edu",
            style='font-size:20px'),
          h2("Special Thanks"),
          p("We learned a lot of inspiration and ideas from students who enrolled in the Applied Data Science(GR5243).",
            style="font-size:18px")
        )
        
        
      )
    ) # end navbarPage
) # end ui

# Define server
server <- function(input, output){
  
  # basic map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/zhengfei0908/cjsmaov8e0rvq1gqgl10ytul5/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiemhlbmdmZWkwOTA4IiwiYSI6ImNqc204YTNpMzF6bG00M3A2NGRmYmx6ZXcifQ.SyAyO42-9ko6-NxGFdpdTQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addCircles(lng = school$LONGITUDE, lat = school$LATITUDE, popup = school$NAME,radius = 10, color = "#FFFF00") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # data filter
  school.select <-  eventReactive(c(input$tuition, input$major, input$state, input$safety, input$sat), {
    #else if (input$state == ""){
    #  school %>% 
    #    filter(TUITION >= input$tuition[1] 
    #           & TUITION <= input$tuition[2]
    #           & any(input$major %in% MAJOR[[1]][[1]])) %>%
    #    arrange(RANK)
    #}
    #else {
    #  school %>% 
    #    filter(TUITION >= input$tuition[1] 
    #           & TUITION <= input$tuition[2]
    #           & any(input$major %in% MAJOR[[1]][[1]])
    #           & STABBR %in% input$state) %>%
    #    arrange(RANK)
    #}
    school.s <- school %>% filter(
      TUITION >= input$tuition[1] 
      & TUITION <= input$tuition[2]
      & SAT_AVG_ALL >= input$sat[1]
      & SAT_AVG_ALL <= input$sat[2]
    )
      
    if (!is.null(input$major)){
      school.s <- school.s %>% 
        filter(any(input$major %in% MAJOR[[1]][[1]])
               & TUITION >= input$tuition[1] 
               & TUITION <= input$tuition[2]
               & SAT_AVG_ALL >= input$sat[1]
               & SAT_AVG_ALL <= input$sat[2])
    }
    
    if (!is.null(input$state)){
      school.s <- school.s %>% 
        filter(STABBR %in% input$state
               & TUITION >= input$tuition[1] 
               & TUITION <= input$tuition[2]
               & SAT_AVG_ALL >= input$sat[1]
               & SAT_AVG_ALL <= input$sat[2])
    }

    # school.s <- school %>% 
    #     filter(TUITION >= input$tuition[1] 
    #            & TUITION <= input$tuition[2]
    #            & any(input$major %in% MAJOR[[1]][[1]])
    #            & STABBR %in% input$state) %>%
    #     arrange(RANK)
    
    if(input$safety != 'All'){
      school.s <- school.s %>%
        filter(SAFE == input$safety
               & TUITION >= input$tuition[1] 
               & TUITION <= input$tuition[2]
               & SAT_AVG_ALL >= input$sat[1]
               & SAT_AVG_ALL <= input$sat[2])
    }
    school.s
  },
  ignoreInit = TRUE)
  
  output$uni_num <- renderText({
    nrow(school.select())
  })


  # change the filter conditions
  observeEvent(c(input$major, input$tuition, input$state, input$safety, input$sat), {
    tmp <- school.select()
    if (nrow(tmp) != 0){
      leafletProxy("map") %>%
        clearShapes() %>%
        addCircles(lng = tmp$LONGITUDE, lat = tmp$LATITUDE, 
                   popup = tmp$NAME, radius = 10, color = "#FFFF00")
    }
    # else {
    #   leafletProxy("map") %>%
    #     clearShapes()
    # }
  })
  
  output$reset <- renderUI({
    count1 <- input$reset_input
    bsCollapse(id=NULL, open = c("Major", "Tuition", "State", "Safety","SAT"), multiple = TRUE,
                 bsCollapsePanel("Major",
                                   selectInput("major", NULL, choices = c(choice = "", index_major$major),
                                               selectize = TRUE, multiple = TRUE)
                                 ),
                 bsCollapsePanel("Tuition",
                                 sliderInput("tuition", NULL, min = 10000, max = 55000, value = c(10000,55000))),
                 bsCollapsePanel("State",
                                 selectInput("state", NULL, choices = c(choice = "", state),
                                             selectize = TRUE, multiple = TRUE)),
                 bsCollapsePanel("Safety",
                                 selectInput('safety', NULL, choices = c('All', 'Fair', 'Good' , "Safe"), selectize=TRUE)),
                 bsCollapsePanel("SAT",
                                 sliderInput("sat", NULL, min = 800, max = 1600, value = c(800,1600)))
               )
    
  })

  # information, adaptable by filtering conditions
  output$uni <- renderUI({
    count2 <- input$reset_input
    bsCollapse(id="uni_choice2", open = c("University(Ranked)"),
               bsCollapsePanel("University(Ranked)",
                 selectInput("uni", NULL,
                             choice = c(Choice = "", school.select()$NAME), selected = character(0))
                )
               )
    
  })
  
  observeEvent(input$uni,{
    if (input$uni == ""){
      return()
    }
    one_school <- school.select() %>% filter(NAME == input$uni)
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(one_school$LONGITUDE, one_school$LATITUDE) %>% 
      clearPopups() %>%
      addPopups(
        one_school$LONGITUDE, one_school$LATITUDE, 
        popup = paste0(
          "<b><a href='http://",
          as.character(one_school$INSTURL),
          "'>",
          as.character(one_school$NAME),
          "</a></b>",
          "<br/>",
          paste(
            as.character(one_school$CITY), 
            as.character(one_school$STABBR)
          ),
          '<br/>',
          as.character(one_school$ZIP)
        ),
        options = popupOptions(closeButton = TRUE)
      ) %>%
      flyTo(one_school$LONGITUDE, one_school$LATITUDE, zoom = 8)
    
    shinyjs::show(id = "conditionalPanel")
  })

  observeEvent(input$map_click, {

    shinyjs::hide(id = "conditionalPanel")
  })

  observeEvent(input$reset_input, {
    shinyjs::hide(id = "conditionalPanel")
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearPopups() %>%
      addCircles(lng = school$LONGITUDE, lat = school$LATITUDE, 
                 popup = school$NAME, radius = 10, color = "#FFFF00") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  
  # add radar plot
  output$radar <- renderPlotly({
    if(is.null(input$uni)){
      return()
    }
    else if(input$uni == ""){
      return()
    }
    college <- school.select() %>%
      filter(NAME == input$uni) %>%
      as.data.frame()

    data <- c((500 - college$RANK)/5, 
              college$TUITION/500,
              as.numeric(college$ADM_RATE)*100,
              college$MN_EARN_WNE_P6/900,
              as.numeric(college$SATVRMID)/8,
              as.numeric(college$SATMTMID)/8,
              as.numeric(college$SATWRMID)/8,
              as.numeric(college$SAT_AVG)/16)

    flag <- c("Rank","Admission Rate", "Tuition", "Earning", "SAT",
               "SAT English", "SAT Math", "SAT Writing" )
    plot_ly(
      type = "scatterpolar",
      r = data,
      theta = flag,
      fill = "toself",
      # area color
      fillcolor = "#ff6699",
      mode = "lines",
      line = list(
        color = "ff80aa"
      )
    ) %>%
      layout(
        polar = list(
          # set graph background color
          bgcolor = "#646868",
          radialaxis = list(
            visible = T,
            showline = T,
            range = c(0, 100),
            linewidth = 3,
            gridcolor = '#ffffff',
            tickcolor = "#ffffff",
            linecolor = "#ffffff"
          )
        ),
        font = list(
          family = 'Arial',
          size = 13,
          color = '#ffffff'
        ),
        showlegend = F,
        # bg color
        paper_bgcolor = "#303030"
      )

  })
  
  output$info1 <- renderUI({
    if(!is.null(input$uni)){
      college <- school.select() %>%
        filter(NAME == input$uni) %>%
        as.data.frame()

      head <- paste("<h2 style='font-family:Palatino' align=center>",
                    as.character(college$NAME), "</h2>")
      # college
      rank <- paste("<p style='font-size:15px'>",
                    as.character(college$RANK), "</p>")

      degree <- paste("<p style='font-size:15px'>",
                       as.character(college$HIGHDEG), "</p>")

      locale <- paste("<p style='font-size:15px'>",
                      as.character(college$LOCALE), "</p>")

      type <- paste("<p style='font-size:15px'>",
                    as.character(college$TYPE), "</p>")

      international <- paste("<p style='font-size:15px'>",
                             as.character(college$PERCENTAGEOFINTERNATIONAL), "</p>")

      undergrads <- paste("<p style='font-size:15px'>",
                          as.character(college$NUMBEROFUNDERGRAD), "</p>")

      table <- paste(
        '<table id = "customers">', "<tr>", 
        "<th>", "<strong>  Ranking </strong>", "</th>",
        "<th>", "<strong>  High Degree </strong>", "</th>",
        "</tr>",
        "<tr>",
        "<td>", rank, "</td>",
        "<td>", degree, "</td>",
        "</tr>",
        "<tr>",
        "<th>", "<strong>  Type </strong>", "</th>",
        "<th>", "<strong>  Locale </strong>", "</th>",
        "</tr>",
        "<tr>",
        "<td>", type, "</td>",
        "<td>", locale, "</td>",
        "</tr>",
        "<tr>",
        "<th>", "<strong>  International </strong>", "</th>",
        "<th>", "<strong>  Undergrads Number </strong>", "</th>",
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
  
  
  # outputOptions(output, "radar", suspendWhenHidden = FALSE)
  
  ### comparison
  output$uni_choice <- renderUI({
    checkboxGroupInput("uni_choice", tags$strong("University"), 
                choices = school.select()$NAME, 
                selected = school.select()$NAME[1:min(8,length(school.select()$NAME))])
  })
  
  
  school.select.comp <- eventReactive(input$uni_choice, {
    school.select() %>%
      filter(NAME %in% input$uni_choice)
  })
  
  output$uni_reset <- renderUI({
    uiOutput("uni_choice")
  })
  
  observeEvent(input$reset_input2,{
    output$uni_reset <- renderUI({
      uiOutput("uni_choice")
    })
  })
  
  
  output$datatable1 <- renderDataTable(options = list(pageLength = 10, autowidth = TRUE),
    {
    school.select() %>%
      select(NAME, CITY, STABBR, RANK, TUITION, 
             PERCENTAGEOFINTERNATIONAL, NUMBEROFUNDERGRAD, CRIMERATE, ADM_RATE, MN_EARN_WNE_P6)
  })

  title.font <- list(
    family = "Helvetica",
    size = 20,
    color = "F8F86E"
  )
  
  y.title.font <- list(
    family = "Helvetica",
    size = 12,
    color = "FFFFFF"
  )
  
  output$basicPlot <- renderPlotly({
    #X <- school.select.comp()$NAME
    #Tuition <- school.select.comp()$TUITION
    if (input$basic_info == "Tuition"){
      tmp <- school.select.comp()
      tmp$NAME <- factor(tmp$NAME, levels = tmp$NAME[order(tmp$TUITION, decreasing = TRUE)])
      plot_ly(tmp,
              x = ~NAME, y = ~TUITION, type = "bar", color = I("#D54B84")) %>%
        layout(title = "Tuition",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    else if(input$basic_info == "UnderGraduate"){
      tmp <- school.select.comp()
      tmp$NAME <- factor(tmp$NAME, levels = tmp$NAME[order(tmp$NUMBEROFUNDERGRAD, decreasing = TRUE)])
      plot_ly(tmp,
              x = ~NAME, y = ~NUMBEROFUNDERGRAD, type = "bar", color = I("#D54B84")) %>%
        layout(title = "Number of Undergraduate",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Number",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    
    else if(input$basic_info == "International"){
      tmp <- school.select.comp()
      tmp$NAME <- factor(tmp$NAME, levels = tmp$NAME[order(tmp$PERCENTAGEOFINTERNATIONAL, decreasing = TRUE)])
      plot_ly(tmp,
              x = ~NAME, y = ~PERCENTAGEOFINTERNATIONAL, type = "bar", color = I("#D54B84")) %>%
        layout(title = "Percentage of International Student",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Percentage",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    
    else if(input$basic_info == "Population"){
      tmp <- school.select.comp()
      tmp$NAME <- factor(tmp$NAME, levels = tmp$NAME[order(tmp$POP, decreasing = TRUE)])
      plot_ly(tmp,
              x = ~NAME, y = ~POP, type = "bar", color = I("#D54B84")) %>%
        layout(title = "Population of City",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Population",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    
    else if(input$basic_info == "CrimeRate"){
      tmp <- school.select.comp()
      tmp$NAME <- factor(tmp$NAME, levels = tmp$NAME[order(tmp$CRIMERATE, decreasing = TRUE)])
      plot_ly(tmp,
              x = ~NAME, y = ~CRIMERATE, type = "bar", color = I("#D54B84")) %>%
        layout(title = "Crime Rate",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Rate",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    
    else if(input$basic_info == "AdmissionRate"){
      tmp <- school.select.comp()
      tmp$NAME <- factor(tmp$NAME, levels = tmp$NAME[order(tmp$ADM_RATE, decreasing = TRUE)])
      plot_ly(tmp,
              x = ~NAME, y = ~ADM_RATE, type = "bar", color = I("#D54B84")) %>%
        layout(title = "Admission Rate",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Rate",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    
    else if(input$basic_info == "Earning"){
      tmp <- school.select.comp()
      tmp$NAME <- factor(tmp$NAME, levels = tmp$NAME[order(tmp$MN_EARN_WNE_P6, decreasing = TRUE)])
      plot_ly(tmp,
              x = ~NAME, y = ~MN_EARN_WNE_P6, type = "bar", color = I("#D54B84")) %>%
        layout(title = "Mean Earnings of Students 6 Years after Entry",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Earnings",
                            tickfont = y.title.font),
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    
  })
  
  output$datatable2 <- renderDataTable(options = list(pageLength = 10, autowidth = TRUE),
                                       {
                                         if (input$satact == "SAT"){
                                           school.select() %>% 
                                             select(NAME, CITY, STABBR, RANK, starts_with("SAT"))
                                         }
                                         else if (input$satact == "ACT"){
                                           school.select() %>% 
                                             select(NAME, CITY, STABBR, RANK, starts_with("ACT"))
                                         }
                                       })
  output$satactPlot <- renderPlotly({
    if (input$satact == "SAT"){
      plot_ly(school.select.comp(),
              x = ~NAME, y = ~SATVRMID, type = "bar", name = "VR") %>%
        add_trace(y = ~SATMTMID, name = "MT") %>%
        add_trace(y = ~SATWRMID, name = "WR") %>%
        layout(title = "SAT",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Grades",
                            tickfont = y.title.font),
               barmode = "group",
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
    
    else if (input$satact == "ACT"){
      plot_ly(school.select.comp(),
              x = ~NAME, y = ~ACTCMMID, type = "bar", name = "CM") %>%
        add_trace(y = ~ACTENMID, name = "EN") %>%
        add_trace(y = ~ACTMTMID, name = "MT") %>%
        add_trace(y = ~ACTWRMID, name = "WR") %>%
        layout(title = "ACT",
               titlefont = title.font,
               xaxis = list(title = "",
                            showticklabels = F),
               yaxis = list(title = "Grades",
                            tickfont = y.title.font),
               barmode = "group",
               paper_bgcolor="#222222",
               plot_bgcolor="#222222",
               margin = list(t = 50))
    }
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)

