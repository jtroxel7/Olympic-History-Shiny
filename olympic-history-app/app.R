library(shiny)
library(tidyverse)
library(janitor)
library(shinydashboard)


# read data
data <- read_delim('data/olympics.txt', delim=',') %>% 
  clean_names()

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Custom CSS for app width */
      body, html {
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
      }

      .shiny-app {
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: auto;
      }
      
      .container {
        max-width: none !important;
      }
      /* Custom CSS for header styling */
      .header {
        background-color: #ababab;
        padding: 10px;
        color: #FFFFFF;
      }
      .title {
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 5px;
      }
      .subtitle {
        font-size: 14px;
        margin-bottom: 10px;
      }
    "))
  ),
  
  mainPanel(
    tags$div(
      class = "header",
      tags$div(
        class = "title",
        "A History of the Olympic Games"
      ),
      tags$div(
        class = "subtitle",
        "By Charlie Lovett and Jack Troxel"
      )
    ),
    
    tabsetPanel(
      id = "tabs",
      type = "pills",
      tabPanel(
        title = "Intro",
        titlePanel("Introduction to the Olympic Games"),
        mainPanel(
          br(),
          p("The Olympic Games, the world's most prestigious sporting event, 
            traces its origins back to ancient Greece. Held every four years, 
            athletes from around the globe gather to compete in a celebration of 
            athleticism, camaraderie, and cultural exchange. The Games showcase 
            a wide array of sports, ranging from track and field to swimming, 
            gymnastics, and team sports. Steeped in tradition, the Olympics 
            unite nations and transcend boundaries, fostering friendship and 
            understanding among diverse cultures. With a history spanning over 
            a century, the Olympic Games represent the pinnacle of sporting 
            achievement and the pursuit of excellence."),
          p("This Shiny App explores some of the data behind the history of the
            Olympic Games. The mix of static and interactive graphics found by 
            navigating through the tabs above offer an insight into the trends
            behind the Olympics."),
          p("The data used for these visualizations was pulledfrom Kaggle 
            and describes data for each Olympian from the Athens 
            games in 1896 to the Rio games in 2016. For each athlete, the 
            dataset shows information such as height, weight, age, nationality, 
            and more. The dataset can be found",
            a("here.",
               href = "https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results")),
          img(src = "sky_rings.jpg", height = 375, width = 500,
              style="display: block; margin-left: auto; margin-right: auto;"),
          br(),
          br(),
          br()
        )
      ),
      tabPanel(
        title = "World Map",
        titlePanel("Map of Past Olympic Locations"),
        mainPanel(
          p("PUT GRAPHIC HERE")
        )
      ),
      tabPanel(
        title = "Medals by Country",
        titlePanel("Medal Counts by Nation"),
        mainPanel(
          p("PUT GRAPHIC HERE")
        )
      ),
      tabPanel(
        title = "Body Size vs. Sport",
        titlePanel("Distribution of Body Size by Sport"),
        sidebarLayout(position = "right",
            sidebarPanel(
              selectInput("sport", 
                          label = "Select Sport",
                          choices = c("All Sports","Alpine Skiing",               
                                      "Archery","Art Competitions","Athletics",                
                                      "Badminton","Baseball","Basketball",               
                                      "Beach Volleyball","Biathlon",                 
                                      "Bobsleigh","Boxing","Canoeing" ,                
                                      "Cross Country Skiing" ,    
                                      "Curling","Cycling","Diving",                   
                                      "Equestrianism","Fencing","Figure Skating",          
                                      "Football","Freestyle Skiing","Golf",                     
                                      "Gymnastics","Handball","Hockey",                   
                                      "Ice Hockey","Jeu De Paume","Judo",                     
                                      "Lacrosse","Luge",     
                                      "Modern Pentathlon","Nordic Combined",          
                                      "Polo","Rhythmic Gymnastics",      
                                      "Rowing","Rugby",                    
                                      "Rugby Sevens","Sailing","Shooting",                 
                                      "Short Track Speed Skating","Skeleton","Ski Jumping",              
                                      "Snowboarding","Softball","Speed Skating",            
                                      "Swimming","Synchronized Swimming","Table Tennis",           
                                      "Taekwondo","Tennis","Trampolining",             
                                      "Triathlon","Tug-Of-War","Volleyball",               
                                      "Water Polo","Weightlifting","Wrestling"),
                          selected = "All Sports"),
              radioButtons("button",
                           "Select Attribute:",
                           choices = c("Height", 
                                       "Weight"),
                           selected = "Height"),
              p(HTML("<small><em>Note: Not all Olympic sports are played by both sexes.</em></small>"))
              ),
            
            mainPanel(
              plotOutput(outputId = "height_weight_plot")
            )
        )
      ),
      tabPanel(
        title = "Additional Info",
        titlePanel("Additional Information / Work Split"),
        mainPanel(
          h4("Additional Information on Graphics"),
          p("What is the core concept(s) or insight(s) into the data that you 
            believe the visualization communicates? If you choose to include 
            animation then explain why animation helps. If you build an app 
            then explain your choice of widgets and what about the data it 
            help users understand."),
          br(),
          h4("Partner Work Split"),
          p("We decided to work on this project so that we could
            create a more informative dashboard than either of us would've been
            able to achieve on our own. In general, we both built of of eachother's 
            strengths and worked together on most aspects of the project. This
            allowed us to explore in depth aspects such as CSS styling for the 
            dashboard. In terms of a specific work split, Charlie was
            focused on the two map graphics (both static and with widgets) while
            Jack focused on visualizing the distributions of body sizes by sport.
            We both believe that the project that we were able to complete is
            significantly more in depth and informational than we would have
            been able to achieve as individuals.")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Server logic goes here
  
  # create dist plot for heights and weights by sport
  output$height_weight_plot <- renderPlot({
    
    if (input$sport == "All Sports") {
      sport_data <- data
    } else if (input$sport != "") {
      sport_data <- subset(data, sport %in% input$sport)
    } else {
      sport_data <- data
    }
    
    x <- case_when(
      input$button == "Height" ~pull(sport_data, height),
      input$button == "Weight" ~pull(sport_data, weight)
    )
    
    x_lab <- case_when(
      input$button == "Height" ~"Height in Centimeters",
      input$button == "Weight" ~"Weight in Kilograms"
    )
    
    x_lim <- case_when(
      input$button == "Height" ~c(125,225),
      input$button == "Weight" ~c(35,135)
    )
    
    subtitle <- case_when(
      input$button == "Height" ~"Height Distribution",
      input$button == "Weight" ~"Weight Distribution"
    )
    
    if (input$sport == "All Sports") {
      title <- "All Olympic Sports"
    } else if (input$sport != "") {
      title <- paste("Olympic", input$sport)
    } else {
      title <- "All Olympic Sports"
    }
    
    
    ggplot(data = sport_data, 
           aes(x = x, 
               color = sex, 
               fill = sex)) +
      geom_density(alpha = 0.25) +
      theme_minimal() +
      scale_x_continuous(
        name = x_lab,
        limits = x_lim
      ) +
      scale_y_continuous(
        name = NULL,
        labels = NULL,
        expand = c(0, 0)
      ) +
      scale_color_manual(
        values = c("#89cffa", "#f7a1c8"),
        labels = c("Male", "Female")
      ) +
      scale_fill_manual(
        values = c("#89cffa", "#f7a1c8"),
        labels = c("Male", "Female")
      ) +
      labs(
        title = title,
        subtitle = subtitle
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.9), 
        legend.justification = c(1, 1)
      )
  })
}

shinyApp(ui, server)
