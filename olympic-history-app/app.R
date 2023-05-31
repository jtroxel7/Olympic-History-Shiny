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
        titlePanel("Body Size vs. Olympic Sport"),
        mainPanel(
          p("PUT GRAPHIC HERE")
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
          p()
        )
      )
    )
  )
)

server <- function(input, output) {
  # Server logic goes here
}

shinyApp(ui, server)
