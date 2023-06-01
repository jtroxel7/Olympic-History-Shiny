library(shiny)
library(tidyverse)
library(janitor)
library(shinydashboard)


# data preparation
olympics_data <- read_delim('data/olympics.txt', delim=',') %>% 
  clean_names()

city_data <- read_csv('data/cities.csv')
city_data$Season <- ifelse(is.na(city_data$Summer), 'Winter', 'Summer')

world_map <- map_data("world")
world_map <- subset(world_map, !(region == "Antarctica"))

regions_data <- read_csv('data/regions.csv') %>% 
  clean_names()

medal_counts <- olympics_data %>%
  group_by(noc, medal) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = medal, values_from = Count, values_fill = 0)
medal_counts <- merge(medal_counts, regions_data, by = "noc", all.x = TRUE)
medal_counts <- medal_counts %>%
  filter(region %in% world_map$region)

merged_data <- left_join(world_map, medal_counts, by = c("region" = "region"))


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
      .content-wrapper .content-header {
          margin-bottom: 0;
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
          plotOutput(outputId = "world_map", width = 780, height = 390)
        )
      ),
      tabPanel(
        title = "Medals by Country",
        titlePanel("Medal Counts by Nation"),
        sidebarLayout(position = "right",
            sidebarPanel(
              radioButtons("medal_button",
                           "Select Medal:",
                           choices = c("Gold", 
                                       "Silver",
                                       "Bronze"),
                           selected = "Gold")),
        mainPanel(
          plotOutput(outputId = "medal_map")
        ))
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
          p("This application provides users with a brief yet descriptive
            and informative exploration of the history of The Olympic Games.
            The static map graphic displays the locations of every Winter or
            Summer Olympics and identifies a few large cities that have hosted
            at least once. The interactive map allows users to understand the
            distribution of medals won by every competing country in the games.
            It is evident that there are a few countries that stand out as ones
            that have one many more medals than others. These include the United
            States, Russia, China, Brazil, and Canada. The seemingly obvious
            interpretation of this is that larger countries win more medals. This
            is because larger populations allow for more potential to have
            talented athletes. Finally, the density plot clearly displays the
            distributions of height and weight for men and women in each event
            in the Olympic Games."),
          br(),
          p("This dataset is extremely large: it contains every single unique
            athlete-event pair in the entire history of the Olympic Games.
            Our application condenses this dataset into a few easily
            understandable graphics that allow the user to dive deeper into the
            meaning of the data. Our widget options allow users to analyze
            similarities and differences in trends and to make conclusions of
            their own."),
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
  # world map rendering
  output$world_map <- renderPlot({
    # creating the world map plot
    ggplot() +
      geom_polygon(data = world_map,
                   aes(x = long, y = lat, group = group),
                   color = 'grey60',
                   fill = 'white') +
      geom_point(data = city_data,
                 aes(x = Longitude, y = Latitude, color = Season),
                 size = 2) +
      annotate(geom = "text",
               x = -118.2436829 - 5,
               y = 34.05223,
               label = 'Los Angeles',
               hjust = "right",
               size = 4,
               color = 'black') +
      annotate(geom = "text",
               x = -43.2075005 + 5,
               y = -22.90278,
               label = 'Rio de\nJaneiro',
               hjust = "left",
               size = 4,
               color = 'black') +
      annotate(geom = "text",
               x = 151.2073212 + 5,
               y = -33.86785,
               label = 'Sydney',
               hjust = "left",
               size = 4,
               color = 'black') +
      annotate(geom = "text",
               x = 139.6916809 + 5,
               y = 35.68953,
               label = 'Tokyo',
               hjust = "left",
               size = 4,
               color = 'black') +
      annotate(geom = "text",
               x = 37.6155548 + 3,
               y = 55.75222 + 4,
               label = 'Moscow',
               hjust = "left",
               size = 4,
               color = 'black') +
      scale_color_manual(labels = c('Summer', 'Winter'),
                         values = c('red', 'blue')) +
      coord_equal(xlim = c(-180, 180), ylim = c(-60, 90), ratio = 1.2) +
      theme_void() +
      theme(legend.justification = c(0.5, 1),
            legend.position = c(0.5, 1),
            legend.direction = 'horizontal',
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 20)) +
      guides(color = guide_legend(override.aes = list(size = 5)))
  })
  
  # creating the world map for medals by country
  output$medal_map <- renderPlot({
    
    medal <- switch(input$medal_button,
                       'Gold' = merged_data$Gold,
                       'Silver' = merged_data$Silver,
                       'Bronze' = merged_data$Bronze)
    color <- switch(input$medal_button,
                    'Gold' = 'gold',
                    'Silver' = '#C0C0C0',
                    'Bronze' = '#CD7F32')
    label <- switch(input$medal_button,
                    'Gold' = 'Gold Medals',
                    'Silver' = 'Silver Medals',
                    'Bronze' = 'Bronze Medals')
    
    ggplot() +
      geom_polygon(data = merged_data,
                   aes(x = long, y = lat, group = group, fill = medal),
                   color = 'grey60') +
      scale_fill_continuous(trans = "log10",
                            breaks = c(1, 10, 100, 1000),
                            na.value = 'white',
                            low = 'white',
                            high = color) +
      coord_equal(xlim = c(-180, 180), ylim = c(-60, 90), ratio = 1.5) +
      labs(fill = label) +
      theme_void()
  })
  
  
  # create dist plot for heights and weights by sport
  output$height_weight_plot <- renderPlot({
    
    if (input$sport == "All Sports") {
      sport_data <- olympics_data
    } else if (input$sport != "") {
      sport_data <- subset(data, sport %in% input$sport)
    } else {
      sport_data <- olympics_data
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