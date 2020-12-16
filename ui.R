library(shinydashboard)
library(plotly)
library(tidyverse)
library(ggplot2)
library(visNetwork)
library(dygraphs)
library(xts)
library(shiny)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "The Visualizers: College Football", 
                  titleWidth = 300),
  dashboardSidebar(width = 300, sidebarMenu(menuItem("Network", tabName = "network", 
                                        icon = icon("project-diagram")),
                                        menuItem("Time Series", tabName = "time_series", 
                                                 icon = icon("project-diagram")),
                                        menuItem("Plotly", tabName = "plotly", 
                                                 icon = icon("project-diagram")),
                                        menuItem("Scatter Plot 2", tabName = "scatterplot2", 
                                                 icon = icon("project-diagram")),
                                        menuItem("Cat Plot", tabName = "catplot", 
                                                 icon = icon("project-diagram")),
                                        menuItem("Choropleth", tabName = "choropleth", 
                                                 icon = icon("globe-americas")))),
  dashboardBody(
    tabItems(

      
      tabItem(tabName = "network",
              visNetworkOutput("network.plot", height = "1000px"))
      
      ,
      
      tabItem(tabName = "time_series", 
              mainPanel(dygraphOutput("testing")),
              helpText("Note: this shows the attendance per individual game for the past 18 years.",
                       " It also shows the 37 (number of games per season) game moving average of
             ",
                       " the attendance per game to better illustrate how the trend of attendance",
                       " is decreasing over time.")
              
      )
      
      ,
      
      tabItem(tabName = "plotly", 
              tabPanel("Plotly", 
                       
                       selectInput(inputId = "y",
                                   label = "Select Year",
                                   choices = seq(2000,2018,by = 1),
                                   selected = 2000),
                       
                       plotlyOutput(outputId = "plotly_plot",
                                    height = 500, width = 800)
              ),
              helpText("Note: this shows the total wins that each",
                       "of the top 10 most popular teams (by attendance per game)
             ",
                       "got by the end of each yearly season, investigating how the performance of",
                       "a team relates to the popularity (attendance). The attendance of
             ",
                       "the top 10 teams have remained relatively steady through the decade and",
                       "there is no significant trend in attendance among the top 10. However,
             ",
                       " it is important to note that earlier years (around pre 2007) did not have",
                       "as many high attendance games as later on. Coupled with the time series,
             ",
                       " it suggests that the problem of decreasing popularity may lie with teams",
                       " that can't win as much.")
      )
      
      ,
      
      tabItem(tabName = "scatterplot2",
              
              fluidPage(
                
                titlePanel("Game Attendance vs Score Difference"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("color", "Categorical Variables to Plot:",
                                c("TV Coverage Status", "Top 25 Tailgating Status", "New Coach Status"),
                                multiple = FALSE)
                  ),
                  
                  plotlyOutput(outputId = "Fill", height = "400px", inline = FALSE)
                )
              )
              
              
      )
      
      ,
      
      tabItem(tabName = "catplot",  
              plotOutput(outputId = "cat.plot", height = "400px")
              )
      
      ,
      
      
      tabItem(tabName = "choropleth", h1("This is our choropleth! :"), 
              tabsetPanel(type = "tabs", 
                          tabPanel("Graph 1", h2("Graph 1"), 
                                   checkboxInput(inputId = "smoothBool",
                                                 label = strong("Show trend line?"),
                                                 value = FALSE),
                                   sliderInput(inputId = "sizeG1",
                                               label = "Choose size of points:",
                                               min = 0.2, max = 4, value = 1, step = 0.2),
                                   plotOutput("graph1")),
                          
                          tabPanel("Graph 2", h2("Graph 2"), 
                                   sliderInput(inputId = "bwG2",
                                               label = "Choose bandwidth:",
                                               min = 0.02, max = 4, value = 1, step = 0.2),
                                   checkboxInput(inputId = "filledBool",
                                                 label = strong("Fill in contours?"),
                                                 value = FALSE),
                                   plotOutput("graph2"))))
      
    )
    
    
  )
)