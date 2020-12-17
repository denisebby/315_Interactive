library(shinydashboard)
library(plotly)
library(tidyverse)
library(ggplot2)
library(visNetwork)
library(dygraphs)
library(xts)
library(shiny)
library(igraph)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "The Visualizers: College Football", 
                  titleWidth = 300),
  dashboardSidebar(width = 300, sidebarMenu(
    menuItem("Introduction", tabName = "intro", 
             icon = icon("lightbulb")),
    menuItem("Dendrogram", tabName = "dendrogram", 
             icon = icon("project-diagram")),
    menuItem("Attendance Distributions", tabName = "conplot", 
             icon = icon("chart-bar")),
    menuItem("Attendance Time Series", tabName = "time_series", 
             icon = icon("calendar-alt")),
    menuItem("Total Wins (Plotly)", tabName = "plotly", 
             icon = icon("chart-bar")),
    menuItem("Team Dominance Network", tabName = "network", 
             icon = icon("project-diagram")),
    menuItem("Attendance Vs. Scores (Plotly)", tabName = "scatterplot2", 
             icon = icon("chart-line")),
    menuItem("Home Team Outcomes", tabName = "catplot", 
             icon = icon("chart-bar")),
    menuItem("Mosaic Plot", tabName = "mosaic_plot", 
             icon = icon("square-full")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h1("College Football Games (2000 to 2018)", 
                 style = "font-size: 50px; color: purple; font-weight: bold; text-shadow: 5px 5px lightblue;",align = "center"),
              img(src='intro-image.jpg', style = "text-align: center; justify-content: center;"),
              helpText("Image Source: https://collegefootballnews.com/2020/10/college-football-news-rankings-1-127-after-week-8"),
              h2("This dataset contains descriptions of 6672 College Football games."), 
              br(), 
              h2("The dataset contains 8 categorical variables, 10 continuous variables, and 7 discrete variables. 
             We are mostly interested in investigating game attendance and game results."), 
              h2("Three variabels concern game attendance: Attendance, Stadium Capacity, and Fill Rate."), 
              h2("Two variables concern game results: Win/Loss, and Point Difference"), 
              br(), 
              h2("Our major goal is to answer the following questions:"), 
              h2("What factors are related to the game attendance and game result? How are they relate to each other?"))
    
      
      ,

      
      tabItem(tabName = "network",
              helpText("Key Takeaways: The distribution of point differences is heavily right skewed, with most 
teams that win winning on average by between 5 and 20 points. We hypothesize
that teams that tend to dominate (win by large number of points than) other 
teams the most will tend to have higher attendance rates. 

The teams with the highest home game win margins are Oklahoma, Alabama, and
Arkansas. The teams with the lowest home game win margins are Kent State, Miami (OH),
and New Mexico. Interestingly, Oklahoma, Alabama, and Arkanasas have some of the 
highest stadium attendance fill rates (top 7) and Kent state has one of the lowest.
There is evidence for a winner's effect in that more people show up for home games
of teams that tend to overpower their opponents."),
              visNetworkOutput("network.plot", height = "750px")
              )
      
      ,
      
      tabItem(tabName = "time_series", 
              mainPanel(dygraphOutput("testing")),
              helpText("Note: This shows the attendance per individual game for the past 18 years.",
                       " It also shows the 37 (number of games per season) game moving average of",
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
              helpText("Note: This shows the total wins that each",
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
                  
                  plotlyOutput(outputId = "Fill", height = "600px", inline = FALSE)
                )
              ),
              
              helpText(paste("Note: This scatterplot investigates the relationship between the fill",
                             "rate of the stadium (fill rate = attendance/stadium capacity) and the",
                             "score difference between the home team and the away team (positive score",
                             "difference means home team won, negative score difference means away team won).",
                             "To explore further, you can choose which of the 3 categorical variables to color",
                             "the points based on.")),
              helpText(paste("TV coverage status tells us that most of the games were on TV",
                             "and we see the highest proportion of games not on TV at lower",
                             "stadium fill rates (under about .6). Top 25 Tailgating status",
                             "shows us that Bleacher report recognizing the school as a top 25",
                             "tailgating destination is associated with the home team winning the",
                             "game and with the fill rate being high (above .8), a large majority",
                             "of the time. From New Coach Status we can see that the first year",
                             "head coaches are pretty evenly distributed among stadium fill rates;",
                             "however, there is less notable clustering at higher fill rates than",
                             "with non first year head coaches."))
              
              
      )
      
      ,
      
      tabItem(tabName = "catplot",  
              selectizeInput(inputId = "var", 
                             label = "Choose Your Filtering Variable!:", 
                             choices = c("TV Coverage Status", "Top 25 Tailgating Status", "New Coach Status"), 
                             selected = "TV Coverage Status", 
                             multiple = FALSE,
                             options = NULL),
              
              plotOutput(outputId = "cat.plot", height = "600px"),
              helpText(paste("Note: This bar chart explores the relationship between",
                             "the home team winning/losing and the other 3 categorical",
                             "variables (TV Coverage, Top 25 Tailgate status, and New Coach Status).")),
              helpText(paste("This bar chart tells us that in our dataset the home team won the game",
                             "over twice as often as the home team lost the game (over 3500 wins, about 1600 losses).",
                             "In regard to TV coverage we see that a higher proportion of the wins are “Not on TV”",
                             "compared to losses “not on TV”. From “Top 25 Tailgate Status” it is clear that the",
                             "colleges recognized as a top 25 tailgating destination have notably higher proportion",
                             "of home game wins to losses than all the colleges as a whole.  New Coach status shows us that first",
                             "year head coaches have a significantly worse win to loss ratio than non first year head",
                             "coaches. This is clear as first year head coaches only have slightly more home game wins than",
                             "losses, while the non first year head coaches have over double the numbers of home game wins",
                             "as they do losses."))
              )
      
      ,
      
      tabItem(tabName = "conplot",  
              selectizeInput(inputId = "varc", 
                             label = "Choose What Distribution to Analyze! :", 
                             choices = c("Attendance", "Stadium Capacity", 
                                         "Stadium Fill Rate"), 
                             selected = "Attendance", 
                             multiple = FALSE,
                             options = NULL),
              checkboxInput(inputId = "density", 
                            label = strong("Show density estimate:")),
              plotOutput(outputId = "con.plot", height = "600px"),
              helpText(paste("Note: This plot shows the distribution of three continuous variables",
                             "in our dataset (Attendance, Stadium Capacity, and Fill Rate) with the",
                             "option of adding the density estimate for any of the 3 continuous variables.")),
              helpText(paste("The attendance variable’s distribution is right skewed, with a center around",
                             "45000, and a spread that goes from 2267 to 110889. The distribution is",
                             "multimodal, with the first and most notable mode occurring at about 15000,",
                             "the second mode occurring around 45000, and the third mode occurring at around",
                             "80000.")),
              helpText(paste("The stadium capacity distribution has a slight right skew,  with a center at about 52000,", 
                             "and a spread that goes from 17000 to 107282. The stadium capacity distribution is multimodal",
                             "with the first and most notable mode occurring around 30000, the second mode occurring around",
                             "50000, and the third mode occurring around 80000.")),
              helpText(paste("The distribution of stadium fill rate is left skewed with a center around .8, and a spread from .06748 to 1.40399.",
                             "The Stadium fill rate distribution is unimodal with the mode occurring around 1.0."))
      ),
      
      
      tabItem(tabName = "mosaic_plot",  
              selectInput(inputId = "mosaic_var", 
                          label = "Choose the Categorical Variable! :",
                          choices = c("New Coach", "Top 25 Tailgating",
                                      "TV Coverage", "Month"),
                          selected = "New Coach"),
              
              plotOutput(outputId = "mosaic", height = "600px"),
              
              helpText("Note: The mosaic plot is an add on to the stacked bar chart. Given the significant standardized residuals, game result is not independent from whether a first-year head coach is coaching the home team, whether the home team is among the top 25 tailgating destinations recognized by Bleacher Report, or the month of the game. The game result is independent of whether the game got TV coverage. ")
     
      )
      
      
      
      
      ,
      
      tabItem(tabName = "dendrogram", 
               
               selectInput(inputId = "cor_method",
                           label = "Choose Correlation Test between Variables",
                           choices = c("pearson", "spearman"),
                           selected = "pearson"),
               
               selectInput(inputId = "cluster_method",
                           label = "Choose Cluster Method",
                           choices = c("complete", "single", "average", "median", "centroid"),
                           selected = "complete"),
               
               sliderInput(inputId = "cluster_num",
                           label = "Choose Number of Clusters",
                           min = 2,
                           max = 8,
                           step = 1,
                           value = 5),
               
               plotOutput(outputId = "dendrogram_plot"),
               helpText(paste("Note: The dendrogram displays the relative distance among all variables. The graph shows",
                               "that various weather variables are related. Stadium and attendance variables are roughly",
                               "related. Team variables are roughly related. The exact correlation depends on the correlation",
                               "test and cluster method used.")))
            
      
      
      
      
      
    )
    
    
  )
)