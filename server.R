library(tidyverse)
library(ggplot2)
library(visNetwork)
library(dygraphs)
library(xts)
library(shiny)
library(plotly)
library(igraph)

################ Themes #####################
dge.theme <-  theme_bw() + 
  theme(plot.title = element_text(size = rel(2), face = "bold",
                                  color = "midnightblue"),
        panel.border = element_rect(linetype = "dashed",
                                    color = "midnightblue"),
        axis.title.y = element_text(color = "midnightblue", 
                                    face = "bold"),
        axis.title.x = element_text(color = "midnightblue", 
                                    face = "bold"),
        legend.title = element_text(color = "midnightblue", 
                                    face = "bold"),
        legend.text = element_text(color = "midnightblue"),
        plot.caption = element_text(color = "midnightblue"),
        plot.subtitle = element_text(color = "midnightblue")) 

katheri2_315_theme <-  theme(plot.background = element_rect(fill = "lightblue"), 
                             axis.text = element_text(size = 12, color = "darkslategrey"),
                             text = element_text(size = 14, face = "italic"))




####################### #READ IN DATA ######################
football <- read_csv("Data/collegeFootball.csv")
football$Date <- as.Date(football$Date, "%m/%d/%Y")

################# Time Series ########################

games_per_day <- football %>% group_by(Date) %>%  summarize(n_game = n())
attendance_per_day <- aggregate(football$Attendance, by = list(Category = football$Date), FUN = sum)

moving_average <- function(tt, time_series, ww) {
  #  Throw an error if the window width is too big
  if (ww > length(time_series))  
    stop("Window width is greater than length of time series")
  
  #  If the window width is greater than the time point, return NA
  if (tt >= ww)
    return(mean(time_series[(tt-ww+1):tt]))
  else
    return(NA)
}

get_moving_averages <- function(time_series, ww) {
  #  Throw an error if the window width is too big
  n <- length(time_series)
  if (ww > n)  
    stop("Window width is greater than length of time series")
  avg <- rep(0, n)
  for(i in 1:n){
    avg[i] <- moving_average(i, time_series, ww)
  }
  
  return(avg = avg)
}

attendance_per_game <- data.frame(date = games_per_day$Date, attend_per_game = attendance_per_day$x/games_per_day$n_game)
moving_avg_attendance <- data.frame(mv_avg = get_moving_averages(attendance_per_game$attend_per_game, 37), date = games_per_day$Date)

data_ts <- xts(attendance_per_game$attend_per_game, attendance_per_game$date)

mv_ts <- xts(moving_avg_attendance$mv_avg, moving_avg_attendance$date)

total_ts <- cbind(data_ts, mv_ts)


################# Network ###########################
# RUN THIS
opp <- football %>%  filter(xfun::is_ascii(Opponent))
opp$Opponent <-  stringr::str_replace(opp$Opponent, '\\*', '')
opp <- opp[-c(1807),]
#head(gsub("\\D", "", opp$Result))
opp$Result <- sub("\x96", " ", opp$Result)
x <- strsplit(opp$Result," ")
df <- plyr::ldply(x, rbind)
opp$team.points <- as.numeric(df$`2`)
opp$opponent.points <- as.numeric(df$`3`)
opp <- opp %>% drop_na()
opp <- mutate(opp, PointDiff = team.points - opponent.points)
opp <- mutate(opp, TVyn = ifelse(TV == "Not on TV", "Not on TV", "on TV"))

# RUN THIS
#get teams that are both teams and opponents
opp2 <- opp %>% filter(Opponent %in% unique(opp$Team))
opp2 <- opp2 %>% select(Team, Opponent, PointDiff)
opp2.grouped <- opp2 %>% group_by(Team,Opponent) %>% summarize(PointDiff = mean(PointDiff))
# flip team and opponent if point diff is negative
opp2.grouped[opp2.grouped$PointDiff < 0, c("Team", "Opponent")] <- opp2.grouped[opp2.grouped$PointDiff < 0, c("Opponent", "Team")] 
opp2.grouped$PointDiff <- abs(opp2.grouped$PointDiff)

# RUN THIS
sources <- opp2.grouped %>%
  distinct(Team) %>%
  rename(label = Team)
sources <- sources %>% arrange(label)
nodes <- sources %>% rowid_to_column("id") %>% mutate(font.color = "purple", font.size = 20, 
                                                      margin = 50, borderWidth = 1)

edges <- opp2.grouped %>% 
  left_join(nodes, by = c("Team" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Opponent" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, PointDiff)
edges$Team <- NULL
colnames(edges)[3] <- "value"
edges$title <- paste("Avg. Point Diff is ",paste(edges$value), sep = "")
################################


function(input, output) {
  
  output$network.plot <- renderVisNetwork({
 
    
    v <- visNetwork(nodes, edges, main = list(text = "College Football Team Dominance Network", 
                                              style = "font-size: 40px;
                                              text-align: center;
                                              border-color: purple;
                                              color: purple;"),
                    submain = list(
                      text = paste("If a team points to another team, then at least for its home games, on average, it beats that other team.",
                            "The edge weight represents the magnitude of the average point difference. The data is based on college football games from 2000-2018.",
                            "To explore, choose a team from the dropdown, drag and zoom in/out, and hover over edges to see actual differences."),
                      style = "font-size: 15px;"),
                    width = "500%", height = "1000px", background = "lightblue") %>%
      visIgraphLayout(layout = "layout_in_circle") %>%
      visNodes(size = 20) %>%
      visEdges(arrows = list(to = list(enabled = T, scaleFactor = 2)),
               color = list(opacity = 0, highlight = "red")) %>% 
      visOptions(highlightNearest = list(enabled = F, hover = F),
                 
                 nodesIdSelection = list(enabled = T, selected = "60",
                 style = 'width: 200px; height: 26px;
                                 background: #f8f8f8;
                                 color: darkblue;
                                 border:none;
                                 outline:none;
                                 text-align: center;
                                 justify-content: center;
                                 margin: 10px;')
                 ) %>% 
      visInteraction(dragNodes = FALSE, 
                     dragView = TRUE, 
                     zoomView = TRUE) %>% 
      #visEvents(selectNode = 
      #            "function(e) { var test = this.body.data.edges; e.edges.forEach(function(item) { test.update({id: item,hidden : false}); }); }") %>% 
      visLayout(randomSeed = 315)
    
    return(v)
    
    
  })
  
  output$testing <- renderDygraph({
    dygraph(total_ts, main = "Attendance Per Game Over Time") %>% 
      dyRangeSelector(dateWindow = c("2000-08-26", "2018-12-01"))
  })
  
  output$plotly_plot <- renderPlotly({
    
    plot_reactive <- reactive({
      football %>% dplyr::filter(as.character(Year) == input$y) %>% group_by(Year, Team) %>%  summarize(attendance = sum(Attendance), max_wins = max(`Current Wins`))
    })
    
    
    m = order(plot_reactive()$attendance, decreasing = TRUE)
    top_10 = data.frame(year = plot_reactive()$Year[m], team = plot_reactive()$Team[m], attendance = plot_reactive()$attendance[m], max_wins = plot_reactive()$max_wins[m]) %>% head(10)
    
    title = paste("Total Wins of Top 10 Teams in", top_10$year)
    
    p <- ggplot(top_10) + geom_bar(aes(x = reorder(team, -max_wins), y = max_wins, fill = attendance), stat = "identity") + 
      labs(title = title, y = "Number of Wins", x = "Teams", fill = "Attendance") + katheri2_315_theme + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  scale_fill_gradient(low = "#79a6d2", high = "#00334d")
    
    p_plotly <- ggplotly(p,height = 500)
    
    return(p_plotly)
    
    
  })
  
  output$Fill <- renderPlotly({
    
    x <- list(
      title = "Score Difference"
    )
    y <- list(
      title = "Stadium Attendance Fill Rate"
    )
    
    if(input$color == "TV Coverage Status"){
      fillrate_plot <-  plot_ly(opp, x = ~PointDiff, y = ~`Fill Rate`, color = ~`TVyn`, type = "scatter") %>%
        layout(xaxis = x, yaxis = y)
    } else if (input$color == "Top 25 Tailgating Status"){
      fillrate_plot <-  plot_ly(opp, x = ~PointDiff, y = ~`Fill Rate`, color = ~`Tailgating`, type = "scatter") %>%
        layout(xaxis = x, yaxis = y)
    } else {
      fillrate_plot <-  plot_ly(opp, x = ~PointDiff, y = ~`Fill Rate`, color = ~`New Coach`, type = "scatter") %>%
        layout(xaxis = x, yaxis = y)
    }
  
    return(fillrate_plot)
    
  })
  
  
  output$Fill2 <- renderPlotly({
    print(input$color)
    if(input$color == "TVyn"){
      fillrate_plot <-  plot_ly(opp, x = ~PointDiff, y = ~`Fill Rate`, type = "scatter") %>%
        layout(title = "Point Difference versus Fill Rate")
    }
    else if(input$color == "Tailgating"){
      fillrate_plot <-  plot_ly(opp, x = ~PointDiff, y = ~`Fill Rate`, type = "scatter") %>%
        layout(title = "Point Difference versus Fill Rate")
    }
    else {
      fillrate_plot <-  plot_ly(opp, x = ~PointDiff, y = ~`Fill Rate`, type = "scatter") %>%
        layout(title = "Point Difference versus Fill Rate")
    }
    return(fillrate_plot)
  })
  
  output$cat.plot <- renderPlot({
    
    if(input$var == "TVyn"){
      cat_plot <- ggplot(opp, aes(x = WinLoss, fill = TVyn)) + geom_bar() + labs()
    }
    else if(input$var == "Tailgating"){
      cat_plot <- ggplot(opp, aes(x = WinLoss, fill = Tailgating)) + geom_bar() + labs()
    }
    else {
      cat_plot <- ggplot(opp, aes(x = WinLoss, fill = `New Coach`)) + geom_bar() + labs()
    }
    return(cat_plot)
  })
  
  
  
  output$main_plot <- renderPlot({
    
    eruption.plot <- ggplot(faithful, aes(x = eruptions)) + 
      geom_histogram(aes(y = ..density..), 
                     bins = as.numeric(input$n_breaks)) +
      labs(x = "Duration (minutes)", title = "Geyser Eruption Duration") +
      dge.theme
    
    if (input$individual_obs){
      eruption.plot <- eruption.plot + geom_rug()
    }
    
    if (input$density) {
      eruption.plot <- eruption.plot + geom_density(color = "blue", 
                                                    adjust = input$bw_adjust)
    }
    
    return(eruption.plot)
    
  })
  
  output$graph1 <- renderPlot({
    
    plot.g1 <- ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_point(size = input$sizeG1) + 
      labs(x = "Waiting time", y = "Eruptions", 
           title = "Eruptions Vs. Waiting Time") + dge.theme 
    
    if (input$smoothBool){
      plot.g1 <- plot.g1 + geom_smooth()
    }
    
    return(plot.g1)
    
  })
  
  output$graph2 <- renderPlot({
    plot.g2 <- ggplot(faithful, aes(x = waiting, y = eruptions)) + 
      labs(x = "Waiting time", y = "Eruptions", 
           title = "Joint Distribution of Eruptions and Waiting Time") + 
      dge.theme 
    
    if (input$filledBool){
      plot.g2 <- plot.g2 + geom_density2d_filled(adjust = input$bwG2)
    } else {
      plot.g2 <- plot.g2 + geom_density2d(adjust = input$bwG2)
    }
    
    return(plot.g2)
  })
  
  
}


