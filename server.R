
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RSelenium)
library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RJSONIO)

team_colors <- read.csv('team_colors.csv')

shinyServer(function(input, output,session) {

  output$date_selector <- renderUI({
    dateInput('date_selector','Select a Date',min = '2017-01-01',max = Sys.Date(),value = Sys.Date()-1)
  })
  
  #open selenium driver
  remDr <- RSelenium::rsDriver(port = 4450L,browser="chrome")
  
  #get recent games
  recent_games <- reactive({
    #url <- "http://stats.nba.com/js/data/widgets/boxscore_breakdown_20170423.json"
    url <- paste0("http://stats.nba.com/js/data/widgets/boxscore_breakdown_",format(input$date_selector,'%Y%m%d'),".json")
    
    #remDr$server$stop()
    #remDr <- RSelenium::rsDriver(port = 4448L,browser="chrome")
    
    remDr$client$navigate(url)
    raw_source <- remDr$client$getPageSource(url)
    #remDr$server$stop()
    text <- read_html(raw_source[[1]]) %>% 
      html_nodes("pre") %>% 
      html_text()
    json_text <- fromJSON(text)$results
    
    recent_games <- as.data.frame(t( sapply(1:length(json_text),function(X){ c(json_text[[X]]$Game,json_text[[X]]$GameID,json_text[[X]]$HomeTeam[3],json_text[[X]]$VisitorTeam[[3]]) }) ))
    colnames(recent_games) <- c('Game','GameID','Home','Visitor')
    recent_games
  })
  
  #Generate UI Selector for Games
  output$game_selector <- renderUI({
    selectInput(inputId = 'game_selector',label = 'Select a Game',choices = recent_games()$Game,selected = recent_games()$GameID[1])
  })
  gameID_selected <- reactive({ as.character(filter(recent_games(),Game==input$game_selector)$GameID) })
  
  
  
  ################
  ################
  ################
#   df <- reactive({
#     #url <- "http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&GameID=0041600104&StartPeriod=1"
#     url <- paste0("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&GameID=",gameID_selected(),"&StartPeriod=1")
#     remDr$client$navigate(url)
#     raw_source <- remDr$client$getPageSource(url)
# 
#     text <- read_html(raw_source[[1]]) %>% 
#       html_nodes("pre") %>% 
#       html_text()
#     
#     json_text <- fromJSON(text)$resultSets[[1]]
#     
#     df <- as.data.frame(t(sapply(1:length(json_text$rowSet), function(X){
#       json_text$rowSet[[X]][sapply(json_text$rowSet[[X]], is.null)] <- NA
#       unlist(json_text$rowSet[[X]]) }
#     )))
#     colnames(df) <- json_text$headers
#     df
#   })
#   
#   scoringPlays <- reactive({
#     df() %>% 
#       filter(!is.na(SCORE)) %>% 
#       select(SCORE,PCTIMESTRING,PERIOD) %>% 
#       separate(SCORE,into = c('vst_score','hm_score')) %>% 
#       separate(PCTIMESTRING, into = c('mins','secs')) %>% 
#       mutate(real_time = as.numeric(mins)+as.numeric(secs)/60) %>% 
#       mutate(total_time = (as.numeric(PERIOD)-1)*12+(12-real_time) ) %>% 
#       mutate(vst_score=as.numeric(vst_score),hm_score=as.numeric(hm_score)) %>% 
#       select(vst_score,hm_score,total_time) %>% 
#       unique()
#   })
  
  df <- reactive({
    url <- paste0("http://data.nba.com/data/10s/v2015/json/mobile_teams/nba/2016/scores/pbp/",gameID_selected(),"_full_pbp.json") #'0041600202'
    remDr$client$navigate(url)
    raw_source <- remDr$client$getPageSource(url)
    
    text <- read_html(raw_source[[1]]) %>% 
      html_nodes("pre") %>% 
      html_text()
    
    json_text <- fromJSON(text)
    
    df <- 
      lapply(1:length(json_text[[1]]$pd),function(Y){
        as.data.frame(t(
          sapply(1:length(json_text[[1]]$pd[[Y]]$pla),function(X){
            c(json_text[[1]]$pd[[Y]]$pla[[X]],period=Y)
          })
        ))
      })
    pbp <- data.frame(do.call(rbind.data.frame, df))
    
    pbp$minute <- sapply(pbp$cl,function(X){ as.numeric(strsplit(as.character(X),split = ':')[[1]][1]) })
    pbp$second <- sapply(pbp$cl,function(X){ as.numeric(strsplit(as.character(X),split = ':')[[1]][2]) })
    pbp
  })
  
  scoringPlays <- reactive({ 
    x <- df() %>% 
      mutate(time = minute+second/60,
             hs= as.numeric(hs),
             vs= as.numeric(vs)) %>% 
      mutate(regular_time = (as.numeric(period)-1)*12+(12-time),
             overtime_time = (as.numeric(period)-5)*5+(5-time)+48) %>% 
      mutate(total_time = ifelse(period <= 4,regular_time,overtime_time)) %>%
      select(total_time,hm_score = hs,vst_score = vs) %>% 
      unique() 
    x$hm_score = x$hm_score
    x$vst_score = x$vst_score
    x
    })
  
  graphData.linear <- reactive({ 
    graphData <- scoringPlays() %>% 
      mutate(score_delta = hm_score-vst_score) %>% 
      mutate(winning = ifelse(score_delta>0,'home','visitor'))
    
    graphData.linear <- cbind.data.frame(
      total_time=seq(1,max(graphData$total_time),length=360), 
      sapply(graphData[,c("vst_score","hm_score","score_delta")], function(T) approxfun(graphData$total_time, T)(seq(1,max(graphData$total_time),length=360)))
    )
    graphData.linear
  })
  
  #Team Colors
  home_team_color <- reactive({
    team_code <- strsplit(input$game_selector,split = ' ')[[1]][1]
    as.character(filter(team_colors,code==team_code)$hex)
  })
  vst_team_color <- reactive({
    team_code <- strsplit(input$game_selector,split = ' ')[[1]][3]
    as.character(filter(team_colors,code==team_code)$hex)
  })

  output$mainPlot <- renderPlot({
    home_color = home_team_color() #'blue'
    vst_color = vst_team_color() #'red'
    
    ggplot(graphData.linear()) +
      geom_ribbon(aes(x=total_time, ymin=pmax(vst_score,hm_score), ymax=hm_score, group=1), fill=vst_color,alpha="0.5") +
      geom_ribbon(aes(x=total_time, ymin=pmax(vst_score,hm_score), ymax=vst_score, group=1), fill=home_color,alpha="0.5") +
      geom_line(aes(total_time,hm_score),color=home_color,size=2) +
      geom_line(aes(total_time,vst_score),color=vst_color,size=2)
  })
  
  output$table <- renderTable({ graphData.linear() })
  output$text <- renderText({ gameID_selected() })
  
  session$onSessionEnded(function() {
    remDr$server$stop()
  })
  
})
