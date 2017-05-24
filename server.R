
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
library(shinydashboard)

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
  #Create named list for game selector choices
  recent_games_named_list <- reactive({
    setNames(as.character(recent_games()$GameID),recent_games()$Game)
  })
  #Generate UI Selector for Games
  output$game_selector <- renderUI({
    selectInput(inputId = 'game_selector',label = 'Select a Game',choices = recent_games_named_list())
  })
  game_selected <- reactive({ as.character(filter(recent_games(),GameID==input$game_selector)$Game) })
  
  
  ################
  ################
  ################
  
  df <- reactive({
    url <- paste0("http://data.nba.com/data/10s/v2015/json/mobile_teams/nba/2016/scores/pbp/",input$game_selector,"_full_pbp.json") #'0041600202'
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
    team_code <- strsplit(game_selected(),split = ' ')[[1]][1]
    as.character(filter(team_colors,code==team_code)$hex)
  })
  vst_team_color <- reactive({
    team_code <- strsplit(game_selected(),split = ' ')[[1]][3]
    as.character(filter(team_colors,code==team_code)$hex)
  })
  home_team_text_color <- reactive({
    team_code <- strsplit(game_selected(),split = ' ')[[1]][1]
    as.character(filter(team_colors,code==team_code)$textcolor)
  })
  vst_team_text_color <- reactive({
    team_code <- strsplit(game_selected(),split = ' ')[[1]][3]
    as.character(filter(team_colors,code==team_code)$textcolor)
  })


  output$mainPlot <- renderPlot({
    home_color = home_team_color() #'blue'
    vst_color = vst_team_color() #'red'
    
    ggplot(graphData.linear()) +
      geom_ribbon(aes(x=total_time, ymin=pmax(vst_score,hm_score), ymax=hm_score, group=1), fill=vst_color,alpha="0.5") +
      geom_ribbon(aes(x=total_time, ymin=pmax(vst_score,hm_score), ymax=vst_score, group=1), fill=home_color,alpha="0.5") +
      geom_line(aes(total_time,hm_score),color=home_color,size=2) +
      geom_line(aes(total_time,vst_score),color=vst_color,size=2) +
      
      scale_y_continuous(breaks=seq(0,175,25))+
      xlab('Time (mins)')+
      ylab('Score')+
      theme(panel.grid.minor = element_blank(),
            axis.title = element_text(size=14),
            axis.text = element_text(size=12))
#     ggplot(graphData.linear()) +
#       geom_ribbon(aes(x=total_time, ymin=pmax(vst_score,hm_score), ymax=hm_score, group=1), fill="red",alpha="0.5") +
#       geom_ribbon(aes(x=total_time, ymin=pmax(vst_score,hm_score), ymax=vst_score, group=1), fill="blue",alpha="0.5") +
#       geom_line(aes(total_time,hm_score),color="blue",size=2) +
#       geom_line(aes(total_time,vst_score),color="red",size=2)
  })

  hs_final <- reactive({ unlist(tail(df(),1)[,'hs']) })
  vs_final <- reactive({ unlist(tail(df(),1)[,'vs']) })
  
  output$homescore_box <- renderUI({
    hm_team_code <- strsplit(game_selected(),split = ' ')[[1]][1]
    vs_team_code <- strsplit(game_selected(),split = ' ')[[1]][3]
    
    tags$div(id='score_box',
      HTML(paste0("<img src='images/",hm_team_code,".png' alt='",hm_team_code,"' style='height:80px;'>"))
      ,tags$span(id='hs_box', hs_final()) #team_logos/BOS.png
      ,tags$span('    ')
      ,HTML(paste0("<img src='images/",vs_team_code,".png' alt='",vs_team_code,"' style='height:80px;'>"))
      ,tags$span(id='vs_box',vs_final())
    )
  })
  
  output$style <- renderUI({
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }",
               "#score_box {margin-top: 10px;
                            margin-bottom: 10px;}",
               "#score_box span {
                         padding: 10px;
                         margin: 10px;
                         vertical-align: top;
                         font-size: 60px;}",
               paste0("#hs_box {background-color:",home_team_color()," ;
                                color:",home_team_text_color(),";}"),
               paste0("#vs_box {background-color:",vst_team_color()," ;
                                color:",vst_team_text_color(),";}")
    )
  })
  
  output$table <- renderTable({ graphData.linear() })
  output$text <- renderText({ gameID_selected() })
  
  session$onSessionEnded(function() {
    remDr$server$stop()
  })
  
})
