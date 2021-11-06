#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 11 Oct 2021
# File: server.R
# More details: https://gigadom.in/
#
############################daT#############################################################################
library(shiny)
library(yorkr)
library(rpart)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rpart.plot)
library(ggthemes)
library(plotly)
library(shinycssloaders)
library(stringr)
# Source files

source("definitions.R")
source("IPLutilities.R")
source("T20Mutilities.R")
source("T20Wutilities.R")
source("BBLutilities.R")
source("NTButilities.R")
source("PSLutilities.R")
source("WBButilities.R")
source("CPLutilities.R")
source("SSMutilities.R")
source("ODIMutilities.R")
source("ODIWutilities.R")
source("analyzeBatsmen.R")
source("analyzeBowlers.R")
source("analyzeMatches.R")
source("analyzeMatches2Teams.R")
source("analyzeTeamPerfOverall.R")
source("printOrPlotMatch.R")
source('printOrPlotMatch2Teams.R')
source('printOrPlotTeamPerfOverall.R')
source("rankPlayers.R")
source("matches2TeamsHelper.R")
source("teamPerfOverallHelper.R")
source("batsmanHelper.R")
source("bowlerHelper.R")
shinyServer(function(input, output,session) {

  output$dateRange3 <- renderUI({
    m <- batsmanHelper(input, output,input$batsmanIPL,"IPL")
    dateRangeInput("dateRange3", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3,{
    updateDateRangeInput(session, "dateRange3",
                         start = input$dateRange3[1],
                         end   = input$dateRange3[2])
  })


  #############
  # IPL Batsmenoiii0o
  output$batsmanPlotsIPL <- renderPlot({
    analyzeBatsmen(input$batsmanIPL,input$batsmanFuncIPL, "IPL",input$dateRange3, input$staticIntv)

  })

  output$batsmanPlotlyIPL <- renderPlotly({
    analyzeBatsmen(input$batsmanIPL,input$batsmanFuncIPL, "IPL",input$dateRange3,input$staticIntv)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotIPL <- renderUI({
    if(input$staticIntv == 1){
      plotOutput("batsmanPlotsIPL")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncIPL =="Dismissals of batsman" || input$batsmanFuncIPL == "Predict Runs of batsman")
        plotOutput("batsmanPlotsIPL")
      else
        plotlyOutput("batsmanPlotlyIPL")
    }

  })

  ########################
  #IPL bowlers
  output$dateRange4 <- renderUI({
    m <- bowlerHelper(input, output,input$bowlerIPL,"IPL")
    dateRangeInput("dateRange4", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4,{
    updateDateRangeInput(session, "dateRange4",
                         start = input$dateRange4[1],
                         end   = input$dateRange4[2])
  })

  output$bowlerPlotsIPL <- renderPlot({
    analyzeBowlers(input$bowlerIPL,input$bowlerFuncIPL,"IPL",input$dateRange4,input$staticIntv1)
  })

  output$bowlerPlotlyIPL <- renderPlotly({
    analyzeBowlers(input$bowlerIPL,input$bowlerFuncIPL, "IPL",input$dateRange4,input$staticIntv1)
  })

  output$bowlerPlotIPL <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsIPL")
    }   else{
      if(input$bowlerFuncIPL == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsIPL")
      else
        plotlyOutput("bowlerPlotlyIPL")
    }

  })






  ######################################## IPL Match  #############################################
  # Analyze and display IPL Match plot
  output$IPLMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"IPL")

  })

  output$IPLMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"IPL")

  })

  # Analyze and display IPL Match table
  output$IPLMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"IPL")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintIPLMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"IPL"))){
      tableOutput("IPLMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTable == 1){
        plotOutput("IPLMatchPlots")
      } else{
        plotlyOutput("IPLMatchPlotly")
      }

    }

  })

  #################################### IPL Matches between 2 teams ######################
  # Analyze Head to head confrontation of IPL teams

  output$dateRange1 <- renderUI({
    m <- matches2TeamsHelper(input, output,"IPL")
    dateRangeInput("dateRange1", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

   observeEvent(input$dateRange1,{
    m <- matches2TeamsHelper(input, output,"IPL")
    updateDateRangeInput(session, "dateRange1",
                         start = input$dateRange1[1],
                         end   = input$dateRange1[2])
  })

  # Analyze and display IPL Matches between 2 teams plot
  output$IPLMatch2TeamsPlots <- renderPlot({
    printOrPlotMatch2Teams(input, output)

  })

  output$IPLMatch2TeamsPlotly <- renderPlotly({
    printOrPlotMatch2Teams(input, output)

  })

  # Analyze and display IPL Match table
  output$IPLMatch2TeamsPrint <- renderTable({
    a <- printOrPlotMatch2Teams(input, output)
    a
  })

  # Output either a table or a plot
  output$plotOrPrintIPLMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1 == 3){
      plotlyOutput("IPLMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output))){
      tableOutput("IPLMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1 == 1){
        plotOutput("IPLMatch2TeamsPlots")
      } else if(input$plotOrTable1 == 2){
        plotlyOutput("IPLMatch2TeamsPlotly")
      }
    }

  })



  ################################ IPL Teams's overall performance ##############################

  output$dateRange2 <- renderUI({
    n <- teamPerfOverallHelper(input, output,"IPL")
    dateRangeInput("dateRange2", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2,{
    m <- teamPerfOverallHelper(input, output,"IPL")
    updateDateRangeInput(session, "dateRange2",
                         start = input$dateRange2[1],
                         end   = input$dateRange2[2])
  })

  # Analyze overall IPL team performance plots
  output$IPLTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output)

  })

  output$IPLTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output)

  })



  # Analyze and display IPL Match table
  output$IPLTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output)
    a

  })
  # Output either a table or a plot
  output$printOrPlotIPLTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("IPLTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output))){
      tableOutput("IPLTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2 == 1){
        plotOutput("IPLTeamPerfOverallPlots")
      } else if(input$plotOrTable2 == 2){
        plotlyOutput("IPLTeamPerfOverallPlotly")
      }
    }
  })


  ################################ Rank IPL ##############################
  # Rank IPL Batsmen


  output$dateRange5<- renderUI({
    m <- helper(IPLTeamNames, "./ipl/iplBattingBowlingDetails")
    dateRangeInput("dateRange5", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5,{
     updateDateRangeInput(session, "dateRange5",
                           start = input$dateRange5[1],
                           end   = input$dateRange5[2])
    updateSliderInput(session, "minMatches", # Set slider at 75$ between min & max
                      min=(helper1(IPLTeamNames, input$dateRange5, "./ipl/iplBattingBowlingDetails")[[1]]),
                      max = (helper1(IPLTeamNames, input$dateRange5, "./ipl/iplBattingBowlingDetails")[[2]]),
                      value =round(((helper1(IPLTeamNames, input$dateRange5, "./ipl/iplBattingBowlingDetails")[[1]]) +
                                    (helper1(IPLTeamNames, input$dateRange5, "./ipl/iplBattingBowlingDetails")[[2]]))/1.333))
  })



  # Analyze and display IPL Match table
  output$IPLRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    rootDir=getwd()
    a <- rankPlayers(input, output, "IPL","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankIPLBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output, "IPL","batsmen"))){
      tableOutput("IPLRankBatsmenPrint")

    }
  })

  ########################################
  # Rank IPL Bowlers

  output$dateRange6<- renderUI({
    m <- helper2(IPLTeamNames, "./ipl/iplBattingBowlingDetails")
    dateRangeInput("dateRange6", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6,{
    updateDateRangeInput(session, "dateRange6",
                         start = input$dateRange6[1],
                         end   = input$dateRange6[2])
    updateSliderInput(session, "minMatches1", # Set slider at 75$ between min & max
                      min=(helper3(IPLTeamNames, input$dateRange6, "./ipl/iplBattingBowlingDetails")[[1]]),
                      max = (helper3(IPLTeamNames, input$dateRange6, "./ipl/iplBattingBowlingDetails")[[2]]),
                      value =round(((helper3(IPLTeamNames, input$dateRange6, "./ipl/iplBattingBowlingDetails")[[1]]) +
                                      (helper3(IPLTeamNames, input$dateRange6, "./ipl/iplBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display IPL Match table
  output$IPLRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"IPL","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankIPLBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"IPL","bowlers"))){
      tableOutput("IPLRankBowlersPrint")

    }
  })







  #########################################T20 Men #################################################
  ################################################################################################
  # T20 Men
  output$dateRange3T20M <- renderUI({
    m <- batsmanHelper(input, output,input$batsmanT20M,"T20M")
    dateRangeInput("dateRange3T20M", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3T20M,{
    updateDateRangeInput(session, "dateRange3T20M",
                         start = input$dateRange3T20M[1],
                         end   = input$dateRange3T20M[2])
  })


  # T20M Batsmen
  output$batsmanPlotsT20M <- renderPlot({
    analyzeBatsmen(input$batsmanT20M,input$batsmanFuncT20M, "T20M",input$dateRange3T20M, input$staticIntvT20M)

  })

  output$batsmanPlotlyT20M <- renderPlotly({
    analyzeBatsmen(input$batsmanT20M,input$batsmanFuncT20M, "T20M",input$dateRange3T20M,input$staticIntvT20M)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotT20M <- renderUI({
    if(input$staticIntvT20M == 1){
      plotOutput("batsmanPlotsT20M")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncT20M =="Dismissals of batsman" || input$batsmanFuncT20M == "Predict Runs of batsman")
        plotOutput("batsmanPlotsT20M")
      else
        plotlyOutput("batsmanPlotlyT20M")
    }

  })


  output$dateRange4T20M <- renderUI({
    m <- batsmanHelper(input, output,input$bowlerT20M,"T20M")
    dateRangeInput("dateRange4T20M", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4T20M,{
    updateDateRangeInput(session, "dateRange4T20M",
                         start = input$dateRange4T20M[1],
                         end   = input$dateRange4T20M[2])
  })

  # Analyze and display bowler plots
  output$bowlerPlotsT20M <- renderPlot({
    analyzeBowlers(input$bowlerT20M,input$bowlerFuncT20M,"T20M",input$dateRange4T20M,input$staticIntv1T20M)
  })

  output$bowlerPlotlyT20M <- renderPlotly({
    analyzeBowlers(input$bowlerT20M,input$bowlerFuncT20M, "T20M",input$dateRange4T20M,input$staticIntv1T20M)
  })

  output$bowlerPlotT20M <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsT20M")
    }   else{
      if(input$bowlerFuncT20M == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsT20M")
      else
        plotlyOutput("bowlerPlotlyT20M")
    }

  })


  ######################################## T20 Men's Match  #############################################
  # Analyze and display T20 Match plot
  output$T20MMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"T20M")

  })

  output$T20MMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"T20M")

  })

  # Analyze and display T20M Match table
  output$T20MMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"T20M")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintT20MMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"T20M"))){
      tableOutput("T20MMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableT20M == 1){
        plotOutput("T20MMatchPlots")
      } else{
        plotlyOutput("T20MMatchPlotly")
      }

    }

  })

  #################################### T20 Men's Matches between 2 teams ######################
  # Set date range
  output$dateRange1T20M <- renderUI({
    m <- matches2TeamsHelper(input, output,"T20M")
    dateRangeInput("dateRange1T20M", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1T20M,{
    m <- matches2TeamsHelper(input, output,"T20M")
    updateDateRangeInput(session, "dateRange1T20M",
                         start = input$dateRange1T20M[1],
                         end   = input$dateRange1T20M[2])
  })

  # Analyze and display T20 Men Matches between 2 teams plot
  output$T20MMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"T20M")

  })

  output$T20MMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"T20M")

  })

  # Analyze and display IPL Match table
  output$T20MMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"T20M")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintT20MMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1T20M == 3){
      plotlyOutput("T20MMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"T20M"))){
      tableOutput("T20MMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1T20M == 1){
        plotOutput("T20MMatch2TeamsPlots")
      } else if(input$plotOrTable1T20M == 2){
        plotlyOutput("T20MMatch2TeamsPlotly")
      }
    }

  })



  ################################ T20 Men's Teams's overall performance ##############################
  # Date Range
  output$dateRange2T20M <- renderUI({
    n <- teamPerfOverallHelper(input, output,"T20M")
    dateRangeInput("dateRange2T20M", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2T20M,{
    m <- teamPerfOverallHelper(input, output,"T20M")
    updateDateRangeInput(session, "dateRange2T20M",
                         start = input$dateRange2T20M[1],
                         end   = input$dateRange2T20M[2])
  })

  # Analyze overall T20 Mens team performance plots
  output$T20MTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"T20M")

  })

  output$T20MTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"T20M")

  })

  # Analyze and display IPL Match table
  output$T20MTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"T20M")
    a

  })

  # Output either a table or a plot
  output$printOrPlotT20MTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("T20MTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"T20M"))){
      tableOutput("T20MTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2T20M == 1){
        plotOutput("T20MTeamPerfOverallPlots")
      } else if(input$plotOrTable2T20M == 2){
        plotlyOutput("T20MTeamPerfOverallPlotly")
      }
    }
  })


  ################################ Rank T20 Men ##############################
  # Rank T20M batsmen performance


  output$dateRange5T20M<- renderUI({
    m <- helper(T20MTeamNames, "./t20/t20BattingBowlingDetails")
    dateRangeInput("dateRange5T20M", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5T20M,{
    updateDateRangeInput(session, "dateRange5T20M",
                         start = input$dateRange5T20M[1],
                         end   = input$dateRange5T20M[2])
    updateSliderInput(session, "minMatchesT20M",
                      min=(helper1(T20MTeamNames, input$dateRange5T20M, "./t20/t20BattingBowlingDetails")[[1]]),
                      max = (helper1(T20MTeamNames, input$dateRange5T20M, "./t20/t20BattingBowlingDetails")[[2]]),
                      value =round(((helper1(T20MTeamNames, input$dateRange5T20M, "./t20/t20BattingBowlingDetails")[[1]]) +
                                      (helper1(T20MTeamNames, input$dateRange5T20M, "./t20/t20BattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display T20M Match table
  output$T20MRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"T20M","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankT20MBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"T20M","batsmen"))){
      tableOutput("T20MRankBatsmenPrint")

    }
  })

  ########################################
  # Rank T20M Bowlers
  output$dateRange6T20M<- renderUI({
    m <- helper2(T20MTeamNames, "./t20/t20BattingBowlingDetails")
    dateRangeInput("dateRange6T20M", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6T20M,{
    updateDateRangeInput(session, "dateRange6T20M",
                         start = input$dateRange6T20M[1],
                         end   = input$dateRange6T20M[2])
    updateSliderInput(session, "minMatches1T20M", # Set slider at 75$ between min & max
                      min=(helper3(T20MTeamNames, input$dateRange6T20M, "./t20/t20BattingBowlingDetails")[[1]]),
                      max = (helper3(T20MTeamNames, input$dateRange6T20M, "./t20/t20BattingBowlingDetails")[[2]]),
                      value =round(((helper3(T20MTeamNames, input$dateRange6T20M, "./t20/t20BattingBowlingDetails")[[1]]) +
                                      (helper3(T20MTeamNames, input$dateRange6T20M, "./t20/t20BattingBowlingDetails")[[2]]))/1.333))
  })
  # Analyze and display T20M Match table
  output$T20MRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"T20M","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankT20MBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"T20M","bowlers"))){
      tableOutput("T20MRankBowlersPrint")

    }
  })

  ###########################################T20 Women ###############################################
  ####################################################################################################
  # T20 Women
  output$dateRange3T20W <- renderUI({
    m <- batsmanHelper(input, output,input$batsmanT20W,"T20W")
    dateRangeInput("dateRange3T20W", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3T20W,{
    updateDateRangeInput(session, "dateRange3T20W",
                         start = input$dateRange3T20W[1],
                         end   = input$dateRange3T20W[2])
  })

  # T20W Batsmen
  output$batsmanPlotsT20W <- renderPlot({
    analyzeBatsmen(input$batsmanT20W,input$batsmanFuncT20W, "T20W",input$dateRange3T20W,input$staticIntvT20W)

  })

  output$batsmanPlotlyT20W <- renderPlotly({
    analyzeBatsmen(input$batsmanT20W,input$batsmanFuncT20W, "T20W",input$dateRange3T20W,input$staticIntvT20W)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotT20W <- renderUI({
    if(input$staticIntvT20W == 1){
      plotOutput("batsmanPlotsT20W")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncT20W =="Dismissals of batsman" || input$batsmanFuncT20W == "Predict Runs of batsman")
        plotOutput("batsmanPlotsT20W")
      else
        plotlyOutput("batsmanPlotlyT20W")
    }

  })

# T20W bowler
  output$dateRange4T20W <- renderUI({
    m <- batsmanHelper(input, output,input$bowlerT20W,"T20W")
    dateRangeInput("dateRange4T20W", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4T20W,{
    updateDateRangeInput(session, "dateRange4T20W",
                         start = input$dateRange4T20W[1],
                         end   = input$dateRange4T20W[2])
  })


  # Analyze and display bowler plots
  output$bowlerPlotsT20W <- renderPlot({
    analyzeBowlers(input$bowlerT20W,input$bowlerFuncT20W,"T20W",input$dateRange4T20W,input$staticIntv1T20W)
  })

  output$bowlerPlotlyT20W <- renderPlotly({
    analyzeBowlers(input$bowlerT20W,input$bowlerFuncT20W, "T20W",input$dateRange4T20W,input$staticIntv1T20W)
  })

  output$bowlerPlotT20W <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsT20W")
    }   else{
      if(input$bowlerFuncT20W == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsT20W")
      else
        plotlyOutput("bowlerPlotlyT20W")
    }

  })


  ######################################## T20 Women's Match  #############################################
  # Analyze and display T20 Match plot
  output$T20WMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"T20W")

  })

  output$T20WMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"T20W")

  })

  # Analyze and display T20W Match table
  output$T20WMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"T20W")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintT20WMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"T20W"))){
      tableOutput("T20WMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableT20W == 1){
        plotOutput("T20WMatchPlots")
      } else{
        plotlyOutput("T20WMatchPlotly")
      }

    }

  })

  #################################### T20 Women's Matches between 2 teams ######################
  # Set date range
  output$dateRange1T20W <- renderUI({
    m <- matches2TeamsHelper(input, output,"T20W")
    dateRangeInput("dateRange1T20W", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1T20W,{
    m <- matches2TeamsHelper(input, output,"T20W")
    updateDateRangeInput(session, "dateRange1T20W",
                         start = input$dateRange1T20W[1],
                         end   = input$dateRange1T20W[2])
  })

  # Analyze and display T20 Men Matches between 2 teams plot
  output$T20WMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"T20W")

  })

  output$T20WMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"T20W")

  })

  # Analyze and display IPL Match table
  output$T20WMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"T20W")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintT20WMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1T20W == 3){
      plotlyOutput("T20WMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"T20W"))){
      tableOutput("T20WMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1T20W == 1){
        plotOutput("T20WMatch2TeamsPlots")
      } else if(input$plotOrTableT20W == 2){
        plotlyOutput("T20WMatch2TeamsPlotly")
      }
    }

  })



  ################################ T20 Women's Teams's overall performance ##############################
  # Date Range
  output$dateRange2T20W <- renderUI({
    n <- teamPerfOverallHelper(input, output,"T20W")
    dateRangeInput("dateRange2T20W", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2T20W,{
    m <- teamPerfOverallHelper(input, output,"T20W")
    updateDateRangeInput(session, "dateRange2T20W",
                         start = input$dateRange2T20W[1],
                         end   = input$dateRange2T20W[2])
  })

  # Analyze overall T20 Womens team performance plots
  output$T20WTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"T20W")

  })

  output$T20WTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"T20W")

  })

  # Analyze and display T20W Match table
  output$T20WTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"T20W")
    a

  })

  # Output either a table or a plot
  output$printOrPlotT20WTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("T20WTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"T20W"))){
      tableOutput("T20WTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2T20W == 1){
        plotOutput("T20WTeamPerfOverallPlots")
      } else if(input$plotOrTable2T20W == 2){
        plotlyOutput("T20WTeamPerfOverallPlotly")
      }
    }
  })

  ################################ Rank T20 Women ##############################
  # Rank T20 Women performance


  output$dateRange5T20W<- renderUI({
    m <- helper(T20WTeamNames, "./t20/t20WomenBattingBowlingDetails")
    dateRangeInput("dateRange5T20W", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5T20W,{
    updateDateRangeInput(session, "dateRange5T20W",
                         start = input$dateRange5T20W[1],
                         end   = input$dateRange5T20W[2])
    updateSliderInput(session, "minMatchesT20W",
                      min=(helper1(T20WTeamNames, input$dateRange5T20W, "./t20/t20WomenBattingBowlingDetails")[[1]]),
                      max = (helper1(T20WTeamNames, input$dateRange5T20W, "./t20/t20WomenBattingBowlingDetails")[[2]]),
                      value =round(((helper1(T20WTeamNames, input$dateRange5T20W, "./t20/t20WomenBattingBowlingDetails")[[1]]) +
                                      (helper1(T20WTeamNames, input$dateRange5T20W, "./t20/t20WomenBattingBowlingDetails")[[2]]))/1.333))
  })
  # Analyze and display T20W Match table
  output$T20WRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"T20W","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankT20WBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"T20W","batsmen"))){
      tableOutput("T20WRankBatsmenPrint")

    }
  })

  ########################################
  # Rank T20 Women Bowlers
  output$dateRange6T20W<- renderUI({
    m <- helper2(T20WTeamNames, "./t20/t20WomenBattingBowlingDetails")
    dateRangeInput("dateRange6T20W", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6T20W,{
    updateDateRangeInput(session, "dateRange6T20W",
                         start = input$dateRange6T20W[1],
                         end   = input$dateRange6T20W[2])
    updateSliderInput(session, "minMatches1T20W", # Set slider at 75$ between min & max
                      min=(helper3(T20WTeamNames, input$dateRange6T20W, "./t20/t20WomenBattingBowlingDetails")[[1]]),
                      max = (helper3(T20WTeamNames, input$dateRange6T20W, "./t20/t20WomenBattingBowlingDetails")[[2]]),
                      value =round(((helper3(T20WTeamNames, input$dateRange6T20W, "./t20/t20WomenBattingBowlingDetails")[[1]]) +
                                      (helper3(T20WTeamNames, input$dateRange6T20W, "./t20/t20WomenBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display T20W Match table
  output$T20WRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"T20W","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankT20WBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"T20W","bowlers"))){
      tableOutput("T20WRankBowlersPrint")

    }
  })

  ###############################################Big Bash League ###########################################
  ##########################################################################################################
  # Big Bash League

  output$dateRange3BBL <- renderUI({
    m <- batsmanHelper(input, output,input$batsmanBBL,"BBL")
    dateRangeInput("dateRange3BBL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3BBL,{
    updateDateRangeInput(session, "dateRange3BBL",
                         start = input$dateRange3BBL[1],
                         end   = input$dateRange3BBL[2])
  })

  output$batsmanPlotsBBL <- renderPlot({
    analyzeBatsmen(input$batsmanBBL,input$batsmanFuncBBL, "BBL",input$dateRange3BBL,input$staticIntvBBL)

  })

  output$batsmanPlotlyBBL <- renderPlotly({
    analyzeBatsmen(input$batsmanBBL,input$batsmanFuncBBL, "BBL",input$dateRange3BBL,input$staticIntvBBL)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotBBL <- renderUI({
    if(input$staticIntvBBL == 1){
      plotOutput("batsmanPlotsBBL")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncBBL =="Dismissals of batsman" || input$batsmanFuncBBL == "Predict Runs of batsman")
        plotOutput("batsmanPlotsBBL")
      else
        plotlyOutput("batsmanPlotlyBBL")
    }

  })


  output$dateRange4BBL <- renderUI({
    m <- batsmanHelper(input, output,input$bowlerBBL,"BBL")
    dateRangeInput("dateRange4BBL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4BBL,{
    updateDateRangeInput(session, "dateRange4BBL",
                         start = input$dateRange4BBL[1],
                         end   = input$dateRange4BBL[2])
  })

  # Analyze and display bowler plots
  output$bowlerPlotsBBL <- renderPlot({
    analyzeBowlers(input$bowlerBBL,input$bowlerFuncBBL,"BBL",input$dateRange4BBL,input$staticIntv1BBL)
  })

  output$bowlerPlotlyBBL <- renderPlotly({
    analyzeBowlers(input$bowlerBBL,input$bowlerFuncBBL, "BBL",input$dateRange4BBL,input$staticIntv1BBL)
  })

  output$bowlerPlotBBL <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsBBL")
    }   else{
      if(input$bowlerFuncBBL == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsBBL")
      else
        plotlyOutput("bowlerPlotlyBBL")
    }

  })


  ######################################## BBL Match  #############################################
  # Analyze and display T20 Match plot
  output$BBLMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"BBL")

  })

  output$BBLMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"BBL")

  })

  # Analyze and display BBL Match table
  output$BBLMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"BBL")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintBBLMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"BBL"))){
      tableOutput("BBLMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableBBL == 1){
        plotOutput("BBLMatchPlots")
      } else{
        plotlyOutput("BBLMatchPlotly")
      }

    }

  })

  #################################### BBL  Matches between 2 teams ######################
  # Set date range
  output$dateRange1BBL <- renderUI({
    m <- matches2TeamsHelper(input, output,"BBL")
    dateRangeInput("dateRange1BBL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1BBL,{
    m <- matches2TeamsHelper(input, output,"BBL")
    updateDateRangeInput(session, "dateRange1BBL",
                         start = input$dateRange1BBL[1],
                         end   = input$dateRange1BBL[2])
  })

  # Analyze and display BBL Matches between 2 teams plot
  output$BBLMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"BBL")

  })

  output$BBLMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"BBL")

  })

  # Analyze and display BBL Match table
  output$BBLMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"BBL")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintBBLMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1BBL == 3){
      plotlyOutput("BBLMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"BBL"))){
      tableOutput("BBLMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1BBL == 1){
        plotOutput("BBLMatch2TeamsPlots")
      } else if(input$plotOrTable1BBL == 2){
        plotlyOutput("BBLMatch2TeamsPlotly")
      }
    }

  })



  ################################ BBL Teams's overall performance ##############################
  # Date Range
  output$dateRange2BBL <- renderUI({
    n <- teamPerfOverallHelper(input, output,"BBL")
    dateRangeInput("dateRange2BBL", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2BBL,{
    m <- teamPerfOverallHelper(input, output,"BBL")
    updateDateRangeInput(session, "dateRange2BBL",
                         start = input$dateRange2BBL[1],
                         end   = input$dateRange2BBL[2])
  })

  # Analyze overall BBL team performance plots
  output$BBLTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"BBL")

  })

  output$BBLTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"BBL")

  })

  # Analyze and display IPL Match table
  output$BBLTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"BBL")
    a

  })

  # Output either a table or a plot
  output$printOrPlotBBLTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("BBLTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"BBL"))){
      tableOutput("BBLTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2BBL == 1){
        plotOutput("BBLTeamPerfOverallPlots")
      } else if(input$plotOrTable2BBL == 2){
        plotlyOutput("BBLTeamPerfOverallPlotly")
      }
    }
  })


  ################################ Rank BBL ##############################

  output$dateRange5BBL<- renderUI({
    m <- helper(BBLTeamNames, "./bbl/bblBattingBowlingDetails")
    dateRangeInput("dateRange5BBL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5BBL,{
    updateDateRangeInput(session, "dateRange5BBL",
                         start = input$dateRange5BBL[1],
                         end   = input$dateRange5BBL[2])
    updateSliderInput(session, "minMatchesBBL",
                      min=(helper1(BBLTeamNames, input$dateRange5BBL, "./bbl/bblBattingBowlingDetails")[[1]]),
                      max = (helper1(BBLTeamNames, input$dateRange5BBL, "./bbl/bblBattingBowlingDetails")[[2]]),
                      value =round(((helper1(BBLTeamNames, input$dateRange5BBL, "./bbl/bblBattingBowlingDetails")[[1]]) +
                                      (helper1(BBLTeamNames, input$dateRange5BBL, "./bbl/bblBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display BBL Match table
  output$BBLRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"BBL","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankBBLBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"BBL","batsmen"))){
      tableOutput("BBLRankBatsmenPrint")

    }
  })

  ########################################
  # Rank BBL Bowlers
  output$dateRange6BBL<- renderUI({
    m <- helper2(BBLTeamNames, "./bbl/bblBattingBowlingDetails")
    dateRangeInput("dateRange6BBL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6BBL,{
    updateDateRangeInput(session, "dateRange6BBL",
                         start = input$dateRange6BBL[1],
                         end   = input$dateRange6BBL[2])
    updateSliderInput(session, "minMatches1BBL", # Set slider at 75$ between min & max
                      min=(helper3(BBLTeamNames, input$dateRange6BBL, "./bbl/bblBattingBowlingDetails")[[1]]),
                      max = (helper3(BBLTeamNames, input$dateRange6BBL, "./bbl/bblBattingBowlingDetails")[[2]]),
                      value =round(((helper3(BBLTeamNames, input$dateRange6BBL, "./bbl/bblBattingBowlingDetails")[[1]]) +
                                      (helper3(BBLTeamNames, input$dateRange6BBL, "./bbl/bblBattingBowlingDetails")[[2]]))/1.333))

  })

  # Analyze and display BBL Match table
  output$BBLRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"BBL","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankBBLBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"BBL","bowlers"))){
      tableOutput("BBLRankBowlersPrint")

    }
  })
  #########################################################Natwest T20 #####################################
  ##########################################################################################################
  # Natwest T20
  output$dateRange3NTB <- renderUI({
    m <- batsmanHelper(input, output,input$batsmanNTB,"NTB")
    dateRangeInput("dateRange3NTB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3NTB,{
    updateDateRangeInput(session, "dateRange3NTB",
                         start = input$dateRange3NTB[1],
                         end   = input$dateRange3NTB[2])
  })

  output$batsmanPlotsNTB <- renderPlot({
    analyzeBatsmen(input$batsmanNTB,input$batsmanFuncNTB, "NTB",input$dateRange3NTB,input$staticIntvNTB)

  })

  output$batsmanPlotlyNTB <- renderPlotly({
    analyzeBatsmen(input$batsmanNTB,input$batsmanFuncNTB, "NTB",input$dateRange3NTB,input$staticIntvNTB)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotNTB <- renderUI({
    if(input$staticIntvNTB == 1){
      plotOutput("batsmanPlotsNTB")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncNTB =="Dismissals of batsman" || input$batsmanFuncNTB == "Predict Runs of batsman")
        plotOutput("batsmanPlotsNTB")
      else
        plotlyOutput("batsmanPlotlyNTB")
    }

  })

#Bowler
  output$dateRange4NTB <- renderUI({
    m <- batsmanHelper(input, output,input$bowlerNTB,"NTB")
    dateRangeInput("dateRange4NTB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4NTB,{
    updateDateRangeInput(session, "dateRange4NTB",
                         start = input$dateRange4NTB[1],
                         end   = input$dateRange4NTB[2])
  })
  # Analyze and display bowler plots
  output$bowlerPlotsNTB <- renderPlot({
    analyzeBowlers(input$bowlerNTB,input$bowlerFuncNTB,"NTB",input$dateRange4NTB,input$staticIntv1NTB)
  })

  output$bowlerPlotlyNTB <- renderPlotly({
    analyzeBowlers(input$bowlerNTB,input$bowlerFuncNTB, "NTB",input$dateRange4NTB,input$staticIntv1NTB)
  })

  output$bowlerPlotNTB <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsNTB")
    }   else{
      if(input$bowlerFuncNTB == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsNTB")
      else
        plotlyOutput("bowlerPlotlyNTB")
    }

  })


  output$NTBMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"NTB")

  })

  output$NTBMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"NTB")

  })

  # Analyze and display NTB Match table
  output$NTBMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"NTB")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintNTBMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"NTB"))){
      tableOutput("NTBMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableNTB == 1){
        plotOutput("NTBMatchPlots")
      } else{
        plotlyOutput("NTBMatchPlotly")
      }

    }

  })

  #################################### NTB  Matches between 2 teams ######################
  # Set date range
  output$dateRange1NTB <- renderUI({
    m <- matches2TeamsHelper(input, output,"NTB")
    dateRangeInput("dateRange1NTB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1NTB,{
    m <- matches2TeamsHelper(input, output,"NTB")
    updateDateRangeInput(session, "dateRange1NTB",
                         start = input$dateRange1NTB[1],
                         end   = input$dateRange1NTB[2])
  })

  # Analyze and display NTB Matches between 2 teams plot
  output$NTBMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"NTB")

  })

  output$NTBMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"NTB")

  })

  # Analyze and display NTB Match table
  output$NTBMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"NTB")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintNTBMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1NTB == 3){
      plotlyOutput("NTBMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"NTB"))){
      tableOutput("NTBMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1NTB == 1){
        plotOutput("NTBMatch2TeamsPlots")
      } else if(input$plotOrTable1NTB == 2){
        plotlyOutput("NTBMatch2TeamsPlotly")
      }
    }

  })



  ################################ NTB Teams's overall performance ##############################
  output$dateRange2NTB <- renderUI({
    n <- teamPerfOverallHelper(input, output,"NTB")
    dateRangeInput("dateRange2NTB", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2NTB,{
    m <- teamPerfOverallHelper(input, output,"NTB")
    updateDateRangeInput(session, "dateRange2NTB",
                         start = input$dateRange2NTB[1],
                         end   = input$dateRange2NTB[2])
  })
  # Analyze overall NTB team performance plots
  output$NTBTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"NTB")

  })

  output$NTBTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"NTB")

  })

  # Analyze and display NTB Match table
  output$NTBTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"NTB")
    a

  })

  # Output either a table or a plot
  output$printOrPlotNTBTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("NTBTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"NTB"))){
      tableOutput("NTBTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2NTB == 1){
        plotOutput("NTBTeamPerfOverallPlots")
      } else if(input$plotOrTable2NTB == 2){
        plotlyOutput("NTBTeamPerfOverallPlotly")
      }
    }
  })

  ################################ Rank NTB ##############################
  # Analyze overall NTB team performance plots


  # Display ranks
  output$dateRange5NTB<- renderUI({
    m <- helper(NTBTeamNames, "./ntb/ntbBattingBowlingDetails")
    dateRangeInput("dateRange5NTB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5NTB,{
    updateDateRangeInput(session, "dateRange5NTB",
                         start = input$dateRange5NTB[1],
                         end   = input$dateRange5NTB[2])
    updateSliderInput(session, "minMatchesNTB",
                      min=(helper1(NTBTeamNames, input$dateRange5NTB, "./ntb/ntbBattingBowlingDetails")[[1]]),
                      max = (helper1(NTBTeamNames, input$dateRange5NTB, "./ntb/ntbBattingBowlingDetails")[[2]]),
                      value =round(((helper1(NTBTeamNames, input$dateRange5NTB, "./ntb/ntbBattingBowlingDetails")[[1]]) +
                                      (helper1(NTBTeamNames, input$dateRange5NTB, "./ntb/ntbBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display NTB Match table
  output$NTBRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"NTB","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankNTBBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"NTB","batsmen"))){
      tableOutput("NTBRankBatsmenPrint")

    }
  })

  ########################################
  # Rank NTB Bowlers
  output$dateRange6NTB<- renderUI({
    m <- helper2(NTBTeamNames, "./ntb/ntbBattingBowlingDetails")
    dateRangeInput("dateRange6NTB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6NTB,{
    updateDateRangeInput(session, "dateRange6NTB",
                         start = input$dateRange6NTB[1],
                         end   = input$dateRange6NTB[2])
    updateSliderInput(session, "minMatches1NTB", # Set slider at 75$ between min & max
                      min=(helper3(NTBTeamNames, input$dateRange6NTB, "./ntb/ntbBattingBowlingDetails")[[1]]),
                      max = (helper3(NTBTeamNames, input$dateRange6NTB, "./ntb/ntbBattingBowlingDetails")[[2]]),
                      value =round(((helper3(NTBTeamNames, input$dateRange6NTB, "./ntb/ntbBattingBowlingDetails")[[1]]) +
                                      (helper3(NTBTeamNames, input$dateRange6NTB, "./ntb/ntbBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display NTB Match table
  output$NTBRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"NTB","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankNTBBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"NTB","bowlers"))){
      tableOutput("NTBRankBowlersPrint")

    }
  })

  ############################################ PSL ##############################################
  ##################################################################################################
  # PSL T20
  output$dateRange3PSL<- renderUI({
    m <- batsmanHelper(input, output,input$batsmanPSL,"PSL")
    dateRangeInput("dateRange3PSL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3PSL,{
    updateDateRangeInput(session, "dateRange3PSL",
                         start = input$dateRange3PSL[1],
                         end   = input$dateRange3PSL[2])
  })
  # Analyze and display batsmen plots
  output$batsmanPlotsPSL <- renderPlot({
    analyzeBatsmen(input$batsmanPSL,input$batsmanFuncPSL, "PSL",input$dateRange3PSL,input$staticIntvPSL)

  })

  output$batsmanPlotlyPSL <- renderPlotly({
    analyzeBatsmen(input$batsmanPSL,input$batsmanFuncPSL, "PSL",input$dateRange3PSL,input$staticIntvPSL)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotPSL <- renderUI({
    if(input$staticIntvPSL == 1){
      plotOutput("batsmanPlotsPSL")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncPSL =="Dismissals of batsman" || input$batsmanFuncPSL == "Predict Runs of batsman")
        plotOutput("batsmanPlotsPSL")
      else
        plotlyOutput("batsmanPlotlyPSL")
    }

  })

  #Bowler
  output$dateRange4PSL<- renderUI({
    m <- batsmanHelper(input, output,input$bowlerPSL,"PSL")
    dateRangeInput("dateRange4PSL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4PSL,{
    updateDateRangeInput(session, "dateRange4PSL",
                         start = input$dateRange4PSL[1],
                         end   = input$dateRange4PSL[2])
  })

  # Analyze and display bowler plots
  output$bowlerPlotsPSL <- renderPlot({
    analyzeBowlers(input$bowlerPSL,input$bowlerFuncPSL,"PSL",input$dateRange4PSL,input$staticIntv1PSL)
  })

  output$bowlerPlotlyPSL <- renderPlotly({
    analyzeBowlers(input$bowlerPSL,input$bowlerFuncPSL, "PSL",input$dateRange4PSL,input$staticIntv1PSL)
  })

  output$bowlerPlotPSL <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsPSL")
    }   else{
      if(input$bowlerFuncPSL == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsPSL")
      else
        plotlyOutput("bowlerPlotlyPSL")
    }

  })


  output$PSLMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"PSL")

  })

  output$PSLMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"PSL")

  })

  # Analyze and display PSL Match table
  output$PSLMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"PSL")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintPSLMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"PSL"))){
      tableOutput("PSLMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTablePSL == 1){
        plotOutput("PSLMatchPlots")
      } else{
        plotlyOutput("PSLMatchPlotly")
      }

    }

  })

  #################################### PSL  Matches between 2 teams ######################
  # Set date range
  output$dateRange1PSL <- renderUI({
    m <- matches2TeamsHelper(input, output,"PSL")
    dateRangeInput("dateRange1PSL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1PSL,{
    m <- matches2TeamsHelper(input, output,"PSL")
    updateDateRangeInput(session, "dateRange1PSL",
                         start = input$dateRange1PSL[1],
                         end   = input$dateRange1PSL[2])
  })

  # Analyze and display PSL Matches between 2 teams plot
  output$PSLMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"PSL")

  })

  output$PSLMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"PSL")

  })

  # Analyze and display PSL Match table
  output$PSLMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"PSL")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintPSLMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1PSL == 3){
      plotlyOutput("PSLMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"PSL"))){
      tableOutput("PSLMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1PSL == 1){
        plotOutput("PSLMatch2TeamsPlots")
      } else if(input$plotOrTable1PSL == 2){
        plotlyOutput("PSLMatch2TeamsPlotly")
      }
    }

  })



  ################################ PSL Teams's overall performance ##############################
  output$dateRange2PSL <- renderUI({
    n <- teamPerfOverallHelper(input, output,"PSL")
    dateRangeInput("dateRange2PSL", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2PSL,{
    m <- teamPerfOverallHelper(input, output,"PSL")
    updateDateRangeInput(session, "dateRange2PSL",
                         start = input$dateRange2PSL[1],
                         end   = input$dateRange2PSL[2])
  })

  # Analyze overall PSL team performance plots
  output$PSLTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"PSL")

  })

  output$PSLTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"PSL")

  })

  # Analyze and display PSL Match table
  output$PSLTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"PSL")
    a

  })

  # Output either a table or a plot
  output$printOrPlotPSLTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("PSLTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"PSL"))){
      tableOutput("PSLTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2PSL == 1){
        plotOutput("PSLTeamPerfOverallPlots")
      } else if(input$plotOrTable2PSL == 2){
        plotlyOutput("PSLTeamPerfOverallPlotly")
      }
    }
  })

  ################################ Rank PSL ##############################
  # Analyze overall PSL team performance plots



  # Display ranks
  output$dateRange5PSL<- renderUI({
    m <- helper(PSLTeamNames, "./psl/pslBattingBowlingDetails")
    dateRangeInput("dateRange5PSL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5PSL,{
    updateDateRangeInput(session, "dateRange5PSL",
                         start = input$dateRange5PSL[1],
                         end   = input$dateRange5PSL[2])
    updateSliderInput(session, "minMatchesPSL",
                      min=(helper1(PSLTeamNames, input$dateRange5PSL, "./psl/pslBattingBowlingDetails")[[1]]),
                      max = (helper1(PSLTeamNames, input$dateRange5PSL, "./psl/pslBattingBowlingDetails")[[2]]),
                      value =round(((helper1(PSLTeamNames, input$dateRange5PSL, "./psl/pslBattingBowlingDetails")[[1]]) +
                                      (helper1(PSLTeamNames, input$dateRange5PSL, "./psl/pslBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display PSL Match table
  output$PSLRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"PSL","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankPSLBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"PSL","batsmen"))){
      tableOutput("PSLRankBatsmenPrint")

    }
  })

  ########################################
  # Rank PSL Bowlers
  output$dateRange6PSL<- renderUI({
    m <- helper2(PSLTeamNames, "./psl/pslBattingBowlingDetails")
    dateRangeInput("dateRange6PSL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6PSL,{
    updateDateRangeInput(session, "dateRange6PSL",
                         start = input$dateRange6PSL[1],
                         end   = input$dateRange6PSL[2])
    updateSliderInput(session, "minMatches1PSL", # Set slider at 75$ between min & max
                      min=(helper3(PSLTeamNames, input$dateRange6PSL, "./psl/pslBattingBowlingDetails")[[1]]),
                      max = (helper3(PSLTeamNames, input$dateRange6PSL, "./psl/pslBattingBowlingDetails")[[2]]),
                      value =round(((helper3(PSLTeamNames, input$dateRange6PSL, "./psl/pslBattingBowlingDetails")[[1]]) +
                                      (helper3(PSLTeamNames, input$dateRange6PSL, "./psl/pslBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display PSL Match table
  output$PSLRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"PSL","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankPSLBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"PSL","bowlers"))){
      tableOutput("PSLRankBowlersPrint")

    }
  })

  ##########################################################################################
  # WBBL T20
  output$dateRange3WBB<- renderUI({
    m <- batsmanHelper(input, output,input$batsmanWBB,"WBB")
    dateRangeInput("dateRange3WBB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3WBB,{
    updateDateRangeInput(session, "dateRange3WBB",
                         start = input$dateRange3WBB[1],
                         end   = input$dateRange3WBB[2])
  })

  # Analyze and display batsmen plots
  output$batsmanPlotsWBB <- renderPlot({
    analyzeBatsmen(input$batsmanWBB,input$batsmanFuncWBB, "WBB",input$dateRange3WBB,input$staticIntvWBB)

  })

  output$batsmanPlotlyWBB <- renderPlotly({
    analyzeBatsmen(input$batsmanWBB,input$batsmanFuncWBB, "WBB",input$dateRange3WBB,input$staticIntvWBB)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotWBB <- renderUI({
    if(input$staticIntvWBB == 1){
      plotOutput("batsmanPlotsWBB")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncWBB =="Dismissals of batsman" || input$batsmanFuncWBB == "Predict Runs of batsman")
        plotOutput("batsmanPlotsWBB")
      else
        plotlyOutput("batsmanPlotlyWBB")
    }

  })

  ################
  #Bowler
  output$dateRange4WBB<- renderUI({
    m <- batsmanHelper(input, output,input$bowlerWBB,"WBB")
    dateRangeInput("dateRange4WBB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4WBB,{
    updateDateRangeInput(session, "dateRange4WBB",
                         start = input$dateRange4WBB[1],
                         end   = input$dateRange4WBB[2])
  })

  # Analyze and display bowler plots
  output$bowlerPlotsWBB <- renderPlot({
    analyzeBowlers(input$bowlerWBB,input$bowlerFuncWBB,"WBB",input$dateRange4WBB,input$staticIntv1WBB)
  })

  output$bowlerPlotlyWBB <- renderPlotly({
    analyzeBowlers(input$bowlerWBB,input$bowlerFuncWBB, "WBB",input$dateRange4WBB,input$staticIntv1WBB)
  })

  output$bowlerPlotWBB <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsWBB")
    }   else{
      if(input$bowlerFuncWBB == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsWBB")
      else
        plotlyOutput("bowlerPlotlyWBB")
    }

  })


  ######################################## WBB Match  #############################################
  # Analyze and display T20 Match plot
  output$WBBMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"WBB")

  })

  output$WBBMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"WBB")

  })

  # Analyze and display WBB Match table
  output$WBBMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"WBB")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintWBBMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"WBB"))){
      tableOutput("WBBMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableWBB == 1){
        plotOutput("WBBMatchPlots")
      } else{
        plotlyOutput("WBBMatchPlotly")
      }

    }

  })

  #################################### WBB  Matches between 2 teams ######################
  # Set date range
  output$dateRange1WBB <- renderUI({
    m <- matches2TeamsHelper(input, output,"WBB")
    dateRangeInput("dateRange1WBB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1WBB,{
    m <- matches2TeamsHelper(input, output,"WBB")
    updateDateRangeInput(session, "dateRange1WBB",
                         start = input$dateRange1WBB[1],
                         end   = input$dateRange1WBB[2])
  })

  # Analyze and display WBB Matches between 2 teams plot
  output$WBBMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"WBB")

  })

  output$WBBMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"WBB")

  })

  # Analyze and display WBB Match table
  output$WBBMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"WBB")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintWBBMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1WBB == 3){
      plotlyOutput("WBBMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"WBB"))){
      tableOutput("WBBMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1WBB == 1){
        plotOutput("WBBMatch2TeamsPlots")
      } else if(input$plotOrTable1WBB == 2){
        plotlyOutput("WBBMatch2TeamsPlotly")
      }
    }

  })



  ################################ WBB Teams's overall performance ##############################
  output$dateRange2WBB <- renderUI({
    n <- teamPerfOverallHelper(input, output,"WBB")
    dateRangeInput("dateRange2WBB", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2WBB,{
    m <- teamPerfOverallHelper(input, output,"WBB")
    updateDateRangeInput(session, "dateRange2WBB",
                         start = input$dateRange2WBB[1],
                         end   = input$dateRange2WBB[2])
  })

  # Analyze overall WBB team performance plots
  output$WBBTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"WBB")

  })

  output$WBBTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"WBB")

  })

  # Analyze and display IPL Match table
  output$WBBTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"WBB")
    a

  })

  # Output either a table or a plot
  output$printOrPlotWBBTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("WBBTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"WBB"))){
      tableOutput("WBBTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2WBB == 1){
        plotOutput("WBBTeamPerfOverallPlots")
      } else if(input$plotOrTable2WBB == 2){
        plotlyOutput("WBBTeamPerfOverallPlotly")
      }
    }
  })

  ################################ Rank WBB ##############################
  # Analyze overall WBB team performance plots

  # Display ranks
  output$dateRange5WBB<- renderUI({
    m <- helper(WBBTeamNames, "./wbb/wbbBattingBowlingDetails")
    dateRangeInput("dateRange5WBB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5WBB,{
    updateDateRangeInput(session, "dateRange5WBB",
                         start = input$dateRange5WBB[1],
                         end   = input$dateRange5WBB[2])
    updateSliderInput(session, "minMatchesWBB",
                      min=(helper1(WBBTeamNames, input$dateRange5WBB, "./wbb/wbbBattingBowlingDetails")[[1]]),
                      max = (helper1(WBBTeamNames, input$dateRange5WBB, "./wbb/wbbBattingBowlingDetails")[[2]]),
                      value =round(((helper1(WBBTeamNames, input$dateRange5WBB, "./wbb/wbbBattingBowlingDetails")[[1]]) +
                                      (helper1(WBBTeamNames, input$dateRange5WBB, "./wbb/wbbBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display WBB Match table
  output$WBBRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"WBB","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankWBBBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"WBB","batsmen"))){
      tableOutput("WBBRankBatsmenPrint")

    }
  })
  #Rank bowlers
  output$dateRange6WBB<- renderUI({
    m <- helper2(WBBTeamNames, "./wbb/wbbBattingBowlingDetails")
    dateRangeInput("dateRange6WBB", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6WBB,{
    updateDateRangeInput(session, "dateRange6WBB",
                         start = input$dateRange6WBB[1],
                         end   = input$dateRange6WBB[2])
    updateSliderInput(session, "minMatches1WBB", # Set slider at 75$ between min & max
                      min=(helper3(WBBTeamNames, input$dateRange6WBB, "./wbb/wbbBattingBowlingDetails")[[1]]),
                      max = (helper3(WBBTeamNames, input$dateRange6WBB, "./wbb/wbbBattingBowlingDetails")[[2]]),
                      value =round(((helper3(WBBTeamNames, input$dateRange6WBB, "./wbb/wbbBattingBowlingDetails")[[1]]) +
                                      (helper3(WBBTeamNames, input$dateRange6WBB, "./wbb/wbbBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display WBB Match table
  output$WBBRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"WBB","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankWBBBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"WBB","bowlers"))){
      tableOutput("WBBRankBowlersPrint")

    }
  })

  ##########################################################################################

  ##########################################################################################
  # ODI Men
  output$dateRange3ODIM<- renderUI({
    m <- batsmanHelper(input, output,input$batsmanODIM,"ODIM")
    dateRangeInput("dateRange3ODIM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3ODIM,{
    updateDateRangeInput(session, "dateRange3ODIM",
                         start = input$dateRange3ODIM[1],
                         end   = input$dateRange3ODIM[2])
  })

  output$batsmanPlotsODIM <- renderPlot({
    analyzeBatsmen(input$batsmanODIM,input$batsmanFuncODIM, "ODIM",input$dateRange3ODIM,input$staticIntvODIM)

  })

  output$batsmanPlotlyODIM <- renderPlotly({
    analyzeBatsmen(input$batsmanODIM,input$batsmanFuncODIM, "ODIM",input$dateRange3ODIM,input$staticIntvODIM)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotODIM <- renderUI({
    if(input$staticIntvODIM == 1){
      plotOutput("batsmanPlotsODIM")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncODIM =="Dismissals of batsman" || input$batsmanFuncODIM == "Predict Runs of batsman")
        plotOutput("batsmanPlotsODIM")
      else
        plotlyOutput("batsmanPlotlyODIM")
    }

  })

  ################
  #Bowler
  output$dateRange4ODIM<- renderUI({
    m <- batsmanHelper(input, output,input$bowlerODIM,"ODIM")
    dateRangeInput("dateRange4ODIM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4ODIM,{
    m <- matches2TeamsHelper(input, output,"ODIM")
    updateDateRangeInput(session, "dateRange4ODIM",
                         start = input$dateRange4ODIM[1],
                         end   = input$dateRange4ODIM[2])
  })

  # Analyze and display bowler plots
  output$bowlerPlotsODIM <- renderPlot({
    analyzeBowlers(input$bowlerODIM,input$bowlerFuncODIM,"ODIM",input$dateRange4ODIM,input$staticIntv1ODIM)
  })

  output$bowlerPlotlyODIM <- renderPlotly({
    analyzeBowlers(input$bowlerODIM,input$bowlerFuncODIM, "ODIM",input$dateRange4ODIM,input$staticIntv1ODIM)
  })

  output$bowlerPlotODIM <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsODIM")
    }   else{
      if(input$bowlerFuncODIM == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsODIM")
      else
        plotlyOutput("bowlerPlotlyODIM")
    }

  })


  output$ODIMMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"ODIM")

  })

  output$ODIMMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"ODIM")

  })

  # Analyze and display ODIM Match table
  output$ODIMMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"ODIM")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintODIMMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"ODIM"))){
      tableOutput("ODIMMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableODIM == 1){
        plotOutput("ODIMMatchPlots")
      } else{
        plotlyOutput("ODIMMatchPlotly")
      }

    }

  })

  #################################### ODIM  Matches between 2 teams ######################
  # Set date range
  output$dateRange1ODIM <- renderUI({
    m <- matches2TeamsHelper(input, output,"ODIM")
    dateRangeInput("dateRange1ODIM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1ODIM,{
    updateDateRangeInput(session, "dateRange1ODIM",
                         start = input$dateRange1ODIM[1],
                         end   = input$dateRange1ODIM[2])
  })

  # Analyze and display ODIM Matches between 2 teams plot
  output$ODIMMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"ODIM")

  })

  output$ODIMMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"ODIM")

  })

  # Analyze and display ODIM Match table
  output$ODIMMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"ODIM")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintODIMMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1ODIM == 3){
      plotlyOutput("ODIMMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"ODIM"))){
      tableOutput("ODIMMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1ODIM == 1){
        plotOutput("ODIMMatch2TeamsPlots")
      } else if(input$plotOrTable1ODIM == 2){
        plotlyOutput("ODIMMatch2TeamsPlotly")
      }
    }

  })



  ################################ ODIM Teams's overall performance ##############################
  output$dateRange2ODIM <- renderUI({
    n <- teamPerfOverallHelper(input, output,"ODIM")
    dateRangeInput("dateRange2ODIM", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2ODIM,{
    updateDateRangeInput(session, "dateRange2ODIM",
                         start = input$dateRange2ODIM[1],
                         end   = input$dateRange2ODIM[2])
  })

  # Analyze overall ODIM team performance plots
  output$ODIMTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"ODIM")

  })

  output$ODIMTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"ODIM")

  })

  # Analyze and display ODIM Match table
  output$ODIMTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"ODIM")
    a

  })

  # Output either a table or a plot
  output$printOrPlotODIMTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("ODIMTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"ODIM"))){
      tableOutput("ODIMTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2ODIM == 1){
        plotOutput("ODIMTeamPerfOverallPlots")
      } else if(input$plotOrTable2ODIM == 2){
        plotlyOutput("ODIMTeamPerfOverallPlotly")
      }
    }
  })


  ##########################################################################################
  # ODI Women
  output$dateRange3ODIW<- renderUI({
    m <- batsmanHelper(input, output,input$batsmanODIW,"ODIW")
    dateRangeInput("dateRange3ODIW", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3ODIW,{
    updateDateRangeInput(session, "dateRange3ODIW",
                         start = input$dateRange3ODIW[1],
                         end   = input$dateRange3ODIW[2])
  })
  output$batsmanPlotsODIW <- renderPlot({
    analyzeBatsmen(input$batsmanODIW,input$batsmanFuncODIW, "ODIW",input$dateRange3ODIW,input$staticIntvODIW)

  })

  output$batsmanPlotlyODIW <- renderPlotly({
    analyzeBatsmen(input$batsmanODIW,input$batsmanFuncODIW, "ODIW",input$dateRange3ODIW,input$staticIntvODIW)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotODIW <- renderUI({
    if(input$staticIntvODIW == 1){
      plotOutput("batsmanPlotsODIW")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncODIW =="Dismissals of batsman" || input$batsmanFuncODIW == "Predict Runs of batsman")
        plotOutput("batsmanPlotsODIW")
      else
        plotlyOutput("batsmanPlotlyODIW")
    }

  })



  ################
  #Bowler
  output$dateRange4ODIW<- renderUI({
    m <- batsmanHelper(input, output,input$bowlerODIW,"ODIW")
    dateRangeInput("dateRange4ODIW", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4ODIW,{
    updateDateRangeInput(session, "dateRange4ODIW",
                         start = input$dateRange4ODIW[1],
                         end   = input$dateRange4ODIW[2])
  })
  # Analyze and display bowler plots
  output$bowlerPlotsODIW <- renderPlot({
    analyzeBowlers(input$bowlerODIW,input$bowlerFuncODIW,"ODIW",input$dateRange4ODIW,input$staticIntv1ODIW)
  })

  output$bowlerPlotlyODIW <- renderPlotly({
    analyzeBowlers(input$bowlerODIW,input$bowlerFuncODIW, "ODIW",input$dateRange4ODIW,input$staticIntv1ODIW)
  })

  output$bowlerPlotODIW <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsODIW")
    }   else{
      if(input$bowlerFuncODIW == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsODIW")
      else
        plotlyOutput("bowlerPlotlyODIW")
    }

  })


  output$ODIWMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"ODIW")

  })

  output$ODIWMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"ODIW")

  })

  # Analyze and display ODIW Match table
  output$ODIWMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"ODIW")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintODIWMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"ODIW"))){
      tableOutput("ODIWMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableODIW == 1){
        plotOutput("ODIWMatchPlots")
      } else{
        plotlyOutput("ODIWMatchPlotly")
      }

    }

  })

  #################################### ODIW  Matches between 2 teams ######################
  # Set date range
  output$dateRange1ODIW <- renderUI({
    m <- matches2TeamsHelper(input, output,"ODIW")
    dateRangeInput("dateRange1ODIW", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1ODIW,{
    m <- matches2TeamsHelper(input, output,"ODIW")
    updateDateRangeInput(session, "dateRange1ODIW",
                         start = input$dateRange1ODIW[1],
                         end   = input$dateRange1ODIW[2])
  })

  # Analyze and display ODIW Matches between 2 teams plot
  output$ODIWMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"ODIW")

  })

  output$ODIWMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"ODIW")

  })

  # Analyze and display ODIW Match table
  output$ODIWMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"ODIW")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintODIWMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1ODIW == 3){
      plotlyOutput("ODIWMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"ODIW"))){
      tableOutput("ODIWMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1ODIW == 1){
        plotOutput("ODIWMatch2TeamsPlots")
      } else if(input$plotOrTable1ODIW == 2){
        plotlyOutput("ODIWMatch2TeamsPlotly")
      }
    }

  })



  ################################ ODIW Teams's overall performance ##############################
  output$dateRange2ODIW <- renderUI({
    n <- teamPerfOverallHelper(input, output,"ODIW")
    dateRangeInput("dateRange2ODIW", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2ODIW,{
    m <- teamPerfOverallHelper(input, output,"ODIW")
    updateDateRangeInput(session, "dateRange2ODIW",
                         start = input$dateRange2ODIW[1],
                         end   = input$dateRange2ODIW[2])
  })

  # Analyze overall ODIW team performance plots
  output$ODIWTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"ODIW")

  })

  output$ODIWTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"ODIW")

  })

  # Analyze and display ODIW Match table
  output$ODIWTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"ODIW")
    a

  })

  # Output either a table or a plot
  output$printOrPlotODIWTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("ODIWTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"ODIW"))){
      tableOutput("ODIWTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2ODIW == 1){
        plotOutput("ODIWTeamPerfOverallPlots")
      } else if(input$plotOrTable2ODIW == 2){
        plotlyOutput("ODIWTeamPerfOverallPlotly")
      }
    }
  })



  ############################################################Caribbean Premier League ##############################
  output$dateRange3CPL<- renderUI({
    m <- batsmanHelper(input, output,input$batsmanCPL,"CPL")
    dateRangeInput("dateRange3CPL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3CPL,{
    updateDateRangeInput(session, "dateRange3CPL",
                         start = input$dateRange3CPL[1],
                         end   = input$dateRange3CPL[2])
  })

  # Analyze and display batsmen plots
  output$batsmanPlotsCPL <- renderPlot({
    analyzeBatsmen(input$batsmanCPL,input$batsmanFuncCPL, "CPL",input$dateRange3CPL,input$staticIntvCPL)

  })

  output$batsmanPlotlyCPL <- renderPlotly({
    analyzeBatsmen(input$batsmanCPL,input$batsmanFuncCPL, "CPL",input$dateRange3CPL,input$staticIntvCPL)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotCPL <- renderUI({
    if(input$staticIntvCPL == 1){
      plotOutput("batsmanPlotsCPL")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncCPL =="Dismissals of batsman" || input$batsmanFuncCPL == "Predict Runs of batsman")
        plotOutput("batsmanPlotsCPL")
      else
        plotlyOutput("batsmanPlotlyCPL")
    }

  })

  ################
  #Bowler
  output$dateRange4CPL<- renderUI({
    m <- batsmanHelper(input, output,input$bowlerCPL,"CPL")
    dateRangeInput("dateRange4CPL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4CPL,{
    updateDateRangeInput(session, "dateRange4CPL",
                         start = input$dateRange4CPL[1],
                         end   = input$dateRange4CPL[2])
  })

  # Analyze and display bowler plots
  output$bowlerPlotsCPL <- renderPlot({
    analyzeBowlers(input$bowlerCPL,input$bowlerFuncCPL,"CPL",input$dateRange4CPL,input$staticIntv1CPL)
  })

  output$bowlerPlotlyCPL <- renderPlotly({
    analyzeBowlers(input$bowlerCPL,input$bowlerFuncCPL, "CPL",input$dateRange4CPL,input$staticIntv1CPL)
  })

  output$bowlerPlotCPL <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsCPL")
    }   else{
      if(input$bowlerFuncCPL == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsCPL")
      else
        plotlyOutput("bowlerPlotlyCPL")
    }

  })


  ######################################## CPL Match  #############################################
  # Analyze and display T20 Match plot
  output$CPLMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"CPL")

  })

  output$CPLMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"CPL")

  })

  # Analyze and display CPL Match table
  output$CPLMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"CPL")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintCPLMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"CPL"))){
      tableOutput("CPLMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableCPL == 1){
        plotOutput("CPLMatchPlots")
      } else{
        plotlyOutput("CPLMatchPlotly")
      }

    }

  })

  #################################### CPL  Matches between 2 teams ######################
  # Set date range
  output$dateRange1CPL <- renderUI({
    m <- matches2TeamsHelper(input, output,"CPL")
    dateRangeInput("dateRange1CPL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1CPL,{
    m <- matches2TeamsHelper(input, output,"CPL")
    updateDateRangeInput(session, "dateRange1CPL",
                         start = input$dateRange1CPL[1],
                         end   = input$dateRange1CPL[2])
  })

  # Analyze and display CPL Matches between 2 teams plot
  output$CPLMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"CPL")

  })

  output$CPLMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"CPL")

  })

  # Analyze and display CPL Match table
  output$CPLMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"CPL")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintCPLMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1CPL == 3){
      plotlyOutput("CPLMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"CPL"))){
      tableOutput("CPLMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1CPL == 1){
        plotOutput("CPLMatch2TeamsPlots")
      } else if(input$plotOrTable1CPL == 2){
        plotlyOutput("CPLMatch2TeamsPlotly")
      }
    }

  })



  ################################ CPL Teams's overall performance ##############################

  output$dateRange2CPL <- renderUI({
    n <- teamPerfOverallHelper(input, output,"CPL")
    dateRangeInput("dateRange2CPL", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2CPL,{
    m <- teamPerfOverallHelper(input, output,"CPL")
    updateDateRangeInput(session, "dateRange2CPL",
                         start = input$dateRange2CPL[1],
                         end   = input$dateRange2CPL[2])

  })

  # Analyze overall CPL team performance plots
  output$CPLTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"CPL")

  })

  output$CPLTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"CPL")

  })

  # Analyze and display IPL Match table
  output$CPLTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"CPL")
    a

  })

  # Output either a table or a plot
  output$printOrPlotCPLTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("CPLTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"CPL"))){
      tableOutput("CPLTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2CPL == 1){
        plotOutput("CPLTeamPerfOverallPlots")
      } else if(input$plotOrTable2CPL == 2){
        plotlyOutput("CPLTeamPerfOverallPlotly")
      }
    }
  })


  ################################ Rank CPL ##############################


  # Display ranks
  output$dateRange5CPL<- renderUI({
    m <- helper(CPLTeamNames, "./cpl/cplBattingBowlingDetails")
    dateRangeInput("dateRange5CPL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5CPL,{
    updateDateRangeInput(session, "dateRange5CPL",
                         start = input$dateRange5CPL[1],
                         end   = input$dateRange5CPL[2])
    updateSliderInput(session, "minMatchesCPL",
                      min=(helper1(CPLTeamNames, input$dateRange5CPL, "./cpl/cplBattingBowlingDetails")[[1]]),
                      max = (helper1(CPLTeamNames, input$dateRange5CPL, "./cpl/cplBattingBowlingDetails")[[2]]),
                      value =round(((helper1(CPLTeamNames, input$dateRange5CPL, "./cpl/cplBattingBowlingDetails")[[1]]) +
                                      (helper1(CPLTeamNames, input$dateRange5CPL, "./cpl/cplBattingBowlingDetails")[[2]]))/1.333))
  })


  # Analyze and display CPL Match table
  output$CPLRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"CPL","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankCPLBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"CPL","batsmen"))){
      tableOutput("CPLRankBatsmenPrint")

    }
  })

  ########################################
  # Rank CPL Bowlers
  output$dateRange6CPL<- renderUI({
    m <- helper2(CPLTeamNames, "./cpl/cplBattingBowlingDetails")
    dateRangeInput("dateRange6CPL", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6CPL,{
    updateDateRangeInput(session, "dateRange6CPL",
                         start = input$dateRange6CPL[1],
                         end   = input$dateRange6CPL[2])
    updateSliderInput(session, "minMatches1CPL", # Set slider at 75$ between min & max
                      min=(helper3(CPLTeamNames, input$dateRange6CPL, "./cpl/cplBattingBowlingDetails")[[1]]),
                      max = (helper3(CPLTeamNames, input$dateRange6CPL, "./cpl/cplBattingBowlingDetails")[[2]]),
                      value =round(((helper3(CPLTeamNames, input$dateRange6CPL, "./cpl/cplBattingBowlingDetails")[[1]]) +
                                      (helper3(CPLTeamNames, input$dateRange6CPL, "./cpl/cplBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display CPL Match table
  output$CPLRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"CPL","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankCPLBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"CPL","bowlers"))){
      tableOutput("CPLRankBowlersPrint")

    }
  })


  ###############################################Suoer Smash League ###########################################
  ##########################################################################################################
  # Super Smash League
  output$dateRange3SSM<- renderUI({
    m <- batsmanHelper(input, output,input$batsmanSSM,"SSM")
    dateRangeInput("dateRange3SSM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange3SSM,{
    updateDateRangeInput(session, "dateRange3SSM",
                         start = input$dateRange3SSM[1],
                         end   = input$dateRange3SSM[2])
  })
  # Analyze and display batsmen plots
  output$batsmanPlotsSSM <- renderPlot({
    analyzeBatsmen(input$batsmanSSM,input$batsmanFuncSSM, "SSM",input$dateRange3SSM,input$staticIntvSSM)

  })

  output$batsmanPlotlySSM <- renderPlotly({
    analyzeBatsmen(input$batsmanSSM,input$batsmanFuncSSM, "SSM",input$dateRange3SSM,input$staticIntvSSM)

  })


  # Analyze and display batsmen plots
  output$batsmanPlotSSM <- renderUI({
    if(input$staticIntvSSM == 1){
      plotOutput("batsmanPlotsSSM")
    }
    else{
      #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
      if(input$batsmanFuncSSM =="Dismissals of batsman" || input$batsmanFuncSSM == "Predict Runs of batsman")
        plotOutput("batsmanPlotsSSM")
      else
        plotlyOutput("batsmanPlotlySSM")
    }

  })

  ################
  #Bowler
  output$dateRange4SSM<- renderUI({
    m <- batsmanHelper(input, output,input$bowlerSSM,"SSM")
    dateRangeInput("dateRange4SSM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange4SSM,{
    updateDateRangeInput(session, "dateRange4SSM",
                         start = input$dateRange4SSM[1],
                         end   = input$dateRange4SSM[2])
  })

  # Analyze and display bowler plots
  output$bowlerPlotsSSM <- renderPlot({
    analyzeBowlers(input$bowlerSSM,input$bowlerFuncSSM,"SSM",input$dateRange4SSM,input$staticIntv1SSM)
  })

  output$bowlerPlotlySSM <- renderPlotly({
    analyzeBowlers(input$bowlerSSM,input$bowlerFuncSSM, "SSM",input$dateRange4SSM,input$staticIntv1SSM)
  })

  output$bowlerPlotSSM <- renderUI({
    if(input$staticIntv1 == 1){
      plotOutput("bowlerPlotsSSM")
    }   else{
      if(input$bowlerFuncSSM == "Bowler's wickets prediction")
        plotOutput("bowlerPlotsSSM")
      else
        plotlyOutput("bowlerPlotlySSM")
    }

  })


  ######################################## SSM Match  #############################################
  # Analyze and display T20 Match plot
  output$SSMMatchPlots <- renderPlot({
    printOrPlotMatch(input, output,"SSM")

  })

  output$SSMMatchPlotly <- renderPlotly({
    printOrPlotMatch(input, output,"SSM")

  })

  # Analyze and display SSM Match table
  output$SSMMatchPrint <- renderTable({
    a <- printOrPlotMatch(input, output,"SSM")
    a

  })
  # Output either a table or a plot
  output$plotOrPrintSSMMatch <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"SSM"))){
      tableOutput("SSMMatchPrint")
    }
    else{ #Else plot
      if(input$plotOrTableSSM == 1){
        plotOutput("SSMMatchPlots")
      } else{
        plotlyOutput("SSMMatchPlotly")
      }

    }

  })

  #################################### SSM  Matches between 2 teams ######################
  # Set date range
  output$dateRange1SSM <- renderUI({
    m <- matches2TeamsHelper(input, output,"SSM")
    dateRangeInput("dateRange1SSM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange1SSM,{
    m <- matches2TeamsHelper(input, output,"SSM")
    updateDateRangeInput(session, "dateRange1SSM",
                         start = input$dateRange1SSM[1],
                         end   = input$dateRange1SSM[2])
  })

  # Analyze and display SSM Matches between 2 teams plot
  output$SSMMatch2TeamsPlots <- renderPlot({
    print("plot")
    printOrPlotMatch2Teams(input, output,"SSM")

  })

  output$SSMMatch2TeamsPlotly <- renderPlotly({
    print("plot")
    printOrPlotMatch2Teams(input, output,"SSM")

  })

  # Analyze and display SSM Match table
  output$SSMMatch2TeamsPrint <- renderTable({
    print("table")
    a <- printOrPlotMatch2Teams(input, output,"SSM")
    a
    #a
  })

  # Output either a table or a plot
  output$plotOrPrintSSMMatch2teams <-  renderUI({

    if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1SSM == 3){
      plotlyOutput("SSMMatch2TeamsPlotly")
    }
    # Check if output is a dataframe. If so, print
    else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"SSM"))){
      tableOutput("SSMMatch2TeamsPrint")
    }
    else{ #Else plot
      if(input$plotOrTable1SSM == 1){
        plotOutput("SSMMatch2TeamsPlots")
      } else if(input$plotOrTable1SSM == 2){
        plotlyOutput("SSMMatch2TeamsPlotly")
      }
    }

  })



  ################################ SSM Teams's overall performance ##############################
  output$dateRange2SSM <- renderUI({
    n <- teamPerfOverallHelper(input, output,"SSM")
    dateRangeInput("dateRange2SSM", label = h4("Date range"),
                   start = n[[1]],
                   end   = n[[2]],
                   min = n[[1]],
                   max= n[[2]])
  })

  observeEvent(input$dateRange2SSM,{
    m <- teamPerfOverallHelper(input, output,"SSM")
    updateDateRangeInput(session, "dateRange2SSM",
                         start = input$dateRange2SSM[1],
                         end   = input$dateRange2SSM[2])

  })

  # Analyze overall SSM team performance plots
  output$SSMTeamPerfOverallPlots <- renderPlot({
    printOrPlotTeamPerfOverall(input, output,"SSM")

  })

  output$SSMTeamPerfOverallPlotly <- renderPlotly({
    printOrPlotTeamPerfOverall(input, output,"SSM")

  })

  # Analyze and display IPL Match table
  output$SSMTeamPerfOverallPrint <- renderTable({
    a <- printOrPlotTeamPerfOverall(input, output,"SSM")
    a

  })

  # Output either a table or a plot
  output$printOrPlotSSMTeamPerfoverall <-  renderUI({

    if(input$overallperfFunc == "Win Loss Team vs All Opposition" && input$plotOrTable2 == 3){
      plotlyOutput("SSMTeamPerfOverallPlotly")
    }
    # Check if output is a dataframe. If so, print
    else  if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"SSM"))){
      tableOutput("SSMTeamPerfOverallPrint")
    }
    else{ #Else plot
      if(input$plotOrTable2SSM == 1){
        plotOutput("SSMTeamPerfOverallPlots")
      } else if(input$plotOrTable2SSM == 2){
        plotlyOutput("SSMTeamPerfOverallPlotly")
      }
    }
  })


  ################################ Rank SSM ##############################


  # Display ranks
  output$dateRange5SSM<- renderUI({
    m <- helper(SSMTeamNames, "./ssm/ssmBattingBowlingDetails")
    dateRangeInput("dateRange5SSM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange5SSM,{
    updateDateRangeInput(session, "dateRange5SSM",
                         start = input$dateRange5SSM[1],
                         end   = input$dateRange5SSM[2])
    updateSliderInput(session, "minMatchesSSM",
                      min=(helper1(SSMTeamNames, input$dateRange5SSM, "./ssm/ssmBattingBowlingDetails")[[1]]),
                      max = (helper1(SSMTeamNames, input$dateRange5SSM, "./ssm/ssmBattingBowlingDetails")[[2]]),
                      value =round(((helper1(SSMTeamNames, input$dateRange5SSM, "./ssm/ssmBattingBowlingDetails")[[1]]) +
                                      (helper1(SSMTeamNames, input$dateRange5SSM, "./ssm/ssmBattingBowlingDetails")[[2]]))/1.333))
  })


  # Analyze and display SSM Match table
  output$SSMRankBatsmenPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"SSM","batsmen")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankSSMBatsmen <-  renderUI({
    # Check if output is a dataframe. If so, print
    if(is.data.frame(a <- rankPlayers(input, output,"SSM","batsmen"))){
      tableOutput("SSMRankBatsmenPrint")

    }
  })

  ########################################
  # Rank SSM Bowlers
  output$dateRange6SSM<- renderUI({
    m <- helper2(SSMTeamNames, "./ssm/ssmBattingBowlingDetails")
    dateRangeInput("dateRange6SSM", label = h4("Date range"),
                   start = m[[1]],
                   end   = m[[2]],
                   min = m[[1]],
                   max= m[[2]])
  })

  observeEvent(input$dateRange6SSM,{
    updateDateRangeInput(session, "dateRange6SSM",
                         start = input$dateRange6SSM[1],
                         end   = input$dateRange6SSM[2])
    updateSliderInput(session, "minMatches1SSM", # Set slider at 75$ between min & max
                      min=(helper3(SSMTeamNames, input$dateRange6SSM, "./ssm/ssmBattingBowlingDetails")[[1]]),
                      max = (helper3(SSMTeamNames, input$dateRange6SSM, "./ssm/ssmBattingBowlingDetails")[[2]]),
                      value =round(((helper3(SSMTeamNames, input$dateRange6SSM, "./ssm/ssmBattingBowlingDetails")[[1]]) +
                                      (helper3(SSMTeamNames, input$dateRange6SSM, "./ssm/ssmBattingBowlingDetails")[[2]]))/1.333))
  })

  # Analyze and display SSM Match table
  output$SSMRankBowlersPrint <- renderTable({
    Sys.sleep(1.5)
    plot(runif(10))
    a <- rankPlayers(input, output,"SSM","bowlers")
    head(a,20)
  })

  # Output either a table or a plot
  output$rankSSMBowlers <-  renderUI({
    # Check if output is a dataframe. If so, print

    if(is.data.frame(a <- rankPlayers(input, output,"SSM","bowlers"))){
      tableOutput("SSMRankBowlersPrint")

    }
  })




})






