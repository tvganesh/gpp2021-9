#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 07 Jan 2021
# File: rankPlayers.R
# More details: https://gigadom.in/
#
#########################################################################################################


rankPlayers <- function(input,output, type="IPL",player="batsmen") {
  cat("Entering rank Players\n")
  currDir= getwd()
  cat("currdir", getwd(),"\n")

  if (type == "IPL"){
    output$Mode <- renderUI({
        selectInput('runsOverSR', 'Mode',choices=runsVsSR,selected=input$runsOverSR)
    })

    output$Mode1 <- renderUI({
      selectInput('wicketsOverER', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverER)
    })
  } else if (type == "T20M"){
    print("T20M")
      output$ModeT20M <- renderUI({
        selectInput('runsOverSRT20M', 'Mode',choices=runsVsSR,selected=input$runsOverSRT20M)
      })

      output$Mode1T20M <- renderUI({
        selectInput('wicketsOverERT20M', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERT20M)
      })

  } else if (type == "T20W"){
    print("Here111")
    output$ModeT20W <- renderUI({
      selectInput('runsOverSRT20W', 'Mode',choices=runsVsSR,selected=input$runsOverSRT20W)
    })

    output$Mode1T20W <- renderUI({
      selectInput('wicketsOverERT20W', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERT20W)
    })

  } else if (type == "BBL"){
    print("BBL")
    output$ModeBBL <- renderUI({
      selectInput('runsOverSRBBL', 'Mode',choices=runsVsSR,selected=input$runsOverSRBBL)
    })

    output$Mode1BBL <- renderUI({
      selectInput('wicketsOverERBBL', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERBBL)
    })

  } else if (type == "NTB"){
    print("NTB")
    output$ModeNTB <- renderUI({
      selectInput('runsOverSRNTB', 'Mode',choices=runsVsSR,selected=input$runsOverSRNTB)
    })

    output$Mode1NTB <- renderUI({
      selectInput('wicketsOverERNTB', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERNTB)
    })

  } else if (type == "PSL"){
    print("PSL")
    output$ModePSL <- renderUI({
      selectInput('runsOverSRPSL', 'Mode',choices=runsVsSR,selected=input$runsOverSRPSL)
    })

    output$Mode1PSL <- renderUI({
      selectInput('wicketsOverERPSL', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERPSL)
    })

  } else if (type == "WBB"){
    print("WBB")
    output$ModeWBB <- renderUI({
      selectInput('runsOverSRWBB', 'Mode',choices=runsVsSR,selected=input$runsOverSRWBB)
    })

    output$Mode1WBB <- renderUI({
      selectInput('wicketsOverERWBB', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERWBB)
    })

  } else if (type == "CPL"){
    print("CPL")
    output$ModeCPL <- renderUI({
      selectInput('runsOverSRCPL', 'Mode',choices=runsVsSR,selected=input$runsOverSRCPL)
    })

    output$Mode1CPL <- renderUI({
      selectInput('wicketsOverERCPL', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERCPL)
    })

  } else if (type == "SSM"){
    print("SSM")
    output$ModeSSM <- renderUI({
      selectInput('runsOverSRSSM', 'Mode',choices=runsVsSR,selected=input$runsOverSRSSM)
    })

    output$Mode1SSM <- renderUI({
      selectInput('wicketsOverERSSM', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERSSM)
    })

  }

  cat("\nRank players date5=",input$dateRange5[1],"input$minMatches1=", input$minMatches1,"input$wicketsOverER=",input$wicketsOverER,"\n")
  if(is.null(input$dateRange5[1])){
    print("1")
    return
  }


  if(type == "IPL"){
    if(player=="batsmen"){
        if(is.null(input$dateRange5)){
          print("Null date. Returning")
          return
        } else {
          print("Date ok")
          a <-rankT20Batsmen(IPLTeamNames,"./ipl/iplBattingBowlingDetails",input$minMatches, input$dateRange5,input$runsOverSR)
        }

    } else if (player =="bowlers"){
      if(is.null(input$dateRange6)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Bowlers(IPLTeamNames,"./ipl/iplBattingBowlingDetails",input$minMatches1, input$dateRange6,input$wicketsOverER)
      }


    }
  } else if (type ==  "T20M"){
    if(player=="batsmen"){
      if(is.null(input$dateRange5T20M)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(T20MTeamNames,"./t20/t20BattingBowlingDetails",input$minMatchesT20M,input$dateRange5T20M,input$runsOverSRT20M)
      }

    } else if (player =="bowlers"){
      if(is.null(input$dateRange6T20M)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(T20MTeamNames,"./t20/t20BattingBowlingDetails",input$minMatches1T20M, input$dateRange6T20M,input$wicketsOverERT20M)
      }

    }
  } else if (type ==  "T20W"){
    if(player=="batsmen"){
      cat("runs vs SR T20WW=",input$runsOverSRT20W,"\n")
      if(is.null(input$dateRange5T20W)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails",input$minMatchesT20W,input$dateRange5T20W,input$runsOverSRT20W)
      }


    } else if (player =="bowlers"){
      if(is.null(input$dateRange6T20W)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails",input$minMatches1T20W, input$dateRange6T20W,input$wicketsOverERT20W)
      }


    }
  } else if (type ==  "BBL"){
    if(player=="batsmen"){
      if(is.null(input$dateRange5BBL)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(BBLTeamNames,"./bbl/bblBattingBowlingDetails",input$minMatchesBBL,input$dateRange5BBL,input$runsOverSRBBL)
      }

    } else if (player =="bowlers"){
      if(is.null(input$dateRange6BBL)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(BBLTeamNames,"./bbl/bblBattingBowlingDetails",input$minMatches1BBL, input$dateRange6BBL,input$wicketsOverERBBL)
      }


    }
  } else if (type ==  "NTB"){
    if(player=="batsmen"){
      if(is.null(input$dateRange5NTB)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(NTBTeamNames,"./ntb/ntbBattingBowlingDetails",input$minMatchesNTB,input$dateRange5NTB,input$runsOverSRNTB)
      }

    } else if (player =="bowlers"){
      if(is.null(input$dateRange6NTB)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(NTBTeamNames,"./ntb/ntbBattingBowlingDetails",input$minMatches1NTB, input$dateRange6NTB,input$wicketsOverERNTB)
      }

    }
  } else if (type ==  "PSL"){
    if(player=="batsmen"){
      if(is.null(input$dateRange5PSL)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(PSLTeamNames,"./psl/pslBattingBowlingDetails",input$minMatchesPSL,input$dateRange5PSL,input$runsOverSRPSL)
      }

    } else if (player =="bowlers"){
      if(is.null(input$dateRange6PSL)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(PSLTeamNames,"./psl/pslBattingBowlingDetails",input$minMatches1PSL, input$dateRange6PSL,input$wicketsOverERPSL)
      }


    }
  } else if (type ==  "WBB"){
    if(player=="batsmen"){
      if(is.null(input$dateRange5WBB)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(WBBTeamNames,"./wbb/wbbBattingBowlingDetails",input$minMatchesWBB,input$dateRange5WBB,input$runsOverSRWBB)
      }

    } else if (player =="bowlers"){
      if(is.null(input$dateRange5WBB)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(WBBTeamNames,"./wbb/wbbBattingBowlingDetails",input$minMatches1WBB, input$dateRange6WBB,input$wicketsOverERWBB)
      }

    }
  } else if (type ==  "CPL"){
    if(player=="batsmen"){
      if(is.null(input$dateRange5CPL)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(CPLTeamNames,"./cpl/cplBattingBowlingDetails",input$minMatchesCPL,input$dateRange5CPL,input$runsOverSRCPL)
      }
    } else if (player =="bowlers"){
      if(is.null(input$dateRange6CPL)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(CPLTeamNames,"./cpl/cplBattingBowlingDetails",input$minMatches1CPL, input$dateRange6CPL,input$wicketsOverERCPL)
      }
    }
  } else if (type ==  "SSM"){
    if(player=="batsmen"){
      if(is.null(input$dateRange5SSM)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <- rankT20Batsmen(SSMTeamNames,"./ssm/ssmBattingBowlingDetails",input$minMatchesSSM,input$dateRange5SSM,input$runsOverSRSSM)
      }
    } else if (player =="bowlers"){
      if(is.null(input$dateRange6SSM)){
        print("Null date. Returning")
        return
      } else {
        print("Date ok")
        a <-rankT20Bowlers(SSMTeamNames,"./ssm/ssmBattingBowlingDetails",input$minMatches1SSM, input$dateRange6SSM,input$wicketsOverERSSM)
      }

    }
  }
  setwd(currDir)
  cat("Exiting rank Players\n")
  a
}
