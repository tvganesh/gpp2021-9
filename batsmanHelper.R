batsmanHelper <- function(input,output,batsman,t20type="IPL"){
    # Return when name is NULL at start
    if(is.null(batsman))
        return()

    print(batsman)

    if(t20type == "IPL"){
        print("ipl")
        dir1="./ipl/iplBattingBowlingDetails/"
        # Check and get the team indices of IPL teams in which the batsman has played
        i <- getTeamIndex(batsman, dir1)
        cat("i=",i)
        # Get the team names
        teamNames <- getTeams(i)
        cat("names=",teamNames)
    }
    else if (t20type == "T20M"){
        print("men")
        dir1="./t20/t20BattingBowlingDetails/"
        i <- getT20MTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getT20MTeams(i)
    } else if (t20type == "T20W"){
        print("Women")
        dir1="./t20/t20WomenBattingBowlingDetails/"
        i <- getT20WTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getT20WTeams(i)

    } else if (t20type == "BBL"){
        print("BBL")
        dir1="./bbl/bblBattingBowlingDetails/"
        i <- getBBLTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getBBLTeams(i)

    } else if (t20type == "NTB"){
        print("NTB")
        dir1="./ntb/ntbBattingBowlingDetails/"
        i <- getNTBTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getNTBTeams(i)

    } else if (t20type == "PSL"){
        print("PSL")
        dir1="./psl/pslBattingBowlingDetails/"
        i <- getPSLTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getPSLTeams(i)

    } else if (t20type == "WBB"){
        print("WBB")
        dir1="./wbb/wbbBattingBowlingDetails/"
        i <- getWBBTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getWBBTeams(i)

    } else if (t20type == "ODIM"){
        print("ODIM")
        dir1="./odi/odiBattingBowlingDetails/"
        i <- getODIMTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getODIMTeams(i)

    } else if (t20type == "ODIW"){
        print("ODIW")
        dir1="./odi/odiWomenBattingBowlingDetails/"
        i <- getODIWTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getODIWTeams(i)

    } else if (t20type == "CPL"){
        print("CPL")
        dir1="./cpl/cplBattingBowlingDetails/"
        i <- getCPLTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getCPLTeams(i)

    } else if (t20type == "SSM"){
        print("SSM")
        dir1="./ssm/ssmBattingBowlingDetails/"
        i <- getSSMTeamIndex(batsman, dir1)
        # Get the team names
        teamNames <- getSSMTeams(i)

    }

    cat("i=",i,"\n")
    cat("analyze=",getwd())

    # Check if file exists in the directory. This check is necessary when moving between matchType

    batsmanDF <- NULL
    # Create a consolidated Data frame of batsman for all IPL teams played
    for (i in seq_along(teamNames)){
        df <- getBatsmanDetails(team=teamNames[i],name=batsman,dir=dir1)
        batsmanDF <- rbind(batsmanDF,df)
    }
    maxDate= as.Date(max(batsmanDF$date))
    minDate= as.Date(min(batsmanDF$date))
    cat("Date min=",as.Date(minDate),"max=",as.Date(maxDate),"\n")
    return(list(minDate,maxDate))
}
