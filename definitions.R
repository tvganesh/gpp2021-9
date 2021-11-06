#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 07 Jan 2021
# File: definitions.R
# More details: https://gigadom.in/
#
#########################################################################################################
# Function names for IPL  batsman analysis
batsmanFuncs <- c("Batsman Runs vs. Deliveries",
                        "Batsman 4s & 6s",
                       "Dismissals of batsman",
                       "Batsman's Runs vs Strike Rate",
                       "Batsman's Moving Average",
                       "Batsman's Cumulative Average Runs",
                       "Batsman's Cumulative Strike Rate",
                       "Batsman's Runs against Opposition",
                       "Batsman's  Runs at Venue",
                       "Predict Runs of batsman")



# Function names for IPL bowler analysis
bowlerFuncs <- c("Mean Economy Rate of bowler",
                     "Mean runs conceded by bowler",
                     "Bowler's Moving Average",
                     "Bowler's Cumulative Avg. Wickets",
                     "Bowler's Cumulative Avg. Economy Rate",
                     "Bowler's Wicket Plot",
                     "Bowler's Wickets against opposition",
                     "Bowler's Wickets at Venues",
                     "Bowler's wickets prediction")


# Functions to analyze IPL matches
matchFuncs <- c("Match Batting Scorecard",
                   "Batting Partnerships",
                   "Batsmen vs Bowlers",
                   "Bowling Wicket Kind",
                   "Match Bowling Scorecard",
                   "Bowling Wicket Runs",
                   "Bowling Wicket Match",
                   "Bowler vs Batsmen",
                   "Match Worm Graph",
                   "Match Worm Wicket Graph",
                   "Team Runs Across 20 overs",
                   "Team Strike rate Across 20 overs",
                   "Team Wickets Across 20 overs",
                   "Team ER Across 20 overs"
                )


# Functions to analyze IPL matches
matches2TeamsFuncs <- c("Team Batsmen Batting Partnerships All Matches",
                           "Team Batting Scorecard All Matches",
                            "Team Batsmen vs Bowlers all Matches",
                            "Team Wickets Opposition All Matches",
                           "Team Bowling Scorecard All Matches",
                            "Team Bowler vs Batsmen All Matches",
                            "Team Bowlers Wicket Kind All Matches",
                            "Team Bowler Wicket Runs All Matches",
                             "Team-vs-team Runs across 20 overs",
                             "Team-vs-team Strike rate across 20 overs",
                             "Team-vs-team Wickets across 20 overs",
                             "Team-vs-team ER across 20 overs",
                             "Top Runs batsmen across 20 overs",
                             "Top Strike rate batsmen across 20 overs",
                             "Top Wickets bowlers across 20 overs",
                             "Top Economy rate bowlers across 20 overs",
                             "Win Loss Head-to-head All Matches")

# Functions to analyze IPL teams' overall performance
teamOverallPerfFunc <- c("Team Batsmen Partnerships Overall",
                            "Team Batting Scorecard Overall",
                           "Team Batsmen vs Bowlers Overall",
                           'Team Bowler vs Batsmen Overall',
                           "Team Bowling Scorecard Overall",
                           "Team Bowler Wicket Kind Overall",
                           "Team Mean Runs Overall across 20 overs",
                           "Team Mean Strike Rate Overall across 20 overs",
                           "Team Mean Wickets Overall across 20 overs",
                           "Team Mean Economy Rate Overall across 20 overs",
                           "Top Runs batsmen  Overall across 20 overs",
                           "Top Strike rate Overall batsmen across 20 overs",
                           "Top Wickets Overall bowlers across 20 overs",
                           "Top Economy rate Overall bowlers across 20 overs",
                           "Win Loss Team vs All Opposition")




# Runs vs Strike Rate
runsVsSR = c("Runs over Strike rate", "Strike rate over Runs")

# Wickets vs Economy Rate
wicketsVsER = c("Wickets over Economy rate", "Economy rate over Wickets")
