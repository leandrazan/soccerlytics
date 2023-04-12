
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soccerlytics

<!-- badges: start -->
<!-- badges: end -->

The goal of soccerlytics is to …

## Installation

You can install the development version of soccerlytics from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("leandrazan/soccerlytics")
```

``` r
#library(soccerlytics)
```

### Load data

The visualistions will be carried out based on the publicly availabe
event data set from the 2012/2013 Champion’s League Final. The data is
made available by StatsBomb.

``` r
# # load Data
# FreeComp <- StatsBombR::FreeCompetitions()
# FreeMatch <-  StatsBombR::FreeMatches(FreeComp %>% filter(season_name == "2012/2013"))
# 
# # filter match with id 18240 (CL final)
# FreeMatch <- FreeMatch %>% filter(match_id == 18240)
# 
# FreeMatch
# 
# eventData <- StatsBombR::get.matchFree(FreeMatch)
# 
# ## clean eventData (unnest several columns)
# eventData <- StatsBombR::allclean(eventData)
# eventData
```

### Tactical line up

To plot the tactical line up, we first extract the lineup dataframes
within the event dataset and use the preparation function
`prep_formation_data` which can then be passed to the `plot_formation`
function, along with the additional information of which player was
substituted, here provided in the dataframe `subPlayers`.

``` r
# dfForm <- prep_formation_data(eventData$tactics.lineup[[1]], team.name = "BVB") %>% 
#   bind_rows(prep_formation_data(eventData$tactics.lineup[[2]], team.name = "Bayern Munich"))
# subPlayers <- eventData %>% filter(type.name == "Substitution") %>% 
#   select(team.name, player.name, minute, second, starts_with("Sub"))
# plot_formation(dfFormation = dfForm, colourHome = "yellow", colourHome2 = "black", colourAway = "red", hometeam = "BVB", 
#                awayteam = "Bayern Munich", subPlayers = subPlayers, 
#                formHome = "4-2-3-1", formAway = "4-2-3-1")
```
