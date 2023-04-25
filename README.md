
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soccerlytics

<!-- badges: start -->
<!-- badges: end -->

soccerlytics offers several functions that visualise analytical aspects
of football event data.

## Installation

You can install the development version of soccerlytics from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("leandrazan/soccerlytics")
```

``` r
library(tidyverse)
library(soccerlytics)
```

### Load data

The visualistions will be carried out based on the publicly availabe
event data set from the 2012/2013 Champion’s League Final. The data is
made available by StatsBomb (<https://github.com/statsbomb/open-data>)
and is provided as example data set in soccerlytics. It can be loaded
with

``` r
data("eventData")
```

### Tactical line up

To plot the tactical line up, we first extract the lineup dataframes
within the event dataset and use the preparation function
`prep_formation_data` which can then be passed to the `plot_formation`
function, along with the additional information of which player was
substituted, here provided in the dataframe `subPlayers`.

``` r
# prepare the dataframes of tactical line ups for both teams and bind the resulting dfs
dfForm <- prep_formation_data(eventData$tactics.lineup[[1]], team.name = "BVB") %>% 
  bind_rows(prep_formation_data(eventData$tactics.lineup[[2]], team.name = "Bayern Munich"))
# get information about players that were substituted
subPlayers <- eventData %>% filter(type.name == "Substitution") %>% 
  select(team.name, player.name, minute, second, starts_with("Sub"))
# visualise the tactical formations
plot_formation(dfFormation = dfForm, colourHome = "#FDE100", colourHome2 = "black",
               colourAway = "#DC052D", hometeam = "BVB", 
               awayteam = "Bayern Munich", subPlayers = subPlayers, 
               formHome = "4-2-3-1", formAway = "4-2-3-1", subicon_just = c(2, -2))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="90%" style="display: block; margin: auto;" />

## Match dynamics

The function `match_dynamics` can visualise the developement of several
statistics over the duration of the match. You can choose the time
intervals within which the statistic of interest (such as Pass accuracy,
ball possession in %, or pressure events adjusted for opponent ball
possession) is computed, to see how it changes over time. Further,
vertical lines with Shots that were taken and goals that were scored are
shown. Here, one can filter for shots with outcomes that are considered
relevant, e.g. goals and saved shots only. Further, one can choose the
official club colours and set the plot theme such that the background is
either white, light gray or dark gray, depending on what is best for
club colours.

We start with ball possession in %, calculated every 5 minutes.

``` r
match_dynamics(eventData, binsize = 5, type  = "Possession", hometeam = "Borussia Dortmund", 
               colourHome = "#FDE100", colourAway = "#DC052D", theme = "dark")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="75%" style="display: block; margin: auto;" />
Now we visualise pass accuracy within 5 minute intervals.

``` r
match_dynamics(eventData, binsize = 5, type  = "Pass", hometeam = "Borussia Dortmund", 
               colourHome = "#FDE100", colourAway = "#DC052D", theme = "dark")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="75%" style="display: block; margin: auto;" />

Compare the latter to pass accurracy within 10 minute intervals, and
plot only shots that were goals.

``` r
match_dynamics(eventData, binsize = 10, type  = "Pass", 
               outcome_names = list(pass = NA, shot = "Goal"), hometeam = "Borussia Dortmund", 
               colourHome = "#FDE100", colourAway = "#DC052D", theme = "dark")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="75%" style="display: block; margin: auto;" />

We can also visualise cumulated xG values.

``` r
match_dynamics(eventData, binsize = 5, type  = "xg", hometeam = "Borussia Dortmund", 
               colourHome = "#FDE100", colourAway = "#DC052D", theme = "dark")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="75%" style="display: block; margin: auto;" />

## Attacking zones

As in Bundesliga Match facts, the soccerlytics package can visualise the
attacks per zone. Here, every pass or carry that starts in the first two
thirds of the pitch and ends in the final third is counted as an attack.
The end location of the event determines to which zone the attack is
assigned.

``` r
dfatt <- compute_attacking_zones(eventData)
plot_attack_zones(dfatt, team = "Borussia Dortmund")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="75%" style="display: block; margin: auto;" />

``` r
plot_attack_zones(dfatt, team = "Bayern Munich", leftToRight = FALSE)
```

<img src="man/figures/README-unnamed-chunk-9-2.png" width="75%" style="display: block; margin: auto;" />

## Shots

The function `plot_shotmaps` can visualise goal scoring opportunities.
It takes an event data set as input and returns a list of ggplots, one
plot with information on the outcome of the shot (e.g. Goal, Saved, …)
and one plot with information on the xG value of the shot, and whether
or not a Goal was scored.

``` r
shotmaps <- plot_shotmaps(eventData)
shotmaps$shot_types
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
shotmaps$shot_xg
```

<img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" />

### Dangerous Passes

Heatmap of dangerous passes, where a pass is considered dangerous when
it was made within a given time window shortly before a shot was taken
(e.g. 15s).

First, one filters the dangerous passes with the function
`get_danger_passes`. The resulting dataframe is then passed to
`plot_danger_passes`. For Borussia Dortmund, the pass locations and the
respective heatmap of dangerous passes look as follows.

``` r
df_dp <- get_danger_passes(df = eventData, shot_window = 20, team = "Borussia Dortmund")
plots <- plot_dangerpass_heatmap(df_dp, shot_window = 20, fill_limits = c(1, 12))
plots$pointplot
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="60%" style="display: block; margin: auto;" />

``` r
plots$heatmap
```

<img src="man/figures/README-unnamed-chunk-11-2.png" width="60%" style="display: block; margin: auto;" />
For Bayern Munich, the heatmap looks as follows

``` r
df_dp <- get_danger_passes(df = eventData, shot_window = 20, team = "Bayern Munich")
plot_dangerpass_heatmap(df_dp, shot_window = 20, fill_limits = c(1, 15),
                        leftToRight = FALSE)$heatmap
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="60%" style="display: block; margin: auto;" />

## Pass heatmap

With `pass_heatmap` you can create a passing heatmap, where for your
chosen number of bins the total number of passes beginning in a given
bin is calculated, seperately for successful and failed passes. Further,
the average direction of passes is visualised with arrows, where the
arrow length is proportional to the pass length.

``` r
pass_heatmap(eventData, team = "Borussia Dortmund")
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

``` r
pass_heatmap(eventData, team = "Bayern Munich", LeftToRight = FALSE, xBins = 6, yBins =4)
```

<img src="man/figures/README-unnamed-chunk-13-2.png" width="100%" />

## Possession loss heatmap

`poss_loss_heatmap` generates a heatmap showing the distribution of
possession losses on the pitch. The number of possession losses per bin
is counted and normalised by the total number of possession losses.
Again, you can choose how many bins in x and y direction are used.

``` r
poss_loss_heatmap(eventData, "Borussia Dortmund", xBins = 6, yBins = 4)
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="60%" style="display: block; margin: auto;" />

``` r
poss_loss_heatmap(eventData, "Bayern Munich", LeftToRight = FALSE, xBins = 6, yBins = 4)
```

<img src="man/figures/README-unnamed-chunk-14-2.png" width="60%" style="display: block; margin: auto;" />

## (position-based) xT Model

The position-based expected threat model developed by Karun Singh (see
<https://karun.in/blog/expected-threat.html> for the original post) is
implemented in the function `compute_xT`. Currently it only works with
data in SPADL format. For more information on the model and how to use
the function, see the respective vignette.

``` r
data(spadlData)
prep_dat <- prepare_data(spadlData, bins_x = 16, bins_y = 12)
xT <- compute_xT(prep_dat$prep_data, itermax = 5)
#> [1] 5
#>   max_diff 
#> 0.02017816
xT_tib <- prep_dat$bin_centers %>% left_join(xT$xT_final)
xT_tib %>% ggplot(aes( x = x_center, y = y_center, fill = xT)) +
  geom_tile(width = 105/16, height = 68/12, colour = "gray")+
  scale_fill_gradient(low = "white", high = "darkred")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  geom_text(aes(label = round(xT, 2)))+ 
  theme(text = element_text(size = 20), axis.title = element_blank())+ 
  ggtitle("xT after 5 iterations")
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="60%" style="display: block; margin: auto;" />
