% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_plot_formations.R
\name{plot_formation}
\alias{plot_formation}
\title{Plot tactical lineup}
\usage{
plot_formation(
  dfFormation,
  colourHome = "red",
  colourHome2 = "white",
  colourAway = "blue",
  colourAway2 = "white",
  subPlayers,
  hometeam = NULL,
  awayteam = NULL,
  formHome = "3-3-2-3",
  formAway = NULL,
  subicon_just = c(1, -3)
)
}
\arguments{
\item{dfFormation}{Data frame with positional information as returned by \code{prep_formation_data}}

\item{colourHome}{Colour of home team (used as fill)}

\item{colourHome2}{Second colour of home team (used as colour of jersey number)}

\item{colourAway}{Colour of away team (used as fill)}

\item{colourAway2}{Second colour of away team (used as colour of jersey number)}

\item{subPlayers}{Dataframe containing names of players that were substituted throughout the game}

\item{hometeam}{Name of home team}

\item{awayteam}{Name of away team}

\item{formHome}{Tactical formation of home team, e.g. "4-3-2-1" (passed to plot subtitle)}

\item{formAway}{Tactical formation of away team}

\item{subicon_just}{numeric values for placing the icons indicating a  substitution}
}
\value{
A ggplot object
}
\description{
Plot tactical lineup
}
\examples{
data(eventData)

dfForm <- prep_formation_data(eventData$tactics.lineup[[1]], team.name = "BVB") \%>\%
  bind_rows(prep_formation_data(eventData$tactics.lineup[[2]], team.name = "Bayern Munich"))
subPlayers <- eventData \%>\% filter(type.name == "Substitution") \%>\%
  select(team.name, player.name, minute, second, starts_with("Sub"))
plot_formation(dfFormation = dfForm, colourHome = "yellow", colourHome2 = "black", colourAway = "red", hometeam = "BVB",
               awayteam = "Bayern Munich", subPlayers = subPlayers,
               formHome = "4-2-3-1", formAway = "4-2-3-1")

}
