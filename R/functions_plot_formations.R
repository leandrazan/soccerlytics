# functions for plotting player formations


get_formation_loc <- function(df) {

  formation_data <- data.frame(
    position.id = c(1,2,3,4,5,6,7,9,10,11,8,12,13,14,15,16,17,18,19,20,21,25,22,23,24),
    location.x = c(10,20,20,20,20,20,30,30,30,30,30,40,40,40,40,40,50,50,50,50,50,58,58,58,58),
    location.y = c(40,10,25,40,55,70,10,25,40,55,70,10,25,40,55,70,10,25,40,55,70,40,25,40,55)
  )

 dplyr::left_join(df, formation_data, by = c("position.id"))

}


#' Prepare a data frame to visualise tactical formation
#'
#' @param df A data frame as contained in statsbomb data in `tactics.lineup`,
#' containing information on player names, position name, position id according to
#' statsbomb's definition, jersey numbers
#' @param team.name The team's name
#' @param player_var_name Variable/column name of player names
#' @param pos_form_var_name Variable/column name of position name
#' @param pos_id_var_name Variable/column name of position id
#' @param jerseynum_var_name Variable/column name of jersey number
#'
#' @return A data frame, columns are renamed and locations of positions are added
#' @export
#'
#' @examples  # see help of `plot_formation`
prep_formation_data <- function(df, team.name, player_var_name = "player.name",
                                pos_form_var_name = "position.name",
                                pos_id_var_name = "position.id",
                                jerseynum_var_name = "jersey_number") {

  dfFormation <- df %>% dplyr::rename(player.name = player_var_name,
                       position.name = pos_form_var_name,
                       position.id = pos_id_var_name,
                       jersey.number = jerseynum_var_name,
                       )
  dfFormation$team.name <- team.name
  get_formation_loc(dfFormation)

}



#' Plot tactical lineup
#'
#' @param dfFormation Data frame with positional information as returned by `prep_formation_data`
#' @param colourHome Colour of home team (used as fill)
#' @param colourHome2 Second colour of home team (used as colour of jersey number)
#' @param colourAway Colour of away team (used as fill)
#' @param colourAway2 Second colour of away team (used as colour of jersey number)
#' @param subPlayers Dataframe containing names of players that were substituted throughout the game
#' @param hometeam Name of home team
#' @param awayteam Name of away team
#' @param formHome Tactical formation of home team, e.g. "4-3-2-1" (passed to plot subtitle)
#' @param formAway Tactical formation of away team
#' @param subicon_just numeric values for placing the icons indicating a  substitution
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' data(eventData)
#'
#' dfForm <- prep_formation_data(eventData$tactics.lineup[[1]], team.name = "BVB") %>%
#'   bind_rows(prep_formation_data(eventData$tactics.lineup[[2]], team.name = "Bayern Munich"))
#' subPlayers <- eventData %>% filter(type.name == "Substitution") %>%
#'   select(team.name, player.name, minute, second, starts_with("Sub"))
#' plot_formation(dfFormation = dfForm, colourHome = "yellow", colourHome2 = "black", colourAway = "red", hometeam = "BVB",
#'                awayteam = "Bayern Munich", subPlayers = subPlayers,
#'                formHome = "4-2-3-1", formAway = "4-2-3-1")
#'
plot_formation <- function(dfFormation, colourHome = "red", colourHome2 = "white",
                           colourAway = "blue", colourAway2 = "white",
                           subPlayers,
                           hometeam = NULL, awayteam = NULL,
                           formHome = "3-3-2-3", formAway = NULL,
                           subicon_just = c(1,-3)) {

  if(is.null(hometeam) & is.null(awayteam)) { stop("Please provide the team name(s).")}

  dfFormation <- dfFormation %>% dplyr::filter(!is.na(player.name))

  dfFormation$team.name <-factor(dfFormation$team.name, levels = c(hometeam, awayteam))

  dfFormation <- dfFormation %>%
     dplyr::mutate(location.x = ifelse(team.name == awayteam, 120 - location.x, location.x),
           location.y = ifelse(team.name == awayteam, 80 - location.y, location.y))

  dfFormation <- dfFormation %>% dplyr::mutate(Substituted = (player.name %in% subPlayers$player.name))

  dfFormation <- dfFormation %>%
    dplyr::mutate(purrr::map_chr(position.name, ~ get_pos_abb(.x)),
           lastName = purrr::map_chr(player.name,
                                     ~ get_last_name(.x)))

  labels_sub <- dfFormation %>% dplyr::filter(Substituted == TRUE)


  formPlot <- dfFormation %>%
    ggplot(aes(x = location.x, y = location.y, fill = team.name)) +
    draw_pitch(dimensions = c(120, 80), palette = "classic")+
    geom_point(size = 12, shape = 21, stroke = 1, color = "white")+
    scale_fill_manual(values = c(colourHome, colourAway))+
    geom_text(aes(label = jersey.number, colour = team.name), size = 6, show.legend = FALSE)+
    scale_colour_manual(values = c(colourHome2, colourAway2))+
    geom_text(data = dfFormation %>% filter(team.name == hometeam), aes(label = lastName), vjust = -2, lwd = 4)+
    geom_text(data = dfFormation %>% filter(team.name == awayteam), aes(label = lastName), vjust = 3, lwd = 4)+
    geom_point(data = labels_sub,
               aes(x = location.x + subicon_just[1], y = location.y + subicon_just[2]),
               color = "white", shape = 19, size = 6,
               show.legend = FALSE)+
    geom_segment(data = labels_sub,
                 aes(x = location.x + subicon_just[1], y = location.y + subicon_just[2] +1,
                     xend = location.x + subicon_just[1],
                     yend =location.y + subicon_just[2]-1),
                 arrow = arrow(length = unit(0.1, "inches")), lwd= 1,
                 lineend = "round", linejoin = "bevel")+
    labs(fill = "Team:", title = "Tactical formations",
         subtitle = paste(hometeam, formHome,  "vs.", awayteam, formAway)) +theme_bw() +
    theme(axis.text = element_blank(), legend.position = "bottom") + theme(axis.title = element_blank())




  formPlot
}


