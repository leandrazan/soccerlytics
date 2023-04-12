
#' Plot match dynamics (variable over time)
#'
#' @param df The event data set
#' @param binsize The bin size (in minutes) within which to compute the variable of interest
#' @param type Name of the variable for which to compute the dynamics. Must be one of
#' `Possession`, `Pass`, `Pressure`, `xg`.
#' @param outcome_names A list containing outcome names for different variables which are then
#'  considered in the statistics, e.g. `outcome_names = list(pass = NA, shot = "Saved", "Goal")` only plots
#'  vertical lines for shots that were either saved or a goal, and only considers passes that were complete
#'  (which is NA in event data)
#' @param ylim Adjusts the limits of y-axis
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' FreeComp <- StatsBombR::FreeCompetitions()
#' FreeMatch <-  StatsBombR::FreeMatches(FreeComp %>% filter(season_name == "2012/2013"))
#' # filter match with id 18240
#' FreeMatch <- FreeMatch %>% filter(match_id == 18240)
#' eventData <- StatsBombR::get.matchFree(FreeMatch)
#' eventData <- StatsBombR::allclean(eventData)
#'
#' match_dynamics(eventData, binsize = 5, type  = "Possession")
#' match_dynamics(eventData, binsize = 5, type  = "Pass")
#' # pass accurracy averaged over 10 min intervals, only shots that were goals
#' match_dynamics(eventData, binsize = 10, type  = "Pass", outcome_names = list(pass = NA, shot = "Goal"))
#' match_dynamics(eventData, binsize = 10, type  = "Pressure")
#' match_dynamics(eventData, binsize = 5, type  = "xg")
#'
match_dynamics <- function(df, binsize, type, outcome_names = list(pass = NA, shot = c("Saved", "Goal", "Blocked", "Off T")),
                           ylim = c(0, 100)) {

  end_time <- df[nrow(df), c("minute", "second")]
  end_time <- end_time$minute*60 + end_time$second

  bins_end <- ceiling(end_time/(binsize*60))*binsize*60

  df  <- df %>% select(minute, second, period, team.name, type.name, pass.outcome.name,
                       dribble.outcome.name, shot.statsbomb_xg, shot.outcome.name,
                       possession, possession_team.name, duration
                       ) %>% unique() %>%
    mutate(seconds = minute*60 + second)



  xx <- seq(0, bins_end, binsize*60)
  labels <- seq(0, (bins_end -1)/60, binsize)
  breaks <-  seq(0, (bins_end -1)/60, binsize)
  nxx <- length(xx)
  binhalftime <- labels[last(which(xx  <= 45*60)) -1]

  df$bins <-  cut(df$seconds, xx, labels = labels, right = FALSE)

  # correct bins for additional time
  df$bins[df$minute > 45 & df$period == 1] <- binhalftime

  # get information on all Shots
  Shots <- df %>%
    select(possession, type.name,  duration, possession_team.name, team.name, bins, minute, shot.outcome.name) %>%
    filter(type.name == "Shot", shot.outcome.name %in% outcome_names$shot)

  ### Passes, Dribbles
  if(type %in%  c("Pass", "Dribble")) {

    df <- df %>% filter(type.name == {{ type }})

    if(!is.null(outcome_names$pass)) {
      df <- df %>% mutate(accurate = ifelse(pass.outcome.name %in% outcome_names$pass, TRUE, FALSE))
    }
    if(!is.null(outcome_names$dribble)) {
        df <- df %>% filter(accurate = ifelse(dribble.outcome.name %in% outcome_names$dribble, TRUE, FALSE))
    }

    dfperc <- df %>%
      group_by(team.name, accurate, bins) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(team.name, bins) %>%
      mutate(percentage = n/sum(n)) %>% ungroup()

    dfperc <- Shots %>% full_join(dfperc %>% filter(accurate == TRUE), by = c("team.name", "bins"))
    plot_out <- dfperc %>% ggplot(aes( x = as.numeric(as.character(bins)), y = percentage*100, colour = team.name)) +
      geom_line(size = 1)+
      #geom_text(aes( x = as.numeric(bins), y= perc, label = round(perc)), vjust = -1, size = 4, show.legend = F)+
      scale_color_manual(values = c("red", "blue"))+
      scale_x_continuous(breaks = breaks, labels = labels)+
      coord_cartesian(ylim = ylim)+
      theme_bw() + theme(legend.position = "bottom", text = element_text(size = 20))+
      scale_linetype_manual(values = c("dashed", "solid"))+
      geom_vline(data  = dfperc %>% filter(type.name == "Shot"),
                 aes( xintercept  = minute, colour = team.name, linetype ="Shot"))+
      geom_point(data = dfperc %>% filter(shot.outcome.name == "Goal"), aes( x = minute, y = 10, colour = team.name), size = 6, show.legend = F)+
      labs( x= "Time", y = "", colour = "Team:", title = "Pass accuracy (%)", linetype = "")


  }

  if(type == "Possession") {

    dfperc <- df %>%
     # select(possession, event_type_name, type_name,  duration, possession_team_name, team_name, bins) %>%
      filter(!(type.name %in% c("Starting XI", "Half Start"))) %>% unique() %>%
      group_by(possession, possession_team.name, bins) %>%
      summarise( sec_total = sum(duration, na.rm = TRUE), .groups = "drop") %>%
      group_by(possession_team.name, bins) %>%
      summarise(min_tot = sum(sec_total)/60, .groups = "drop") %>%
      group_by(bins) %>%
      mutate(poss_perc = min_tot/sum(min_tot)) %>% ungroup()


    dfperc <- Shots %>% full_join(dfperc %>% rename(team.name = possession_team.name), by = c("team.name", "bins"))
    plot_out <- dfperc %>%
      ggplot(aes(x= as.numeric(as.character(bins)), y = poss_perc*100, colour = team.name))+
      geom_line(size = 1)+
      scale_color_manual(values = c("red", "blue"))+
      scale_x_continuous(breaks = breaks, labels = labels)+
      labs( x= "Time", y = "", colour = "Team:", title = "Possession (%)")+
      theme_bw() + theme(legend.position = "bottom", text = element_text(size = 20))+
      scale_linetype_manual(values = c("dashed", "solid"))+
      geom_vline(data  = dfperc %>% filter(type.name == "Shot"),
                 aes( xintercept  = minute, colour = team.name, linetype ="Shot"))+
      geom_point(data = dfperc %>% filter(shot.outcome.name == "Goal"), aes( x = minute, y = 10, colour = team.name), size = 6, show.legend = F)+
      labs( x= "Time", y = "", colour = "Team:", title = "Possession (%)", linetype = "")
  }

  if(type == "Pressure") {

    pressure_events <- df %>%
      filter(type.name == "Pressure") %>% unique()

    pressure_sum <- pressure_events %>%
      group_by(team.name, bins) %>%
      summarise(cum_pressure_duration = sum(duration, na.rm = TRUE), .groups = "drop")

    poss_secs <- df %>%
      filter(!(type.name %in% c("Starting XI", "Half Start"))) %>% unique() %>%
      group_by(possession, possession_team.name, bins) %>%
      summarise( sec_total = sum(duration, na.rm = TRUE), .groups = "drop") %>%
      group_by(possession_team.name, bins) %>%
      summarise(sec_tot = sum(sec_total), .groups = "drop")
    teams <- unique(poss_secs$possession_team.name)

    poss_secs <- poss_secs %>% mutate(opponent_team = ifelse(possession_team.name == teams[1], teams[2], teams[1]))

    pressure_sum <- pressure_sum %>% left_join(poss_secs %>% select(-possession_team.name), by = c("team.name" = "opponent_team",
                                                                                                      "bins" = "bins"))

    pressure_perOppPoss <- pressure_sum %>% mutate(pressPerOpp = cum_pressure_duration/(sec_tot))

    dfpress <- Shots %>% full_join(pressure_perOppPoss, by = c("team.name", "bins"))

    plot_out <- dfpress %>%
        ggplot(aes( x = as.numeric(as.character(bins)), y = pressPerOpp, colour = team.name)) +
        geom_vline(data = dfpress %>% filter(type.name == "Shot"), aes( xintercept  = minute, colour = team.name, linetype ="Shot"))+
        scale_linetype_manual(values = c("dashed", "solid"))+
        geom_line(size = 1)+
        geom_point(data = dfpress %>% filter(shot.outcome.name == "Goal"), aes(x = minute, y = .005, colour = team.name),
                   size = 6, show.legend = F)+
        scale_color_manual(values = c("red", "blue"))+
        scale_x_continuous(breaks = breaks, labels = labels)+
        labs( x = "Time", y = "", colour = "Team:",
              title = "Possession Adjusted pressure over time", subtitle = "cumulated pressure duration/opponent possession", linetype = " ")+
      theme_bw() + theme(legend.position = "bottom", text = element_text(size = 20))


  }

  # timeline of cumulated xg
  if(type == "xg") {
    dfxg <- df %>%
      filter(type.name == "Shot") %>% unique() %>%
      mutate(isGoal = ifelse(shot.outcome.name == "Goal", 1, 0))

    dfxg <- dfxg %>% group_by(team.name) %>%
      mutate( xG = cumsum(shot.statsbomb_xg))

    dfxg <- dfxg %>% bind_rows(
      tibble(minute = 0, second = 0, seconds = 0, period = 1, xG = 0, team.name = unique(dfxg$team.name)))

    plot_out <- dfxg %>% ungroup() %>%  ggplot(aes( x = seconds, y = xG, colour = team.name))+
      geom_line(size = 1)+
      scale_x_continuous(breaks = seq(0, ceiling(end_time/60), 15)*60, labels = seq(0, ceiling(end_time/60), 15)) +
      geom_point(data = dfxg %>% filter(isGoal == TRUE), aes( x = seconds, y = xG, colour = team.name), shape = 19, size = 4)+
      geom_text(data = dfxg %>% filter(isGoal == TRUE), aes(x = seconds, y = xG ), label = "Goal", size = 4,  vjust = -1.1, hjust = 0,
                show.legend = FALSE)+
      scale_colour_manual(values = c("red", "blue"))+
      labs( x= "Time", y = "cumulated xG", colour = "Team:", title =  "xG Dynamics")+
      theme_bw() + theme(legend.position = "bottom", text = element_text(size = 20))

  }

  plot_out
}

