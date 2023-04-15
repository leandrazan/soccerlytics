#### Pass Heatmaps




#' Passing heatmap
#'
#' @param df Event data set
#' @param team Name of the team for which to make a passing heatmap
#' @param LeftToRight Logical; whether you want the team to play from left to right
#'  in the visualisation
#' @param pitch_dim Dimension of the pitch
#' @param xBins Number of bins in x direction
#' @param yBins Number of bins in y direction
#'
#' @return A ggplot object: pass heatmap for successful and failed passes along with
#' average passing direction
#' @export
#'
#' @examples
#' data(eventData)
#' pass_heatmap(eventData, team = "Borussia Dortmund")
#' pass_heatmap(eventData, team = "Bayern Munich", LeftToRight = FALSE, xBins = 6, yBins =4)
pass_heatmap <- function(df, team, LeftToRight = TRUE, pitch_dim = c(120, 80), xBins = 8, yBins = 5) {

  reverse_scale <- ifelse(LeftToRight, scale_y_reverse, scale_x_reverse)

  x.range <- seq(0, pitch_dim[1], length.out = xBins+1)
  y.range <- seq(0, pitch_dim[2], length.out = yBins+1)

  allPasses <- df %>%
    filter(type.name == "Pass", team.name == team) %>%
    unique() %>%
    select(team.name, player.name, minute, second, location.x, location.y,
           pass.end_location.x, pass.end_location.y, pass.angle, type.name,  pass.outcome.name)

  allPasses$pass.outcome.name[is.na(allPasses$pass.outcome.name)] <- "Successful"

  allPasses <- correct_locations(allPasses, pitch_dim = pitch_dim)

  allPasses$xbin <- cut(allPasses$location.x, breaks = x.range)
  allPasses$ybin <- cut(allPasses$location.y, breaks = y.range)
  PassBins_x <- tibble(xbin = unique(levels(allPasses$xbin)),
                     x_center = RcppRoll::roll_mean(x.range, 2))

  PassBins_y <- tibble(ybin = unique(levels(allPasses$ybin)),
                     y_center = RcppRoll::roll_mean(y.range, 2))

  PassBins <- expand_grid(PassBins_x, PassBins_y)

  allPasses <- allPasses %>% filter(!(pass.outcome.name == "Injury Clearance")) %>%
    mutate(failed = ifelse(pass.outcome.name %in% c("Out", "Incomplete", "Pass Offside", "Unknown"), 1, 0))

  allPasses <- allPasses %>% left_join(PassBins, by = c("xbin", "ybin"))
  PassEnd <- allPasses %>% group_by(failed, xbin, ybin, x_center, y_center) %>%
    summarise_at(vars(pass.end_location.x, pass.end_location.y),  ~ mean(.x, na.rm = TRUE)) %>%
    ungroup()


  allPasses %>%
    ggplot()+
    reverse_scale() +
    geom_bin_2d(aes( x = location.x, y = location.y),
                binwidth = c(diff(x.range)[1], diff(y.range)[1]), colour = "gray")+
    draw_pitch(dimensions = c(120, 80), fill = NA, palette = "bw")+
    viridis::scale_fill_viridis(option = "F", direction = -1, begin = 0.4)+
    geom_segment(data = PassEnd,aes(x = x_center, y = y_center,
                                    xend = x_center + (pass.end_location.x - x_center)/3,
                                    yend =  y_center + (pass.end_location.y - y_center)/3),
                 arrow = arrow(length = unit(0.1, "cm")), lineend = "round", linejoin = "bevel", size = 1, colour = "gray20")+
    facet_wrap( ~ failed, labeller = as_labeller(c("0" = "Successful", "1" = "Failed")))+
    labs(fill = "Number of Passes:", title = team, subtitle = "Pass Heatmap")+
    theme_bw()+
    theme(legend.position = "bottom", text = element_text(size = 20), axis.text = element_blank(),
          axis.title = element_blank())

  }



# allPasses$location_x[allPasses$location_x == 120] <- 119.999
# allPasses %>% filter(team_name == awayteam) %>%
#   mutate(failed = ifelse(outcome_name %in% c("Out", "Incomplete", "Pass Offside"), 1, 0)) %>%
#   ggplot()+
#   scale_x_reverse()+
#   geom_bin_2d(aes( x = location_x, y = location_y), binwidth = c(15, 10), colour = "gray")+
#   fc_annotate_pitch(dimensions = c(120, 80), fill = NA, palette = "bw")+
#   viridis::scale_fill_viridis(option = "F", direction = -1, begin = 0.4)+
#   facet_wrap( ~ failed, labeller = as_labeller(c("0" = "Successful", "1" = "Failed")))+
#   labs(fill = "Number of Passes:", title = "Portsmouth", subtitle = "Pass Heatmap")
# ggsave(paste0(getwd(), "/Plots/Portsmouth/PassHeatMap.pdf"), device = "pdf", height = 8, width = 18)
#
#
#
# count_lost_poss <- function(poss_vec, possession, team) {
#
#   teams <- unique(poss_vec)
#   opponent <- teams[!(teams == team)]
#
#   nposs <- length(poss_vec)
#   count_loss <- 0
#   index_loss <- numeric()
#   # first_poss <- which(poss_vec == team)
#   # poss_vec <- poss_vec[first_poss:nposs]
#   for( j in 1: (nposs -1)) {
#     if(poss_vec[j] == team & poss_vec[j+1] == opponent) {
#       count_loss <- count_loss +1
#       index_loss <- c(index_loss, possession[j])
#     }
#   }
#
#   df_indexloss <- tibble(possession = possession) %>% mutate(loss = ifelse(possession %in% index_loss, TRUE, FALSE))
#  list(count_loss = count_loss, df_indexloss = df_indexloss)
# }
#
# ## Barnsley
# poss_nest <- events %>% filter(!(event_type_name %in% c("Half Start", "Starting XI"))) %>% unique() %>%
#   select(possession, possession_team_name, minute, second, timestamp, team_name, event_type_name,
#          location_x, location_y, outcome_name, duration)
#
# poss_nest <- poss_nest %>% group_by(possession, possession_team_name) %>% nest()
#
# lost_poss <- count_lost_poss(poss_vec = poss_nest$possession_team_name, possession = poss_nest$possession, team = "Barnsley")
#
# poss_nest <- poss_nest %>% left_join(lost_poss$df_indexlos, by = "possession")
#
# poss_nest <- poss_nest %>%
#   mutate(last_event = purrr::map(data, ~ {
#     ndata <- nrow(.x)
#     xtmp <- .x[ndata, ]
#     event_name <- xtmp$event_type_name
#     event_x <- xtmp$location_x
#     event_y <- xtmp$location_y
#     tibble(last_event_name = event_name, location_x = event_x, location_y = event_y)
#     } ))
#
# poss_nest <- poss_nest %>% unnest(cols = last_event)
#
# poss_lost <- poss_nest %>% filter(!(last_event_name %in% c("Injury Stoppage", "Shot")))
#
# df <- within(poss_lost, {
#   grp.x = cut(location_x, seq(0, 120, 24), labels =  seq(24/2, 120, 24))
#   grp.y = cut(location_y, seq(0, 80, 80/3), labels =  seq(80/6, 80, 80/3))
# })
#
#
# poss_total <- df %>% group_by(grp.x, grp.y) %>%
#   summarise(n_total = n(),  .groups = "drop")
#
# df <-  df %>% filter(loss == TRUE) %>% group_by(grp.x, grp.y) %>%
#   tally()
#
# df <- df %>% left_join(poss_total, by = c("grp.x", "grp.y")) %>%
#   mutate(avg = n/n_total) %>% ungroup()
#
# lostposs_barn <- df %>%
#   ggplot(aes( x = as.numeric(as.character(grp.x)), y =as.numeric(as.character(grp.y)), fill = avg*100 )) +
#   geom_tile()+
#   scale_y_reverse()+
#   fc_annotate_pitch(dimension = c(120, 80), fill = NA, palette = "bw") +
#   viridis::scale_fill_viridis(option = "F", direction = -1, begin = 0.4, limits = c(0, 80))+
#   labs(title = "Lost Possession", subtitle = "Barnsley", fill = "Percentage possession was lost:")+
#   theme(axis.text = element_blank(), axis.title = element_blank())
#
# ##########
#
# ## Portsmouth
#
# team <- "Portsmouth"
# poss_nest <- events %>% filter(!(event_type_name %in% c("Half Start", "Starting XI"))) %>% unique() %>%
#   select(possession, possession_team_name, minute, second, timestamp, team_name, event_type_name,
#          location_x, location_y, outcome_name, duration)
#
# poss_nest <- poss_nest %>% group_by(possession, possession_team_name) %>% nest()
#
# lost_poss <- count_lost_poss(poss_vec = poss_nest$possession_team_name, possession = poss_nest$possession, team = team)
#
# poss_nest <- poss_nest %>% left_join(lost_poss$df_indexloss, by = "possession")
#
# poss_nest <- poss_nest %>%
#   mutate(last_event = purrr::map(data, ~ {
#     ndata <- nrow(.x)
#     xtmp <- .x[ndata, ]
#     event_name <- xtmp$event_type_name
#     event_x <- xtmp$location_x
#     event_y <- xtmp$location_y
#     tibble(last_event_name = event_name, location_x = event_x, location_y = event_y)
#   } ))
#
# poss_nest <- poss_nest %>% unnest(cols = last_event)
#
# poss_lost <- poss_nest %>% filter(!(last_event_name %in% c("Injury Stoppage", "Shot")))
#
# df <- within(poss_lost, {
#   grp.x = cut(location_x, seq(0, 120, 24), labels =  seq(24/2, 120, 24))
#   grp.y = cut(location_y, seq(0, 80, 80/3), labels =  seq(80/6, 80, 80/3))
# })
#
#
# poss_total <- df %>% group_by(grp.x, grp.y) %>%
#   summarise(n_total = n(),  .groups = "drop")
#
# df <-  df %>% filter(loss == TRUE) %>% group_by(grp.x, grp.y) %>%
#   tally()
#
# df <- df %>% left_join(poss_total, by = c("grp.x", "grp.y")) %>%
#   mutate(avg = n/n_total) %>% ungroup()
#
# poss_nest <- events %>% filter(!(event_type_name %in% c("Half Start", "Starting XI"))) %>% unique() %>%
#   select(possession, possession_team_name, minute, second, timestamp, team_name, event_type_name,
#          location_x, location_y, outcome_name, duration)
#
# poss_nest <- poss_nest %>% group_by(possession, possession_team_name) %>% nest()
# #



# lostposs_port <- df %>%
#   ggplot(aes( x = as.numeric(as.character(grp.x)), y =as.numeric(as.character(grp.y)), fill = avg*100 )) +
#   geom_tile()+
#   scale_x_reverse()+
#   fc_annotate_pitch(dimension = c(120, 80), fill = NA, palette = "bw") +
#   viridis::scale_fill_viridis(option = "F", direction = -1, begin = 0.4, limits = c(0, 80))+
#   labs(title = "Lost Possession", subtitle = team, fill = "Times possession was lost:")+
#   theme(axis.text = element_blank(), axis.title = element_blank())
#
#
# ggpubr::ggarrange(lostposs_barn, lostposs_port, common.legend = TRUE, legend = "bottom")
#
# ggsave(paste0(getwd(), "/Plots/LostPossessionInSpace.pdf"), device = "pdf", height = 8, width = 18)
#
# #################################################################################
#
# ## obv ###
#
# obv_events <- events %>% select(team_name, event_type_name, location_x, location_y, starts_with("obv")) %>%
#   unique() %>% filter(!is.na(obv_for_net))
#
# obv_events
#
# obv_events <-  within(obv_events, {
#   grp.x = cut(location_x, seq(0, 120, 15), labels =  seq(15/2, 120, 15))
#   grp.y = cut(location_y, seq(0, 80, 10), labels =  seq(5, 80, 10))
# })
#
# obv_stats <- obv_events %>% group_by(team_name, grp.x, grp.y) %>%
#   summarise_at(vars(obv_for_after: obv_total_net), sum )
#
# obv_stats %>%
# ggplot(aes( x = as.numeric(as.character(grp.x)), y =as.numeric(as.character(grp.y)), fill = obv_for_net )) +
#   geom_tile()+
#   scale_x_reverse()+
#   fc_annotate_pitch(dimension = c(120, 80), fill = NA, palette = "bw") +
#   viridis::scale_fill_viridis(option = "F", direction = -1, begin = 0.4, limits = c(-.2, .2))+
#   labs(title = "",  fill = "Net obv for:")+
#   facet_wrap(~team_name)+
#   theme(axis.text = element_blank(), axis.title = element_blank())
