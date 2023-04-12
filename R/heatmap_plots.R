# #### Pass Heatmaps
# events <-  read_csv("Barnsley_Portsmouth_3845285.csv", guess_max = 4000)
#
#
# # add a column with categorial variable for 15-minute time periods
# # also get rid off some columns to have unique observations of the variables
# # that are considered in the following
#
# df  <- events %>% select(!starts_with("freeze")) %>% unique()
#
# allPasses <- df %>% filter(event_type_name == "Pass") %>%
#   select(team_name, player_name, minute, second, location_x, location_y,
#          end_location_x, end_location_y, pass_angle, type_name, event_type_name, outcome_name,
#          bins)
#
# allPasses %>% filter(team_name == hometeam) %>%
#   mutate(failed = ifelse(outcome_name %in% c("Out", "Incomplete", "Pass Offside", "Unknown"), 1, 0)) %>%
#   ggplot()+
#   scale_y_reverse()+
#   geom_bin_2d(aes( x = location_x, y = location_y), binwidth = c(15, 10), colour = "gray")+
#   fc_annotate_pitch(dimensions = c(120, 80), fill = NA, palette = "bw")+
#   viridis::scale_fill_viridis(option = "F", direction = -1, begin = 0.4)+
#   facet_wrap( ~ failed, labeller = as_labeller(c("0" = "Successful", "1" = "Failed")))+
#   labs(fill = "Number of Passes:", title = "Barnsley", subtitle = "Pass Heatmap")
#
# ggsave(paste0(getwd(), "/Plots/PassHeatMap.pdf"), device = "pdf", height = 8, width = 18)
#
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
#
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
