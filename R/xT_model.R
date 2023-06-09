# library(tidyverse)
#
# library(jsonlite)
#
# filepath <- "C:/Users/leaz9/Desktop/events_England.json"
#
# events <- jsonlite::read_json(filepath, simplifyVector = TRUE)
# events <- as_tibble(events)
#
# events %>% glimpse()
#
# events[1,]$positions
# events <- events %>% mutate(start_x = purrr::map_dbl(positions, ~ .x$x[1]),
#                             end_x = purrr::map_dbl(positions, ~ .x$x[2]),
#                             start_y = purrr::map_dbl(positions, ~ .x$y[1]),
#                             end_y = purrr::map_dbl(positions, ~ .x$y[2]))
#
# events
# events <- events %>%  mutate(tags = purrr::map(tags, ~ translate_wyscout_ids_vec(.x$id)))
#
# events$eventName %>% unique()
# events$subEventName %>% unique()
#
# # pitch is scaled to percent -> scale back to 105m x 68m
# events <- events %>%
#   mutate_at(vars(start_x, end_x), ~ {.x*105/100}) %>%
#   mutate_at(vars(start_y, end_y), ~ {.x*68/100})
#
# # y-axis is reversed in data (top left is 0, bottom is 68) -> reverse in
# # whole dataset for plotting
#
# events <- events %>% mutate_at(vars(start_y, end_y), ~{68 -.x})
#
# # new variable containing following event
# events$next_event <- c(events[2:nrow(events), ]$subEventName, 0)
#
# # new logical variable, whether ball was kicked out with that action
# events <- events %>% mutate(kickedOut = ifelse(next_event == "Ball out of the field", 1, 0))
#
#
# ########### MOVE EVENTS ###################
# # get all ball moving events
# move_events <- events %>%
#   filter(subEventName %in% c('Simple pass', 'High pass', 'Head pass', 'Smart pass', 'Cross'))
#
# # filtering events where ball was not kicked out of the field
# move_events <- move_events %>% filter(kickedOut == 0)
#
# # for binning: nudge locations minimally
# move_events <- correct_locations(move_events, pitch_dim = c(105, 68), name_x = "start_x",
#                                  name_y = "start_y")
# move_events <- correct_locations(move_events, pitch_dim = c(105, 68), name_x = "end_x",
#                                  name_y = "end_y")
#
# move_plot <- move_events %>%
#   ggplot() +
#   geom_bin_2d(aes(x = start_x, y = start_y, fill = after_stat(count/sum(count))),
#               binwidth = c(105/16, 68/12), colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkblue")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   theme_bw()+
#   theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
#         legend.key.width = unit(1, "cm"))+
#   labs(fill = "Number of ball moving actions")
# move_plot
# # get tibble with counts per bin
# move_counts <- layer_data(move_plot) %>% select(xbin, ybin, x, y, count) %>% as_tibble()
#
#
#
# x_breaks <- seq(0, 105, length.out = 17)
# x_breaks
# y_breaks <- rev(seq(0, 68, length.out = 13))
# y_breaks
#
# x_centers <- tibble(bin_start_x = factor(1:16), x_center = RcppRoll::roll_mean(x_breaks, 2))
# y_centers <- tibble(bin_start_y = factor(1:12), y_center = RcppRoll::roll_mean(y_breaks, 2))
#
# bin_centers <- expand_grid(x_centers, y_centers)
# bin_centers
#
# move_events$bin_start_x <- cut(move_events$start_x, breaks = x_breaks, include.lowest = TRUE,
#                                labels = 1:16)
# move_events$bin_start_y <- cut(move_events$start_y, breaks = y_breaks,
#                                labels = 1:12)
#
# move_events$bin_end_x <- cut(move_events$end_x, breaks = x_breaks,
#                              labels = 1:16)
# move_events$bin_end_y <- cut(move_events$end_y, breaks = y_breaks,
#                              labels = 1:12)
#
# move_events <- move_events %>% select(contains("bin"), start_x, end_x, subEventName, next_event)
#
# count_move_events <- move_events %>% group_by(bin_start_x, bin_start_y) %>%
#   summarise(n_move_events = n(), .groups = "drop")
#
# move_matrix <- count_move_events %>% arrange(bin_start_x, bin_start_y) %>%
#   spread(key = bin_start_x, value = n_move_events) %>%
#   select(-bin_start_y) %>%
#   as.matrix()
# move_matrix
#
# ########### SHOT EVENTS ###################
# # get all shots
# shots <- events %>%
#   filter(subEventName == "Shot")
# shots_plot <- shots %>%
#   ggplot() +
#   geom_bin_2d(data = expand_grid(x = seq(0.01, 104.99, 105/16), y = seq(0.01, 67.99, 68/12)),
#               aes(x = x, y = y), fill = "white", colour = "gray" , binwidth = c(105/16, 68/12))+
#   geom_bin_2d(aes(x = start_x, y = start_y),
#               binwidth = c(105/16, 68/12), colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkgreen")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   theme_bw()+
#   theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
#         legend.key.width = unit(1, "cm"))+
#   labs(fill = "Number of ball moving actions")
# shots_plot
#
# shots$bin_start_x <- cut(shots$start_x, breaks = x_breaks, include.lowest = TRUE,
#                                labels = 1:16)
# shots$bin_start_y <- cut(shots$start_y, breaks = y_breaks,
#                                labels = 1:12)
#
# shots
#
# shots <- shots %>% select(contains("bin"), start_x, start_y, subEventName, tags)
#
# count_shots <- shots %>% group_by(bin_start_x, bin_start_y) %>%
#   summarise(n_shots = n(), .groups = "drop")
#
# # fill with zeros where no shot was taken
# count_shots <- expand_grid(bin_start_x = factor(1:16), bin_start_y = factor(1:12)) %>%
#   left_join(count_shots, by = c("bin_start_x", "bin_start_y")) %>%
#   replace_na(replace = list(n_shots = 0))
#
# shot_matrix <- count_shots %>% arrange(bin_start_x, bin_start_y) %>%
#   spread(key = bin_start_x, value = n_shots) %>%
#   select(-bin_start_y) %>%
#   as.matrix()
# shot_matrix
#
# ###### GOALS #########
#
# goals <- shots %>% mutate(Goal = purrr::map_lgl(tags, ~ any(.x == "Goal"))) %>%
#   filter(Goal == TRUE)
#
# goals
#
# goals_plot <- goals %>%
#   ggplot() +
#   geom_bin_2d(data = expand_grid(x = seq(0.01, 104.99, 105/16), y = seq(0.01, 67.99, 68/12)),
#               aes(x = x, y = y), fill = "white", colour = "gray" , binwidth = c(105/16, 68/12))+
#   geom_bin_2d(aes(x = start_x, y = start_y),
#               binwidth = c(105/16, 68/12), colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   theme_bw()+
#   theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
#         legend.key.width = unit(1, "cm"))+
#   labs(fill = "Number of Goals")
# goals_plot
#
# count_goals <- goals %>% group_by(bin_start_x, bin_start_y) %>%
#   summarise(n_goals = n(), .groups = "drop")
#
# # fill with zeros where no shot was taken
# count_goals <- expand_grid(bin_start_x = factor(1:16), bin_start_y = factor(1:12)) %>%
#   left_join(count_goals, by = c("bin_start_x", "bin_start_y")) %>%
#   replace_na(replace = list(n_goals = 0))
#
# goal_matrix <- count_goals %>% left_join(count_shots) %>%
#   mutate(n_goals = ifelse(n_shots >= 5, n_goals, 0)) %>%
#   select(-n_shots) %>%
#   arrange(bin_start_x, bin_start_y) %>%
#   spread(key = bin_start_x, value = n_goals) %>%
#   select(-c(bin_start_y)) %>%
#   as.matrix()
# goal_matrix
#
#
# ## move probability
# # percentage of moving actions per bin
#
# all_counts <- count_move_events %>%
#   left_join(count_shots, by = c("bin_start_x", "bin_start_y")) %>%
#   left_join(count_goals, by = c("bin_start_x", "bin_start_y"))
#
# all_counts
#
# all_counts <- all_counts %>% mutate(move_prob = n_move_events/(n_move_events + n_shots),
#                                     shot_prob = 1 - move_prob) %>%
#   left_join(bin_centers, by = c("bin_start_x", "bin_start_y"))
#
# all_counts
#
# move_prob_plot <- all_counts %>%
#   ggplot() +
#   geom_tile(aes(x = x_center, y = y_center, fill = move_prob*100),
#             width = c(105/16), height =68/12, colour = "gray")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   scale_fill_gradient(low = "darkred", high = "darkblue")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   theme_bw()+
#   theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
#         legend.key.width = unit(1, "cm"))+
#   labs(fill = "Move probability (%)")
# move_prob_plot
#
# shot_prob_plot <- all_counts %>%
#   ggplot() +
#   geom_tile(aes(x = x_center, y = y_center, fill = shot_prob*100),
#             width = c(105/16), height =68/12, colour = "gray")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   scale_fill_gradient(low = "darkred", high = "darkblue")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   theme_bw()+
#   theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
#         legend.key.width = unit(1, "cm"))+
#   labs(fill = "Move probability (%)")
# shot_prob_plot
#
# goal_prob_plot <- all_counts %>%
#   mutate(goal_prob = ifelse(n_shots > 5, n_goals/n_shots, 0)) %>%
#   ggplot() +
#   geom_tile(aes(x = x_center, y = y_center, fill = goal_prob*100),
#             width = c(105/16), height =68/12, colour = "gray")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   scale_fill_gradient(low = "white", high = "darkgreen")+
#   draw_pitch(dimensions = c(105, 68), fill = NA)+
#   theme_bw()+
#   theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
#         legend.key.width = unit(1, "cm"))+
#   labs(fill = "Goal probability (%)")
# goal_prob_plot
#
# ########################################################################
# ############## transition matrices #####################################
#
# move_nest <- move_events %>% group_by(bin_start_x, bin_start_y, bin_end_x, bin_end_y) %>%
#   nest()
#
#
# move_nest <- bin_centers %>% expand_grid( expand_grid(bin_end_x = factor(1:16), bin_end_y = factor(1:12))) %>%
#   left_join(move_nest)
#
#
# transition_matrices <- move_nest %>% mutate(n = purrr::map_dbl(data, ~ {
#   if(is.null(.x)){ return(0)}
#   else {return(nrow(.x)) } } )) %>%
#   group_by(bin_start_x, bin_start_y) %>% nest() %>%
#   mutate(n_start = purrr::map_dbl(data, ~ sum(.x$n))) %>%
#   unnest(cols = data) %>%
#   mutate(trans_prob = n/n_start) %>%
#   ungroup()
#
# transmat <- transition_matrices %>% select(-c(data, n, n_start)) %>%
#   group_by(bin_start_x, bin_start_y, x_center, y_center) %>%
#   nest()
#
# transmat <- transmat %>% mutate(transmat = purrr::map(data, ~ {.x %>% spread(key = bin_end_x, value = trans_prob) %>%
#     select(-bin_end_y) %>%
#     as.matrix()}))
#
# transmat[1, ]$transmat
#
# df1 <- all_counts %>%  mutate(goal_prob = ifelse(n_shots > 5, n_goals/n_shots, 0)) %>%
#   select(bin_start_x, bin_start_y, move_prob, shot_prob, goal_prob, x_center, y_center) %>%
#   left_join(transmat %>% select(-data))
#
# xT <- array(0, dim = c(12, 16))
# goal_prob <- goal_matrix/shot_matrix
# goal_prob[is.nan(goal_prob)] <- 0
# goal_prob
# shot_prob <- shot_matrix/(move_matrix + shot_matrix)
# shot_prob
# move_prob <- move_matrix/(move_matrix + shot_matrix)
# xt_plots <- list()
# err <- numeric()
# shoot_exp_pay <- goal_prob * shot_prob
# for(j in 1:100) {
#
#   move_cond_pay <- matrix(unlist(purrr::map(df1$transmat,  ~ {
#     sum(.x *xT)})), nrow = 12)
#   xT <- shoot_exp_pay + move_prob*move_cond_pay
#
#   err[j] <- sum((xT_old - xT)^2)
#   xT_old <- xT
# }
#   xTlong <- xT %>% as_tibble() %>% mutate(bin_start_y = factor(1:12)) %>%
#     pivot_longer(cols = 1:16, names_to = "bin_start_x", values_to = "xT") %>%
#     mutate(bin_start_x = factor(bin_start_x))
#
#  xt_plots[[j]] <-  xTlong %>% left_join(bin_centers, by = c("bin_start_y", "bin_start_x")) %>%
#     ggplot(aes( x = x_center, y = y_center, fill = xT))+
#     geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#     geom_text(aes(label = round(xT,2)), colour = "darkblue", size = 2)+
#     scale_fill_gradient(low = "white", high = "darkred", limits = c(0, 0.5))+
#     draw_pitch(dimensions = c(105, 68), fill = NA, color = "black")+
#     theme_bw()+ theme(legend.position = "bottom", axis.text = element_blank(),
#                       axis.title = element_blank())+
#    labs(title = paste("xT after", j , "moves"))
#
# }
#
# ggpubr::ggarrange(plotlist = xt_plots, common.legend = TRUE, ncol = 3, nrow = 4)
#
# ggpubr::ggarrange(plotlist = xt_plots[41:50], common.legend = TRUE, ncol = 3, nrow = 4)
#
