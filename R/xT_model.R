library(tidyverse)

library(jsonlite)

filepath <- "C:/Users/leaz9/Desktop/events_England.json"

events <- jsonlite::read_json(filepath)
events <- as_tibble(events)

events %>% glimpse()

events[1,]$positions
events <- events %>% mutate(start_x = purrr::map_dbl(positions, ~ .x$x[1]),
                  end_x = purrr::map_dbl(positions, ~ .x$x[2]),
                  start_y = purrr::map_dbl(positions, ~ .x$y[1]),
                  end_y = purrr::map_dbl(positions, ~ .x$y[2]))

events
events <- events %>%  mutate(tags = purrr::map(tags, ~ {
  tagvec <- .x$id
  if(is.null(tagvec)) {
    return(tibble())
  } else {
    names(tagvec) <- paste0("tag_id_", 1:length(tagvec))
    return(as_tibble(t(tagvec)))
  }
}))
events <- events %>% unnest(cols = tags)

events <- events %>% mutate_at(vars(starts_with("tag_id")), translate_wyscout_ids_vec)

events %>% select(starts_with("tag_id")) %>% unique() %>% tail()

events$eventName %>% unique()
events$subEventName %>% unique()

# pitch is scaled to percent -> scale back to 105m x 68m
events <- events %>%
  mutate_at(vars(start_x, end_x), ~ {.x*105/100}) %>%
  mutate_at(vars(start_y, end_y), ~ {.x*68/100})

# y-axis is reversed in data (top left is 0, bottom is 68) -> reverse in
# whole dataset for plotting

events <- events %>% mutate_at(vars(start_y, end_y), ~{68 -.x})

# new variable containing following event
events$next_event <- c(events[2:nrow(events), ]$subEventName, 0)

# new logical variable, whether ball was kicked out with that action
events <- events %>% mutate(kickedOut = ifelse(next_event == "Ball out of the field", 1, 0))

########### MOVE EVENTS ###################
# get all ball moving events
move_events <- events %>%
  filter(subEventName %in% c('Simple pass', 'High pass', 'Head pass', 'Smart pass', 'Cross'))

# filtering events where ball was not kicked out of the field
move_events <- move_events %>% filter(kickedOut == 0)

# for binning: nudge locations minimally
move_events <- correct_locations(move_events, pitch_dim = c(105, 68), name_x = "start_x",
                  name_y = "start_y")
move_events <- correct_locations(move_events, pitch_dim = c(105, 68), name_x = "end_x",
                                                                      name_y = "end_y")

move_plot <- move_events %>%
  ggplot() +
  geom_bin_2d(aes(x = start_x, y = start_y, fill = after_stat(count/sum(count))),
              binwidth = c(105/16, 68/12), colour = "gray")+
  scale_fill_gradient(low = "white", high = "darkblue")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
        legend.key.width = unit(1, "cm"))+
  labs(fill = "Number of ball moving actions")
move_plot
# get tibble with counts per bin
move_counts <- layer_data(move_plot) %>% select(xbin, ybin, x, y, count) %>% as_tibble()


########### SHOT EVENTS ###################
# get all shots
shots <- events %>%
  filter(subEventName == "Shot")
shots_plot <- shots %>%
  ggplot() +
  geom_bin_2d(data = expand_grid(x = seq(0.01, 104.99, 105/16), y = seq(0.01, 67.99, 68/12)),
              aes(x = x, y = y), fill = "white", colour = "gray" , binwidth = c(105/16, 68/12))+
  geom_bin_2d(aes(x = start_x, y = start_y),
              binwidth = c(105/16, 68/12), colour = "gray")+
  scale_fill_gradient(low = "white", high = "darkgreen")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
        legend.key.width = unit(1, "cm"))+
  labs(fill = "Number of ball moving actions")
shots_plot

shots_count <- layer_data(shots_plot, i = 2) %>%
  filter(!(fill == "white")) %>% select(xbin, ybin, x, y, count) %>% as_tibble()
shots_count

###### GOALS #########

goals <- shots %>% filter( tag_id_1 == "Goal" | tag_id_2 == "Goal" | tag_id_3 == "Goal" |
                           tag_id_4 == "Goal" | tag_id_5 == "Goal" | tag_id_6 == "Goal" )
goals

goals_plot <- goals %>%
  ggplot() +
  geom_bin_2d(data = expand_grid(x = seq(0.01, 104.99, 105/16), y = seq(0.01, 67.99, 68/12)),
              aes(x = x, y = y), fill = "white", colour = "gray" , binwidth = c(105/16, 68/12))+
  geom_bin_2d(aes(x = start_x, y = start_y),
              binwidth = c(105/16, 68/12), colour = "gray")+
  scale_fill_gradient(low = "white", high = "darkred")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
        legend.key.width = unit(1, "cm"))+
  labs(fill = "Number of Goals")
goals_plot

goals_count <- layer_data(goals_plot, i = 2) %>%
  filter(!(fill == "white")) %>% select(xbin, ybin, x, y, count) %>% as_tibble()
goals_count

## move probability
# percentage of moving actions per bin

all_counts <- move_counts %>% rename(move_count = count) %>%
  left_join(shots_count %>% rename(shot_count = count), by = c("xbin", "ybin", "x", "y")) %>%
  left_join(goals_count %>% rename(goals_count = count), by = c("xbin", "ybin", "x", "y"))

all_counts <- all_counts %>% replace_na(replace = list(shot_count = 0, goals_count = 0))
all_counts <- all_counts %>% mutate(move_prob = move_count/(move_count + shot_count),
                                    shot_prob = 1 - move_prob)

move_prob_plot <- all_counts %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = move_prob*100),
             width = c(105/16), height =68/12, colour = "gray")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  scale_fill_gradient(low = "darkred", high = "darkblue")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
        legend.key.width = unit(1, "cm"))+
  labs(fill = "Move probability (%)")

shot_prob_plot <- all_counts %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = shot_prob*100),
            width = c(105/16), height =68/12, colour = "gray")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  scale_fill_gradient(low = "white", high = "darkgreen")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
        legend.key.width = unit(1, "cm"))+
  labs(fill = "Shot probability (%)")
shot_prob_plot

all_counts %>% filter(shot_count > 5) %>%
  mutate(goal_prob = ifelse(goals_count > 0 , goals_count/shot_count, 0)) %>%
  ggplot() +
  geom_bin_2d(data = expand_grid(x = seq(0.01, 104.99, 105/16), y = seq(0.01, 67.99, 68/12)),
              aes(x = x, y = y), fill = "white", colour = "gray" , binwidth = c(105/16, 68/12))+
  geom_tile(aes(x = x, y = y, fill = goal_prob*100),
            width = c(105/16), height =68/12, colour = "gray")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  scale_fill_gradient(low = "white", high = "darkgreen")+
  draw_pitch(dimensions = c(105, 68), fill = NA)+
  theme_bw()+
  theme(legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(),
        legend.key.width = unit(1, "cm"))+
  labs(fill = "Goal probability (%)")

x_breaks <- layer_data(move_plot)$xmax %>% unique()
x_breaks
y_breaks <- layer_data(move_plot)$ymax %>% unique()
y_breaks
move_events$bin_start_x <- cut(move_events$start_x, breaks = c(0, x_breaks),
                               labels = 2:17)
move_events$bin_start_y <- cut(move_events$start_y, breaks = c(0, y_breaks),
                               labels = 1:12)

move_events$bin_end_x <- cut(move_events$end_x, breaks = c(0, x_breaks),
                               labels = 2:17)
move_events$bin_end_y <- cut(move_events$end_y, breaks = c(0, y_breaks),
                               labels = 1:12)

count_move_start <-  move_events %>% group_by(bin_start_x, bin_start_y) %>%
 tally() %>% ungroup()

count_move_start

transition_matrices <- list()

for(j in 1:nrow(count_move_start)) {
  data.tmp <- count_move_start[j, ]
  bin_tmp <- move_events %>% filter(bin_start_x == data.tmp$bin_start_x,
                                    bin_start_y == data.tmp$bin_start_y)
  count_move_end <- bin_tmp %>% group_by(bin_end_x, bin_end_y) %>%
    tally() %>% ungroup()
  trans_mat <- array(data = 0, dim = c(12,16))
  for(k in 1:nrow(count_move_end)) {
    end_tmp <- count_move_end[k, ]
    trans_mat[as.numeric(end_tmp$bin_end_y), as.numeric(end_tmp$bin_end_x)] <- end_tmp$n
  }
  trans_mat <- trans_mat/count_move_start$n
 transition_matrices <- append(transition_matrices, list(trans_mat))
}


move_nest <- move_events %>% group_by(bin_start_x, bin_start_y, bin_end_x, bin_end_y) %>%
  nest()

transition_matrices <- move_nest %>% mutate( n = purrr::map_dbl(data, ~ nrow(.x))) %>%
  unnest(cols = data) %>%
  group_by(bin_start_x, bin_start_y) %>%
  nest() %>%
  mutate(n_start = purrr::map_dbl(data, ~nrow(.x))) %>%
  unnest(cols = data) %>%
  select(bin_start_x, bin_start_y, bin_end_x, bin_end_y, n_start, n) %>%
  unique() %>%
  mutate(trans_prob = n/n_start) %>%
  ungroup()

bin_pos <- layer_data(move_plot) %>%
  select(xbin, ybin, x,y)

bin_pos <- bin_pos %>%
  mutate(xbin = factor(xbin, levels = unique(bin_pos$xbin)),
         ybin = factor(ybin, levels = unique(bin_pos$ybin))) %>%
  rename(bin_end_x = xbin, bin_end_y = ybin)

transition_matrices <- transition_matrices %>% left_join(bin_pos)

aa <- transition_matrices %>% group_by(bin_start_x, bin_start_y) %>%
  nest() %>%
  mutate(data = purrr::pmap(list(.x = bin_start_x, .y = bin_start_y, .z = data),
                            .f = function(.x, .y, .z) {
                              .z$start_x <-  .x
                                .z$end_x <- .y
                                .z}))
aa %>%
    mutate(plot_trans_prob = purrr::map(data, ~ {
    .x %>%
      ggplot() +
      geom_tile(aes( x = x, y = y, fill = trans_prob), width = 105/16, height = 68/12,
                colour = "gray")+
      draw_pitch(fill = NA, dimensions = c(105, 68))+
      scale_fill_gradient(low = "white", high = "darkred")+
      theme_bw()+
      theme(legend.position = "bottom")
  }))

aa <- aa %>% arrange(bin_start_x, bin_start_y)
aa %>% mutate(plot_trans_prob = purrr::pmap(list(bin_start_x, bin_start_y)))
aa[1:5, ]$plot_trans_prob

ggplot() +
  geom_tile(data = aa[1, ]$data[[1]],
            aes( x = x, y = y, fill = trans_prob), width = 105/16, height = 68/12,
            colour = "gray")+
  draw_pitch(fill = NA, dimensions = c(105, 68))+
  scale_fill_gradient(low = "white", high = "darkred")+
  theme_bw()+
  theme(legend.position = "bottom")



move_nest[1:10, ]$data
