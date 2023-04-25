#' Prepare data for computing xT
#'
#' @param df An event data set in SPADL format
#' @param pitch_dim A list with items
#' *`from` original pitch dimension (numeric vector with `x` and `y` coordinate,
#'  in that order)
#' *`to` pitch dimension to convert to (numeric vector with `x` and `y` coordinate,
#'  in that order)
#' @param reverse_y logical; whether to reverse the `y`-values (if upper left corner of
#' pitch is set to 0 instead of e.g. 68)
#' @param bins_x Number of bins in `x`-direction
#' @param bins_y Number of bins in `y`-direction
#'
#' @return A list with items
#' * `prep_data` The prepped event data with columns `start_x`, `start_y`, `end_x`,
#'  `end_y`, `actiontype`, `result`, `bin_start_x`, `bin_start_y`, `bin_end_x`, `bin_end_y`,
#'  with coordinates transformed (if required) and respective bins values for coordinates
#' * `bin_centers` All combinations of bins and their centers. Useful for plotting.
#'
#' @export
#'
#' @examples
#' data(spadlData)
#' prep_dat <- prepare_data(spadlData)
#'
#' # visualise grid:
#' prep_dat$bin_centers %>%
#' ggplot(aes( x  = x_center, y = y_center))+
#'  geom_tile(fill = "white", colour = "gray") +
#'  geom_text(aes( label = paste0("x:", bin_start_x, ", y:", bin_start_y))) +
#'  draw_pitch(c(105, 68), fill = NA)
#'
prepare_data <- function(df, pitch_dim = list(from = c(105, 68), to = c(105, 68)),
                         reverse_y = TRUE, bins_x = 16, bins_y = 12) {
  # scale pitch from ... to ....
  df <- df %>%
    mutate_at(vars(start_x, end_x), ~ {.x*pitch_dim$to[1]/pitch_dim$from[1]}) %>%
    mutate_at(vars(start_y, end_y), ~ {.x*pitch_dim$to[2]/pitch_dim$from[2]})

  # if y-axis is reversed in data (top left is 0, bottom is pitch_dim[2]) -> reverse in
  # whole dataset for plotting
  if(reverse_y) {
    df <- df %>% mutate_at(vars(start_y, end_y), ~{pitch_dim$to[2] -.x})
  }

  x_breaks <- seq(0, pitch_dim$to[1], length.out = bins_x + 1)
  y_breaks <- rev(seq(0, pitch_dim$to[2], length.out = bins_y + 1))

  x_centers <- tibble(bin_start_x = factor(1:bins_x), x_center = RcppRoll::roll_mean(x_breaks, 2))
  y_centers <- tibble(bin_start_y = factor(1:bins_y), y_center = RcppRoll::roll_mean(y_breaks, 2))

  bin_centers <- expand_grid(x_centers, y_centers)

  df$bin_start_x <- cut(df$start_x, breaks = x_breaks, include.lowest = TRUE,
                                 labels = 1:bins_x)
  df$bin_start_y <- cut(df$start_y, breaks = y_breaks,
                                 labels = bins_y:1)

  df$bin_end_x <- cut(df$end_x, breaks = x_breaks,
                               labels = 1:bins_x)
  df$bin_end_y <- cut(df$end_y, breaks = y_breaks,
                               labels = bins_y:1)
  prep_data <- df %>% select(start_x, start_y, end_x, end_y, actiontype, result, bin_start_x, bin_start_y,
                            bin_end_x, bin_end_y)
  return(list(prep_data = prep_data, bin_centers = bin_centers))

}

get_move_events <- function(df, move_events = c("pass", "dribble", "cross", "goalkick")) {
  df %>% filter(actiontype %in% move_events)
}

get_shot_events <- function(df, shotname = "shot") {
  df %>% filter(actiontype == shotname)
}

#' Computes bin-wise probabilites for moving ball/shooting/scoring
#'
#' @param df Event data set in SPADL format with bin memberships for coordinates,
#' as returned by \link{prepare_data}.
#' @param bins_x Number of bins in `x` direction
#' @param bins_y Number of bins in `x` direction
#' @return A tibble with a probability of shooting, scoring and moving the ball
#' at a given bin on the grid, for all possible combinations of bins.
#' @export
#'
#' @examples
#' data(spadlData)
#' prep_dat <- prepare_data(spadlData)
#' compute_action_prob(prep_dat$prep_data)
compute_action_prob <- function(df, bins_x = 16, bins_y = 12) {
  move_df <- get_move_events(df)
  shot_df <- get_shot_events(df)

  count_move <- move_df %>% group_by(bin_start_x, bin_start_y) %>%
    summarise(count_move = n(), .groups = "drop")
  count_shots <- shot_df %>% group_by(bin_start_x, bin_start_y) %>%
    summarise(count_shots = n(), .groups = "drop")
  count_goal <- shot_df %>%
    filter(result == "success") %>%
    group_by(bin_start_x, bin_start_y) %>%
    summarise(count_goal = n(), .groups = "drop")

  all_counts <- count_move %>% left_join(count_shots, by = c("bin_start_x", "bin_start_y")) %>%
    left_join(count_goal, by = c("bin_start_x", "bin_start_y"))

  all_probs <- all_counts %>%
    replace_na(list(count_move = 0, count_shots = 0, count_goal = 0)) %>%
    mutate(move_prob = count_move/(count_move + count_shots),
           shot_prob = 1 - move_prob,
           goal_prob = ifelse(count_shots >= 5, count_goal/count_shots, 0)) %>%
    select(-starts_with("count"))

  if(!(nrow(all_probs) == bins_x*bins_y)) {
    bin_combis <- expand_grid(bin_start_x = factor(1:bins_x),
                              bin_start_y = factor(1:bins_y))
    all_probs <- bin_combis %>% left_join(all_probs, by = c("bin_start_x", "bin_start_y"))
  }
  all_probs
}

#' Compute transition matrices
#'
#' @inheritParams compute_action_prob
#' @param df
#' @param bins_x
#' @param bins_y
#'
#' @return A tibble with `bins_x` times `bins_y` rows, containing a tibble with
#' the transition probabilities for each combination of x bin and y bin in a column
#' named `trans_prob`.
#'
#' @export
#'
#' @examples
#' data(spadlData)
#' prep_dat <- prepare_data(spadlData)
#' compute_trans_mat(prep_dat$prep_data)
compute_trans_mat <- function(df, bins_x = 16, bins_y = 12) {
  move_df <- get_move_events(df)
  # move_df <- move_df %>% filter(result == "success")
  move_df <- move_df[complete.cases(move_df), ]

  trans_mat <- move_df %>% group_by(bin_start_x, bin_start_y, bin_end_x, bin_end_y) %>%
    tally() %>%
    ungroup() %>%
    group_by(bin_start_x, bin_start_y) %>%
    mutate(n_start = sum(n, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(trans_prob = n/n_start)

  bin_combis <- expand_grid(bin_start_x = factor(1:bins_x),
                            bin_start_y = factor(1:bins_y),
                            bin_end_x = factor(1:bins_x),
                            bin_end_y = factor(1:bins_y))

  transitions <- bin_combis %>%
    left_join(trans_mat, by = c("bin_start_x", "bin_start_y", "bin_end_x", "bin_end_y")) %>%
    select(-c(n, n_start)) %>%
    replace_na(list(trans_prob = 0))

  transitions %>% group_by(bin_start_x, bin_start_y) %>%
    nest(trans_probs = c(bin_end_x, bin_end_y, trans_prob)) %>% ungroup()

 }

#' Compute xT values
#'
#' @inheritParams compute_trans_mat
#' @param df
#' @param bins_x
#' @param bins_y
#' @param itermax The maximal number of iterations to carry out
#' @param eps The precision to calculate the xT value of a cell; iterations are
#' carried out until precision or maximal number of iteration is reached
#'
#' @return A list with elements
#' * `xT_final`: a tibble containing ball moving, shooting and scoring probability as
#' well as the expected threat for each cell.
#' * `heatmaps` A list with a heatmap for each iteration.
#' @export
#'
#' @examples
#' data(spadlData)
#' prep_dat <- prepare_data(spadlData)
#' xT <- compute_xT(prep_dat$prep_data, itermax = 50, eps = 1e-03)
#' xT$xT_final
#'
#' ## plot heatmap of xT:
#' xT_tib <- prep_dat$bin_centers %>% left_join(xT$xT_final)
#'
#' xT_tib %>% ggplot(aes( x = x_center, y = y_center, fill = xT)) +
#'   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#'   scale_fill_gradient(low = "white", high = "darkred")+
#'   draw_pitch(dimensions = c(105, 68), fill = NA)+
#'   geom_text(aes(label = round(xT, 2)))+ theme(axis.title = element_blank())
#'
#' # visualise the 5 iterations:
#'
#' plotlist <- purrr::map2(xT$heatmaps, 1:length(xT$heatmaps), ~ {
#'  prep_dat$bin_centers %>% left_join(.x, by = c("bin_start_x", "bin_start_y")) %>%
#'  ggplot(aes( x = x_center, y = y_center, fill = xT)) +
#'   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#'   scale_fill_gradient(low = "white", high = "darkred")+
#'   draw_pitch(dimensions = c(105, 68), fill = NA)+
#'   geom_text(aes(label = round(xT, 2)))+ theme(axis.title = element_blank())+
#'   ggtitle(paste("xT after", .y, "iterations"))
#' })
#' ggpubr::ggarrange(plotlist = plotlist, common.legend = TRUE)
#'
#' ##### with less cells: 12 in x direction, 8 in y direction
#'
#' prep_dat <- prepare_data(spadlData, bins_x = 12, bins_y = 8)
#' xT <- compute_xT(prep_dat$prep_data, itermax = 100, eps = 1e-03, bins_x = 12, bins_y = 8)
#' xT$xT_final
#'
#' ## plot heatmap of xT:
#' xT_tib <- prep_dat$bin_centers %>% left_join(xT$xT_final)
#'
#' xT_tib %>% ggplot(aes( x = x_center, y = y_center, fill = xT)) +
#'   geom_tile(width = 105/12, height = 68/8, colour = "gray")+
#'   scale_fill_gradient(low = "white", high = "darkred")+
#'   draw_pitch(dimensions = c(105, 68), fill = NA)+
#'   geom_text(aes(label = round(xT, 3)))+ theme(axis.title = element_blank())
compute_xT <- function(df, bins_x = 16, bins_y = 12, itermax = 100, eps = 1e-05) {

  action_probs <- compute_action_prob(df, bins_x = bins_x, bins_y = bins_y)


  shot_exp_pay <- action_probs$shot_prob*action_probs$goal_prob

  transition_df <- compute_trans_mat(df, bins_x = bins_x, bins_y = bins_y)

  xT <- numeric(bins_x*bins_y)
  diff <- 1
  iter <- 0
  heatmaps <- list()
  xT_tmp <- action_probs %>% select(bin_start_x, bin_start_y)
  while(any(diff > eps) & iter < itermax) {
    iter <- iter + 1
    move_pay <- action_probs$move_prob * purrr::map_dbl(transition_df$trans_probs, ~ sum(.x$trans_prob*xT, na.rm = TRUE))

    xTnew <- shot_exp_pay + move_pay

    diff <- xTnew - xT
    xT <- xTnew
    xT_tmp$xT <- xT

    heatmaps <- append(heatmaps, list(xT_tmp))

  }
  print(iter)
  print(c("max_diff" = max(diff)))

  action_probs$xT <- xT

  return(list(xT_final = action_probs, heatmaps = heatmaps))

}
####################################


# prep_dat <- prepare_data(data,
#                          pitch_dim = list(from = c(120, 80), to = c(105, 68)))
#
# xT <- compute_xT(df = prep_dat$prep_data, eps = 1e-04, itermax = 5)
#
# xT
#
# xT <- prep_dat$bin_centers %>% left_join(xT)
# xT
#
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = xT)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(xT, 3)))
#
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = shot_prob)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(shot_prob, 2)))
#
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = goal_prob)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(goal_prob, 2)))
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = move_prob)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(move_prob, 2)))
#
# ################
#
# xT$xT_github <- as.vector(obj@xT)
# xT$shot_github <- as.vector(obj@shot_prob_matrix)
# xT$goal_github <- as.vector(obj@scoring_prob_matrix)
# xT$move_github <- as.vector(obj@move_prob_matrix)
#
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = xT_github)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(xT_github, 2)))
#
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = shot_github)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(shot_github, 2)))
#
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = goal_github)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(goal_github, 2)))
#
#
# xT %>% ggplot(aes( x = x_center, y = y_center, fill = move_github)) +
#   geom_tile(width = 105/16, height = 68/12, colour = "gray")+
#   scale_fill_gradient(low = "white", high = "darkred")+
#   scoutr::fc_annotate_pitch(dimensions = c(105, 68), fill = NA)+
#   geom_text(aes(label = round(move_github, 2)))
#
# obj@scoring_prob_matrix
#
