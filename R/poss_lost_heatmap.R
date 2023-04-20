


count_lost_poss <- function(poss_vec, possession, team) {

  teams <- unique(poss_vec)
  opponent <- teams[!(teams == team)]

  nposs <- length(poss_vec)
  count_loss <- 0
  index_loss <- numeric()

  for( j in 1: (nposs -1)) {
    if(poss_vec[j] == team & poss_vec[j+1] == opponent) {
      count_loss <- count_loss +1
      index_loss <- c(index_loss, possession[j])
    }
  }

  df_indexloss <- tibble(possession = possession) %>% mutate(loss = ifelse(possession %in% index_loss, TRUE, FALSE))
 list(count_loss = count_loss, df_indexloss = df_indexloss)
}

#' Heatmap of possession losses
#'
#' @inheritParams pass_heatmap
#' @param LeftToRight Logical; whether you want the team to play from left to right
#'  in the visualisation
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' data(eventData)
#' poss_loss_heatmap(eventData, "Borussia Dortmund")
#' poss_loss_heatmap(eventData, "Bayern Munich", LeftToRight = FALSE, xBins = 6, yBins = 4)
poss_loss_heatmap <- function(df, team, xBins = 8, yBins = 5, pitch_dim = c(120, 80),
                              LeftToRight = TRUE) {


  reverse_scale <- ifelse(LeftToRight, scale_y_reverse, scale_x_reverse)

  df <- correct_locations(df, pitch_dim = pitch_dim)
  poss_nest <- df %>% filter(!(type.name %in% c("Half Start", "Starting XI"))) %>% unique() %>%
    select(possession, possession_team.name, minute, second, timestamp, team.name, type.name,
           location.x, location.y, contains("outcome"), duration)

  poss_nest <- poss_nest %>% group_by(possession, possession_team.name) %>% nest()


  lost_poss <- count_lost_poss(poss_vec = poss_nest$possession_team.name,
                               possession = poss_nest$possession, team = team)

  poss_nest <- poss_nest %>% left_join(lost_poss$df_indexlos, by = "possession")

  poss_nest <- poss_nest %>%
    mutate(last_event = purrr::map(data, ~ {
      ndata <- nrow(.x)
      xtmp <- .x[ndata, ]
      event_name <- xtmp$type.name
      while(event_name == "Substitution") {
        ndata <- ndata - 1
        xtmp <- .x[ndata, ]
        event_name <- xtmp$type.name
      }
      event_x <- xtmp$location.x
      event_y <- xtmp$location.y
      tibble(last_event_name = event_name, location_x = event_x, location_y = event_y)
      } ))

  poss_nest <- poss_nest %>% unnest(cols = last_event)

  poss_lost <- poss_nest %>% filter(!(last_event_name %in% c("Injury Stoppage", "Shot")))

  x.range <- seq(0, pitch_dim[1], length.out = xBins+1)
  y.range <- seq(0, pitch_dim[2], length.out = yBins+1)

  df <- within(poss_lost, {
    grp.x = cut(location_x, breaks = x.range, labels = RcppRoll::roll_mean(x.range, 2))
    grp.y = cut(location_y, breaks = y.range, labels = RcppRoll::roll_mean(y.range, 2))
  })



  loss_total <- sum(df$loss)

  df <-  df %>% filter(loss == TRUE) %>% group_by(grp.x, grp.y) %>%
    tally()

  df <- df %>%
    mutate(avg = n/loss_total) %>% ungroup()

  df <- df[complete.cases(df), ]
  df %>%
    ggplot(aes( x = as.numeric(as.character(grp.x)), y =as.numeric(as.character(grp.y)), fill = avg*100 )) +
    geom_tile(colour = "gray")+
    reverse_scale()+
    draw_pitch(dimension = c(120, 80), fill = NA, palette = "bw", reverse_scale = reverse_scale) +
    viridis::scale_fill_viridis(option = "F", direction = -1, begin = 0.4)+
    labs(title = "Distribution of possession losses", subtitle = team, fill = "Lost Possession (in %):")+
    theme_bw()+
    theme(axis.text = element_blank(), axis.title = element_blank(), legend.position = "bottom")

}
