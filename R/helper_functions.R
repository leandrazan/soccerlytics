## get abbreviations of player positions
get_pos_abb <- function(pos_name) {

  name_split <- stringr::str_split(pos_name, " ")[[1]]

  start_letters <- purrr::map(purrr::map(name_split, ~ stringr::str_split(.x, "")[[1]]), ~.x[1])

  paste(unlist(start_letters), collapse = "")
}

# get last names

get_last_name <- function(playername) {
  str_name <- unlist(str_split(playername, " "))
  str_name[length(str_name)]
}


get_jersey_numbers <- function(df) {

  df %>% select(formation_jersey_number, player_name = formation_player_name) %>%
  unique() %>% filter(!is.na(player_name))

}

correct_locations <- function(df, pitch_dim = c(120, 80)) {
  df$location.x[df$location.x <= 0] <- 0.001
  df$location.x[df$location.x >= pitch_dim[1]] <- pitch_dim[1] - 0.001

  df$location.y[df$location.y <= 0] <- 0.001
  df$location.y[df$location.y >= pitch_dim[2]] <- pitch_dim[2] - 0.001

  df
}
