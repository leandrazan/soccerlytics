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

correct_locations <- function(df, pitch_dim = c(120, 80), name_x = "location.x",
                              name_y = "location.y") {
  df$location.x <- df[ , name_x]
  df$location.y <- df[, name_y]

  df$location.x[df$location.x <= 0] <- 0.001
  df$location.x[df$location.x >= pitch_dim[1]] <- pitch_dim[1] - 0.001

  df$location.y[df$location.y <= 0] <- 0.001
  df$location.y[df$location.y >= pitch_dim[2]] <- pitch_dim[2] - 0.001

  df[ , name_x] <- df$location.x
  df[, name_y] <- df$location.y

  if(!(name_x == "location.x")) {
  df <- df %>% select(-location.x)
  }
  if(!(name_y == "location.y")) {
    df <- df %>% select(-location.y)
  }
  df
}


translate_wyscout_ids <- function(id) {
  if(is.na(id)) {return(NA)}
  else {

    if(!is.character(id)) {id <- as.character(id)}

   id_name <- switch(id,
           "101" = 	"Goal",
           "102"=		"Own goal",
           "301"=	"Assist",
           "302"=	"Key pass",
           "1901"=		"Counter attack",
           "401"	= "Left foot",
           "402"=		"Right foot",
           "403"=	"Head/body",
           "1101"=		"Direct",
           "1102"=		"Indirect",
           "2001"="Dangerous ball lost",
           "2101"=	"Blocked",
           "801"="High",
           "802"=	"Low",
           "1401"=	"Interception",
           "1501"=	"Clearance",
           "201"=		"Opportunity",
           "1301"=	"Feint",
           "1302"=	"Missed ball",
           "501"=		"Free space right",
           "502"=	"Free space left",
           "503"=	"Take on left",
           "504"=	"Take on right",
           "1601"=	"Sliding tackle",
           "601"	=	"Anticipated",
           "602"	=	"Anticipation",
           "1701"=	"Red card",
           "1702"=	"Yellow card",
           "1703"=		"Second yellow card",
           "1201"=	"Goal low center",
           "1202"=	 "Goal low right",
           "1203"= "Goal center",
           "1204"	= "Goal center left",
           "1205"	= "Goal low left",
           "1206"	= "Goal center right",
           "1207"	= "Goal high center",
           "1208"	="Goal high left",
           "1209"	= "Goal high right",
           "1210"	= "Out low right",
           "1211"	= "Out center left",
           "1212"= "Out low left",
           "1213"	= "Out center right",
           "1214"	= "Out high center",
           "1215"	= "Out high left",
           "1216"	="Out high right",
           "1217"	= "Post low right",
           "1218"= "Post center left",
           "1219"	= "Post low left",
           "1220"	= "Post center right",
           "1221"	= "Post high center",
           "1222"	="Post high left",
           "1223"	= "Post high right",
           "901"	= "Through",
           "1001"	=	"Fairplay",
           "701"	=	"Lost",
           "702"	=	"Neutral",
           "703"	=	"Won",
           "1801"	=	"Accurate",
           "1802"	=	"Not accurate"
                    )
   id_name
  }
}

translate_wyscout_ids_vec <- Vectorize(translate_wyscout_ids)
