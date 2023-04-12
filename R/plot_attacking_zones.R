# library(tidyverse)
#
# source(paste0(getwd(), "/R Barnsley/compute_attacking_zones.R"))
#
# theme_set(theme_bw() + theme(text = element_text(size = 16), legend.position = "bottom",
#                              axis.title = element_blank()))
#
#
# events <- read_csv("Barnsley_Portsmouth_3845285.csv", guess_max = 4000)
#
# dfAttack <- compute_attacking_zones(events)
# dfAttack
#
#
#
# plot_attack_zones(dfAttack, team = "Barnsley") + theme(legend.key.width = unit(1, "cm"))
# ggsave(paste0(getwd(), "/Plots/attackZones.pdf"), device = "pdf", height = 8, width = 12)
#
# plot_attack_zones(dfAttack, team = "Portsmouth", leftToRight = FALSE)+ theme(legend.key.width = unit(1, "cm"))
# ggsave(paste0(getwd(), "/Plots/Portsmouth/attackZones.pdf"), device = "pdf", height = 8, width = 12)
