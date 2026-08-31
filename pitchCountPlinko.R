
library(readr)
library(ggplot2)
library(lemon)
library(dplyr)
yd1 <- read_csv("HarwichGames/20250802-CCBLYarmouth-1.csv")
#View(yd1)

yd2 <- read_csv("HarwichGames/20250729-CCBLYarmouth-2.csv")
#View(yd2)


combined_yd <- bind_rows(yd1, yd2)


pitchTypes <- c("Fastball" = "firebrick",
                "Curveball" = "dodgerblue",
                "Slider" = "gold",
                "ChangeUp" = "green4",
                "Cutter" = "darkblue",
                'Sinker' = "orangered1",
                "Splitter" = "darkorchid4",
                "Knuckleball" = "pink")

#set up counts
count_positions <- data.frame(
  count_state = c("0-0", "0-1", "1-0", "0-2", "1-1", "2-0", "1-2", "2-1", "3-0", "2-2", "3-1", "3-2"),
  x = c(0, -1, 1, -2, 0, 2, -2, 0, 2, -1, 1, 0),
  y = c(0, -1, -1, -2, -2, -2, -3, -3, -3, -4, -4, -5)
)


#BENTLEY

 bentStats <- combined_yd %>% filter(Pitcher == "Bentley, Noah")

library(ggplot2)
library(ggridges)
library(ggforce)
ggplot(bentStats, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType, fill = TaggedPitchType)) + 
  geom_point() +
  stat_ellipse(geom = "polygon", alpha = 0.3, color = NA) +  
  xlim(-25, 25) + ylim(-25, 25) + coord_equal() + 
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  theme_bw() + 
  labs(title = "Pitch Movement Plot",
       x = "Horz. Break (in.)",
       y = "IVB (in.)",
       color = "Pitch Type",
       fill = "Pitch Type") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = pitchTypes) + 
  scale_fill_manual(values = pitchTypes)   



bentStats_RHH <- bentStats %>% filter(BatterSide == "Right")

#number of pitches
pitch_counts_RHH <- bentStats_RHH %>%
  count(TaggedPitchType, sort = TRUE)

# # View the count table
# print(pitch_counts)

#count coordinate positions
linedf_RHH = data.frame(
  count_state = c("0-0","0-0", "0-1", "0-1", "1-0", "1-0", "0-2", "1-1", "1-1", "2-0", "2-0", "1-2", "2-1", "2-1", "3-0", "2-2", "3-1"),
  next_count_state = c("0-1", "1-0", "0-2", "1-1", "1-1", "2-0", "1-2", "1-2", "2-1", "2-1", "3-0", "2-2", "2-2", "3-1", "3-1", "3-2", "3-2"),
  x = c(-0.2828, 0.2828, -1.2828, -0.717, 0.717, 1.283, -2.000, -0.358, 0.000, 1.620, 2.000, -1.717, -0.283, 0.283, 1.717, -0.717, 0.717),
  y = c(-0.2828, -0.2828, -1.2828, -1.2828, -1.283, -1.283, -2.400, -2.179, -2.400, -2.000, -2.400, -3.283, -3.283, -3.283, -3.283, -4.283, -4.283),
  xend = c(-0.7172, 0.7172, -1.7172, -0.283, 0.283, 1.717, -2.000, -1.642, 0.000, 0.358, 2.000, -1.283, -0.717, 0.717, 1.283, -0.283, 0.283),
  yend = c(-0.7172, -0.7172, -1.7172, -1.717, -1.717, -1.717, -2.600, -2.821, -2.600, -2.821, -2.600, -3.717, -3.717, -3.717, -3.717, -4.717, -4.717)
)

#set up count column
bentStats_RHH <- bentStats_RHH %>%
  mutate(count_state = paste0(Balls, "-", Strikes))

#get actual donut percentages
pitch_by_count <- bentStats_RHH %>%
  count(count_state, TaggedPitchType) %>%
  group_by(count_state) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

#sort by count
bentStats_RHH <- bentStats_RHH %>%
  arrange(GameID, Inning, Batter, PitchofPA) %>%
  group_by(GameID, Inning, Batter) %>%
  mutate(PA_ID = cur_group_id()) %>%
  ungroup()

#set next count column
bentStats_RHH <- bentStats_RHH %>%
  group_by(PA_ID) %>%
  arrange(PitchofPA) %>%
  mutate(
    next_balls = lead(Balls),
    next_strikes = lead(Strikes),
    next_count_state = paste0(next_balls, "-", next_strikes),
      paste0(next_balls, "-", next_strikes)
    ) %>%
  ungroup()

#determine next count percentages
count_transitions <- bentStats_RHH %>%
  count(count_state, next_count_state) %>%
  group_by(count_state) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup() %>%
  filter(
    next_count_state != "NA-NA",      #drop terminal states
    count_state != next_count_state   #drop self-loops (like 1-2 → 1-2)
  )

#print(count_transitions)

#extra df for bigger white points
linedfpt2 = data.frame(
  count_state = c("1-1","2-0"),
  next_count_state = c("1-2","2-1"),
  x = c(-0.358, 1.620),
  y = c(-2.179, -2.000),
  xend = c(-1.642, 0.358),
  yend = c(-2.821, -2.821)
)

#add next count percentages to line df
linedf_RHH <- linedf_RHH %>%
  left_join(count_transitions %>% select(count_state, next_count_state, prob),
            by = c("count_state", "next_count_state"))

#get donut parts
 plot_data_RHH <- pitch_by_count %>%
  left_join(count_positions, by = "count_state") %>%
  arrange(count_state, TaggedPitchType) %>%
  group_by(count_state) %>%
  mutate(
    start = cumsum(lag(percent, default = 0)) * 2 * pi,
    end = cumsum(percent) * 2 * pi,
    mid = (start + end) / 2
  ) %>%
  ungroup()
 
#plot plot plot plot plot plot
pR <- ggplot() +
  geom_arc_bar(
    data = plot_data_RHH,
    aes(x0 = x, y0 = y, r0 = 0.2, r = 0.4, start = start, end = end, fill = TaggedPitchType),
    color = "white"
  ) +
  geom_text(
    data = count_positions,
    aes(x = x, y = y, label = count_state),
    size = 4,
    fontface = "bold"
  ) +
  geom_segment(
    data = linedf_RHH,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black",
    alpha = 0.6
  ) +
  geom_point(
    data = linedf_RHH,
    aes(x = (x + xend) / 2, y = (y + yend) / 2),
    color = "white", 
    size = 5
  ) +
  geom_point(
    data = linedfpt2,
    aes(x = (x + xend) / 2, y = (y + yend) / 2),
    color = "white", 
    size = 7
  ) +
  geom_text(
    # data = linedf,
    data = linedf_RHH %>% filter(!is.na(prob)),
    aes(x = (x + xend)/2, y = (y + yend)/2,
        label = scales::percent(prob, accuracy = 1)),
    size = 3, color = "black"
  ) +
  scale_fill_manual(values = pitchTypes, na.value = "grey80") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")

###############################################################################

bentStats_LHH <- bentStats %>% filter(BatterSide == "Left")

pitch_counts_LHH <- bentStats_LHH %>%
  count(TaggedPitchType, sort = TRUE)

bentStats_LHH <- bentStats_LHH %>%
  mutate(count_state = paste0(Balls, "-", Strikes))

linedf_LHH = data.frame(
  count_state = c("0-0","0-0", "0-1", "0-1", "1-0", "1-0", "0-2", "1-1", "1-1", "2-0", "2-0", "1-2", "2-1", "2-1", "3-0", "2-2", "3-1"),
  next_count_state = c("0-1", "1-0", "0-2", "1-1", "1-1", "2-0", "1-2", "1-2", "2-1", "2-1", "3-0", "2-2", "2-2", "3-1", "3-1", "3-2", "3-2"),
  x = c(-0.2828, 0.2828, -1.2828, -0.717, 0.717, 1.283, -2.000, -0.358, 0.000, 1.620, 2.000, -1.717, -0.283, 0.283, 1.717, -0.717, 0.717),
  y = c(-0.2828, -0.2828, -1.2828, -1.2828, -1.283, -1.283, -2.400, -2.179, -2.400, -2.000, -2.400, -3.283, -3.283, -3.283, -3.283, -4.283, -4.283),
  xend = c(-0.7172, 0.7172, -1.7172, -0.283, 0.283, 1.717, -2.000, -1.642, 0.000, 0.358, 2.000, -1.283, -0.717, 0.717, 1.283, -0.283, 0.283),
  yend = c(-0.7172, -0.7172, -1.7172, -1.717, -1.717, -1.717, -2.600, -2.821, -2.600, -2.821, -2.600, -3.717, -3.717, -3.717, -3.717, -4.717, -4.717)
)


pitch_by_count <- bentStats_LHH %>%
  count(count_state, TaggedPitchType) %>%
  group_by(count_state) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

bentStats_LHH <- bentStats_LHH %>%
  arrange(GameID, Inning, Batter, PitchofPA) %>%
  group_by(GameID, Inning, Batter) %>%
  mutate(PA_ID = cur_group_id()) %>%
  ungroup()

bentStats_LHH <- bentStats_LHH %>%
  group_by(PA_ID) %>%
  arrange(PitchofPA) %>%
  mutate(
    next_balls = lead(Balls),
    next_strikes = lead(Strikes),
    next_count_state = paste0(next_balls, "-", next_strikes),
    paste0(next_balls, "-", next_strikes)
  ) %>%
  ungroup()


count_transitions <- bentStats_LHH %>%
  count(count_state, next_count_state) %>%
  group_by(count_state) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup() %>%
  filter(
    next_count_state != "NA-NA",      # drop terminal states
    count_state != next_count_state   # drop self-loops (like 1-2 → 1-2)
  )


print(count_transitions)


count_positions <- data.frame(
  count_state = c("0-0", "0-1", "1-0", "0-2", "1-1", "2-0", "1-2", "2-1", "3-0", "2-2", "3-1", "3-2"),
  x = c(0, -1, 1, -2, 0, 2, -2, 0, 2, -1, 1, 0),
  y = c(0, -1, -1, -2, -2, -2, -3, -3, -3, -4, -4, -5)
)

linedf_LHH <- linedf_LHH %>%
  left_join(count_transitions %>% select(count_state, next_count_state, prob),
            by = c("count_state", "next_count_state"))

plot_data_LHH <- pitch_by_count %>%
  left_join(count_positions, by = "count_state") %>%
  arrange(count_state, TaggedPitchType) %>%
  group_by(count_state) %>%
  mutate(
    start = cumsum(lag(percent, default = 0)) * 2 * pi,
    end = cumsum(percent) * 2 * pi,
    mid = (start + end) / 2
  ) %>%
  ungroup()

pL <- ggplot() +
  geom_arc_bar(
    data = plot_data_LHH,
    aes(x0 = x, y0 = y, r0 = 0.2, r = 0.4, start = start, end = end, fill = TaggedPitchType),
    color = "white"
  ) +
  geom_text(
    data = count_positions,
    aes(x = x, y = y, label = count_state),
    size = 4,
    fontface = "bold"
  ) +
  geom_segment(
    data = linedf_LHH,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "black",
    alpha = 0.6
  ) +
  geom_point(
    data = linedf_LHH,
    aes(x = (x + xend) / 2, y = (y + yend) / 2),
    color = "white", 
    size = 5
  ) +
  geom_point(
    data = linedfpt2,
    aes(x = (x + xend) / 2, y = (y + yend) / 2),
    color = "white", 
    size = 7
  ) +
  geom_text(
    data = linedf_LHH %>% filter(!is.na(prob)),
    aes(x = (x + xend)/2, y = (y + yend)/2,
        label = scales::percent(prob, accuracy = 1)),
    size = 3, color = "black"
  ) +
  scale_fill_manual(values = pitchTypes, na.value = "grey80") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")


###############################################################################

#get legend

pR_withLegend <- ggplot() +
  geom_arc_bar(
    data = plot_data_RHH,
    aes(x0 = x, y0 = y, r0 = 0.2, r = 0.4, start = start, end = end, fill = TaggedPitchType),
    color = "white",
    show.legend = FALSE
  ) +
  geom_point(
    data = plot_data_RHH %>% distinct(TaggedPitchType),
    aes(x = 0, y = 0, fill = TaggedPitchType),
    shape = 21, size = 5, color = "black"
  ) +
  scale_fill_manual(values = pitchTypes, na.value = "grey80") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(
      title = "Pitch Type",
      override.aes = list(shape = 21)
    )
  )

library(gridExtra)

#plot next to each other
legend <- g_legend(pR_withLegend)
pL_nolegend <- pL + theme(legend.position = "none")
pR_nolegend <- pR + theme(legend.position = "none")
grid.arrange(
  arrangeGrob(pL_nolegend, pR_nolegend, ncol = 2),
  legend,
  ncol = 1,
  heights = c(10, 1)
)




