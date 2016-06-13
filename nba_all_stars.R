library("dplyr")
library("reshape2")
library("ggplot2")
library("gridExtra")

# ----------

## Load the dataset.

# Set the working directory.
# setwd("")

nba_all_stars <- read.csv("./nba_all_stars/nba_all_stars.csv", header = TRUE, 
                          stringsAsFactors = FALSE, strip.white = TRUE,
                          sep = ",", fileEncoding = "UTF-8")


## Clean the names of the players (remove *, ^ and [.]).
nba_all_stars <- mutate(nba_all_stars,
                        player = gsub("\\*|\\^|\\[\\w*\\]", "", player))


## Save the file.
# write.table(nba_all_stars, "./nba_all_stars/nba_all_stars_clean.csv", 
#            sep =",", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")


## Keep only the players drafted in 1990 or after.

# Load the file with the personnal information downloaded and 
# pre-processed in Python.
players_info <- read.csv("./nba_all_stars/nba_all_stars_info.csv", 
                         header = TRUE, stringsAsFactors = FALSE, sep = ";",
                         fileEncoding = "UTF-8")

# Select the players drafted in 1990 or after.
players_info_1990 <- filter(players_info, draft_year >= 1990)

# Save the names of the players drafted in 1990 or after.
# write.table(players_info_1990$player, "./nba_all_stars/players_1990.txt",
#             sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE,
#             fileEncoding = "UTF-8")

# Merge "players_info_1990" and "nba_all_stars" together.
df <- left_join(players_info_1990, nba_all_stars, by = "player")

# ----------

## Helpers.

# ggplot2 theme.
theme_light_grey <- function(base_size = 16, text_size = 18) {
  bg_color <- "#f4f4f4"
  bg_rect <- element_rect(fill = bg_color, color = bg_color)
  
  theme_bw(base_size = base_size) +
    theme(axis.title = element_text(size = text_size),
          plot.background = bg_rect,
          panel.background = bg_rect,
          panel.border = element_rect(fill = NA, colour = "grey80"),
          legend.background = bg_rect,
          panel.grid.major = element_line(colour = "grey80", size = 0.25),
          panel.grid.minor = element_line(colour = "grey90", size = 0.25),
          legend.key.width = unit(1.5, "line"),
          legend.key = element_blank())
}

# A special format for the plots with two digits.
two_digits <- function(x) {
  format(x, nsmall = 2, scientific = FALSE)
}

# Colors used by ggplot2.
ggplot_colors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

scales::show_col(ggplot_colors(n = 5))

# Proportion.
proportion <- function(x) {
  cbind(count = table(x, useNA = "ifany"), 
        proportion = table(x, useNA = "ifany") %>%
          prop.table() %>%
          round(2))
}

# ----------

## Number of selections.

# Histogram of the number of selections.
ggplot(data = df, aes(x = number_of_selections)) +
  geom_histogram(bins = 16) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16)) +
  xlab("\nnumber of selections to the NBA All-Star Game") + 
  ylab("count\n") +
  theme_light_grey()

# ggsave("./nba_all_stars/plots/number_of_selections_histogram_2.pdf",
#        width = 12, height = 9)

# Players with more than 15 selections.
df[df$number_of_selections >= 15, c("player", "number_of_selections")]

# ----------

## Positions of the players.

# Recode the positions.
df[df$position == "Point guard, shooting guard", "position"] <- 
  "point guard / shooting guard"
df[df$position == "Power forward / Center[1]", "position"] <- 
  "power forward / Center"

# 5 players are listed as "Forward". 4 of them are "Small Forward" and 
# the last one - Antoine Walker - is a "Power Forward".
df[df$player == "Antoine Walker", "position"] <- "power forward"
df[df$position == "Forward", "position"] <- "small forward"

# There is a player listed as "Point Guard / Center" which seems ... odd.
df[df$position == "Point Guard / Center", "player"]
# Vin Baker was basically a Center.
df[df$position == "Point Guard / Center", "position"] <- "center"

# When two positions are listed we consider only the first one.
df <- mutate(df, position_5 = gsub(pattern = " */.*", replacement = "", 
                                   position) %>%
               tolower())

# Allen Iverson and Penny Hardaway are considered as "Guards". 
# We changed that to "Point guards".
df[df$position_5 == "guard", "player"]
df[df$position_5 == "guard", "position_5"] <- "point guard"

# The 5 positions: point guard (PG), shooting guard (SG), 
# small forward (SF), power forward (PF), and center (C).
# See: https://en.wikipedia.org/wiki/Basketball_positions

# Convert "postion_5" to a factor.
df$position_5 <- factor(x = df$position_5,
                        levels = c("point guard", "shooting guard", 
                                   "small forward", "power forward", "center"))
proportion(df$position_5)

# Barplot of the positions of the players.
ggplot(data = df, aes(x = position_5)) +
  geom_bar(width = 0.6) +
  scale_y_continuous(limits = c(0, 35)) +
  xlab("\npositions of the players") + 
  ylab("count\n") +
  theme_light_grey()

# ggsave("./nba_all_stars/plots/positions_barplot.pdf",
#        width = 12, height = 9)

# Dotplot of the positions of the players.
df %>%
  group_by(position_5) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = position_5)) +
    geom_point(size = 4) +
    scale_x_continuous(breaks = c(22, 26, 30, 34),
                       minor_breaks = c(24, 28, 32),
                       limits = c(20, 36)) +
    xlab("\ncount") + 
    ylab("positions of the players\n") +
    theme_light_grey()

# ggsave("./nba_all_stars/plots/positions_dotplot.pdf",
#        width = 12, height = 9)

# ----------

## Height and weight.

# Keep only the metric measures.
df <- mutate(df, height = gsub(pattern = ".*\\((.*)\\)", replacement = "\\1", 
                               listed_height) %>%
                                  substr(1, 4) %>%
                                  as.numeric(),
                 weight = gsub(pattern = ".*\\((.*)\\)", replacement = "\\1", 
                               listed_weight) %>% 
                                  substr(1, 3) %>%
                                  as.numeric())

# Boxplots of height and weight.
g1 <- ggplot(data = df, aes(x = factor(""), y = height)) +
  geom_boxplot() +
  xlab("") + 
  ylab("height (in m)\n") +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  theme_light_grey()

g2 <- ggplot(data = df, aes(x = factor(""), y = weight)) +
  geom_boxplot() +
  xlab("") + 
  ylab("weight (in kg)\n") +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  theme_light_grey()

grid.arrange(g1, g2, ncol = 2)

# g <- arrangeGrob(g1, g2, ncol = 2)
# ggsave("./nba_all_stars/plots/weigh_height_boxplot.pdf", g,
#        width = 12, height = 9)

# Boxplots of heigth and weight by position.
g1 <- ggplot(data = df, aes(x = position_5, y = height)) +
  geom_boxplot() +
  xlab("") + 
  ylab("\nheight (in m)\n") +
  scale_y_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  theme_light_grey() +
  coord_flip()

g2 <- ggplot(data = df, aes(x = position_5, y = weight)) +
  geom_boxplot() +
  xlab("") + 
  ylab("\nweight (in kg)") +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  theme_light_grey() +
  coord_flip()

grid.arrange(g1, g2, ncol = 1)

# g <- arrangeGrob(g1, g2, ncol = 1)
# ggsave("./nba_all_stars/plots/weigh_height_boxplot_positions.pdf", g,
#        width = 12, height = 9)

# Scatterplot of height and weight.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(aes(color = position_5), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_scatterplot.pdf",
#        width = 12, height = 9)

# Interesting values.
df[df$height > 2.25, "player"]  # Yao Ming.
df[df$weight > 140, "player"]  # Shaquille O'Neal & Yao Ming.
df[df$height < 1.8, "player"]  # Isaiah Thomas.
df[df$weight < 80, "player"]  # A. Iverson, C. Paul, K. Anderson & N. Van Exel. 

# Add the names of the "giants" and of the "little one" to the plot.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(aes(color = position_5), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  geom_text(data = df[df$player %in% c("Yao Ming", 
                                       "Shaquille O'Neal", 
                                       "Isaiah Thomas"), ],
            aes(label = player), size = 4.5, vjust = 2) +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_scatterplot_names.pdf",
#        width = 12, height = 9)

# Point guards.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(color = "grey40", size = 2.5) +
  geom_point(data = df[df$position_5 == "point guard", ],
             aes(color = "#F8766D"), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  scale_colour_manual(values = "#F8766D", labels = "point guard") +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_point_guards.pdf",
#        width = 12, height = 9)

# Shooting guards.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(color = "grey40", size = 2.5) +
  geom_point(data = df[df$position_5 == "shooting guard", ],
             aes(color = "#A3A500"), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  scale_colour_manual(values = "#A3A500", labels = "shooting guard") +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_shooting_guards.pdf",
#        width = 12, height = 9)

# Small forwards.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(color = "grey40", size = 2.5) +
  geom_point(data = df[df$position_5 == "small forward", ],
             aes(color = "#00BF7D"), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  scale_colour_manual(values = "#00BF7D", labels = "small forward") +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_small_forwards.pdf",
#        width = 12, height = 9)

# Power forwards.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(color = "grey40", size = 2.5) +
  geom_point(data = df[df$position_5 == "power forward", ],
             aes(color = "#00B0F6"), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  scale_colour_manual(values = "#00B0F6", labels = "power forward") +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_power_forwards.pdf",
#        width = 12, height = 9)

# Centers.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(color = "grey40", size = 2.5) +
  geom_point(data = df[df$position_5 == "center", ],
             aes(color = "#E76BF3"), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  scale_colour_manual(values = "#E76BF3", labels = "center") +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_centers.pdf",
#        width = 12, height = 9)

# Fit a local polynomial regression.
ggplot(data = df, aes(x = height, y = weight)) +
  geom_smooth(color = "grey65", fill = "grey80") +
  geom_point(aes(color = position_5), size = 2.5) +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave("./nba_all_stars/plots/weigh_height_poly_regression.pdf",
#        width = 12, height = 9)

# Fit 5 linear regressions (one for each position).
ggplot(data = df, aes(x = height, y = weight)) +
  geom_smooth(aes(color = position_5), fill = "grey80", method = "lm") +
  xlab("\nheight (in m)") + 
  ylab("weight (in kg)\n") +
  scale_x_continuous(labels = two_digits, limits = c(1.75, 2.30)) +
  scale_y_continuous(breaks = c(80, 100, 120, 140),
                     minor_breaks = c(90, 110, 130),
                     limits = c(70, 150)) +
  theme_light_grey() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(fill = NA)))

# ggsave("./nba_all_stars/plots/weigh_height_linear_regressions_positions.pdf",
#        width = 12, height = 9)

# ----------

## Years of selection.

years_selection <- data.table::tstrsplit(df$years, ";")

split_years <- function(x) {
  inf <- gsub("–.*", "", x) %>%
    trimws() %>%
    as.numeric()
  sup <- gsub(".*–", "", x) %>%
    gsub(";.*", "", .) %>%
    as.numeric()
  if (is.na(x)) {
    inf_sup <- inf
  } else {
    inf_sup <- seq(inf, sup, by = 1)
  }
  return(inf_sup)
}

years_selection <- lapply(years_selection, lapply, split_years)

years_selection <- do.call(Map, c(c, years_selection))

years_selection <- lapply(seq(1990, 2016, by = 1), function(year) {
  lapply(years_selection, function(selections) ifelse(year %in% selections, 
                                                      1, 0))
})

names_var <- names(df)

df <- years_selection %>%
  data.table::rbindlist() %>%
  data.table::transpose() %>%
  as.data.frame() %>%
  cbind(df, .)

names(df) <- c(names_var, paste0("year_", seq(1990, 2016, by = 1)))

# ----------

## Nationalities.

proportion(df$nationality)

# Joakim Noah has no nationality listed. 
# He is French (and also American and Swedish).
df[df$nationality == "", "nationality"] <- "French"

# Keep only the first nationality listed for each player.
df <- mutate(df, nationality_short = gsub(" /.*", "", nationality))

# Recode all the non-americans as Other.
df$nationality_other <- ifelse(df$nationality_short == "American", 
                               "American", "Other")
proportion(df$nationality_other)

ggplot(data = df, aes(x = nationality_other)) +
  geom_bar() +
  xlab("\nnationality") + 
  ylab("count\n") +
  theme_light_grey()

# ----------

# List of data frames.

list_df <- vector("list", 27)

list_df[[1]] <- df[df$year_1990 == 1, ]
list_df[[2]] <- df[df$year_1991 == 1, ]
list_df[[3]] <- df[df$year_1992 == 1, ]

for (i in seq_along(list_df)) {
  list_df[[i]] <- df[df$i == 1, ]
}

i <- paste0("year_", seq(1990, 2016, by = 1))
df_years_selection <- lapply(i, function(x) df[df[[x]] == 1, ])

lapply(df_years_selection, function(x) proportion(x$nationality_other))

# ----------

## .

df$college_2 <- gsub("\\s\\(.*?\\)", ",", df$college) %>%
  gsub(",*$", "", .)

toto <- as.data.frame(proportion(df$college_2))
toto$college_name <- row.names(toto)

arrange(toto, desc(count)) %>%
  head(n = 10)

# ----------

## Enrich data with stats.

nba_stats <- read.csv("./nba_all_stars/nba_all_stars_stats.csv", 
                      header = TRUE, stringsAsFactors = FALSE, sep = ";", 
                      fileEncoding = "UTF-8")

nba_stats <- rename(nba_stats, player = X)