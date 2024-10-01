# Libraries ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(purrr)
b2b <- read_excel("/Users/revanthrao/Desktop/UC Davis/Moneyball B2B Project/B2B Data FINAL.xlsx")
full_season <- read_excel("/Users/revanthrao/Desktop/UC Davis/Moneyball B2B Project/Full Season Data FINAL.xlsx")

fs_data_2023 = full_season %>%
  filter(Year == 2023)
b2b_data_2023 = b2b %>%
  filter(Year == 2023)

# All FG% Plots -----------------------------------------------------------
years <- 2019:2023
plots <- list()
for (year in years) {
  fs_data_year <- full_season %>% filter(Year == year)
  b2b_data_year <- b2b %>% filter(Year == year)
  
  mean_fg <- b2b_data_year %>%
    group_by(Name) %>%
    summarize(mean_FG = mean(`FG%`, na.rm = TRUE))
  
  colnames(mean_fg) <- c("Name", "FG%")
  
  fg_diff_year <- data.frame(
    Name = sort(fs_data_year$Player),
    Difference = fs_data_year$`FG%` - mean_fg$`FG%`
  )
  
  plot <- ggplot(fg_diff_year, aes(x = Name, y = Difference)) + 
    geom_point() + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("FG% Difference for", year))
  
  plots[[year - 2018]] <- plot
}

grid.arrange(
  plots[[1]], plots[[2]],
  plots[[3]], plots[[4]],
  ncol = 2
)


# Minutes -----------------------------------------------------------------
b2b$Year <- as.numeric(b2b$Year)
data_2023 <- filter(b2b, Year == 2023)
average_minutes_2023 <- data_2023 %>%
  group_by(Name) %>%
  summarise(average_minutes = round(mean(MP), digits = 1))

fs_2023 = full_season %>%
  filter(Year == 2023) %>%
  arrange(Player) %>%
  select(MP)

min_diff_2023 = data.frame(Name = average_minutes_2023$Name,
                           Difference = fs_data_2023$MP - average_minutes_2023$average_minutes)

average_minutes_2023$full_season_minutes = fs_2023$MP
min_diff_2023 = data.frame(Name = average_minutes_2023$Name,
                           Difference = fs_data_2023$MP - average_minutes_2023$average_minutes)
colnames(average_minutes_2023) = c("Player", "B2B_Minutes", "Full_Season_Minutes")
average_minutes_2023$Difference = average_minutes_2023$Full_Season_Minutes - average_minutes_2023$B2B_Minutes

age_groups <- list(
  group_1 = c("Edwards", "Adebayo", "Fox", "Sabonis", "Mitchell", "Morant", "Jackson", "Brown", "Tatum", "Markkanen", "Doncic", "Gilgeous-Alexander", "Haliburton", "Williamson"),  # Players aged 21-26
  group_2 = c("Lillard", "Antetokounmpo", "Embiid", "Randle", "Holiday", "Irving", "Jokic", "Siakam", "George"),  # Players aged 27-32
  group_3 = c("DeRozan", "Durant", "James", "Curry")   # Players aged 33-38
)

assign_age_group <- function(name) {
  if (name %in% age_groups$group_1) {
    return("group_1")
  } else if (name %in% age_groups$group_2) {
    return("group_2")
  } else if (name %in% age_groups$group_3) {
    return("group_3")
  } else {
    return(NA)
  }
}

min_diff_2023 <- min_diff_2023 %>%
  mutate(Age_Group = sapply(Name, assign_age_group))
mean_diff_by_age_group <- min_diff_2023 %>%
  group_by(Age_Group) %>%
  summarise(Mean_Difference = mean(Difference))

years <- 2019:2023
min_plots <- list()
for (year in years) {
  fs_data_year <- full_season %>% filter(Year == year)
  b2b_data_year <- b2b %>% filter(Year == year)
  
  mean_min <- b2b_data_year %>%
    group_by(Name) %>%
    summarize(mean_Min = mean(MP, na.rm = TRUE))
  
  colnames(mean_min) <- c("Name", "MP")
  
  mp_diff_year <- data.frame(
    Name = sort(fs_data_year$Player),
    Difference = fs_data_year$MP - mean_min$MP
  )
  
  min_plot <- ggplot(mp_diff_year, aes(x = Name, y = Difference)) + 
    geom_point() + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("MP Difference for", year))
  
  min_plots[[year - 2018]] <- min_plot
}

for (year in years) {
  fs_data_year <- full_season %>% filter(Year == year)
  b2b_data_year <- b2b %>% filter(Year == year)
  
  mean_min <- b2b_data_year %>%
    group_by(Name) %>%
    summarize(mean_Min = mean(MP, na.rm = TRUE))
  
  colnames(mean_min) <- c("Name", "MP")
  
  mp_diff_year <- data.frame(
    Name = sort(fs_data_year$Player),
    Difference = fs_data_year$MP - mean_min$MP
  )
  
  min_plot <- ggplot(mp_diff_year, aes(x = Name, y = Difference)) + 
    geom_point() + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("MP Difference for", year))
  
  min_plots[[year - 2018]] <- min_plot
}

grid.arrange(
  min_plots[[1]], min_plots[[2]],
  min_plots[[3]], min_plots[[4]],
  ncol = 2
)

# Points ------------------------------------------------------------------
pts_plots = list()
for (year in years) {
  fs_data_year <- full_season %>% filter(Year == year)
  b2b_data_year <- b2b %>% filter(Year == year)
  
  mean_pts <- b2b_data_year %>%
    group_by(Name) %>%
    summarize(mean_PTS = mean(PTS, na.rm = TRUE))
  
  colnames(mean_pts) <- c("Name", "PTS")
  
  pts_diff_year <- data.frame(
    Name = sort(fs_data_year$Player),
    Difference = fs_data_year$PTS - mean_pts$PTS
  )
  
  pts_plot <- ggplot(pts_diff_year, aes(x = Name, y = Difference)) + 
    geom_point() + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("PTS Difference for", year))
  
  pts_plots[[year - 2018]] <- pts_plot
}

grid.arrange(
  pts_plots[[1]], pts_plots[[2]],
  pts_plots[[3]], pts_plots[[4]],
  ncol = 2
)

# Hypothesis Testing ------------------------------------------------------
b2b$Year <- as.numeric(b2b$Year)
data_2023 <- filter(b2b, Year == 2023)
average_fg_2023 <- data_2023 %>%
  group_by(Name) %>%
  summarise(average_fg = round(mean(`FG%`), digits = 1))

fs_fg_2023 = full_season %>%
  filter(Year == 2023) %>%
  arrange(Player) %>%
  select(`FG%`)

min_diff_fg_2023 = data.frame(Name = average_fg_2023$Name,
                              Difference = fs_fg_2023$`FG%` - average_fg_2023$average_fg)

min_diff_fg_2023 <- min_diff_fg_2023 %>%
  mutate(Age_Group = sapply(Name, assign_age_group))
mean_diff_fg_by_age_group <- min_diff_fg_2023 %>%
  group_by(Age_Group) %>%
  summarise(Mean_Difference = mean(Difference))

full_season = full_season %>% arrange(Year, Player)
pts_total = b2b %>% group_by(Year, Name) %>% summarise(mean_pts = mean(PTS))

(pts_ht = t.test(x = full_season$PTS, y = pts_total$mean_pts))

mins_total = b2b %>% group_by(Year, Name) %>% summarise(mean_mins = mean(MP))

mins_ht = t.test(x = full_season$MP, y = mins_total$mean_mins)

fg_total = b2b %>% group_by(Year, Name) %>% summarise(mean_fg = mean(`FG%`))

(fg_ht = t.test(x = full_season$`FG%`, fg_total$mean_fg))

# Minutes Plots -----------------------------------------------------------
fs_2023 = full_season %>% filter(Year == 2023) %>% arrange(Player)
fs_2022 = full_season %>% filter(Year == 2022) %>% arrange(Player)
fs_2021 = full_season %>% filter(Year == 2021) %>% arrange(Player)
fs_2020 = full_season %>% filter(Year == 2020) %>% arrange(Player)
fs_2019 = full_season %>% filter(Year == 2019) %>% arrange(Player)

average_minutes_2022 = b2b %>% filter(Year == 2022) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_minutes = round(mean(MP), digits = 1))
average_minutes_2021 = b2b %>% filter(Year == 2021) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_minutes = round(mean(MP), digits = 1))
average_minutes_2020 = b2b %>% filter(Year == 2020) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_minutes = round(mean(MP), digits = 1))
average_minutes_2019 = b2b %>% filter(Year == 2019) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_minutes = round(mean(MP), digits = 1))

diff_data_2023_min = data.frame(Name = fs_2023$Player, Difference = fs_2023$MP - average_minutes_2023$average_minutes)
diff_data_2022_min = data.frame(Name = fs_2022$Player, Difference = fs_2022$MP - average_minutes_2022$average_minutes)
diff_data_2021_min = data.frame(Name = fs_2021$Player, Difference = fs_2021$MP - average_minutes_2021$average_minutes)
diff_data_2020_min = data.frame(Name = fs_2020$Player, Difference = fs_2020$MP - average_minutes_2020$average_minutes)
diff_data_2019_min = data.frame(Name = fs_2019$Player, Difference = fs_2019$MP - average_minutes_2019$average_minutes)

mins_plot_2023 <- ggplot(diff_data_2023_min, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Minutes Difference for 2023")

mins_plot_2022 <- ggplot(diff_data_2022_min, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Minutes Difference for 2022")

mins_plot_2021 <- ggplot(diff_data_2021_min, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Minutes Difference for 2021")

mins_plot_2020 <- ggplot(diff_data_2020_min, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Minutes Difference for 2020")

mins_plot_2019 <- ggplot(diff_data_2019_min, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Minutes Difference for 2019")

grid.arrange(
  mins_plot_2022, mins_plot_2021,
  mins_plot_2020, mins_plot_2019,
  ncol = 2
)

min_diff_2023 <- diff_data_2023_min %>%
  mutate(Age_Group = sapply(Name, assign_age_group))
mean_diff_by_age_group <- min_diff_2023 %>%
  group_by(Age_Group) %>%
  summarise(Mean_Difference = mean(Difference))

# Field Goal Plots --------------------------------------------------------
average_fg_2023 = b2b %>% filter(Year == 2023) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_fg = round(mean(`FG%`), digits = 1))
average_fg_2022 = b2b %>% filter(Year == 2022) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_fg = round(mean(`FG%`), digits = 1))
average_fg_2021 = b2b %>% filter(Year == 2021) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_fg = round(mean(`FG%`), digits = 1))
average_fg_2020 = b2b %>% filter(Year == 2020) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_fg = round(mean(`FG%`), digits = 1))
average_fg_2019 = b2b %>% filter(Year == 2019) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_fg = round(mean(`FG%`), digits = 1))

diff_data_2023_fg = data.frame(Name = fs_2023$Player, Difference = fs_2023$`FG%` - average_fg_2023$average_fg)
diff_data_2022_fg = data.frame(Name = fs_2022$Player, Difference = fs_2022$`FG%` - average_fg_2022$average_fg)
diff_data_2021_fg = data.frame(Name = fs_2021$Player, Difference = fs_2021$`FG%` - average_fg_2021$average_fg)
diff_data_2020_fg = data.frame(Name = fs_2020$Player, Difference = fs_2020$`FG%` - average_fg_2020$average_fg)
diff_data_2019_fg = data.frame(Name = fs_2019$Player, Difference = fs_2019$`FG%` - average_fg_2019$average_fg)

fg_plot_2023 <- ggplot(diff_data_2023_fg, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "FG% Difference for 2023")

fg_plot_2022 <- ggplot(diff_data_2022_fg, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "FG% Difference for 2022")

fg_plot_2021 <- ggplot(diff_data_2021_fg, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "FG% Difference for 2021")

fg_plot_2020 <- ggplot(diff_data_2020_fg, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "FG% Difference for 2020")

fg_plot_2019 <- ggplot(diff_data_2019_fg, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "FG% Difference for 2019")

fg_diff_2023 <- diff_data_2023_fg %>%
  mutate(Age_Group = sapply(Name, assign_age_group))
mean_diff_fg_by_age_group <- fg_diff_2023 %>%
  group_by(Age_Group) %>%
  summarise(Mean_Difference = mean(Difference))

grid.arrange(
  fg_plot_2022, fg_plot_2021,
  fg_plot_2020, fg_plot_2019,
  ncol = 2
)

# Points Plots ------------------------------------------------------------
average_pts_2023 = b2b %>% filter(Year == 2023) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_pts = round(mean(PTS), digits = 1))
average_pts_2022 = b2b %>% filter(Year == 2022) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_pts = round(mean(PTS), digits = 1))
average_pts_2021 = b2b %>% filter(Year == 2021) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_pts = round(mean(PTS), digits = 1))
average_pts_2020 = b2b %>% filter(Year == 2020) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_pts = round(mean(PTS), digits = 1))
average_pts_2019 = b2b %>% filter(Year == 2019) %>% arrange(Name) %>% group_by(Name) %>% summarise(average_pts = round(mean(PTS), digits = 1))

diff_data_2023_pts = data.frame(Name = fs_2023$Player, Difference = fs_2023$PTS - average_pts_2023$average_pts)
diff_data_2022_pts = data.frame(Name = fs_2022$Player, Difference = fs_2022$PTS - average_pts_2022$average_pts)
diff_data_2021_pts = data.frame(Name = fs_2021$Player, Difference = fs_2021$PTS - average_pts_2021$average_pts)
diff_data_2020_pts = data.frame(Name = fs_2020$Player, Difference = fs_2020$PTS - average_pts_2020$average_pts)
diff_data_2019_pts = data.frame(Name = fs_2019$Player, Difference = fs_2019$PTS - average_pts_2019$average_pts)

pts_plot_2023 <- ggplot(diff_data_2023_pts, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Points Difference for 2023")

pts_plot_2022 <- ggplot(diff_data_2022_pts, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Points Difference for 2022")

pts_plot_2021 <- ggplot(diff_data_2021_pts, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Points Difference for 2021")

pts_plot_2020 <- ggplot(diff_data_2020_pts, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Points Difference for 2020")

pts_plot_2019 <- ggplot(diff_data_2019_pts, aes(x = Name, y = Difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Points Difference for 2019")

pts_diff_2023 <- diff_data_2023_pts %>%
  mutate(Age_Group = sapply(Name, assign_age_group))
mean_diff_pts_by_age_group <- pts_diff_2023 %>%
  group_by(Age_Group) %>%
  summarise(Mean_Difference = mean(Difference))

grid.arrange(
  pts_plot_2022, pts_plot_2021,
  pts_plot_2020, pts_plot_2019,
  ncol = 2
)

# Yearly Hypothesis Testing -----------------------------------------------
pts_2023 = b2b %>% filter(Year == 2023) %>% group_by(Name) %>% summarise(mean_pts = mean(PTS))
pts_2022 = b2b %>% filter(Year == 2022) %>% group_by(Name) %>% summarise(mean_pts = mean(PTS))
pts_2021 = b2b %>% filter(Year == 2021) %>% group_by(Name) %>% summarise(mean_pts = mean(PTS))
pts_2020 = b2b %>% filter(Year == 2020) %>% group_by(Name) %>% summarise(mean_pts = mean(PTS))
pts_2019 = b2b %>% filter(Year == 2019) %>% group_by(Name) %>% summarise(mean_pts = mean(PTS))

pts_2023_ht = t.test(x = fs_2023$PTS, y = pts_2023$mean_pts)
pts_2022_ht = t.test(x = fs_2022$PTS, y = pts_2022$mean_pts)
pts_2021_ht = t.test(x = fs_2021$PTS, y = pts_2021$mean_pts)
pts_2020_ht = t.test(x = fs_2020$PTS, y = pts_2020$mean_pts)
pts_2019_ht = t.test(x = fs_2019$PTS, y = pts_2019$mean_pts)

mins_2023 = b2b %>% filter(Year == 2023) %>% group_by(Name) %>% summarise(mean_mins = mean(MP))
mins_2022 = b2b %>% filter(Year == 2022) %>% group_by(Name) %>% summarise(mean_mins = mean(MP))
mins_2021 = b2b %>% filter(Year == 2021) %>% group_by(Name) %>% summarise(mean_mins = mean(MP))
mins_2020 = b2b %>% filter(Year == 2020) %>% group_by(Name) %>% summarise(mean_mins = mean(MP))
mins_2019 = b2b %>% filter(Year == 2019) %>% group_by(Name) %>% summarise(mean_mins = mean(MP))

mins_2023_ht = t.test(x = fs_2023$MP, y = mins_2023$mean_mins)
mins_2022_ht = t.test(x = fs_2022$MP, y = mins_2022$mean_mins)
mins_2021_ht = t.test(x = fs_2021$MP, y = mins_2021$mean_mins)
mins_2020_ht = t.test(x = fs_2020$MP, y = mins_2020$mean_mins)
mins_2019_ht = t.test(x = fs_2019$MP, y = mins_2019$mean_mins)

fg_2023 = b2b %>% filter(Year == 2023) %>% group_by(Name) %>% summarise(mean_fg = mean(`FG%`))
fg_2022 = b2b %>% filter(Year == 2022) %>% group_by(Name) %>% summarise(mean_fg = mean(`FG%`))
fg_2021 = b2b %>% filter(Year == 2021) %>% group_by(Name) %>% summarise(mean_fg = mean(`FG%`))
fg_2020 = b2b %>% filter(Year == 2020) %>% group_by(Name) %>% summarise(mean_fg = mean(`FG%`))
fg_2019 = b2b %>% filter(Year == 2019) %>% group_by(Name) %>% summarise(mean_fg = mean(`FG%`))

fg_2023_ht = t.test(x = fs_2023$`FG%`, y = fg_2023$mean_fg)
fg_2022_ht = t.test(x = fs_2022$`FG%`, y = fg_2022$mean_fg)
fg_2021_ht = t.test(x = fs_2021$`FG%`, y = fg_2021$mean_fg)
fg_2020_ht = t.test(x = fs_2020$`FG%`, y = fg_2020$mean_fg)
fg_2019_ht = t.test(x = fs_2019$`FG%`, y = fg_2019$mean_fg)