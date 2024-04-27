#### load libraries ####
library(tidyverse)
library(arrow)
library(patchwork)

#### load dataset ####
##### tracktor dataset #####
df_tracktor_fix <- read_parquet("../data/df_tracktor_fix.parquet") %>%
  ungroup()

##### flytracker dataset #####
df_flytracker_fix <- read_parquet("../data/df_flytracker_fix.parquet") %>%
  ungroup()

##### mismatch dataset #####
df_comparison_rand100_fix <- 
  read.table("../data/df_comparison_rand100_fix.tsv", header = TRUE)


#### behavioral indices ####
##### tracktor dataset #####
## data first 5 min
df_tracktor_fix_f5min <- df_tracktor_fix %>%
  filter(seconds_total < 300)

## s5min (from 299.5-599)
df_tracktor_fix_s5min_2995_5990 <- df_tracktor_fix %>%
  filter(299 < seconds_total, seconds_total < 599.5)

###### moving speed ######
df_tracktor_fix_f5min_speed <- df_tracktor_fix_f5min %>%
  dplyr::mutate(speed = speed * 30 /100) %>%
  group_by(prefix, id) %>%
  dplyr::summarize(speed = mean(speed, na.rm = T)) %>%
  arrange(prefix, id)

###### boldness ######
df_tracktor_fix_f5min_boldness <- df_tracktor_fix_f5min %>%
  dplyr::mutate(pos_x = pos_x - 50,
                pos_y = pos_y - 50,
                center_time = if_else(pos_x**2 + pos_y**2 < 25**2, 0.5, 0)) %>%
  group_by(prefix, id) %>%
  dplyr::summarize(boldness = sum(center_time, na.rm = T)) %>%
  arrange(prefix, id)

###### nnd ######
df_tracktor_fix_f5min_nnd <- df_tracktor_fix_f5min %>%
  group_by(prefix) %>%
  dplyr::summarize(nnd = mean(nnd * 30 / 100, na.rm = T)) %>%
  ungroup()

###### freezing duration ######
df_tracktor_fix_s5min_2995_5990_speed <- df_tracktor_fix_s5min_2995_5990 %>%
  dplyr::mutate(stim_time = (seconds_total + 0.5) %% 15 - 0.5,
                speed = speed * 30 / 100) %>%
  group_by(prefix, id, stim_time) %>%
  dplyr::summarize(speed = mean(speed, na.rm = TRUE))

df_tracktor_fix_s5min_2995_5990_speed_normbyf5minave <-
  df_tracktor_fix_s5min_2995_5990_speed %>%
  group_by(prefix, id, stim_time) %>%
  dplyr::summarize(speed = mean(speed, na.rm = TRUE)) %>%
  left_join(df_tracktor_fix_f5min_speed %>%
              rename(speed_f5min_ave = speed)) %>%
  group_by(prefix, id, stim_time) %>%
  dplyr::summarize(speed = mean(speed, na.rm = TRUE),
                   speed_f5min_ave = mean(speed_f5min_ave, na.rm = TRUE)) %>%
  mutate(speed_normbyf5minave = speed / speed_f5min_ave)

df_tracktor_fix_s5min_2995_5990_freezing_duration <-
  df_tracktor_fix_s5min_2995_5990_speed_normbyf5minave %>% 
  filter(stim_time != -0.5, speed_normbyf5minave > 1) %>%
  group_by(prefix, id) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  pivot_wider(id_cols = prefix,
              names_from = id, values_from = stim_time,
              values_fill = 14) %>%
  pivot_longer(cols = contains("Fly"), names_to = "id", values_to = "freezing_duration") %>%
  group_by(prefix, id) %>%
  dplyr::summarize(freezing_duration = mean(freezing_duration, na.rm = T)) %>%
  ungroup()
# add any rows if all individuals keep freezing for 14 s 
if(setdiff(unique(df_tracktor_fix_s5min_2995_5990_speed_normbyf5minave$prefix),
           unique(df_tracktor_fix_s5min_2995_5990_freezing_duration$prefix)) %>%
   length() > 0){
  df_tracktor_fix_s5min_2995_5990_freezing_duration <-
    df_tracktor_fix_s5min_2995_5990_freezing_duration %>%
    bind_rows(data.frame(prefix = setdiff(unique(df_tracktor_fix_s5min_2995_5990_speed_normbyf5minave$prefix),
                                          unique(df_tracktor_fix_s5min_2995_5990_freezing_duration$prefix)),
                         freezing_duration = 14))
}

##### flytracker dataset #####
## data first 5 min
df_flytracker_fix_f5min <- df_flytracker_fix %>%
  filter(seconds_total < 300)

## s5min (from 299.5-599)
df_flytracker_fix_s5min_2995_5990 <- df_flytracker_fix %>%
  filter(299 < seconds_total, seconds_total < 599.5)

###### moving speed ######
df_flytracker_fix_f5min_speed <- df_flytracker_fix_f5min %>%
  dplyr::mutate(speed = speed * 30 /100) %>%
  group_by(prefix, id) %>%
  dplyr::summarize(speed = mean(speed, na.rm = T)) %>%
  arrange(prefix, id)

###### boldness ######
df_flytracker_fix_f5min_boldness <- df_flytracker_fix_f5min %>%
  dplyr::mutate(pos_x = pos_x - 50,
                pos_y = pos_y - 50,
                center_time = if_else(pos_x**2 + pos_y**2 < 25**2, 0.5, 0)) %>%
  group_by(prefix, id) %>%
  dplyr::summarize(boldness = sum(center_time, na.rm = T)) %>%
  arrange(prefix, id)

###### nnd ######
df_flytracker_fix_f5min_nnd <- df_flytracker_fix_f5min %>%
  group_by(prefix) %>%
  dplyr::summarize(nnd = mean(nnd * 30 / 100, na.rm = T)) %>%
  ungroup()

###### freezing duration ######
df_flytracker_fix_s5min_2995_5990_speed <- df_flytracker_fix_s5min_2995_5990 %>%
  dplyr::mutate(stim_time = (seconds_total + 0.5) %% 15 - 0.5,
                speed = speed * 30 / 100) %>%
  group_by(prefix, id, stim_time) %>%
  dplyr::summarize(speed = mean(speed, na.rm = TRUE))

df_flytracker_fix_s5min_2995_5990_speed_normbyf5minave <-
  df_flytracker_fix_s5min_2995_5990_speed %>%
  group_by(prefix, id, stim_time) %>%
  dplyr::summarize(speed = mean(speed, na.rm = TRUE)) %>%
  left_join(df_flytracker_fix_f5min_speed %>%
              rename(speed_f5min_ave = speed)) %>%
  group_by(prefix, id, stim_time) %>%
  dplyr::summarize(speed = mean(speed, na.rm = TRUE),
                   speed_f5min_ave = mean(speed_f5min_ave, na.rm = TRUE)) %>%
  mutate(speed_normbyf5minave = speed / speed_f5min_ave)

df_flytracker_fix_s5min_2995_5990_freezing_duration <-
  df_flytracker_fix_s5min_2995_5990_speed_normbyf5minave %>% 
  filter(stim_time != -0.5, speed_normbyf5minave > 1) %>%
  group_by(prefix, id) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  pivot_wider(id_cols = prefix,
              names_from = id, values_from = stim_time,
              values_fill = 14) %>%
  pivot_longer(cols = contains("Fly"), names_to = "id", values_to = "freezing_duration") %>%
  group_by(prefix, id) %>%
  dplyr::summarize(freezing_duration = mean(freezing_duration, na.rm = T)) %>%
  ungroup()
# add any rows if all individuals keep freezing for 14 s 
if(setdiff(unique(df_flytracker_fix_s5min_2995_5990_speed_normbyf5minave$prefix),
           unique(df_flytracker_fix_s5min_2995_5990_freezing_duration$prefix)) %>%
   length() > 0){
  df_flytracker_fix_s5min_2995_5990_freezing_duration <-
    df_flytracker_fix_s5min_2995_5990_freezing_duration %>%
    bind_rows(data.frame(prefix = setdiff(unique(df_flytracker_fix_s5min_2995_5990_speed_normbyf5minave$prefix),
                                          unique(df_flytracker_fix_s5min_2995_5990_freezing_duration$prefix)),
                         freezing_duration = 14))
}


#### make plots ####
##### tracking discrepancy #####
g_comp <- 
  ggplot(df_comparison_rand100_fix %>%
           group_by(dataset, prefix) %>%
           dplyr::summarize(mean_discrepancy = mean(mean_discrepancy, 
                                                    na.rm = T),
                            num_discrepancy = mean(num_discrepancy, 
                                                   na.rm = T)) %>%
           transform(dataset = factor(dataset, 
                                      levels = c("tracktor-raw", "tracktor-final"))),
         aes(x = dataset,
             y = mean_discrepancy * 100)) +
  stat_summary(fun.data = mean_se,
               position = position_dodge(width = 0.5)) +
  ggpubr::stat_compare_means(paired = T, label.y = 0.25) +
  coord_cartesian(ylim = c(0, 0.3)) +
  ylab("Mean proportion of discrepancy with flytracker (%)") +
  theme_bw() +
  theme(axis.title.x = element_blank())
g_comp

##### behavioral indices #####
df_integrated <-
  bind_rows(df_tracktor_fix_f5min_speed %>%
              group_by(prefix) %>%
              dplyr::summarize(speed = mean(speed, na.rm = TRUE)) %>%
              mutate(dataset = "tracktor"),
            df_flytracker_fix_f5min_speed %>%
              group_by(prefix) %>%
              dplyr::summarize(speed = mean(speed, na.rm = TRUE)) %>%
              mutate(dataset = "flytracker")) %>%
  inner_join(bind_rows(df_tracktor_fix_f5min_boldness %>%
                         group_by(prefix) %>%
                         dplyr::summarize(boldness = mean(boldness, na.rm = TRUE)) %>%
                         mutate(dataset = "tracktor"),
                       df_flytracker_fix_f5min_boldness %>%
                         group_by(prefix) %>%
                         dplyr::summarize(boldness = mean(boldness, na.rm = TRUE)) %>%
                         mutate(dataset = "flytracker"))) %>%
  inner_join(bind_rows(df_tracktor_fix_f5min_nnd %>%
                          group_by(prefix) %>%
                          dplyr::summarize(nnd = mean(nnd, na.rm = TRUE)) %>%
                          mutate(dataset = "tracktor"),
                        df_flytracker_fix_f5min_nnd %>%
                          group_by(prefix) %>%
                          dplyr::summarize(nnd = mean(nnd, na.rm = TRUE)) %>%
                          mutate(dataset = "flytracker"))) %>%
  inner_join(bind_rows(df_tracktor_fix_s5min_2995_5990_freezing_duration %>%
                        group_by(prefix) %>%
                        dplyr::summarize(freezing_duration = mean(freezing_duration, na.rm = TRUE)) %>%
                        mutate(dataset = "tracktor"),
                      df_flytracker_fix_s5min_2995_5990_freezing_duration %>%
                        group_by(prefix) %>%
                        dplyr::summarize(freezing_duration = mean(freezing_duration, na.rm = TRUE)) %>%
                        mutate(dataset = "flytracker"))) %>%
  pivot_longer(cols = !c(prefix, dataset), names_to = "var", values_to = "value") %>%
  pivot_wider(id_cols = c(prefix, var), names_from = dataset, values_from = value) %>%
  dplyr::mutate(var = str_to_title(var) %>% str_replace("_", " "),
                var = case_when(var == "Nnd" ~ "NND",
                                TRUE ~ var)) %>%
transform(var = factor(var, levels = c("Speed", "Boldness", "NND", "Freezing duration")))
  

g_behavior <-
  ggplot(df_integrated,
         aes(x = tracktor, y = flytracker)) +
  stat_smooth(linewidth = 2, color= "grey", method = "lm") +#, formula = y ~ poly(x, degree = 2, raw = TRUE) - 1) + #formula = y ~ log(x)) + #method = "lm") +
  geom_point(shape = 16, alpha = .6, size = 2) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(#stat(eq.label),
                          after_stat(rr.label),
                          after_stat(p.value.label),
                          sep = "~~~")),
                        label.x = "left",
                        label.y = "top",
                        parse = TRUE, size = 4) + 
  facet_wrap(~ var, ncol = 4, scales = "free") +
  xlab("Tracktor") +
  ylab("FlyTracker") +
  theme_bw() +
  theme(strip.background = element_blank())
g_behavior

g_fig2 <-
  g_comp +
  g_behavior + 
  plot_layout(widths = c(1, 4))
ggsave("../figures/Figure2.pdf", g_fig2, w = 6, h = 3)

