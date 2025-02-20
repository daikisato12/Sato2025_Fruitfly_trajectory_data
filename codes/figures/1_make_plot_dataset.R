#### load packages ####
targetPackages <- c('tidyverse','arrow','ggpmisc','corrplot','pals','openxlsx')
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, repos = "http://cran.us.r-project.org")
for(package in targetPackages) library(package, character.only = T)

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


##### Our data #####
df_ours <- read_tsv("../data/data_behavioral_metrics.tsv") %>%
  filter(n_inds == "Single") %>%
  dplyr::select(strain, sex, speed_mm_s_mean)

##### DGRPool #####
# Dataset was downloaded from https://dgrpool.epfl.ch on 2025/2/12
df_line <- read_tsv("../data/DGRPool/dgrp_lines.tsv")
df_study <- read_tsv("../data/DGRPool/studies.tsv") %>%
  arrange(study_id)
df_phenotype <- read_tsv("../data/DGRPool/phenotypes.tsv") %>%
  arrange(study_id)
df_data <- read_tsv("../data/DGRPool/all_phenotype_mean_data.tsv") %>%
  pivot_longer(cols = !DGRP, names_sep = "_", names_to = c("phenotype_id", "sex"), values_to = "value") %>%
  dplyr::mutate(DGRP = paste0("DGRP", parse_number(DGRP)),
                phenotype_id = as.numeric(phenotype_id),
                sex = if_else(sex == "F", "Female", "Male")) %>%
  dplyr::rename(strain = DGRP) %>%
  arrange(phenotype_id)

##### Harbison et al. 2013 Waking activity #####
# study_id_tmp <- 15
# phenotype_id_tmp <- 1489
df_data1 <- df_data %>%
  filter(phenotype_id == 1489) %>%
  right_join(df_ours) %>%
  na.omit() %>%
  group_by(sex) %>%
  dplyr::mutate(n = n(),
                study = "Harbison et al. 2013") %>%
  ungroup()


##### Zhou et al. 2016 Total activity control #####
# study_id_tmp <- 37
# phenotype_id_tmp <- 2845
df_data2 <- df_data %>%
  filter(phenotype_id == 2845) %>%
  right_join(df_ours) %>%
  na.omit() %>%
  group_by(sex) %>%
  dplyr::mutate(n = n(),
                study = "Zhou et al. 2016") %>%
  ungroup()


##### Rohde et al. 2019 Locomotor activity control #####
# Filtering does not work.
# study_id_tmp <- 43
# phenotype_id_tmp <- 2890
# df_tmp <- df_data %>%
#   filter(phenotype_id == phenotype_id_tmp) %>%
#   right_join(df_ours) %>%
#   na.omit()

df_data3 <- read_tsv("../data/DGRPool/Rohde2019/43_raw_3.tsv") %>%
  dplyr::select(DGRP, sex, Locomot_Activity_SUC) %>%
  dplyr::mutate(DGRP = paste0("DGRP", parse_number(DGRP)),
                sex = if_else(sex == "F", "Female", "Male")) %>%
  dplyr::rename(strain = DGRP,
                value = Locomot_Activity_SUC) %>%
  group_by(strain, sex) %>%
  dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
  right_join(df_ours) %>%
  na.omit() %>%
  group_by(sex) %>%
  dplyr::mutate(n = n(),
                study = "Rohde et al. 2019") %>%
  ungroup()

##### Watanabe et al. 2020 Basal activity #####
# Dataset was downloaded from https://academic.oup.com/g3journal/article/10/4/1247/6026179 on 2025/2/12
df_data4 <- openxlsx::read.xlsx("../data/Watanabe2021/TableS3_final.xlsx") %>%
  dplyr::select(1:3) %>%
  pivot_longer(cols = !Basal.Activity, names_to = "sex", values_to = "value") %>%
  dplyr::rename(strain = Basal.Activity) %>%
  dplyr::mutate(strain = paste0("DGRP", strain)) %>%
  right_join(df_ours) %>%
  na.omit() %>%
  group_by(sex) %>%
  dplyr::mutate(n = n(),
                study = "Watanabe et al. 2020") %>%
  ungroup()


##### Watanabe 2021 Climbing index control #####
# study_id_tmp <- 41
# phenotype_id_tmp <- 2847
df_data5 <- df_data %>%
  filter(phenotype_id == 2847) %>%
  right_join(df_ours) %>%
  na.omit() %>%
  group_by(sex) %>%
  dplyr::mutate(n = n(),
                study = "Watanabe et al. 2021") %>%
  ungroup()


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


##### correlation in locomotor activity among studies #####
desired_order <- c(
  "This study",
  "Watanabe et al. 2021",
  "Watanabe et al. 2020",
  # "Riddle 2020",
  "Rohde et al. 2019",
  "Zhou et al. 2016",
  "Harbison et al. 2013"
)


##### Correlation with our dataset #####
df_data_all <-
  bind_rows(df_data1,
            df_data2) %>%
  bind_rows(df_data3) %>%
  bind_rows(df_data5) %>%
  bind_rows(df_data6) %>%
  transform(study = factor(study, levels = desired_order))

g_all1 <- 
  ggplot(df_data_all, 
         aes(x = speed_mm_s_mean,
             y = value)) +
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
  geom_text(aes(x = Inf, y = 0, label = paste0("italic(N) == ", n)),
            parse = TRUE,
            hjust = 1.2, vjust = -0.6, size = 4,
            check_overlap = TRUE) + #check_overlap = TRUE,
  facet_grid(study ~ sex, scales = "free") +
  xlab("Mean moving speed (mm/s) measured in this study") +
  ylab("Locomotor activity measured in a previous study") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        # panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.ticks = element_line(linewidth = 0.3, color = "black"))

g_all1
ggsave("../figures/Figure3a.pdf", g_all1, w = 5, h = 7)

##### Correlation among all datasets #####
df_data_all2 <-
  df_ours %>%
  dplyr::rename(value = speed_mm_s_mean) %>%
  dplyr::mutate(study = "This study") %>%
  bind_rows(df_data %>%
              filter(phenotype_id == 1489) %>%
              dplyr::select(!phenotype_id) %>%
              dplyr::mutate(study = "Harbison et al. 2013")) %>%
  bind_rows(df_data %>%
              filter(phenotype_id == 2845) %>%
              dplyr::select(!phenotype_id) %>%
              dplyr::mutate(study = "Zhou et al. 2016")) %>%
  bind_rows(read_tsv("../data/DGRPool/Rohde2019/43_raw_3.tsv") %>%
              dplyr::select(DGRP, sex, Locomot_Activity_SUC) %>%
              dplyr::mutate(DGRP = paste0("DGRP", parse_number(DGRP)),
                            sex = if_else(sex == "F", "Female", "Male")) %>%
              dplyr::rename(strain = DGRP,
                            value = Locomot_Activity_SUC) %>%
              group_by(strain, sex) %>%
              dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
              dplyr::mutate(study = "Rohde et al. 2019")) %>%
  bind_rows(openxlsx::read.xlsx("../data/Watanabe2021/TableS3_final.xlsx") %>%
              dplyr::select(1:3) %>%
              pivot_longer(cols = !Basal.Activity, names_to = "sex", values_to = "value") %>%
              dplyr::rename(strain = Basal.Activity) %>%
              dplyr::mutate(strain = paste0("DGRP", strain)) %>%
              dplyr::mutate(study = "Watanabe et al. 2020")) %>%
  bind_rows(df_data %>%
              filter(phenotype_id == 2847) %>%
              dplyr::select(!phenotype_id) %>%
              dplyr::mutate(study = "Watanabe et al. 2021")) %>%
  transform(study = factor(study, levels = desired_order))

corr_matrix_female <- df_data_all2 %>%
  bind_rows(data.frame(strain = "DGRP21",
                       study = "Rohde et al. 2019",
                       sex = "Female",
                       value = NA)) %>%
  filter(sex == "Female") %>%
  dplyr::select(strain, study, value) %>%
  pivot_wider(names_from = study, values_from = value) %>%
  dplyr::select(-strain) %>%
  cor(use = "pairwise.complete.obs")

corr_matrix_female <-
  corr_matrix_female[desired_order, desired_order]

corr_matrix_male <- df_data_all2 %>%
  filter(sex == "Male") %>%
  dplyr::select(strain, study, value) %>%
  pivot_wider(names_from = study, values_from = value) %>%
  dplyr::select(-strain) %>%
  cor(use = "pairwise.complete.obs")

corr_matrix_male <-
  corr_matrix_male[desired_order, desired_order]

###### correlation plot ######
pdf("../figures/Figure3b1.pdf", w = 8, h = 8)
corrplot::corrplot(corr_matrix_female, method = "color", shade.col = NA, na.label = "square", na.label.col = "grey", tl.col="black", tl.srt=45, tl.cex = 1.5, cl.cex = 1,
                   col=as.vector(pals::ocean.curl(100)), order="original", type = "upper")
dev.off()

pdf("../figures/Figure3b2.pdf", w = 8, h = 8)
corrplot::corrplot(corr_matrix_male, method = "color", shade.col = NA, na.label = "square", na.label.col = "grey", tl.col="black", tl.srt=45, tl.cex = 1.5, cl.cex = 1,
                   col=as.vector(pals::ocean.curl(100)), order="original", type = "upper")
dev.off()


###### mean correlation plot ######
df_corr <- corr_matrix_female %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  pivot_longer(cols = !rowname, names_to = "pair", values_to = "cor") %>%
  dplyr::mutate(sex = "Female") %>%
  bind_rows(corr_matrix_male %>%
              as.data.frame() %>%
              tibble::rownames_to_column() %>%
              pivot_longer(cols = !rowname, names_to = "pair", values_to = "cor") %>%
              dplyr::mutate(sex = "Male")) %>%
  dplyr::rename(study = rowname) %>%
  transform(study = factor(study, levels = rev(desired_order)))


g_all3 <-
  ggplot(df_corr,
       aes(x = cor, y = study)) +
  stat_summary(fun.data = mean_se,
               position = position_dodge(width = 0.5)) +
  xlab("Pearson's r") +
  facet_wrap( ~ sex, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.ticks = element_line(linewidth = 0.3, color = "black"))
g_all3
ggsave("../figures/Figure3c.pdf", g_all3, w = 4, h = 3)
