rm(list = ls())
set.seed(420)


# Load the libraries ------------------------------------------------------

pacman::p_load(tidyverse, viridis, conflicted)
pacman::p_load(ggplot2, patchwork)
## set conflict preference
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lmer", "lme4")
conflict_prefer("intersect", "dplyr")


# Set directory -----------------------------------------------------------

## set path to your working directory (repo) and
path_to_repo = "/Users/bidas/Documents/05_postdoc/Deborah_Gordon/04_research/2022/02_bead_data"

## where to save figures
path_to_figures = paste0(path_to_repo,
                         "/manuscript/figures/tmp/")


# load functions ----------------------------------------------------------

## theme for publication quality figures
source(paste0(path_to_repo,"/functions/theme_publication.R"))

## save plots as png
bd.saveplot <- function(plot,
                        name="myplot",
                        width,
                        height) {
  savingto <- paste0(
    path_to_figures,
    name,
    ".png"
  )
  
  writeLines(paste0("Plot is being saved to \n",savingto))
  png(savingto, 
      width = width, height = height, units = "cm", res = 300)
  print(plot)
  trash <- dev.off()
}


## check pairwise dissimilarity using mean (median) and 95% CI
source(paste0(path_to_repo,"/functions/check_pairwise_significance.R"))

# Load colony registry ----------------------------------------------------

## save the file for easy call
colony.name.register <- readRDS(
  paste0(
    path_to_repo, 
    "/results/raw_data/",
    "colony_name_register.RDS"
  )
)

# Set parameters ----------------------------------------------------------

## color schemes
##
## year of obs
yr_colors <-c("grey60","grey60")
## age classes
age_colors <- c("#8D72A1", "#F26C7F", "#FBAD73")
## bead classes
# bead_colors <- c(low="#1A80D9", high="#F23030") # low=blue, high=red
bead_colors <- c(low="#FBB369", high="#3868A4")
## foraging classes
foraging_colors <- c("#9CD5A2", "#F29544")

## set dodge parameter for plotting
pd <- position_dodge(0.4)



# Load data ---------------------------------------------------------------

# bead experiment + foraging counts
dat <- readRDS(
  paste0(
    path_to_repo,
    "/results/raw_data/",
    "data_for_manuscript.RDS"
  )
)

# old and new names for colonies
colony.name.register <- readRDS(
  paste0(
    path_to_repo,
    "/results/raw_data/",
    "colony_name_register.RDS"
  )
)

# high - low classification
high.low <- readRDS(
  paste0(
    path_to_repo, 
    "/data/beads_fast_slow_2021_2022"
  )
)

# foraging data for all days in 2022
tidy.for.2022 <- readRDS(
  paste0(
    path_to_repo, 
    "/data/data_cct_uofa/tidy_for_2022.Rds"
  )
)


# Prep data ---------------------------------------------------------------

# high colonies in 2022
high.cols.2022 <- high.low |> 
  filter(
    yr == "2022" &
      name == "fast"
  ) |> 
  pull(colonyID) |> 
  as.character() |> 
  unique()

# low colonies in 2022
low.cols.2022 <- high.low |> 
  filter(
    yr == "2022" &
      name == "slow"
  ) |> 
  pull(colonyID) |> 
  as.character() |> 
  unique()

# all dates for 2022 season
dates.2022 <- tidy.for.2022 %>% pull(date) %>% unique()
# dates at which the tree replicates were performed
exp.dates.2022 <- dat %>% filter(yr=="2022") %>% pull(date) %>% unique()
exp.dates.2022 <- exp.dates.2022[c(1,3,5)]


# Prep data for plotting --------------------------------------------------

# Foraging data for 2022 season
for.2022 <-
  tidy.for.2022 %>% 
  left_join(
    colony.name.register, 
    by=c("colonyID"="old_name")
  ) %>% 
  select(
    colonyID, 
    new_name, 
    everything()
  ) %>% 
  na.omit() %>% 
  select(
    colonyID = new_name,
    date, 
    foraging_30s
  ) %>% 
  mutate(
    foraging_30s = foraging_30s/2,
    high_low = ifelse(
      colonyID %in% high.cols.2022, 
      "high",
      ifelse(
        colonyID %in% low.cols.2022, 
        "low",
        NA
      )
    ),
    high_low = factor(
      high_low, 
      levels = c("low","high")
    )
  ) %>% 
  na.omit() %>%
  ## format and arrange data
  arrange(colonyID) %>%
  mutate(
    day = date-dates.2022[1]+1,
    day = paste0("D",as.numeric(day))
  ) %>%
  select(
    colonyID, 
    high_low, 
    day,
    everything()
  ) %>%
  ## scale the foraging data for each colony
  group_by(colonyID) %>% 
  mutate(
    mean_for = mean(foraging_30s, na.rm = T),
    sd_for = sd(foraging_30s, na.rm = T),
    z_foraging = (foraging_30s - mean_for)/sd_for,
    max_foraging = max(foraging_30s, na.rm = T),
    prop_foraging = foraging_30s/max_foraging
  ) %>% 
  ungroup() |>
  mutate_if(is.numeric, list(~round(.,1)))


# Summarize data ----------------------------------------------------------

summ_by_colony <- for.2022 |> 
  group_by(colonyID, high_low) |> 
  summarise(
    n_days_foraging_counts = length(unique(date)),
    which_days = paste(
      unique(date),
      collapse = ", "
    )
  ) |> 
  ungroup() |> 
  arrange(
    high_low,
    colonyID
  )

summ_by_date <- for.2022 |>
  rowwise() |> 
  mutate(
    colonyID = strsplit(colonyID, "-")[[1]][2] |> 
      as.integer()
  ) |>
  group_by(date, high_low) |> 
  summarise(
    n_colonies_foraging_counts = length(unique(colonyID)),
    which_colonies = paste(
      unique(sort(colonyID)),
      collapse = ", "
    )
  ) |> 
  ungroup() |> 
  arrange(
    date,
    high_low
  )
  

## Save summary data -------------------------------------------------------

# # write.csv(
# #   summ_by_colony,
# #   "./results/tables/supp_n_foraging_counts_by_colony.csv",
# #   row.names = F
# # )
# # 
# write.csv(
#   summ_by_date,
#   "./results/tables/supp_n_foraging_counts_by_date.csv",
#   row.names = F
# )



# Save foraging data ------------------------------------------------------
for.2022 |>
  glimpse() |>
  select(
    colonyID,
    date,
    foraging_30s
  ) |>
  left_join(
    colony.name.register |> 
      rename(colonyID_real = old_name, colonyID = new_name),
    join_by(colonyID)
  ) |> 
  relocate(colonyID_real, .before = 1) |> 
  write.csv(
    "./manuscript/supp_files/raw_data/foraging_counts_2022.csv",
    # "./results/tables/supp_foraging_counts_2022.csv",
    row.names = F
  )


# Plot data ---------------------------------------------------------------


# Raw foraging ------------------------------------------------------------

# fig.29 <- 
#   for.2022 %>% 
#   
#   # ggplot(aes(x=date, y=foraging_30s)) +
#   ggplot(aes(x=date, y=foraging_30s)) +
#   
#   geom_hline(yintercept = c(0), 
#              alpha=.5,
#              col="black", lty=1, lwd=1) +
#   
#   geom_vline(xintercept = exp.dates.2022, 
#              alpha=.5,
#              col="grey60", lty=3, lwd=1) +
#   
#   geom_line(alpha=0.75,
#             aes(group=colonyID, col=high_low),
#             size=1) +
#   
#   ## separate the curves for low and high
#   facet_grid(high_low~., scales = "fixed") +
#   
#   ## specify your colors
#   scale_fill_manual(values = bead_colors) +
#   scale_color_manual(values = bead_colors) +
#   
#   # each vertical line would represent 1 day;
#   scale_x_date(breaks = exp.dates.2022,
#                date_labels = "%d %b") +
#   scale_y_continuous(
#     breaks = c(30,60,90)
#   ) +
#   
#   ## set the theme
#   theme_bw(20) +
#   labs(y = "foraging rate (ants/30s)") +
#   
#   ## remove the legend and place it below the plot
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         legend.title = element_blank()) +
#   
#   ## change spacing around the plot
#   theme(plot.margin = margin(1,1.5,1,1, "cm"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())


fig.30 <- 
  for.2022 %>% 
  
  ## obtain the average time series of the two groups
  group_by(high_low,date) %>% 
  summarize(z_avg = mean(foraging_30s),
            z_sd = sd(foraging_30s),
            low_z = z_avg - z_sd,
            high_z = z_avg + z_sd) %>% 
  
  ggplot(aes(x=date, y=z_avg)) +
  
  geom_hline(yintercept = c(0), 
             alpha=.5,
             col="black", lty=1, lwd=1) +
  
  geom_vline(xintercept = exp.dates.2022, 
             alpha=.5,
             col="grey60", lty=3, lwd=1) +
  
  geom_ribbon(aes(ymin = low_z, 
                  ymax = high_z, 
                  fill = high_low), 
              alpha = 0.3) +
  geom_line(alpha=1,
            aes(col=high_low),
            size=3) +
  geom_point(alpha=1,
             shape=21, col="black", stroke=1,
             aes(fill=high_low),
             size=3.5) +
  
  ## separate the curves for low and high
  # facet_grid(high_low~., scales = "fixed") +
  
  ## specify your colors
  scale_fill_manual(values = bead_colors) +
  scale_color_manual(values = bead_colors) +
  
  # each vertical line would represent 1 day;
  scale_x_date(breaks = exp.dates.2022,
               date_labels = "%d %b") +
  scale_y_continuous(
    breaks = c(30,60)
  ) +
  ## set the theme
  theme_bw(20) +
  labs(y = "foraging rate (ants/30s)") +
  
  ## remove the legend and place it below the plot
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  
  ## change spacing around the plot
  theme(plot.margin = margin(1,1.5,1,1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


fig.30


# Z-scaled foraging -------------------------------------------------------


# fig.31 <- 
#   for.2022 %>% 
#   
#   ggplot(aes(x=date, y=z_foraging)) +
#   
#   geom_hline(yintercept = c(0), 
#              alpha=.5,
#              col="black", lty=1, lwd=1) +
#   
#   geom_vline(xintercept = exp.dates.2022, 
#              alpha=.5,
#              col="grey60", lty=3, lwd=1) +
#   
#   geom_line(alpha=0.75,
#             aes(group=colonyID, col=high_low),
#             size=1) +
#   
#   ## separate the curves for low and high
#   facet_grid(high_low~., scales = "fixed") +
#   
#   ## specify your colors
#   scale_fill_manual(values = bead_colors) +
#   scale_color_manual(values = bead_colors) +
#   
#   # each vertical line would represent 1 day;
#   scale_x_date(breaks = exp.dates.2022,
#                date_labels = "%d %b") +
#   scale_y_continuous(
#     breaks = c(-2,0,2)
#   ) +
#   
#   ## set the theme
#   theme_bw(20) +
#   labs(y = "foraging rate (z-score)") +
#   
#   ## remove the legend and place it below the plot
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         legend.title = element_blank()) +
#   
#   ## change spacing around the plot
#   theme(plot.margin = margin(1,1.5,1,1, "cm"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())


fig.32 <- 
  for.2022 %>% 
  
  ## obtain the average time series of the two groups
  group_by(high_low,date) %>% 
  summarize(z_avg = mean(z_foraging),
            z_sd = sd(z_foraging),
            low_z = z_avg - z_sd,
            high_z = z_avg + z_sd) %>% 
  
  ggplot(aes(x=date, y=z_avg)) +
  
  geom_hline(yintercept = c(0), 
             alpha=.5,
             col="black", lty=1, lwd=1) +
  
  geom_vline(xintercept = exp.dates.2022, 
             alpha=.5,
             col="grey60", lty=3, lwd=1) +
  
  geom_ribbon(aes(ymin = low_z, 
                  ymax = high_z, 
                  fill = high_low), 
              alpha = 0.3) +
  geom_line(alpha=1,
            aes(col=high_low),
            size=3) +
  geom_point(alpha=1,
             shape=21, col="black", stroke=1,
             aes(fill=high_low),
             size=3.5) +
  
  ## separate the curves for low and high
  # facet_grid(high_low~., scales = "fixed") +
  
  ## specify your colors
  scale_fill_manual(values = bead_colors) +
  scale_color_manual(values = bead_colors) +
  
  # each vertical line would represent 1 day;
  scale_x_date(breaks = exp.dates.2022,
               date_labels = "%d %b") +
  scale_y_continuous(
    breaks = c(-2,0,2),
    limits = c(-2.5,2.5)
  ) +
  ## set the theme
  theme_bw(20) +
  labs(y = "foraging rate (z-score)") +
  
  ## remove the legend and place it below the plot
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  
  ## change spacing around the plot
  theme(plot.margin = margin(1,1.5,1,1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


fig.32


# proportion foraging -----------------------------------------------------


fig.34 <- 
  for.2022 %>% 
  
  ## obtain the average time series of the two groups
  group_by(high_low,date) %>% 
  summarize(z_avg = mean(prop_foraging),
            z_sd = sd(prop_foraging),
            low_z = z_avg - z_sd,
            high_z = z_avg + z_sd) %>% 
  
  ggplot(aes(x=date, y=z_avg)) +
  
  # geom_hline(yintercept = c(0), 
  #            alpha=.5,
  #            col="black", lty=1, lwd=1) +
  
  geom_vline(xintercept = exp.dates.2022, 
             alpha=.5,
             col="grey60", lty=3, lwd=1) +
  
  geom_ribbon(aes(ymin = low_z, 
                  ymax = high_z, 
                  fill = high_low), 
              alpha = 0.3) +
  geom_line(alpha=1,
            aes(col=high_low),
            size=3) +
  geom_point(alpha=1,
             shape=21, col="black", stroke=1,
             aes(fill=high_low),
             size=4) +
  
  ## separate the curves for low and high
  # facet_grid(high_low~., scales = "fixed") +
  
  ## specify your colors
  scale_fill_manual(values = bead_colors) +
  scale_color_manual(values = bead_colors) +
  
  # each vertical line would represent 1 day;
  scale_x_date(breaks = exp.dates.2022,
               date_labels = "%d %b") +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0","0.5","1"),
    limits = c(0,1.1)
  ) +
  ## set the theme
  theme_bw(25) +
  labs(y = "Foraging rate (normalized)") +
  
  ## remove the legend and place it below the plot
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  
  ## change spacing around the plot
  theme(plot.margin = margin(1,1.5,1,1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




fig.34

bd.saveplot(
  fig.34,
  name = "prop_foraging_5Jan24",
  width = 20,
  height = 12
)


# Coefficient of variation ------------------------------------------------

cv.dat <- for.2022 %>% 
  group_by(high_low,colonyID) %>% 
  summarize(for_avg = mean(foraging_30s),
            for_sd = sd(foraging_30s)) %>%
  mutate(cv = for_sd/for_avg) %>% 
  ungroup() %>% 
  
  group_by(high_low) %>% 
  summarize(mean.val = mean(cv, na.rm = T),
            sd.val = sd(cv, na.rm = T),
            n.val = n()) %>%
  mutate(se.val = sd.val/sqrt(n.val),
         ci95 = qt(1 - (0.05/2), n.val - 1) * se.val,
         low95 = mean.val - ci95,
         high95 = mean.val + ci95) %>%
  ungroup() 

print(cv.dat)

# # plot it
# cv.dat %>% 
#   
#   ggplot(aes(x=high_low, y=mean.val, col=high_low)) +
# 
#   geom_hline(yintercept = 0.5, lwd=1, col="grey60", alpha=.75) +
#   
#   geom_errorbar(aes(ymin=mean.val-ci95, ymax=mean.val+ci95),
#                 position = pd,
#                 width=0.3, size=4) +
#   geom_point(size=5, alpha=.8,
#              shape=21, aes(bg=high_low), col="black", stroke=2,
#              position = position_dodge(3)) +
#   # facet_grid(~counted_when) +
#   theme_bw(20) +
#   theme(legend.position = "none") +
#   theme(axis.title.x = element_blank()) +
#   labs(x="xxx", 
#        y="coefficient of variation\nfor foraging rate", 
#        title="mean ± 95% CI") +
#   scale_y_continuous(limits = c(0,1), n.breaks = 3) +
#   scale_fill_manual(values = bead_colors) +
#   scale_color_manual(values = bead_colors) +
#   ## change spacing around the plot
#   theme(plot.title = element_text(hjust=.5),
#         plot.margin = margin(1,2,1,4, "cm"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())



# check for correlation ---------------------------------------------------

bar <- 
  for.2022 %>% 
  ## scale the foraging data for each colony
  group_by(colonyID) %>%
  mutate(
    z_foraging_30s = scale(foraging_30s)
  ) %>% 
  ungroup() %>% 
  ## obtain the average time series of the two groups
  group_by(
    high_low,
    date
  ) %>% 
  summarize(z_avg = mean(z_foraging_30s),
            z_sd = sd(z_foraging_30s))

low.bar <- bar %>% filter(high_low=="low") %>% pull(z_avg) %>% round(.,3)
high.bar <- bar %>% filter(high_low=="high") %>% pull(z_avg) %>% round(.,3)

cor.test(
  low.bar, 
  high.bar,
  method = "kendall",
  alternative = "greater"
)
