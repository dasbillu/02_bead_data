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


# Set parameters ----------------------------------------------------------

## minimum number of beads (out of 50) that
## the colony must collect to be part of the analyses
atleast.beads <- 40
##
## color schemes
##
## year of obs
yr_colors <-c("grey60","grey60")
## age classes
age_colors <- c("#8D72A1", "#F26C7F", "#FBAD73")
## bead classes
# bead_colors <- c(slow="#1A80D9", fast="#F23030") # slow=blue, fast=red
bead_colors <- c(slow="#FBB369", fast="#3868A4")
## foraging classes
foraging_colors <- c("#9CD5A2", "#F29544")


# Load data ---------------------------------------------------------------

s.tidydat.mod <- readRDS(
  paste0(
    path_to_repo,
    "/results/raw_data/",
    "data_for_manuscript.RDS"
  )
)


# Load colony registry ----------------------------------------------------

## save the file for easy call
colony.name.register <- readRDS(
  paste0(
    path_to_repo, 
    "/results/raw_data/",
    "colony_name_register.RDS"
  )
)

## Split data for Day 1 and 2
dat.d1 <- s.tidydat.mod %>%
  filter(counted_when == "D1_bN") %>%
  rename(cum_prop1 = cum_prop)
dat.d2 <- s.tidydat.mod %>%
  filter(counted_when %in% c("D2_M", "D2_bN")) %>%
  rename(cum_prop1 = cum_prop)


# Summary data ------------------------------------------------------------

## which colony-reps have missing foraging data
dat.d1 %>% 
  # filter(yr=="2022") %>% 
  filter(is.na(foraging_30s)) %>%
  select(colonyID,rep, yr) %>%
  distinct()
writeLines("2 colony-reps in 2021 and 5 colony-reps in 2022 have missing foraging data")


## Datetime of experiments
s.tidydat.mod %>%
  filter(counted_when=="D1_bN") %>%
  group_by(yr) %>%
  summarize(n_reps = length(unique(date)),
            which_days = paste(unique(date), collapse = "; "))


# PLOT --------------------------------------------------------------------

# Variation in abiotic conditions -----------------------------------------

clim.dat <- read.csv(
  file = paste0(
    path_to_repo,
    "/data/",
    "all_climate_data_beadexp_2021_2022.csv"
  )
)

writeLines("How many unique dates, in each year, are we using to calculate mean and 95% CI?")
clim.dat %>% 
  select(1) %>% 
  distinct() %>% 
  mutate(date = as.character(date)) %>% 
  group_by(date) %>% 
  mutate(yr = str_split_fixed(date, n=2, pattern = "-")[[1]][1]) %>% 
  mutate(day = str_split_fixed(date, n=3, pattern = "-")[[3]][1]) %>% 
  ungroup() %>% 
  group_by(yr) %>% 
  summarise(n.days = length(unique(day)),
            which.month = "August",
            which.days = paste(unique(day), collapse = ", "))

clim.dat.summ <-
  clim.dat %>% 
    as_tibble() %>% 
  select(date, time, temp_F, rH, precip=precip_accum_inch) %>% 
  distinct() %>% 
  mutate(temp_C=(temp_F-32)*(5/9)) %>% 
  group_by(date) %>% 
  mutate(yr = str_split(date, "-")[[1]][1]) %>% 
  ungroup() %>% 
  mutate(yr = as.factor(yr)) %>% 
  pivot_longer(cols = temp_F:temp_C,
               names_to = "what",
               values_to = "value") %>%
  
  # ggplot(aes(x=what, y=value, fill=yr)) +
  # scale_color_manual(values = yr_colors) +
  # scale_fill_manual(values = yr_colors) +
  # geom_boxplot(width=.4, position = position_dodge(width = .60)) +
  # theme_Publication(20) +
  # theme(legend.position = "none",
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank())
  
  filter(what != "temp_F") %>% 
    
  group_by(yr, what) %>%
  summarize(mean.val = round(mean(value, na.rm = T),2),
            sd.val = round(sd(value, na.rm = T),2),
            n.val = n()) %>%
  mutate(se.val = round(sd.val/sqrt(n.val), 2),
         ci95 = round(qt(1 - (0.05/2), n.val - 1)*se.val, 2)) %>%
  ungroup()
  
clim.dat.summ %>% 
    ggplot(aes(x=yr, y=mean.val, col=yr)) +
    geom_errorbar(aes(ymin=mean.val-ci95, ymax=mean.val+ci95), width=0.2, size=2, col="black") +
    geom_point(size=6, position = position_dodge(width = 3)) +
    facet_wrap(~as.factor(what), scales = "free_y") +
    theme_Publication(base_size = 20) +
    scale_color_manual(values = yr_colors) +
    scale_fill_manual(values = yr_colors) +
    theme(legend.position = "none",
          axis.title.x = element_blank()) +
    labs(x="",
         y="",
         subtitle = "precip inch; humidity %rH; temperature ºC")


# Variation across years --------------------------------------------------

fig10a <-
  dat.d1 %>%
  ggplot(aes(x=yr, y=cum_prop1)) +
  geom_boxplot(size=1, alpha=0.5, aes(fill=yr)) +
  geom_jitter(size=4, alpha=0.35, width = 0.1) +
  theme_Publication(25) +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  scale_y_continuous(n.breaks = 3) +
  scale_fill_manual(values = yr_colors) +
  labs(x="year",
       y="Beads removed",
       title = "Day 1")

fig10b <-
  dat.d2 %>%
  ggplot(aes(x=yr, y=cum_prop1)) +
  geom_boxplot(size=1, alpha=0.5, aes(fill=yr)) +
  geom_jitter(size=4, alpha=0.35, width = 0.1) +
  theme_Publication(25) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_y_continuous(n.breaks = 3) +
  scale_fill_manual(values = yr_colors) +
  labs(x="year",
       y="",
       title = "Day 2")

# bd.saveplot(
#   name = "beads_yr_rep",
#   width = 25,height=15,
fig10a+fig10b
# )


# ANALYSES ----------------------------------------------------------------

# Part 1

# Variation across years --------------------------------------------------

# > Checking for difference across year [Day 1]

writeLines("is the difference among replicates explained by the year of the experiment?")
kruskal.test(cum_prop1 ~ yr, data = dat.d1) 
writeLines("Yes.")


# > Checking for difference across year [Day 2]
writeLines("is the difference among replicates explained by the year of the experiment?")
kruskal.test(cum_prop1 ~ yr, data = dat.d2) 


# Variation in abiotic conditions -----------------------------------------

clim.dat |> 
  mutate(
    yr = lubridate::ymd(date) |> lubridate::year()
  ) %>%
  # kruskal.test(temp_F ~ yr, data = .)
  # kruskal.test(rH ~ yr, data = .)
  kruskal.test(precip_accum_inch ~ yr, data = .)

# Same colony, different year ---------------------------------------------
col.bothyears <- intersect(dat.d1[dat.d1$yr=="2021",]$colonyID,
                           dat.d1[dat.d1$yr=="2022",]$colonyID)



# Part 2


# Variation in foraging data ----------------------------------------------

# ```{r analyses_v7, fig.width=8, fig.height=10}
n.dat.d1 <-
  s.tidydat.mod %>% 
  select(colonyID,
         rep,
         age_obs,
         maxF = max_foraging_30s) %>% 
  distinct() %>% 
  right_join(dat.d1, by=c("colonyID","rep","age_obs"))

n.dat.d1 <-
  n.dat.d1 %>% 
  group_by(colonyID, yr) %>% 
  summarize(n.reps=length(unique(rep))) %>% 
  ungroup() %>% 
  right_join(n.dat.d1)

writeLines("Do colonies differ in their maximum foraging between 2021 and 2022?")
n.dat.d1 %>% 
  filter(age_class == "5+_yr") %>% 
  # select(colonyID, maxF, yr, age_class) %>% 
  select(colonyID, foraging_30s, yr, age_class) %>% 
  distinct() %>% 
  kruskal.test(data=.,
               foraging_30s ~ yr)

writeLines("Let's look at the 6 colonies for which we have data for both years")
n.dat.d1 %>% 
  filter(colonyID %in% col.bothyears) %>% 
  # select(colonyID, maxF, yr, age_class) %>% 
  select(colonyID, foraging_30s, yr, age_class) %>% 
  distinct() %>% 
  kruskal.test(data=.,
               foraging_30s ~ yr)

fig20a <-
  dat.d1 %>%
  ggplot(aes(x=yr, y=foraging_30s/(2))) +
  geom_boxplot(size=1, alpha=0.5, aes(fill=yr)) +
  geom_jitter(size=4, alpha=0.35, width = 0.1) +
  theme_Publication(25) +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  scale_y_continuous(breaks = c(0,40,80), limits = c(0,82)) +
  scale_fill_manual(values = yr_colors) +
  labs(x="year",
       y="Foraging rate (ants/30s)",
       title = "Day 1")

fig20b <-
  dat.d2 %>%
  ggplot(aes(x=yr, y=foraging_30s/(2))) +
  geom_boxplot(size=1, alpha=0.5, aes(fill=yr)) +
  geom_jitter(size=4, alpha=0.35, width = 0.1) +
  theme_Publication(25) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  scale_y_continuous(breaks = c(0,40,80), limits = c(0,82)) +
  scale_fill_manual(values = yr_colors) +
  labs(x="year",
       y="",
       title = "Day 2")

# bd.saveplot(
#   name    = "beads_yr_rep",
#   width   = 20,
#   height  = 25,
(fig10a+fig10b)/(fig20a+fig20b)
# )

