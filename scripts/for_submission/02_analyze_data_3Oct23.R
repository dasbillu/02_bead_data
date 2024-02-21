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

clim.dat <-
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
  
clim.dat %>% 
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

bd.saveplot(
  name    = "beads_yr_rep",
  width   = 20,
  height  = 25,
(fig10a+fig10b)/(fig20a+fig20b)
)


# Part III

# Colony Consistency ------------------------------------------------------

# We asked if colonies, in a given year, behave consistently across 
# different replicates in their rate of waste removal, 
# i.e., are some colonies slow and some fast in removing beads from the colony?

### Colony consistency

# > Data for 2022

## Classification based on raw values of prop. bead returns
## for two groups
cumprop.quantiles <- n.dat.d1 %>% 
  filter(yr=="2022") %>% 
  pull(cum_prop1) %>% 
  quantile(., c(0.50,0.50)) %>% 
  round(.,2) 

col.dat.d1 <-
  n.dat.d1 %>% 
  filter(yr=="2022") %>% 
  mutate(bead_class2 = ifelse(cum_prop1 <= cumprop.quantiles[1], 
                              "slow", 
                              "fast")) %>%
  filter(n.reps>=2) %>% 
  select(colonyID, rep, bead_class2) %>% 
  distinct() %>% 
  mutate(rep=as.character(rep)) %>% 
  group_by(colonyID) %>% 
  summarize(
    n.reps = length(unique(rep)),
    bead_class_reps = paste(bead_class2, collapse = ", "),
    n.fast = sum(bead_class2=="fast"),
    n.slow = sum(bead_class2=="slow")
    ) %>% 
  ungroup() %>% 
  mutate(n.fast=n.fast/n.reps,
         n.slow=n.slow/n.reps) %>%
  select(-bead_class_reps) %>%
  pivot_longer(cols = n.fast:n.slow,
               values_to = "freq")

col.dat.d1 <-
  col.dat.d1 %>% 
  group_by(colonyID) %>% 
  summarize(
    mfreq = max(freq)
  ) %>% 
  right_join(col.dat.d1) %>% 
  mutate(yr="2022") %>% 
  mutate(name=ifelse(name=="n.fast","fast","slow")) %>% 
  mutate(name=factor(name,levels = c("slow","fast")))


fig21 <-
  col.dat.d1 %>% 
  filter(n.reps>=2) %>% 
  mutate(colonyID = as.character(colonyID)) %>% 
  arrange(mfreq) %>% 
  mutate(
    colonyID = factor(
      colonyID,
      levels = c(
        paste0(
          "C-",
          c(
            27,23,8,7,5,
            26,19,15,13,9,4,
            25,16,12,
            24,22,21,20,18,17,14,1,
            11,10,3
          )
        )
      )
    )
  ) %>%
  ggplot(aes(x=colonyID, y=freq, group=name, fill=name)) +
  geom_bar(stat="identity", col="black", size=1) +
  geom_hline(yintercept = 0.5, size=3, alpha=0.75, col="black", lty=1) +
  theme_Publication(20) +
  scale_color_manual(values=bead_colors) +
  scale_fill_manual(values=bead_colors) +
  labs(y="proportion of replicates",
       x="colony ID") +
  scale_y_continuous(breaks=c(0,0.5,1), labels = c("0", "0.5", "1")) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # axis.text.y=element_blank(),
        legend.title = element_blank())


# > Data for 2021

## Classification based on raw values of prop. bead returns
cumprop.quantiles <- dat.d1 %>% 
  filter(yr=="2021") %>% 
  pull(cum_prop1) %>% 
  quantile(., c(0.50,0.50)) %>% 
  round(.,2) # for two bins

col.dat.d1.2021 <- 
  n.dat.d1 %>% 
  filter(yr=="2021") %>% 
   mutate(bead_class2 = ifelse(cum_prop1 <= cumprop.quantiles[1], "slow", "fast")) %>%
  filter(n.reps>=2) %>% 
  select(colonyID, rep, bead_class2) %>% 
  mutate(rep=as.character(rep)) %>% 
  group_by(colonyID) %>% 
  summarize(
    n.reps = length(unique(rep)),
    bead_class_reps = paste(bead_class2, collapse = ", "),
    n.fast = sum(bead_class2=="fast"),
    # n.inter = sum(bead_class2=="intermediate"),
    n.slow = sum(bead_class2=="slow")
  ) %>% 
  ungroup() %>% 
  mutate(
    n.fast=n.fast/n.reps,
    # n.inter=n.inter/n.reps,
    n.slow=n.slow/n.reps
  ) %>%
  select(-bead_class_reps) %>% 
  pivot_longer(cols = n.fast:n.slow,
               values_to = "freq")

col.dat.d1.2021 <- 
  col.dat.d1.2021 %>% 
  group_by(colonyID) %>% 
  summarize(
    mfreq = max(freq)
  ) %>% 
  right_join(col.dat.d1.2021) %>% 
  mutate(yr="2021") %>% 
  mutate(name=ifelse(name=="n.fast","fast","slow")) %>% 
  mutate(name=factor(name,levels = c("slow","fast")))


fig26 <-
  col.dat.d1.2021 %>% 
  filter(n.reps>=2) %>% 
  mutate(colonyID = as.character(colonyID)) %>% 
  arrange(mfreq) %>% 
  mutate(
    colonyID = factor(
      colonyID,
      levels = c(
        paste0(
          "C-",
          c(
            8,5,2,
            7,6,
            3,
            4,1
          )
        )
      )
    )
  ) %>%
  ggplot(aes(x=colonyID, y=freq, group=name, fill=name)) +
  geom_bar(stat="identity", col="black", size=1) +
  geom_hline(yintercept = 0.5, size=3, alpha=0.75, col="black", lty=1) +
  theme_Publication(20) +
  scale_color_manual(values=bead_colors) +
  scale_fill_manual(values=bead_colors) +
  scale_y_continuous(breaks=c(0,0.5,1), labels = c("0", "0.5", "1")) +
  labs(y="proportion of replicates",
       x="colony ID") +
  coord_flip() +
  theme(legend.position = "none",
        # axis.text.y=element_blank(),
        legend.title = element_blank())

bd.saveplot(
  name = "colonybehavior_acrossreplicates_2021_2022",
  width = 15, 
  height = 18,
  # the plot
  fig26+fig21
)


# Save classification  ----------------------------------------------------
fast_slow_classification <- 
  rbind(col.dat.d1, 
        col.dat.d1.2021) %>% 
  select(yr, colonyID, mfreq, name, freq) %>% 
  filter(mfreq>.5) %>% 
  filter(mfreq==freq) %>% 
  select(-mfreq)

fast_slow_classification %>% 
  saveRDS(
    .,
    file=paste0(
      path_to_repo, 
      "/data/beads_fast_slow_2021_2022"
    )
  )

# Save Metadata ----------------------------------------------------------------

dat.d1 %>% 
  # filter(!is.na(foraging_30s)) %>%
  group_by(colonyID, age_obs) %>% 
  summarize(
    # which years
    which_yrs = paste(unique(yr), collapse = "; "),
    # number of unique replicates
    n_bead_reps = length(unique(rep)),
    # how many reps had foraging data
    n_for_reps = length(na.omit(foraging_30s))
  ) %>% 
  ungroup() %>% 
  
  # add slow/fast classification
  left_join(
    fast_slow_classification |> 
      select(
        -freq,
        which_yrs = yr
      ),
    by = c(
      "which_yrs",
      "colonyID"
    )
  ) |>
  as.data.frame() |> 
  mutate(
    name = as.character(name),
    name = ifelse(
      is.na(name),
      "Intermediate",
      name
    )
  ) |> 
  mutate(
    # bead_for = ifelse(
    #   n_bead_reps >= 2 & n_for_reps >= 2, 
    #   "", 
    #   "x"
    # ),
    bead_for = case_when(
      which_yrs == "2021" 
      ~ "",
      name == "Intermediate"
      ~ "",
      !(
        n_bead_reps >= 2 & 
          n_for_reps >= 2
      )
      ~ "x",
      .default = "included"
    ),
    tmp = extract_numeric(as.character(colonyID))
  ) %>%
  arrange(which_yrs,
          desc(tmp)) %>%
  select(-tmp) |>
  left_join(
    colony.name.register |> 
      rename(colonyID_real = old_name, colonyID = new_name),
    join_by(colonyID)
  ) |> 
  relocate(colonyID_real, .before = 1) |> 
  glimpse() |> 
  # save file
  write.csv(
    paste0(
      path_to_repo,
      "/manuscript/supp_files/raw_data/",
      "01_table_colony_metadata.csv"
    ),
    row.names = F
  )


# 1-sample z test --------------------------------------------------------

#### 2022 only

col.dat.d1 %>% 
  group_by(colonyID) %>% 
  summarise(maxfreq = max(freq),
            consistent = ifelse(maxfreq > 0.5, 1, 0)) %>% 
  ungroup() %>% 
  group_by(consistent) %>% 
  summarise(n())

## perform one sample z-test (without Yate's continuity correction)
## source: https://www.statology.org/one-proportion-z-test-in-r/
prop.test(x=22,
          n=25, 
          p=0.5, alternative="greater",
          correct = F)


#### 2022 & 2021

## perform one sample z-test (without Yate's continuity correction)
prop.test(
  x=28, # consistent colonies
  n=33, # total colonies
  p=0.5, alternative="greater",
  correct = F,
  conf.level = 0.95
)
## REFs: 
# - https://www.statology.org/one-proportion-z-test-in-r/
# - https://en.wikipedia.org/wiki/Yates%27s_correction_for_continuity



# Day 1 vs. Day 2 ---------------------------------------------------------

## ALL DAYS - ANALYSIS

# 2022 data

fast.slow.cols.2022 <- 
  rbind(col.dat.d1, col.dat.d1.2021) %>% 
  select(yr, colonyID, mfreq, name, freq) %>% 
  filter(mfreq>.5) %>% 
  filter(mfreq==freq) %>% 
  select(-mfreq, -freq) %>% 
  filter(yr=="2022")


fast.cols.2022 <- fast.slow.cols.2022 %>% filter(name=="fast") %>% pull(colonyID) %>% as.character()
slow.cols.2022 <- fast.slow.cols.2022 %>% filter(name=="slow") %>% pull(colonyID) %>% as.character()

pd <- position_dodge(0.4)

fig27 <- s.tidydat.mod %>% 
  select(yr, colonyID, rep, counted_when, cum_prop, age_class) %>% 
  filter(yr=="2022") %>% 
  left_join(fast.slow.cols.2022) %>% 
  filter(name %in% c("fast", "slow")) %>% 
  filter(counted_when %in% c("D1_bN", "D2_bN")) %>% 
  mutate(counted_when = ifelse(counted_when=="D1_bN", "Day1", "Day2")) %>% 
  
  group_by(counted_when, name) %>% 
  summarize(m_cum_prop = mean(cum_prop, na.rm = T),
            n = n(),
            se = sd(cum_prop, na.rm = T)/sqrt(n)) %>% 
  mutate(ci95 = qt(1 - (0.05/2), n - 1) * se) %>% 
  
  ggplot(aes(x=counted_when, y=m_cum_prop, col=name)) +
  geom_line(position=pd, aes(group=name), size=3, alpha=.8) +
  geom_errorbar(aes(ymin=m_cum_prop-ci95, ymax=m_cum_prop+ci95),
                position = pd,
                width=0.3, size=4) +
  geom_point(size=5, alpha=.8,
             shape=21, aes(bg=name), col="black", stroke=2,
             position = pd) +
  theme_Publication(25) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank()) +
  labs(x="day", y="beads returned", title="year: 2022") +
  scale_y_continuous(limits = c(0,1), n.breaks = 3) +
  scale_fill_manual(values = bead_colors) +
  scale_color_manual(values = bead_colors)


# 2021 data

fast.slow.cols.2021 <- 
  rbind(col.dat.d1, col.dat.d1.2021) %>% 
  select(yr, colonyID, mfreq, name, freq) %>% 
  filter(mfreq>.5) %>% 
  filter(mfreq==freq) %>% 
  select(-mfreq, -freq) %>% 
  filter(yr=="2021")


fast.cols.2021 <- fast.slow.cols.2021 %>% filter(name=="fast") %>% pull(colonyID) %>% as.character()
slow.cols.2021 <- fast.slow.cols.2021 %>% filter(name=="slow") %>% pull(colonyID) %>% as.character()

fig28 <-
  s.tidydat.mod %>% 
  select(yr, colonyID, rep, counted_when, cum_prop, age_class) %>% 
  filter(yr=="2021") %>% 
  left_join(fast.slow.cols.2021) %>% 
  filter(name %in% c("fast", "slow")) %>% 
  filter(counted_when %in% c("D1_bN", "D2_bN")) %>% 
  mutate(counted_when = ifelse(counted_when=="D1_bN", "Day1", "Day2")) %>% 
  
  group_by(counted_when, name) %>% 
  summarize(
    m_cum_prop = mean(cum_prop, na.rm = T),
    n = n(),
    se = sd(cum_prop, na.rm = T)/sqrt(n)
  ) %>% 
  mutate(ci95 = qt(1 - (0.05/2), n - 1) * se) %>% 
  
  ggplot(aes(x=counted_when, y=m_cum_prop, col=name)) +
    geom_line(position=pd, aes(group=name), size=3, alpha=.8) +
    geom_errorbar(aes(ymin=m_cum_prop-ci95, ymax=m_cum_prop+ci95),
                  position = pd,
                  width=0.3, size=4) +
    geom_point(size=5, alpha=.8,
               shape=21, aes(bg=name), col="black", stroke=2,
               position = pd) +
    theme_Publication(25) +
    theme(legend.position = "none") +
    theme(axis.title.x = element_blank()) +
    labs(x="day", y="beads returned", title="year: 2021") +
    scale_y_continuous(limits = c(0,1), n.breaks = 3) +
    scale_fill_manual(values = bead_colors) +
    scale_color_manual(values = bead_colors)

# Mean (± 95% CI) cumulative proportion of beads returned on 
# Day 1 (D1_bN) and 2 (D2_bN)
fig28+fig27


# KW test -----------------------------------------------------------------

foo <- 
  dat.d2 |>
  left_join(
    fast_slow_classification |> 
      select(-freq),
    by = c("yr", "colonyID")
  ) |> 
  mutate(
    name = as.character(name)
  ) |> 
  filter(
    name %in% c(
      "slow",
      "fast"
    )
  ) |> 
  filter(
    yr == "2022"
  )

kruskal.test(
    cum_prop1 ~ name,
    data = foo
  )
