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
path_to_figures = paste0(path_to_repo,"/results/figures/")


# load functions ----------------------------------------------------------

## theme for publication quality figures
source(paste0(path_to_repo,"/functions/theme_publication.R"))

## save plots as png
bd.saveplot <- function(plot,name="myplot",width,height) {
  savingto <- paste0(path_to_figures,name,".png")
  writeLines(paste0("Plot is being saved to \n",savingto))
  png(savingto, 
      width = width, height = height, units = "cm", res = 300)
  print(plot)
  trash <- dev.off()
}

## check pairwise dissimilarity using mean (median) and 95% CI
source(paste0(path_to_repo,"/functions/check_pairwise_significance.R"))


# Set parameters ----------------------------------------------------------

# analyze data for which year of observation
which.yr <- 2022

# bead_colors <- c(slow="#1A80D9", fast="#F23030") # slow=blue, fast=red
bead_colors <- c(low="#FBB369", high="#3868A4")

# Load data ---------------------------------------------------------------

dat <- readRDS(
  paste0(
    path_to_repo,
    "/results/raw_data/",
    "data_for_manuscript.RDS"
  )
) |> 
  rowwise() |> 
  mutate(
    age_class = strsplit(
      as.character(age_class), 
      "_"
    )[[1]][1],
    age_class = factor(
      age_class,
      levels = c(
        "1-2",
        "3-4",
        "5+"
      )
    )
  )

# YEAR: 2022
# day: 1st day of experiment
dat.d1 <- 
  dat %>%
  filter(yr == which.yr) |> 
  filter(counted_when=="D1_bN") %>%
  rename(cum_prop1=cum_prop)


# LOAD SLOW-FAST CLASSIFICATION -------------------------------------------

## load data for 2021 or 2022
foo <- 
  readRDS(
    file = paste0(path_to_repo, 
                  "/data/beads_fast_slow_2021_2022")
  ) %>%
  filter(yr==which.yr)

## Fast/Slow colonies in yr
fast.cols <- foo %>% filter(name=="fast") %>% pull(colonyID) %>% as.character()
slow.cols <- foo %>% filter(name=="slow") %>% pull(colonyID) %>% as.character()



# ANALYSES ---------------------------------------------------------------

# 1. Variation across AGE-GROUPS ----------------------------------------------

age.summary <- 
  dat.d1 %>% 
  select(age_class, cum_prop1) %>% 
  group_by(age_class) %>% 
  summarise(
    mean = mean(cum_prop1, na.rm = T),
    # median = median(cum_prop1, na.rm=T),
    ## calculating 95% CI using lm 
    ## source: https://bookdown.org/logan_kelly/r_practice/p09.html
    l95 = confint(lm(cum_prop1 ~ 1), level = .95)[1],
    u95 = confint(lm(cum_prop1 ~ 1), level = .95)[2],
    # but we can also do it manually using the 1.96*SE formula as shown below
    # l2 = mean - 1.96*(sd(cum_prop1, na.rm=T)/sqrt(n())),
    # u2 = mean + 1.96*(sd(cum_prop1, na.rm=T)/sqrt(n()))
    n = n()
  ) %>% 
  ungroup() |> 
  mutate_if(is.numeric, round, 3)

writeLines("here is the result as a table:")
age.summary


## PLOT --------------------------------------------------------------------

fig11 <- 
  ggplot(age.summary, 
         aes(y=mean, x=age_class)) +
  geom_jitter(
    aes(y=cum_prop1, x=age_class), 
    data=dat.d1, 
    alpha=0.3, 
    width=0.1, 
    size=7
    ) +
  geom_errorbar(aes(ymin=l95, ymax=u95), 
                position = position_dodge(width = 30),
                width=.3, size=2) +
  geom_point(size=9, 
             position = position_dodge(0.3), 
             alpha = 0.95,
             col="black") +
  scale_y_continuous(
    limits = c(0,1),
    breaks = c(0,0.5,1), 
    labels = c("0","0.5","1")
  ) +
  theme_Publication(25) +
  labs(
    x = "Colony age (years)",
    y = "Proportion of beads removed"
  ) +
  theme(legend.position = "none")

bd.saveplot(
  name="beads_age_kruskalwallis",
  width = 20, height = 15,
  fig11
)


## KRUSKAL-WALLIS TEST -----------------------------------------------------

writeLines("Q: is the % beads returned on day 1 different between different foraging classes?")
kruskal.test(
  cum_prop1 ~ age_class,
  data = dat.d1
)
writeLines("Ans: No")



# 2. Age; slow vs. fast colonies ------------------------------------------

age.summary2 <- 
  dat.d1 |> 
  mutate(
    fast_slow = case_when(
      colonyID %in% fast.cols
      ~ "high",
      colonyID %in% slow.cols
      ~ "low",
      .default = NA
    )
  ) |> 
  filter(
    !is.na(fast_slow)
  ) |>
  group_by(
    colonyID,
    age_obs,
    age_class,
    fast_slow
  ) |>
  summarize(
    mean_prop = median(cum_prop1, na.rm = T),
    sd_prop = sd(cum_prop1, na.rm = T)
  ) |> 
  ungroup() |> 
  arrange(
    fast_slow,
    age_obs
  )


## age composition -------------------------------------------------------

age.summary2 |> 
  janitor::tabyl(
    fast_slow,
    age_class
  )


## age v. trash removal v. high/low ----------------------------------------

fig12 <- age.summary2 |> 
  ggplot(
    aes(
      x = log2(age_obs),
      y = mean_prop,
      group = fast_slow,
      fill = fast_slow,
      color = fast_slow
    )
  ) +
  # geom_errorbar(
  #   aes(
  #     ymin=mean_prop-sd_prop, 
  #     ymax=mean_prop+sd_prop
  #   ), 
  #   width=.1, 
  #   size=2,
  #   alpha = .75,
  #   position = position_dodge(width = 1.5)
  # ) +
  geom_point(
    shape = 21,
    color = "black",
    stroke = 1.5,
    size = 7,
    alpha = .75,
    position = position_dodge(0)
  ) +
  scale_x_continuous(
    limits = c(1,5),
    breaks = 1:5,
    labels = 2^(1:5)
  ) +
  labs(
    x = "Colony age (years)",
    y = "Proportion of beads (median)"
  ) +
  theme_Publication(25) +
  scale_fill_manual(
    values = bead_colors
  ) +
  scale_color_manual(
    values = bead_colors
  ) +
  theme(
    legend.position = "none"
  )

bd.saveplot(
  name="beads_age_high_low",
  width = 20, height = 15,
fig12
)


















