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
conflicted::conflicts_prefer(dplyr::lag)


# Set directory -----------------------------------------------------------

## set path to your working directory (repo) and
path_to_repo = "/Users/bidas/Documents/05_postdoc/Deborah_Gordon/04_research/2022/02_bead_data"

## where to save figures
path_to_figures = paste0(path_to_repo,
                         "/manuscript/figures/tmp/")


# load functions ----------------------------------------------------------

## theme for publication quality figures
source(paste0(path_to_repo,"/functions/theme_publication.R"))
source(paste0(path_to_repo,"/functions/utils.R"))

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
yr_colors <-c("grey30","orange")
## age classes
age_colors <- c("#8D72A1", "#F26C7F", "#FBAD73")
## bead classes
# bead_colors <- c(slow="#1A80D9", fast="#F23030") # slow=blue, fast=red
bead_colors <- c(slow="#FBB369", fast="#3868A4")
## foraging classes
foraging_colors <- c("#9CD5A2", "#F29544")

# ## Analyzing data for
# which.yr <- 2022
# writeLines(paste0("Currently analyzing data for year: ", which.yr))


# Load data ---------------------------------------------------------------

dat <- readRDS(
# readRDS(
  paste0(
    path_to_repo,
    "/results/raw_data/",
    "data_for_manuscript.RDS"
  )
) |> 
  ## FILTER: keep data for Day 1 only ----
  filter(
    counted_when == "D1_bN"
  ) 

# colony.name.register <- readRDS(
#   paste0(
#     path_to_repo, 
#     "/results/raw_data/",
#     "colony_name_register.RDS"
#   )
# )


# ANALYSES ----------------------------------------------------------------

# Format data -------------------------------------------------------------

for.beads <- dat %>%
  mutate(
    colonyID2 = paste0(colonyID,"_",age_obs)
  ) |> 
  arrange(yr, age_obs, colonyID) %>% 
  mutate(
    prop_foraging = round(foraging_30s/max_foraging_30s, 2),
    cum_prop = round(cum_prop, 2)
  ) %>%
  select(
    yr, 
    rep, 
    colonyID, 
    colonyID2,
    age_obs, 
    age_class,
    prop_foraging, 
    foraging_30s, 
    max_foraging_30s,
    beads_collected,
    prop_beads_returned = cum_prop
 ) 
  

# TO DO: COME BACK HERE -------------------------------------------------
# Check if the following is necessary.
# filter to keep only colonies with at least two points
for.beads <-
  for.beads %>%
  group_by(yr, colonyID) %>%
  summarize(n_obs = n()) %>%
  ungroup() %>%
  filter(n_obs>=2) %>%
  right_join(for.beads) %>%
  na.omit()


# Check distributions -----------------------------------------------------

for.beads |> 
  select(
    yr,
    colonyID,
    prop_beads_returned,
    age_class,
    foraging_30s,
    prop_foraging
  ) |> 
  mutate(
    age_class = as.numeric(age_class)
  ) |> 
  tidyr::pivot_longer(
    cols = !c(yr, colonyID),
    names_to = "what",
    values_to = "value"
  ) |> 
  
  ggplot(
    aes(
      x = value,
      fill = yr
    )
  ) +
  geom_density(alpha = .5) +
  facet_wrap( ~ what, scales = "free", nrow = 2) +
  theme_minimal(20) +
  scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(n.breaks = 3) +
  scale_fill_manual(
    values = yr_colors
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = "Distributions"
  )


# Plot EDA  ----------------------------------------------------------

p1 <- for.beads |> 
  rowwise() |> 
  mutate(
    rep = strsplit(as.character(rep), "-")[[1]][1] |> 
      factor(
        levels = paste0("rep", 1:6)
      )
  ) |> 
  ungroup() |> 
  ggplot(
    aes(
      x = prop_foraging,
      # x = log2(age_obs),
      # x = yr,
      y = prop_beads_returned
      # y = qlogis(prop_beads_returned) # logit scale
    )
  ) +
  geom_point(
    shape = 21,
    color = "black",
    aes(
      fill = yr
    ),
    size = 3,
    alpha = 0.7
  ) +
  scale_x_continuous(
    breaks = c(0, .5, 1),
    labels = c(0, 0.5, 1)
  ) +
  scale_y_continuous(
    breaks = c(0, .5, 1),
    labels = c(0, 0.5, 1)
  ) +
  scale_color_manual(
    values = c("grey20", "orange")
  ) +
  scale_fill_manual(
    values = c("grey20", "orange")
  ) +
  theme_bw(20) +
  theme(
    legend.position = "bottom"
  )

p1 +
  facet_wrap(
    ~ rep,
    nrow = 2,
    scales = "fixed"
  ) +
  geom_smooth(
    method = "glm",
    se = TRUE,
    aes(
      col = yr
    )
  )

# ## Save plot
# bd.saveplot(
#   name = "foraging_bead_relation_v2",
#   width = 24, 
#   height = 12,
#   # the plot
#   p2 + p3
# )


# MODEL -------------------------------------------------------------------

## Prep data ----

dat.mod <- for.beads |> 
  mutate(
    beads_returned = round(
      prop_beads_returned*beads_collected, 
      0
    )
  ) |> 
  filter(
    !is.na(foraging_30s)
  ) |> 
  group_by(
    colonyID,
    yr
  ) |> 
  mutate(
    n_reps_yr = n()
  ) |> 
  ungroup() |> 
  filter(
    n_reps_yr > 1
  )


# GLMM --------------------------------------------------------------------

## null model ----
mm_null <- lme4::glmer(
  cbind(
    beads_returned,
    beads_collected - beads_returned
  ) ~ 1 + 
    (1|rep),
  data = dat.mod, 
  family = binomial(link = "logit")
)

mm0 <- update(
  mm_null,
  . ~ . +
    (1|colonyID)
)
anova(
  mm_null,
  mm0
)

## Age class ----
# Can't talk about it since all 2021 colonies are from only
# one age class.

## Raw foraging counts ----
mm0a <- update(
  mm0,
  . ~ . + 
    foraging_30s
)
anova(mm0a, mm0)

## Prop foraging ----
mm0b <- update(
  mm0,
  . ~ . +
    prop_foraging
)
anova(mm0b, mm0)


## Interaction: yr, prop foraging ----
mm1 <- update(
  mm0b,
  . ~ . + 
    yr: prop_foraging
)
anova(
  mm1,
  mm0b
)

## yr ----
mm2 <- update(
  mm1,
  . ~ . + yr
)
anova(
  mm1, mm2
)
# the odds of bead returns changes year to year (chisq = 16, df = 1, p < 0.001)

## Colony effects

## colonyID: slope (yr) ----
mm3 <- update(
  mm2, 
  . ~ . - 
    (1|colonyID) +
    (yr|colonyID)
)
anova(
  mm2, mm3
)

## replicates: intercept ----
mm3a <- update(mm2, . ~ . - (1|rep))
anova(
  mm3, mm3a
)

# the odds of bead returns vary from replicate to replicate (chisq = 10, df = 1, p = 0.001).


## Specify model ----
glmm <- mm3


## QC model ----
plot(glmm)

## Export summary table ----
summary(glmm) |> 
  purrr::pluck(coefficients) |> 
  as.data.frame() |> 
  tibble::rownames_to_column(
    var = "Variable"
  ) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  write.csv(
    "./results/tables/02_resubmission_model_summary.csv",
    row.names = FALSE
  )

## plot predictions ----
### bead returns ~ prop foraging ----
pred.glmm <- emmeans::emmeans(
  glmm,
  c("yr", "prop_foraging"),
  at = list(prop_foraging = c(0, 0.25, 0.5, .75, 1)),
  type = "response"
) |> 
  broom.mixed::tidy() |> 
  janitor::clean_names() |> 
  # filter(
  #  p_value < 0.05 
  # ) |> 
  rename(
    prop_beads_returned = prob
  )

p2a <- p1 +
  geom_ribbon(
    aes(
      ymin = prop_beads_returned - std_error*qnorm(0.975),
      ymax = prop_beads_returned + std_error*qnorm(0.975),
      fill = yr
    ),
    alpha = 0.35,
    data = pred.glmm
  ) +
  geom_line(
    data = pred.glmm,
    aes(col = yr),
    size = 2
  ) +
  labs(
    y = "Proportion beads returned",
    x = "Proportion foraging"
  ) +
  facet_grid(~ yr) +
  labs(
    title = "Predicted relation",
    subtitle = "(95% Confidence Interval)"
  )

#### Save plot ----
bd.saveplot(
  name = "pred_glmm_foraging",
  width = 20,
  height = 14,
  # the plot
  plot = p2a
)

# Simple model for 2022 ---------------------------------------------------

dat.2022 <- dat.mod |> 
  filter(
    yr == "2022"
  ) |> 
  mutate(
    colonyID = colonyID |> 
      forcats::fct_drop()
  )

colonies <- dat.mod |> 
  group_by(colonyID) |> 
  reframe(
    n_yr = length(unique(na.omit(yr)))
  ) |> 
  filter(n_yr == 2) |> 
  pull(colonyID) |> 
  as.character()

colonies.2022 <- dat.2022 |> 
  pull(colonyID) |> 
  as.character() |> 
  unique()

res <- list()

# # define iterations
# # max = 7 (max num of colonies in 2021)
# max = 7
# 
# set.seed(123)
# 
# sub.colonies <- sample(colonies, max)

for (i in 1:length(colonies.2022)) {
  
  d <- dat.2022 |> 
    mutate(
      colonyID = fct_relevel(
        colonyID,
        colonies.2022[i]
      )
    )
  
  ref <- levels(d$colonyID)[1]
  
  m <- lme4::glmer(
    cbind(
      beads_returned,
      beads_collected - beads_returned
    ) ~ colonyID + 
      (1|rep),
    data = d, 
    family = binomial(link = "logit")
  )
  
  res[[i]] <- summary(m) |>
    # summary(m) |>
    pluck(coefficients) |> 
    as.data.frame() |> 
    tibble::rownames_to_column(
      var = "colonyID"
    ) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    filter(
      !stringr::str_detect(
        colony_id,
        "Intercept|prop_foraging"
      )
    ) |> 
    mutate(
      ref = ref,
      colonyID = stringr::str_replace(
        colony_id,
        "colonyID",
        ""
      )
    ) |> 
    filter(
      colonyID %in% colonies
    ) |> 
    mutate(
      group = case_when(
        pr_z >= 0.05 ~
          "same",
        estimate < 0 ~
          "lower",
        .default = "higher"
      ) |> 
        factor(
          levels = c(
            "lower",
            "same",
            "higher"
          )
        ),
      yr = "2022"
    )
  
}


res |> 
  setNames(
    colonies.2022
  ) |> 
  str(max.level = 2)


# res |> 
#   plot_colony_diff(
#     iter = 5, which_colonies = colonies
#   ) +
#   theme_bw(20)


# Simple model for 2021 ---------------------------------------------------

dat.2021 <- dat.mod |> 
  filter(
    yr == "2021"
  ) |> 
  mutate(
    colonyID = colonyID |> 
      forcats::fct_drop()
  )

colonies.2021 <- dat.2021 |> 
  pull(colonyID) |> 
  as.character() |> 
  unique()

res.2021 <- list()

# # define iterations
# # max = 7 (max num of colonies in 2021)
# max = 7
# 
# set.seed(123)
# 
# sub.colonies <- sample(colonies, max)

for (i in 1:length(colonies.2021)) {
  
  d <- dat.2021 |> 
    mutate(
      colonyID = fct_relevel(
        colonyID,
        colonies.2021[i]
      )
    )
  
  ref <- levels(d$colonyID)[1]
  
  m <- lme4::glmer(
    cbind(
      beads_returned,
      beads_collected - beads_returned
    ) ~ colonyID +
      prop_foraging + 
      (1|rep),
    data = d, 
    family = binomial(link = "logit")
  )
  
  res.2021[[i]] <- summary(m) |>
    # summary(m) |>
    pluck(coefficients) |> 
    as.data.frame() |> 
    tibble::rownames_to_column(
      var = "colonyID"
    ) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    filter(
      !stringr::str_detect(
        colony_id,
        "Intercept|prop_foraging"
      )
    ) |> 
    mutate(
      ref = ref,
      colonyID = stringr::str_replace(
        colony_id,
        "colonyID",
        ""
      )
    ) |> 
    filter(
      colonyID %in% colonies
    ) |> 
    mutate(
      group = case_when(
        pr_z >= 0.05 ~
          "same",
        estimate < 0 ~
          "lower",
        .default = "higher"
      ) |> 
        factor(
          levels = c(
            "lower",
            "same",
            "higher"
          )
        ),
      yr = "2021"
    )
  
}


res.2021 |> 
  setNames(
    colonies.2021
  ) |> 
  str(max.level = 2)


# res.2021 |>
#   plot_colony_diff(iter = 4, which_colonies = colonies.2021)
# 
# res |> 
#   plot_colony_diff(iter = 1, which_colonies = colonies)

# gridExtra::grid.arrange(
#   plot_colony_diff(res.2021, iter = 1, which_colonies = colonies) + theme(legend.position = "none"),
#   plot_colony_diff(res.2021, iter = 3, which_colonies = colonies) + theme(legend.position = "none"),
#   # plot_colony_diff(res.2021, iter = 3, which_colonies = colonies) + theme(legend.position = "none"),
#   # plot_colony_diff(res.2021, iter = 4, which_colonies = colonies) + theme(legend.position = "none"),
#   # plot_colony_diff(res.2021, iter = 5, which_colonies = colonies) + theme(legend.position = "none"),
#   plot_colony_diff(res, iter = 1, which_colonies = colonies) + theme(legend.position = "none"),
#   plot_colony_diff(res, iter = 17, which_colonies = colonies) + theme(legend.position = "none"),
#   # plot_colony_diff(res, iter = 3, which_colonies = colonies) + theme(legend.position = "none"),
#   # plot_colony_diff(res, iter = 4, which_colonies = colonies) + theme(legend.position = "none"),
#   # plot_colony_diff(res, iter = 5, which_colonies = colonies) + theme(legend.position = "none"),
#   nrow = 2
# )


# Compare colonies across the two years -----------------------------------

dat.both <- rbind(
  do.call(rbind, res.2021),
  do.call(rbind, res)
) |> 
  as_tibble() |> 
  select(
    yr,
    colonyID,
    group
  ) |> 
  group_by_all() |> 
  tally() |> 
  ungroup() |> 
  group_by(
    yr, colonyID
  ) |> 
  mutate(
    prop = round(n/sum(n), 2)
  )

p3 <- dat.both |>
# dat.both |> 
  ggplot(
    aes(
      x = group,
      y = colonyID
    )
  ) +
  geom_tile(
    aes(
      fill = prop
    ),
    col = "white",
    size = 1.2
  ) +
  facet_grid(
    ~ yr
  ) +
  labs(
    x = "\nOdds of bead returns\n(retaive to another colony)",
    y = ""
  ) +
  theme_bw(20) +
  scale_fill_viridis_c(
    option = "E",
    begin = 0,
    end = 1,
    direction = -1,
    name = "",
    breaks = seq(0, 1, by = 0.5),
    labels = seq(0, 1, by = 0.5),
    limits = c(0, 1)
  ) +
  theme(
    legend.position = "right"
  ) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_blank()
    # axis.line = element_line(colour = "black")
  )


#### Save plot ----
bd.saveplot(
  name = "pred_glmm_colony_consistency_yrs",
  width = 18,
  height = 12,
  # the plot
  plot = p3
)



# # BRMS --------------------------------------------------------------------

## brm 1 ----
brm1 <- brms::brm(
  beads_returned | trials(beads_collected) ~
    yr +
    yr*prop_foraging +
    prop_foraging +
    (1|rep) +
    (yr|colonyID),
  data = dat.mod, 
  family = binomial(link = "logit"),
  # warmup = 500, 
  # iter = 2000, 
  # chains = 2, 
  # inits = "0", 
  cores = 4,
  seed = 123
)
summary(brm1)

# Check model -------------------------------------------------------------

# specify model
mod = brm1

# check convergence
plot(mod, nvariables = 4, ask = FALSE)

# # check predicted relation
# brms::conditional_effects(mod, "prop_foraging:yr")
# # # conditional_effects(mod, "age_class")

# plot estimates
# SOURCE: https://www.rensvandeschoot.com/tutorials/generalised-linear-models-with-brms/
library(dplyr)
library(ggplot2)
p_brm1 <- brms::mcmc_plot(
  mod,
  type = "areas",
  # transformations = "exp",
  prob = 0.95
) +
  geom_vline(
    xintercept = 0,
    # xintercept = 1,
    color = "maroon",
    size = 1,
    lty = 1
  ) +
  # scale_x_continuous(
  #   limits = c(-5, 5),
  #   breaks = c(-5, -2.5, 0, 2.5, 5),
  #   labels = c(-5, -2.5, 0, 2.5, 5)
  # ) +
  labs(
    x = "Estimate",
    title = "Bayesian\nbinomial regression"
  ) +
  theme_Publication(25)



# # interpreting coef
# # SOURCE: https://www.rensvandeschoot.com/tutorials/generalised-linear-models-with-brms/
# exp(
#   fixef(brm3)[3,-2]*sd(
#     pull(dat.mod, prop_foraging), 
#     na.rm = T
#   )
# )
# # 1 SD increase in prop_foraging
# # (1 - 0.63) = 37% decrease in the odds of ants bringing out a bead

#### Save plot ----
bd.saveplot(
  name = "posterior_dist_estimates_brm",
  width = 24,
  height = 24,
  # the plot
  plot = p_brm1
)


# Plot findings -----------------------------------------------------------

pred.brm <- emmeans::emmeans(
  mod,
  c("yr", "prop_foraging"),
  at = list(prop_foraging = c(0, 0.25, 0.5, .75, 1)),
  type = "response"
) |> 
  broom.mixed::tidy() |> 
  janitor::clean_names() |> 
  # filter(
  #  p_value < 0.05 
  # ) |> 
  rename(
    prop_beads_returned = prob
  )

p_brm2 <- p1 +
  geom_ribbon(
    aes(
      ymin = lower_hpd,
      ymax = upper_hpd,
      fill = yr
    ),
    alpha = 0.35,
    data = pred.brm
  ) +
  geom_line(
    data = pred.brm,
    aes(col = yr),
    size = 2
  ) +
  facet_grid(~ yr) +
  labs(
    title = "Predicted relation",
    subtitle = "(95% Credible Interval)"
  )

#### Save plot ----
bd.saveplot(
  name = "pred_brms_foraging",
  width = 24,
  height = 16,
  # the plot
  plot = p_brm2
)