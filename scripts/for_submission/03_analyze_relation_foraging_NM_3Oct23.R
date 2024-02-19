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

## Analyzing data for
which.yr <- 2022
writeLines(paste0("Currently analyzing data for year: ", which.yr))


# Load data ---------------------------------------------------------------

dat <- readRDS(
  paste0(
    path_to_repo,
    "/results/raw_data/",
    "data_for_manuscript.RDS"
  )
)

# colony.name.register <- readRDS(
#   paste0(
#     path_to_repo, 
#     "/results/raw_data/",
#     "colony_name_register.RDS"
#   )
# )

## Subset observed foraging rates
## for first day of experiment
for.dat.2022 <-
  dat %>% 
  filter(
    yr=="2022" &
      counted_when == "D1_bN"
  ) %>% 
  select(foraging_30s) %>% 
  na.omit() %>% 
  filter(foraging_30s!=0) %>% 
  arrange(foraging_30s) %>% 
  pull() %>% 
  unique()

for.dat.2021 <-
  dat %>%
  filter(
    yr=="2021" &
      counted_when == "D1_bN"
  ) %>% 
  select(foraging_30s) %>% 
  na.omit() %>% 
  filter(foraging_30s!=0) %>% 
  arrange(foraging_30s) %>% 
  pull() %>% 
  unique()

if (which.yr=="2021") {
  for.data <- for.dat.2021
} else if (which.yr=="2022") {
  for.data <- for.dat.2022
} else {
  print("which.yr can be either 2021 or 2022")
}



# Simulate trash accumulation ---------------------------------------------

prop.trash <- c(0.08)             ## 8% of all incoming items are trash
duration.foraging <- c(3)         ## duration of foraging = 3 hours

## incoming rate = 1/2 of total (in+out) foraging rate
rates.incoming <- for.data/2

# ## incoming foraging rate - quantiles
rates.incoming <- c(min(rates.incoming), 
                    quantile(rates.incoming, 
                             probs = c(0.1,0.3,0.5,0.7,0.9)), 
                    max(rates.incoming))

n.rows <- length(duration.foraging)
n.cols <- length(rates.incoming)

list.prop.trash <- list()

for (k in 1:length(prop.trash)) {
  
  items <- matrix(data = NA, n.rows, n.cols)
  trash <- matrix(data = NA, n.rows, n.cols)
  beads <- matrix(data = NA, n.rows, n.cols)
  
  for (i in 1:length(duration.foraging)) {
    # set duration in hours
    d <- duration.foraging[[i]]
    
    for (j in 1:length(rates.incoming)) {
      # set rate of incoming ants (#ants/30seconds) = (in+out)/2
      r <- rates.incoming[[j]]
      
      # number of items brought in
      items[i,j] <- r*2*60*d
      
      # number of trash
      trash[i,j] <- prop.trash[[k]]*items[i,j]
      
      # percent of trash that is beads
      beads[i,j] <- (50/trash[i,j])*100
    }
  }
  rownames(items) <- paste0(duration.foraging,"-hours")
  colnames(items) <- paste0(rates.incoming,"-ants/30s")
  rownames(trash) <- paste0(duration.foraging,"-hours")
  colnames(trash) <- paste0(rates.incoming,"-ants/30s")
  rownames(beads) <- paste0(duration.foraging,"-hours")
  colnames(beads) <- paste0(rates.incoming,"-ants/30s")
  
  ## save the matrices into the list
  list.prop.trash[[k]] <- list(items,trash,beads)
  names(list.prop.trash[[k]])[1] <- paste0("percent_trash_",prop.trash[[k]],"_n_forageditems")
  names(list.prop.trash[[k]])[2] <- paste0("percent_trash_",prop.trash[[k]],"_n_trash")
  names(list.prop.trash[[k]])[3] <- paste0("percent_trash_",prop.trash[[k]],"_percent_beads")
  names(list.prop.trash)[k] <- paste0("percent_trash_",prop.trash[[k]])
}

# Let's check the output
# names(list.prop.trash)

writeLines("Scenario: when 8% of all foraged items are trash")
writeLines("Numbers = % trash accumulated that is beads")
list.prop.trash[[1]][[3]] %>% round(.,1) %>% t()


# Table simulation results ------------------------------------------------

rates.incoming <- rates.incoming %>% as.numeric() %>% round(.,2)
foraged.items <- list.prop.trash[[1]] %>% pluck(1) %>% as.numeric() %>% round(.,0)
n.trash <- list.prop.trash[[1]] %>% pluck(2) %>% as.numeric() %>% round(.,0)
percent.beads <- list.prop.trash[[1]] %>% pluck(3) %>% as.numeric() %>% round(.,2)

data.frame(
  breaks_in_2022 = c(
    "minimum",
    "10th percentile",
    "30th percentile",
    "median",
    "70th percentile",
    "90th percentile",
    "maximum"
  ),
  avg_foraging_rate = rates.incoming,
  n_foraged_items = foraged.items,
  n_foraged_trash_items = n.trash,
  n_beads_provided = 50,
  percent_trash_beads = percent.beads
) #%>%
#   write.csv(.,
#             paste0(
#               path_to_repo,
#               "/results/tables/",
#               "02_table_foraging_trash_proportions.csv"
#             ),
#             row.names = F
#             )


# Plot the simulation -----------------------------------------------------

min.abline <- round(min(list.prop.trash[[1]][[3]]),0)
max.abline <- round(max(list.prop.trash[[1]][[3]]),0)
mid.abline <- round(median(list.prop.trash[[1]][[3]]),0)
par(mfrow=c(1,1), mar=c(5,5,5,5))
plot(x=rates.incoming, y=list.prop.trash[[1]][[3]], 
     type = "b", 
     ylab = "% of trash that are beads",
     xlab = "avg. foraging rate (ants/30s)",
     main = paste0("foraged items are trash = 8%\nduration of foraging = 3h"),
     # xlim = c(0,50), 
     # ylim = c(0,23),
     # xaxt='n', yaxt='n',
     pch=21, col="black", bg="orange", cex=2,
     cex.lab=1.5, cex.main=1.5)
# axis(side = 1, at = c(0,10,20,30,40,50), cex.axis=1.5)
# axis(side = 2, at = c(0,min.abline,mid.abline,max.abline), cex.axis=1.5)
abline(h=c(mid.abline), col="grey60", lty="dashed", lwd=1.5)




# ANALYSES ----------------------------------------------------------------


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


# Format data -------------------------------------------------------------

for.beads <-
  dat %>% 
  ## keep only D1 - before noon data
  filter(counted_when=="D1_bN") %>% 
  ## keep only data for yr==2022
  filter(
    yr == which.yr
  ) %>%
  arrange(yr, age_obs, colonyID) %>% 
  mutate(
    colonyID2=paste0(colonyID,"-",age_obs)
  ) %>% 
  
  select(yr, colonyID, age_obs, colonyID2, 
         foraging_30s, max_foraging_30s, 
         cum_prop, beads_collected) %>% 
  distinct() %>% 
  na.omit() %>% 
  # only keep rows with non-zero bead returns
  filter(
    cum_prop!=0
  ) %>% 
  mutate(
    beads_color=ifelse(colonyID%in%fast.cols, bead_colors[2],
                       ifelse(colonyID%in%slow.cols, bead_colors[1], "grey90"))
  ) %>% 
  filter(beads_color!="grey90") %>% 
  mutate(slow_fast=ifelse(colonyID%in%fast.cols, "fast", "slow")) %>% 
  mutate(slow_fast=factor(slow_fast, levels = c("slow", "fast")))


# filter to keep only colonies with at least two points
for.beads <-
  for.beads %>% 
  group_by(yr, colonyID) %>% 
  summarize(n_obs = n()) %>% 
  ungroup() %>% 
  filter(n_obs>=2) %>% 
  right_join(for.beads) %>% 
  na.omit()



# Calculate colony averages -----------------------------------------------

## observed values of foraging and bead returns
for.real <- for.beads$foraging_30s/2
beads.real <- for.beads$cum_prop

# for.col <- for.beads$for_color
beads.col <- for.beads$beads_color

# calculate the medians across all observations
median.for <- median(for.real, na.rm=T)
median.beads <- median(beads.real, na.rm = T)

colonies <- for.beads %>% pull(colonyID2) %>% as.character() %>% unique()

## make a list to save the centroids
for.real.centroid <- list()
beads.real.centroid <- list()

par(mfrow=c(5,4), mar=c(3,2,3,1))

for (j in 1:length(colonies)) {
  colony <- colonies[j]
  
  # obtain the observed values
  for.real <- for.beads[for.beads$colonyID2==colony,]$foraging_30s/2
  beads.real <- for.beads[for.beads$colonyID2==colony,]$cum_prop
  
  # obtain classification colors
  beads.col <- for.beads[for.beads$colonyID2==colony,]$beads_color
  
  # calculate the centroid
  for.real.centroid[[j]] <- mean(for.real)
  beads.real.centroid[[j]] <- mean(beads.real)
  
}


# Plot relation  ----------------------------------------------------------

s.for.beads <- 
  for.beads %>% 
  mutate(incoming_30s = foraging_30s/2) %>%
  select(colonyID2, incoming_30s, cum_prop, slow_fast) %>% 
  
  group_by(colonyID2) %>% 
  summarize(incoming_30s = mean(incoming_30s),
            cum_prop = mean(cum_prop)) %>% 
  ungroup() %>% 
  
  left_join(for.beads %>% select(colonyID2, slow_fast) %>% distinct(), by="colonyID2") %>% 
  
  group_by(slow_fast) %>% 
  mutate(z_incoming = scale(incoming_30s)) %>% 
  mutate(z_cum_prop = scale(cum_prop)) %>% 
  mutate(l_incoming = log10(incoming_30s)) %>% 
  mutate(l_cum_prop = log10(cum_prop)) %>% 
  ungroup() %>% 
  
  group_by(colonyID2) %>% 
  mutate(age = str_split(colonyID2,"-")[[1]][3]) %>% 
  mutate(age=as.numeric(age)) %>% 
  ungroup() %>% 
  mutate(age_class=ifelse(age<5, "2-4yr", "5+yr")) %>%
  mutate(age_class=factor(age_class, levels=c("2-4yr", "5+yr"))) %>%
  
  select(colonyID2, age, age_class, everything())


p1 <- 
  s.for.beads %>% 
  
  ggplot(aes(x=incoming_30s, y=cum_prop, fill=slow_fast)) +
  
  geom_hline(yintercept = median.beads, lty=2, size = 2, alpha=.5) +
  geom_vline(xintercept = median.for, lty=2, size = 2, alpha=.5) +
  
  geom_point(size=5,
             aes(bg=slow_fast), shape = 21,
             # aes(shape=age_class, bg=slow_fast),
             col="black",
             stroke=1.5,
             alpha=.8) +
  ### Theme
  scale_y_continuous(n.breaks = 4, limits = c(0,1)) +
  scale_x_continuous(n.breaks = 4) +
  theme_bw(20) +
  # facet_grid(~slow_fast, scales = "fixed") +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(values=bead_colors) +
  scale_fill_manual(values=bead_colors) +
  scale_shape_manual(values = c(21,24)) +
  labs(x="Foraging rate (ants/30s)",
       y="Proportion of beads removed")

## Save plot
bd.saveplot(
  name = "foraging_bead_relation_v1",
  width = 12, 
  height = 12,
  # the plot
  p1
)


# Fisher's exact tests ----------------------------------------------------

library(GeneOverlap)

colonies2 <- for.beads %>% pull(colonyID) %>% as.character() %>% unique()

# # create a set of fast and slow colonies as per initial classification
# set1 <- list("fast"=fast.cols, 
#              "slow"=slow.cols)

# colonies split by median proportion of beads removed
set1 <- list(
  "high_rubbish_removal" = colonies2[
    which(unlist(beads.real.centroid) > median.beads)
  ],
  "low_rubbish_removal" = colonies2[
    which(unlist(beads.real.centroid) < median.beads)
  ]
)

# colonies split by median foraging
set2 <- list(
  "high_foraging" = colonies2[
    which(unlist(for.real.centroid) > median.for)
  ],
  "low_foraging" = colonies2[
    which(unlist(for.real.centroid) < median.for)
  ]
)

gom.obj <- newGOM(set1, 
                  set2,
                  genome.size = length(colonies2))

writeLines("The p-values for the Fisher's exact test:")
getMatrix(gom.obj, name = "pval") %>% round(.,4)
writeLines("The odds-ratio:")
getMatrix(gom.obj, name = "odds.ratio") %>% round(.,2)


# log-Plot --------------------------------------------------------------------


p2 <- 
  s.for.beads %>% 
  
  ggplot(aes(x=l_incoming, y=l_cum_prop, fill=slow_fast)) +
  
  theme_bw(20) +
  geom_smooth(aes(group=slow_fast, col=slow_fast),
              size=3,
              formula="y~x",
              method = "lm") +
  
  geom_point(size=5,
             shape=21,
             col="black", aes(bg=slow_fast),
             # col="black", aes(shape=age_class, bg=slow_fast),
             stroke=1.5,
             alpha=.8) +
  ### Theme
  scale_y_continuous(n.breaks = 4, limits = c(-1.5, 0)) +
  scale_x_continuous(n.breaks = 4) +
  theme_bw(20) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(values=bead_colors) +
  scale_fill_manual(values=bead_colors) +
  scale_shape_manual(values = c(21,24)) +
  labs(x="avg. foraging rate (ants/30s)",
       y="proportion of beads returned"
       # title = "log-10 scaled"
       )



# Model fit ---------------------------------------------------------------

## Using a linear regression to see if the slope's are non-zero
mod <- lm(l_cum_prop ~ l_incoming*slow_fast, data = s.for.beads) # with slow colonies as the reference
# mod <- lm(l_cum_prop ~ l_incoming*slow_fast, data = s.for.beads %>% mutate(slow_fast=factor(slow_fast, levels = c("fast","slow")))) # with fast colonies as reference
mod %>% summary()

# ## > Table the model summary: < manuscript >
# mod |> 
#   broom::tidy(conf.int=F) |> 
#   mutate_if(is.numeric, round, 3) |> 
#   mutate(
#     term = c(
#       "Intercept",
#       "log_foraging",
#       "slow_fast-fast",
#       "log_foraging:slow_fast-fast"
#     )
#   ) |> 
#   janitor::clean_names()

# writeLines("The confidence intervals for: FAST colonies")
mod.fast <- lm(l_cum_prop ~ l_incoming, data = s.for.beads %>% filter(slow_fast=="fast"))
mod.fast %>% summary()

  # writeLines("The confidence intervals for: SLOW colonies")
mod.slow <- lm(l_cum_prop ~ l_incoming, data = s.for.beads %>% filter(slow_fast=="slow"))
mod.slow %>% summary()



# Plot model fit ----------------------------------------------------------

## fit model predictions
p3 <- 
  s.for.beads %>% 
  
  mutate(ypred=ifelse(slow_fast=="slow",
                      10^(coef(mod.slow)[1])*(incoming_30s)^coef(mod.slow)[2],
                      mean(s.for.beads[s.for.beads$slow_fast=="fast",]$cum_prop))) %>% 
  
  ## Plot the curves
  ggplot(aes(x=incoming_30s, y=cum_prop, fill=slow_fast)) +
  
  geom_line(aes(x=incoming_30s, y=ypred, col=slow_fast),
            size=3) +
  
  geom_point(size=5,
             aes(bg=slow_fast), shape = 21,
             # aes(shape=age_class, bg=slow_fast),
             col="black",
             stroke=1.5,
             alpha=.8) +
  ### Theme
  scale_y_continuous(n.breaks = 4, limits = c(0,1)) +
  scale_x_continuous(n.breaks = 4) +
  theme_bw(20) +
  # facet_grid(~slow_fast, scales = "fixed") +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(values=bead_colors) +
  scale_fill_manual(values=bead_colors) +
  scale_shape_manual(values = c(21,24)) +
  labs(x="Avg. foraging rate (ants/30s)",
       y="proportion of beads removed")


## Save plot
bd.saveplot(
  name = "foraging_bead_relation_v2",
  width = 24, 
  height = 12,
  # the plot
  p2 + p3
)

