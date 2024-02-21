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



# Bead data - 2021 ----------------------------------------------------

# read bead data
bead.dat <- read.csv(paste0(path_to_repo, "/data/beads_out_new.csv"),
                     stringsAsFactors = F, header = T) %>% as_tibble()

# read colony-age data
colony.dat <- read.csv(paste0(path_to_repo, 
                              "/data/foraging_counts_08_13_22.csv"),
                       stringsAsFactors = F,
                       header = T) %>% 
                as_tibble() %>% 
                select(
                  colonyID = colony_ID,
                  everything()
                ) %>% 
                mutate(
                  colonyID = factor(colonyID)
                ) %>% 
                # create age-classes
                mutate(
                  age_class = ifelse(
                    age_2022 <= 2, 
                    "1-2_yr_old",
                    ifelse(
                      age_2022 > 2 & age_2022 <= 4, 
                      "3-4_yr_old",
                      "older_than_5_yr"
                    )
                  )
                )

# Specify the dates for green/pink beads
green.days <- c("8/23/21","8/24/21", "8/27/21","8/28/21") %>% as.Date(format = "%m/%d/%y")
pink.days <- c("8/25/21","8/26/21") %>% as.Date(format = "%m/%d/%y")


# Summarize data - 2021 ---------------------------------------------------

writeLines("What are the names of the colonies? \nShown as: Colony-ID")
bead.dat %>% 
  select(colonyID) %>%
  distinct() %>% 
  mutate(colonyID=as.factor(colonyID)) %>% 
  left_join(colony.dat, by="colonyID") %>%
  arrange(age_2022) %>% 
  head(4)

# Clean data: 2021 --------------------------------------------------------

old.summ <-
  bead.dat %>%
  arrange(colonyID) %>% 
  mutate(
    date = as.Date(date, format = "%m/%d/%y"),
    colonyID = factor(colonyID)
  ) %>%
  
  # add a column that specifies the color of beads used
  mutate(bead_col = ifelse(date %in% green.days[1:2], "rep1-green",
                           ifelse(date %in% green.days[3:4], "rep3-green",
                                  ifelse(date %in% pink.days, "rep2-pink", NA)))) %>%
  mutate(bead_col = factor(bead_col, levels = c("rep1-green","rep2-pink","rep3-green"))) %>% 
  
  # Keep only data for first two days of observations after presenting beads
  select(
    num_out:n_returned_Day2, 
    bead_col
  ) %>%
  
  # format columns appropriately
  mutate(
    n_collected = readr::parse_number(n_collected),
    n_returned_Day1 = as.numeric(n_returned_Day1),
    n_returned_Day2 = as.numeric(n_returned_Day2)
  ) %>% 
  
  # keep only colonies for which we have data on both days
  filter(
    !is.na(n_returned_Day1) &
      !is.na(n_returned_Day2)
  ) %>% 
  
  # keep only colonies for which we have at least X number of collected beads 
  filter(
    n_collected >= atleast.beads
  ) %>% 
  
  # Cumulative prop 
  # create the cumulative sums
  mutate(
    cum_prop_day1 = n_returned_Day1/n_collected,
    cum_prop_day2 = (n_returned_Day1+n_returned_Day2)/n_collected
  ) %>%
  pivot_longer(
    cols = starts_with("cum_prop"),
    names_to = "day_of_obs",
    values_to = "cum_prop"
  ) %>% 
  mutate(
    day_of_obs = as.numeric(readr::parse_number(day_of_obs)),
    date = date+day_of_obs-1
  ) %>% 
  select(
    num_out, 
    date, colonyID,
    n_collected, 
    bead_col, 
    day_of_obs, 
    cum_prop
  ) %>% 
  
  ## add age information
  left_join(
    colony.dat, by="colonyID"
  ) %>% 
  mutate(
    age_obs = age_2022-1,
    age_class = ifelse(
      age_obs <= 2, 
      "1-2_yr_old",
      ifelse(
        age_obs >= 5, 
        "older_than_5_yr", 
        "3-4_yr_old"
      )
    )
  ) %>% 
  mutate(age_class = factor(
    age_class, 
    levels = c(
      "1-2_yr_old", 
      "3-4_yr_old", 
      "older_than_5_yr"
    ))
  ) %>% 
  
  # rename day of obs
  mutate(day_of_obs = paste0("D",day_of_obs,"_bN")) %>% 
  mutate(day_of_obs = factor(day_of_obs, levels = c("D1_bN", "D2_bN"))) %>% 
  mutate(bead_col = factor(paste0(bead_col,"-21"))) %>% 
  
  ## change column names to make them compatible to the 2022 dataset
  select(
    colonyID, 
    date,
    bead_color = bead_col,
    age_obs, 
    age_class, 
    beads_collected = n_collected,
    counted_when = day_of_obs,
    cum_prop
  )


# Bead data - 2022 --------------------------------------------------------

filename <- paste0(
  path_to_repo,
  "/data/bead_data_2022/all_bead_data_2022/2022_bead_data_master_replicate_one_two_three_29Aug22.csv")

## Read data
dat.2022 <- read.csv(filename,
                     header = T, stringsAsFactors = F,
                     na.strings = c(""," ","did_not_count","NA",
                                    "did_not_check",
                                    "it_rained_could_not_check")) %>% 
  as_tibble() %>% 
  
  # select the relevant columns
  select(date, colonyID, age_class, age_2022, 
         bead_color, beads_put_out, beads_collected,
         beads_returned_Day1_beforenoon,
         beads_returned_Day1_afternoon,
         beads_returned_Day2_morning,
         beads_returned_Day2_beforenoon,
         beads_returned_Day2_afternoon,
         beads_returned_Day3_beforenoon,
         beads_returned_Day4_morning,
         beads_returned_Day4_beforenoon) %>% 
  
  # calculate the cumulative proportion of beads returned
  group_by(date, bead_color) %>% 
  
  # filter out the colonies for which we still need to check the data
  filter(!(colonyID=="1560" & bead_color=="green")) %>% 
  filter(!(colonyID=="1288" & bead_color=="green")) %>%
  filter(!(colonyID=="1546" & bead_color=="green")) %>%
  
  # convert all the bead count columns as numeric
  mutate_at(vars(matches("beads_")), as.numeric) %>% 
  ungroup()


# Tidy data - 2022 --------------------------------------------------------


## Create a tidy dataset and calculate the cumulative proportion of beads returned by colonies
tidydat.2022 <-
  dat.2022 %>% 
  
  # replace all the NAs with 0 to be able to calculate proportions
  mutate_at(vars(matches("beads_")), function(x) ifelse(is.na(x), 0, x)) %>%
  
  # day 1 - before noon
  mutate(cum_prop_returned_D1_beforenoon = round(beads_returned_Day1_beforenoon/beads_collected, 2)) %>% 
  mutate(cum_prop_returned_D1_afternoon = round(cum_prop_returned_D1_beforenoon+beads_returned_Day1_afternoon/beads_collected, 2)) %>% 
  mutate(cum_prop_returned_D2_morning = round(cum_prop_returned_D1_afternoon+beads_returned_Day2_morning/beads_collected, 2)) %>% 
  mutate(cum_prop_returned_D2_beforenoon = round(cum_prop_returned_D2_morning+beads_returned_Day2_beforenoon/beads_collected,2)) %>%
  mutate(cum_prop_returned_D2_afternoon = round(cum_prop_returned_D2_beforenoon+beads_returned_Day2_afternoon/beads_collected,2)) %>% 
  mutate(cum_prop_returned_D3_beforenoon = round(cum_prop_returned_D2_beforenoon+beads_returned_Day3_beforenoon/beads_collected,2)) %>% 
  mutate(cum_prop_returned_D4_morning = round(cum_prop_returned_D3_beforenoon+beads_returned_Day4_morning/beads_collected,2)) %>% 
  mutate(cum_prop_returned_D4_beforenoon = round(cum_prop_returned_D4_morning+beads_returned_Day4_beforenoon/beads_collected,2)) %>% 
  
  arrange(age_2022) %>% 
  
  select(
    colonyID,
    age_2022, age_class,
    bead_color,
    cum_prop_returned_D1_beforenoon:cum_prop_returned_D4_beforenoon,
    everything()
  ) %>% 
  
  ## keep only colonies that collected at least 40 (of 50) beads offered
  filter(beads_collected >= atleast.beads) %>% 
  
  pivot_longer(cols = starts_with("cum_prop"),
               names_to = "counted_when",
               values_to = "cum_prop") %>% 
  
  ## format the ordinal columns correctly
  mutate(counted_when = ifelse(counted_when == "cum_prop_returned_D1_beforenoon", "D1_bN",
                               ifelse(counted_when == "cum_prop_returned_D1_afternoon", "D1_aN",
                                      ifelse(counted_when == "cum_prop_returned_D2_morning", "D2_M",
                                             ifelse(counted_when == "cum_prop_returned_D2_beforenoon", "D2_bN",
                                                    ifelse(counted_when == "cum_prop_returned_D2_afternoon", "D2_aN",
                                                           ifelse(counted_when == "cum_prop_returned_D3_beforenoon", "D3_bN",
                                                                  ifelse(counted_when == "cum_prop_returned_D4_morning", "D4_M",
                                                                         "D4_bN")))))))) %>%
  mutate(counted_when = factor(counted_when, 
                               levels = c("D1_bN", "D1_aN", 
                                          "D2_M", "D2_bN", "D2_aN",
                                          "D3_bN",
                                          "D4_M", "D4_bN"))) %>% 
  mutate(bead_color = ifelse(bead_color %in% c("light_blue", "dark_blue"), "blue", bead_color))  %>% 
  mutate(colonyID = factor(colonyID), bead_color=factor(bead_color)) %>% 
  mutate(date = as.Date(date, format="%m/%d/%y")) %>% 
  
  mutate(age_obs = age_2022) %>% 
  
  # ## remove the timepoints that we do not have data for  
  # pink beads (round one)
  filter(!(bead_color=="pink" & counted_when == "D1_aN")) %>%
  filter(!(bead_color=="pink" & counted_when == "D2_aN")) %>%
  filter(!(bead_color=="pink" & counted_when == "D4_M")) %>%
  filter(!(bead_color=="pink" & counted_when == "D4_bN")) %>%
  # green beads (round two)
  filter(!(bead_color=="green" & counted_when == "D2_aN")) %>%
  filter(!(bead_color=="green" & counted_when == "D3_bN")) %>%
  filter(!(bead_color=="green" & counted_when == "D4_M")) %>%
  # blue beads (round three)
  filter(!(bead_color=="blue" & counted_when == "D1_aN")) %>%
  filter(!(bead_color=="blue" & counted_when == "D2_bN")) %>%
  filter(!(bead_color=="blue" & counted_when == "D4_bN")) %>% 
  
  ## select columns to make them compatible with 2021 dataset
  select(
    colonyID, date,
    bead_color,
    age_2022, age_obs, age_class, 
    beads_collected, counted_when, cum_prop
  )


# There are NAs in the bead_color column, check
tidydat.2022 %>% filter(is.na(bead_color))
# It appears that for colony 1504 and 1506, we do not have bead color infor for one of the replicates.
# Let's check to see if the other two color are already in the dataset, then by elimination the third and remaining color
# would be the correct entry.
tidydat.2022 %>% filter(colonyID %in% c("1504","1506") & 
                          counted_when %in% c("D1_bN","D2_M")) %>% arrange(colonyID)
# Yes it is blue. Add the information
#
## CORRECTION HAPPENED HERE
#
tidydat.2022[is.na(tidydat.2022$bead_color),]$bead_color <- "blue"
# Rename the bead color values to reflect replicate
tidydat.2022 <- 
  tidydat.2022 %>% 
  mutate(bead_color = recode_factor(bead_color, 
                                    pink="rep4-pink", 
                                    green="rep5-green", 
                                    blue="rep6-blue"))
## Re-summarize the data
tidydat.2022 %>% summary()


## Obtain data for Day 1 and Day 2 only
tidydat.2022.sub <-
  tidydat.2022 %>% 
  separate(counted_when, c("day","counted_when"), sep="_") %>% 
  filter(day %in% c("D1","D2") & counted_when %in% c("bN","M")) %>% 
  group_by(colonyID, bead_color, day) %>% 
  summarize(
    cum_prop2 = max(cum_prop),
    counted_when = paste(counted_when, collapse = ", "),
    n_counts = n()
  ) %>% 
  mutate(
    counted_when = ifelse(
      n_counts==1,
      counted_when,
      "bN"
    ),
    counted_when = paste0(day,"_",counted_when)
  ) %>% 
  select(-day,-n_counts) %>%
  
  left_join(
    tidydat.2022, 
    by=c("colonyID","bead_color","counted_when")
  ) %>% 
  
  select(
    colonyID, date, bead_color,
    age_obs, age_class, 
    beads_collected, counted_when, cum_prop
  ) %>% 
  ungroup() |>
  # fix dates to show actual date
  mutate(
    day = extract_numeric(counted_when),
    date = date + day - 1
  ) |>
  select(
    -day
  )


# Combine 2021 and 2022 data ----------------------------------------------

# Current dataset contains: All colonies from 2021 and 2022, 
# for which we have counts of the number of beads returned 
# by the colony on Day 1 and 2.
# 
# Note, for some colonies, day 2 data was observed in the morning 
# (counted_when = D2_M) instead of later in the day (D2_bN).

tidydat <-
  rbind(old.summ, tidydat.2022.sub) %>%
  mutate(
    counted_when = factor(
      counted_when,
      levels = c("D1_bN","D2_M", "D2_bN")
    ),
    age_class = factor(
      age_class, 
      levels = c("1-2_yr_old","3-4_yr_old","older_than_5_yr")
    )
  ) %>% 
  separate(
    counted_when, 
    c("day","counted_when"), 
    sep = "_"
  ) %>%
  group_by(bead_color) %>% 
  mutate(rep = str_split(bead_color, "-")[[1]][1]) %>%  
  ungroup()


# Clean combined data -----------------------------------------------------

tidydat %>%
  # filter(cum_prop>1) %>% 
  ## There are two values that are above 1, 
  ## and a closer inspection of those trials (see below)
  filter(
    colonyID %in% c("1440","1533") & 
      rep %in% c("rep5","rep6")
  )
  
## shows that in both cases, D1 value was very high and 
## it is safe to assume a cum-prop of 1 on Day 2

tidydat <- 
  tidydat %>% 
  mutate(
    cum_prop = ifelse(
      cum_prop > 1,
      1,
      cum_prop
    )
  )


# Foraging data - 2021 ----------------------------------------------------

filename3 <- paste0(path_to_repo, "/data/foraging_data.csv")

tidy.for.2021 <-
  read.csv(filename3) %>% as_tibble() %>%
  mutate(date = as.Date(date, format="%m/%d/%y")) %>%
  mutate(colonyID = as.factor(colonyID)) 

tidy.for.2021 <- 
  tidy.for.2021 %>% 
  # doing something, notice
  group_by(colonyID) %>% 
  summarise(max_foraging_30s = max(foraging_30s, na.rm = T)) %>% 
  ungroup() %>% 
  right_join(tidy.for.2021) %>%
  mutate(prop_foraging_30s = foraging_30s/max_foraging_30s) %>% 
  mutate_if(is.numeric, round, 2)

for.tidydat.2021 <-
old.summ %>% 
  mutate(
    colonyID = as.factor(colonyID),
    obs_day = parse_number(as.character(counted_when))
  ) %>%
  left_join(
    tidy.for.2021, 
    by=c("colonyID", "date")
  ) %>% 
  select(
    colonyID:bead_color, 
    obs_day, 
    max_foraging_30s, 
    foraging_30s, 
    prop_foraging_30s
  ) %>% 
  right_join(
    old.summ, 
    by=c("colonyID", "date", "bead_color")
  ) %>% 
  group_by(colonyID,bead_color) %>%  
  mutate(
    obs_day = paste0("D", obs_day[1])
  ) %>% 
  ungroup() %>%
  select(
    colonyID,
    date,
    bead_color,
    age_obs,
    age_class, 
    beads_collected,
    max_foraging_30s,
    foraging_30s,
    prop_foraging_30s,
    counted_when,
    cum_prop
  )


# Foraging data - 2022 ----------------------------------------------------

path_to_repo2 = paste0(
  path_to_repo,
  "/../",
  "/01_field_season_2022/04_field_work/02_rprojects/01_checking_data"
)

# set path to the behavioral data within your repository 
path_to_data2 = "/01_data/"

## Specify the name of the master file as saved on your computer
filename2 <- "arizona_2022_master_list_Aug29_1440h.csv"

column.names <- c(
  "date",
  "was_it_raining",
  "observer",
  "colony_ID",
  "first_obs",
  "second_obs",
  "time",
  "in_1",
  "out_1",
  "in_2",
  "out_2",
  "in_3",
  "out_3",
  "in_4",
  "out_4",
  "in_5",
  "out_5",
  "in_6",
  "out_6",
  "in_7",
  "out_7",
  "in_8",
  "out_8"
)

for.dat <- 
  read.table(paste0(path_to_repo2, path_to_data2, filename2),
             sep = ",",
             header = T,
             na.strings = c(""," "),
             stringsAsFactors = F,
             skip = 1) %>% 
  as_tibble() %>% 
  mutate(date = as.POSIXct(date, format="%m/%d/%Y", tz="MST")) %>% 
  select(1:length(column.names))

colnames(for.dat) <- column.names

for.dat <-
  for.dat %>% 
  # remove columns containing first and second obs
  select(-first_obs, -second_obs, -was_it_raining) %>% 
  
  group_by(date,observer,colony_ID,time) %>% 
  mutate(
    tot_in = sum(
      in_1,in_2,in_3,in_4,in_5,in_6,in_7,in_8, 
      na.rm = T
    ),
    tot_out = sum(
      out_1,out_2,out_3,out_4,out_5,out_6,out_7,out_8, 
      na.rm=T
    ),
    foraging_30s = tot_in + tot_out
  ) %>% 
  ungroup() %>% 
  mutate(
    colony_ID = as.factor(colony_ID),
    observer = as.factor(observer)
  ) %>% 
  pivot_longer(.,
               cols = in_1:out_8,
               names_to = "inout",
               values_to = "n_ants") %>% 
  
  group_by(inout) %>% 
  mutate(inout2 = str_split_fixed(inout, "_", 2)[1]) %>%
  ungroup() %>% 
  
  select(date,
         colonyID = colony_ID,
         foraging_30s) %>% 
  distinct()

## add prop_foraging data
tidy.for.2022 <-
  for.dat %>% 
  select(date, 
         colonyID,
         foraging_30s) %>% 
  distinct() %>% 
  mutate(date = as.Date(date, format="%m/%d/%y")) %>%
  mutate(colonyID = as.factor(colonyID)) %>% 
  # doing something, notice
  group_by(colonyID) %>% 
  summarise(max_foraging_30s = max(foraging_30s, na.rm = T)) %>% 
  ungroup() %>% 
  right_join(for.dat, by="colonyID") %>% 
  mutate(prop_foraging_30s = foraging_30s/max_foraging_30s) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(date = as.Date(date))


for.tidydat.2022 <-
  tidydat.2022.sub %>% 
  mutate(
    colonyID = as.factor(colonyID),
    obs_day = parse_number(as.character(counted_when))
  ) %>%
  left_join(
    tidy.for.2022, 
    by=c("colonyID", "date")
  ) %>% 
  select(
    colonyID:bead_color, 
    obs_day, 
    max_foraging_30s, 
    foraging_30s, 
    prop_foraging_30s
  ) %>% 
  right_join(
    tidydat.2022.sub, 
    by=c("colonyID", "date", "bead_color")
  ) %>% 
  group_by(colonyID,bead_color) %>% 
  mutate(obs_day = paste0("D",obs_day[1])) %>% 
  ungroup() %>% 
  select(
    colonyID,
    date,
    bead_color,
    age_obs,
    age_class, 
    beads_collected,
    max_foraging_30s,
    foraging_30s,
    prop_foraging_30s,
    counted_when, 
    cum_prop
  )

# for.tidydat.2022 |> View()

# Combine foraging data ---------------------------------------------------

which.colonies <- 
  tidydat %>% 
  select(colonyID, age_obs) %>% 
  distinct() %>% 
  arrange(age_obs, colonyID)

s.tidydat <-
  rbind(for.tidydat.2021, for.tidydat.2022) %>% 
  ## 3 age-classes
  mutate(
    age_class = ifelse(
      age_obs <= 2, 
      "1-2_yr",
      ifelse(
        age_obs > 2 & age_obs <= 4, 
        "3-4_yr",
        "5+_yr"
      )
    )
  ) %>%
  mutate(
    age_class = factor(age_class, 
                       levels = c("1-2_yr", "3-4_yr","5+_yr")),
    colonyID = factor(colonyID, 
                      levels = unique(which.colonies$colonyID))
  )


# Clean combined foraging data --------------------------------------------

## Remove colony-rep with prop > 1
m <- s.tidydat %>% 
  filter(cum_prop>1) %>% 
  select(
    colonyID,
    bead_color
  ) %>% 
  distinct() %>% 
  as.matrix()

foo <-
  s.tidydat %>% 
  # remove colony-trials that have a cumulative proportion above 1
  filter(
    !(colonyID%in%m[1,1] & 
        bead_color%in%m[1,2])
  ) %>% 
  filter(
    !(colonyID%in%m[2,1] & 
        bead_color%in%m[2,2])
  ) %>% 
  select(
    rep = bead_color, 
    everything()
  ) %>%
  distinct() %>% 
  # format year
  group_by(rep) %>% 
  mutate(
    yr = str_split(rep,"-")[[1]][1] %>% parse_number(.),
    yr = as.numeric(ifelse(yr %in% c(1:3), "2021", "2022")),
    yr = factor(yr, levels = c("2021","2022"))
  ) %>% 
  ungroup() %>% 
  select(
    yr, 
    date, 
    rep, 
    colonyID, 
    beads_collected, 
    counted_when, 
    cum_prop,
    age_obs, 
    age_class,
    max_foraging_30s,
    foraging_30s
  )


# EXCLUDE COLONIES --------------------------------------------------

not.in.manuscript <- c(
  # not enough samples
  "961",
  "969",
  "1423",
  "1474",
  "1524",
  "1317",
  "1533",
  "1546",
  # multiple entries on same day
  "1536"
)

# RENAME COLONIES ---------------------------------------------------------

# old names
all.colony.names <- 
  foo %>% 
  select(colonyID) %>% 
  pull() %>% 
  as.character() %>% 
  unique()

# colonies used in mansuscript
colonies.in.ms <- 
  setdiff(
    all.colony.names,
    not.in.manuscript
  )

# new names
new.colony.names <- paste0(
  "C-",
  1:length(colonies.in.ms)
)

colony.name.register <- data.frame(
  old_name = colonies.in.ms, 
  new_name = new.colony.names
)

## save the file for easy call
saveRDS(
  colony.name.register,
  paste0(
    path_to_repo, 
    "/results/raw_data/",
    "colony_name_register.RDS"
  )
)

## save as csv
write.csv(
  colony.name.register,
  paste0(
    path_to_repo, 
    "/results/raw_data/",
    "colony_name_register.csv"
  )
)


# Get data used in the manuscript ---------------------------------------------

s.tidydat.mod <- 
  foo %>%
  filter(
    colonyID %in% colonies.in.ms
  ) |> 
  left_join(
    colony.name.register, 
    by=c("colonyID"="old_name")
  ) %>% 
  select(
    yr:rep, 
    old_colonyID = colonyID, 
    colonyID = new_name,
    everything()
  ) %>% 
  mutate(
    colonyID = as.factor(colonyID)
  )


# Save data for further analyses ------------------------------------------

saveRDS(
  s.tidydat.mod,
  paste0(
    path_to_repo,
    "/results/raw_data/",
    "data_for_manuscript.RDS"
  )
)


# Save data for submission ------------------------------------------------

s.tidydat.mod |>
  select(
    year                    = yr,
    date,
    replicate_ID            = rep,
    real_colony_ID          = old_colonyID,
    colony_ID               = colonyID,
    age_at_obs              = age_obs,
    age_class,
    daytime_of_obs          = counted_when,
    beads_collected,
    cum_prop_beads_removed  = cum_prop,
    foraging_rate           = foraging_30s,
    max_foraging_rate       = max_foraging_30s
  ) |> 
  mutate(
    daytime_of_obs = as.character(daytime_of_obs),
    daytime_of_obs = case_when(
      daytime_of_obs == "D1_bN" ~ "Day 1; around 11 am",
      daytime_of_obs == "D2_M" ~ "Day 2; around 8 am",
      daytime_of_obs == "D2_bN" ~ "Day 2; around 11 am",
      .default = NA
    )
  ) |>
  write.csv(
    paste0(
      path_to_repo,
      "/manuscript/supp_files/raw_data/",
      # "raw_data_bead_returns_foraging_rate_2021_2022.csv"
      "bead_experiment_data_2021_2022.csv"
    ),
    row.names = F
  )

