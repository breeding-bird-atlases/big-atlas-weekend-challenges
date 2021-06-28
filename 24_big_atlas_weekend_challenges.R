# 20210610
# Big Atlas Weekend Challenge Winners

# eBird provides a snapshot of atlas data at the start of the event, including
# a list of current atlasers. Grouped checklists should not be merged.

# Projects involved in this event include Maine, New York, MD-DC, 
# North Carolina.

library(dplyr)
library(lubridate)
library(here)

# year of event
year <- 2021

# data from eBird from event
baw <- read.csv(here("data", "ebird", "2_standardized",
                       paste0("big-atlas-weekend_", year, ".csv")))

baw_checklists <- read.csv(here("data", "ebird", "2_standardized",
                                paste0("big-atlas-weekend_checklists_", 
                                       year, ".csv")))

# pre-event data from eBird
prebaw <- read.csv(here("data", "ebird", "2_standardized",
                          paste0("pre-big-atlas-weekend_", year, ".csv")))

# pre-event atlasers from eBird
# needs to have name, email, user_id, project_code columns
atlasers <- read.csv(here("data", "ebird", "2_standardized",
                          paste0("big-atlas-weekend_", year,
                                 "_atlasers.csv")))

# list of all priority blocks from each region
priority_blocks <- read.csv(here("data", "ebird", "2_standardized",
                                 paste0("big-atlas-weekend_", year,
                                        "_priority-blocks.csv")))

# list of grassland birds (taken from NY's list: 
# https://drive.google.com/file/d/1y4F8MUsdFf4JwJdUQ7A6NK1uWyb3IvWR/view)
grassland_birds <- c("Upland Sandpiper",
                     "Northern Bobwhite",
                     "Northern Harrier",
                     "American Kestrel",
                     "Barn Owl",
                     "Short-eared Owl",
                     "Horned Lark",
                     "Sedge Wren",
                     "Eastern Bluebird",
                     "Loggerhead Shrike",
                     "Clay-colored Sparrow",
                     "Vesper Sparrow",
                     "Savannah Sparrow",
                     "Grasshopper Sparrow",
                     "Henslow’s Sparrow",
                     "Dickcissel",
                     "Bobolink",
                     "Eastern Meadowlark")

priority_birds <- read.csv(here("data", "bba3", "priority_species.csv"))
priority_birds <- priority_birds$common_name

# project_codes
me <- "EBIRD_ATL_ME"
ny <- "EBIRD_ATL_NY"
md <- "EBIRD_ATL_MD_DC"
nc <- "EBIRD_ATL_NC"

# Data Prep -------------------------------------------------------------------

# Convert internal breeding codes to public codes
int_codes <- read.csv(here("data", "ebird", "0_metadata", 
                           "ebird_internal_codes.csv"))

baw <- left_join(baw, int_codes, by = c("breeding_code" = "INTERNAL"))

# edit the block codes to match partial blocks in ebird, which start with "o"
priority_blocks$atlas_block <- gsub("o", "", priority_blocks$atlas_block)
prebaw$block <- gsub("o", "", prebaw$block)
baw$block <- gsub("o", "", baw$block)

# NY has a few duplicate blocks so remove those
priority_blocks <- distinct(priority_blocks)

# Add all blocks into pre-event dataset so that 0-hour blocks are included
prebaw <- full_join(prebaw, priority_blocks, 
                    by = c("block" = "atlas_block")) %>%
  distinct()

# Add pre-event dataset to event dataset
baw <- full_join(baw, prebaw[, c("block", "block_effort", "priority")], 
                 by = "block") %>%
  distinct()

# add zero-obs checklist effort to event dataset
baw <- full_join(baw, baw_checklists) %>%
  distinct()

# Find new atlasers
atlasers$first_list <- as_datetime(atlasers$first_list, 
                                   format = "%m/%d/%Y %H:%M")
atlasers$latest_list <- as_datetime(atlasers$latest_list,
                                    format = "%m/%d/%Y %H:%M")
new_atlasers <- filter(atlasers, 
                       first_list > as_datetime("2021-06-25 17:59:59") &
                         latest_list < as_datetime("2021-06-28 0:00:00"))

# State Challenges ------------------------------------------------------------

# o Nocturnal Hours -----------------------------------------------------------
## Complete nocturnal checklists with duration
baw_noc <- baw[! duplicated(baw$sub_id), ] %>%
  group_by(user_id, proj_id) %>%
  filter((nocturnal == 1 & 
           proj_id %in% c(ny, md, nc) &
           priority == "yes" &
           all_obs_reported == 1) |
           (nocturnal == 1 &
              proj_id %in% me &
              priority == "yes"))

winners <- baw_noc %>%
  group_by(proj_id) %>%
  slice_sample(n = 1) %>%
  mutate(contest = "Nocturnal Hours")

top_ten <- baw_noc %>%
  summarize(challenge_value = sum(duration_hrs, na.rm = TRUE)) %>%
  group_by(proj_id) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Nocturnal Hours")

# o Confirmed Codes -----------------------------------------------------------
## Complete checklists that have at least one species with confirmed code
baw_c4 <- baw %>%
  group_by(user_id, proj_id) %>%
  filter((category_code == "C4" &
            proj_id %in% c(ny, md, nc) &
           priority == "yes" &
           all_obs_reported == 1) |
           (proj_id %in% me &
              category_code == "C4" &
              priority == "yes"))

top_ten <- baw_c4 %>%
  summarize(challenge_value = length(category_code)) %>%
  group_by(proj_id) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Confirmed Codes") %>%
  bind_rows(top_ten)

baw_c4 <- baw_c4[! duplicated(baw_c4$sub_id), ]

winners <- baw_c4 %>%
  group_by(proj_id) %>%
  slice_sample(n = 1) %>%
  mutate(contest = "Confirmed Codes") %>%
  bind_rows(winners)

# o Confirmed Grassland/Priority Birds ----------------------------------------
## Complete checklists with confirmed grassland/priority species
## New York, Maine, and Maryland-DC only
baw_grass <- baw %>%
  group_by(user_id, proj_id) %>%
  filter((primary_com_name %in% grassland_birds &
           proj_id %in% ny &
           category_code == "C4" &
           all_obs_reported == 1) |
           (primary_com_name %in% grassland_birds &
              proj_id %in% me &
              category_code == "C4") |
           (primary_com_name %in% priority_birds &
              proj_id %in% md &
              category_code == "C4" &
              all_obs_reported == 1))

top_ten <- baw_grass %>%
  summarize(challenge_value = length(category_code)) %>%
  group_by(proj_id) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Priority Species") %>%
  bind_rows(top_ten)

baw_grass <- baw_grass[! duplicated(baw_grass$sub_id), ]

winners <- baw_grass %>% 
  group_by(proj_id) %>%
  slice_sample(n = 1) %>%
  mutate(contest = "Priority Species") %>%
  bind_rows(winners)

# o Checklists in <20 hr Blocks -----------------------------------------------
## Complete checklists longer than five minutes from blocks with <20 hrs of 
## diurnal effort.
baw_20 <- baw[! duplicated(baw$sub_id), ] %>%
  group_by(user_id, proj_id) %>%
  filter((block_effort <= 20 &
           duration_hrs >= 0.83 &
           proj_id %in% c(ny, md, nc) &
           priority == "yes" &
           all_obs_reported == 1) | 
           (block_effort <= 20 &
              duration_hrs >= 0.83 &
              proj_id %in% me &
              priority == "yes")) 

winners <- baw_20 %>%
  group_by(proj_id) %>%
  slice_sample(n = 1) %>%
  mutate(contest = "Low Effort Blocks") %>%
  bind_rows(winners)

top_ten <- baw_20 %>%
  summarize(challenge_value = length(sub_id)) %>%
  group_by(proj_id) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Low Effort Blocks") %>%
  bind_rows(top_ten)

# o Checklists in 0 hr Blocks -------------------------------------------------
## Complete checklists longer than five minutes from blocks that had 0 hours of
## diurnal effort.
baw_0 <- baw[! duplicated(baw$sub_id), ] %>%
  group_by(user_id, proj_id) %>%
  filter((is.na(block_effort) &
           duration_hrs >= 0.83 &
           priority == "yes" &
           proj_id %in% c(ny, md, nc) &
           all_obs_reported == 1) |
           (is.na(block_effort) &
              duration_hrs >= 0.83 &
              priority == "yes" &
              proj_id %in% me)) 

winners <- baw_0 %>%
  group_by(proj_id) %>%
  slice_sample(n = 1) %>%
  mutate(contest = "Zero Hr Blocks") %>%
  bind_rows(winners)

top_ten <- baw_0 %>%
  summarize(challenge_value = length(sub_id)) %>%
  group_by(proj_id) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Zero Hr Blocks") %>%
  bind_rows(top_ten)

# o Atlas Checklists ----------------------------------------------------------
## Complete checklist with at least one coded species or nocturnal
baw_atlas <- baw %>%
  filter(category_code %in% c("C2", "C3", "C4") | nocturnal == 1)

baw_atlas <- baw_atlas[! duplicated(baw_atlas$sub_id), ] %>%
  group_by(user_id, proj_id) %>%
  filter((all_obs_reported == 1 &
            proj_id %in% c(ny, md, nc)) |
           (proj_id %in% me)) 

winners <- baw_atlas %>%
  group_by(proj_id) %>%
  slice_sample(n = 1) %>%
  mutate(contest = "Atlas Checklists") %>%
  bind_rows(winners)

top_ten <- baw_atlas %>%
  summarize(challenge_value = length(sub_id)) %>%
  group_by(proj_id) %>%
  slice_max(challenge_value, n = 10) %>%
  mutate(contest = "Atlas Checklists") %>%
  bind_rows(top_ten)

# o First-time Checklists -----------------------------------------------------
## Any atlaser who contributes their first checklist
baw_first <- baw %>% filter(user_id %in% new_atlasers$user_id)

winners <- baw_first %>%
  group_by(proj_id) %>%
  slice_sample(n = 1) %>%
  mutate(contest = "New Atlaser") %>%
  bind_rows(winners)

write.csv(winners, here("output", paste0("big-atlas-weekend_winners_",
                                         year, ".csv")),
          row.names = FALSE)

write.csv(top_ten, here("output", paste0("big-atlas-weekend_summary_",
                                         year, ".csv")),
          row.names = FALSE)

# Inter-state Challenges ------------------------------------------------------

# o Nocturnal Hours -----------------------------------------------------------
state_winner <- baw[! duplicated(baw$sub_id), ] %>%
  filter(nocturnal == 1 & 
           priority == "yes" & 
           all_obs_reported == 1) %>%
  group_by(proj_id) %>%
  summarize(challenge_value = (sum(duration_hrs, na.rm = TRUE)) / 
              (length(unique(user_id)))) %>%
  mutate(contest = "Nocturnal Hours")

# o Confirmed Codes -----------------------------------------------------------
num_priority_blocks <- priority_blocks %>% 
  filter(priority == "yes") %>% 
  group_by(state) %>% 
  summarize(num_blocks = length(unique(atlas_block)))

state_winner <- baw %>%
  filter(category_code == "C4" & 
           priority == "yes" & 
           all_obs_reported == 1) %>%
  mutate(num_blocks = case_when(
    proj_id == me ~ 
      num_priority_blocks$num_blocks[num_priority_blocks$state == 
                                       "Maine"],
    proj_id == ny ~
      num_priority_blocks$num_blocks[num_priority_blocks$state == 
                                       "New York"],
    proj_id == md ~
      num_priority_blocks$num_blocks[num_priority_blocks$state == 
                                       "Maryland-DC"],
    proj_id == nc ~
      num_priority_blocks$num_blocks[num_priority_blocks$state == 
                                       "North Carolina"],
    TRUE ~ NA_integer_
  )) %>%
  group_by(proj_id) %>%
  summarize(challenge_value = length(category_code) / unique(num_blocks)) %>%
  mutate(contest = "Confirmed Codes") %>%
  bind_rows(state_winner)

# o Atlas Checklists ----------------------------------------------------------
baw_atlas_state <- baw %>%
  filter((category_code %in% c("C2", "C3", "C4") | nocturnal == 1) &
          all_obs_reported == 1) 

state_winner <- baw_atlas_state[! duplicated(baw_atlas_state$sub_id), ] %>%
  group_by(proj_id) %>%
  summarize(challenge_value = length(sub_id) / length(unique(user_id))) %>%
  mutate(contest = "Atlas Checklists") %>%
  bind_rows(state_winner)

# o Calculate Winner
## First = 23 points, second = 13, third = 7, fourth = 0
state_winner <- state_winner %>%
  arrange(contest, desc(challenge_value)) %>%
  group_by(contest) %>%
  mutate(ranking = rank(-challenge_value, ties.method = "min")) %>%
  mutate(points = case_when(
    ranking == 1 ~ 23,
    ranking == 2 ~ 13,
    ranking == 3 ~ 7,
    TRUE ~ 0
  )) %>%
  group_by(proj_id) %>%
  summarize(winner = max(sum(points))) %>%
  arrange(desc(winner))

write.csv(state_winner, here("output", paste0("big-atlas-weekend_state-", 
                                              "winner_", year, ".csv")),
          row.names = FALSE)
