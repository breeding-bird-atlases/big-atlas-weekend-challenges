# 20210610
# Big Atlas Weekend Challenge Winners

# eBird provides a snapshot of atlas data at the start of the event, including
# a list of current atlasers. Grouped checklists should not be merged.

# Projects involved in this event include Maine, New York, MD-DC, 
# North Carolina.

library(dplyr)
library(lubridate)
library(suncalc)
library(here)

# year of event
year <- 2021

# data from eBird from event
baw <- read.csv(here("data", "ebird", "2_standardized",
                       paste0("big-atlas-weekend_", year, ".csv")))

# pre-event data from eBird
prebaw <- read.csv(here("data", "ebird", "2_standardized",
                          paste0("pre-big-atlas-weekend_", year, ".csv")))

# pre-event atlasers from eBird
# needs to have name, email, user_id, project_code columns
atlasers <- read.csv(here("data", "ebird", "2_standardized",
                          paste0("big-atlas-weekend_", year,
                                 "atlasers.csv")))

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

# project_codes
me <- "EBIRD_ATL_ME"
ny <- "EBIRD_ATL_NY"
md <- "EBIRD_ATL_MD_DC"
nc <- "EBIRD_ATL_NC"

# applicable protocols
protocols <- c("Traveling", "Stationary", "Area", "Random")

# Data Prep -------------------------------------------------------------------

# Calculate pre-event block effort
# get rid of non-coded observations 
prebaw_di <- prebaw %>% 
  filter(breeding_bird_atlas_category %in% c("C2", "C3", "C4"))

# get unique coded checklists
prebaw_di <- prebaw_di[! duplicated(prebaw_di$checklist_id), ]

# remove incomplete and nocturnal checklists
prebaw_di <- prebaw_di %>% 
  filter(all_species_reported == TRUE &
           nocturnal == "diurnal")

# Sum block effort
prebaw_di <- prebaw_di %>% 
  group_by(atlas_block) %>%
  mutate(block_effort = sum(duration_minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  select(atlas_block, block_effort)

prebaw_di <- prebaw_di[! duplicated(prebaw_di$atlas_block), ]

prebaw_di <- full_join(prebaw_di, priority_blocks, by = "atlas_block")

baw <- full_join(baw, prebaw_di, by = "atlas_block")

# filter out non-portal checklists and inappropriate protocols
baw <- baw %>%
  filter(project_code %in% c(me, ny, md, nc) | 
           protocol_type %in% protocols) %>%
  select("checklist_id",
         "global_unique_identifier",
         "common_name",
         "project_code",
         "breeding_bird_atlas_code",
         "breeding_bird_atlas_category",
         "latitude",
         "longitude",
         "observation_date",
         "time_observations_started",
         "observer_id",
         "duration_minutes",
         "all_species_reported",
         "atlas_block",
         "priority",
         "state",
         "block_effort")

# label nocturnal checklists
# nocturnal = 20 min after sunset, 40 min before sunrise
baw$observation_datetime <- as_datetime(paste(baw$observation_date, 
                                              baw$time_observations_started), 
                                        tz = "EST")

baw[, (length(baw) + 1):(length(baw) + 7)] <-   
  getSunlightTimes(data = data.frame(date = as.Date(baw$observation_date),
                                     lat = baw$latitude,
                                     lon = baw$longitude), 
                   keep = c("sunrise", "nauticalDawn", 
                            "sunset", "nauticalDusk"), 
                   tz = "EST")

baw[, c("date", "lat", "lon")] <- NULL

# getSunlightTimes() doesn't account for daylight savings time
## compensate for this
### DST starts March 14 2021 02:00:00
### DST ends November 07 2021 02:00:00
dst <- as_datetime("2021-03-14 02:00:00", tz = "EST") %--% 
  as_datetime("2021-11-07 02:00:00", tz = "EST")

indx <- which(baw$observation_datetime %within% dst)
baw[indx, c("sunrise")] <- baw[indx, c("sunrise")] + hours(1)
baw[indx, c("nauticalDawn")] <- baw[indx, c("nauticalDawn")] + hours(1)
baw[indx, c("sunset")] <- baw[indx, c("sunset")] + hours(1)
baw[indx, c("nauticalDusk")] <- baw[indx, c("nauticalDusk")] + hours(1)

rm(indx)

# add columns for what ebird considers nocturnal
baw$ebird_dawn <- baw$sunrise - hms::as_hms(40*60)
baw$ebird_dusk <- baw$sunset + hms::as_hms(20*60)

baw <- baw %>% 
  rename(nautical_dawn = nauticalDawn, nautical_dusk = nauticalDusk) %>%
  mutate(nocturnal = if_else(observation_datetime <= ebird_dawn | 
                               observation_datetime >= ebird_dusk, 
                             "nocturnal", "diurnal"))

# change the checklist's designation to diurnal if it extends past dawn
notna <-which(!is.na(baw$duration_minutes))
indx <- which(baw$nocturnal[notna] == "nocturnal" &
                ms(paste(baw$duration_minutes[notna], 0)) > 
                abs(as_datetime(baw$ebird_dawn[notna]) -
                      as_datetime(baw$observation_datetime[notna])))

bba3[indx, "nocturnal"] <- "diurnal"

rm(indx, notna)

# State Challenges ------------------------------------------------------------

# o Nocturnal Hours -----------------------------------------------------------
## Complete nocturnal checklists with duration
baw_noc <- baw[! duplicated(baw$checklist_id), ]

baw_noc <- baw_noc %>%
  group_by(observer_id, project_code) %>%
  filter(nocturnal == "nocturnal" & 
           priority == "yes" &
           all_species_reported == TRUE) %>%
  summarize(duration = sum(duration_minutes, na.rm = TRUE)) %>%
  group_by(project_code) %>%
  slice_max(duration)

# o Confirmed Codes -----------------------------------------------------------
## Complete checklists that have at least one species with confirmed code
baw_c4 <- baw %>%
  group_by(observer_id, project_code) %>%
  filter(breeding_bird_atlas_category == "C4" &
           priority == "yes" &
           all_species_reported == TRUE) %>%
  summarize(confirmations = length(breeding_bird_atlas_category)) %>%
  group_by(project_code) %>%
  slice_max(confirmations)

# o Confirmed Grassland Birds -------------------------------------------------
## Complete checklists with confirmed grassland species
baw_grass <- baw %>%
  group_by(observer_id, project_code) %>%
  filter(common_name %in% grassland_birds &
           breeding_bird_atlas_category == "C4" &
           all_species_reported == TRUE) %>%
  summarize(confirmations = length(breeding_bird_atlas_category)) %>%
  group_by(project_code) %>%
  slice_max(confirmations)

# o Checklists in <20 hr Blocks -----------------------------------------------
## Complete checklists longer than five minutes from blocks with <20 hrs of 
## diurnal effort.
baw_20 <- baw[! duplicated(baw$checklist_id), ]

baw_20 <- baw_20 %>%
  group_by(observer_id, project_code) %>%
  filter(block_effort <= 1200 &
           duration_minutes >= 5 &
           priority == "yes" &
           all_species_reported == TRUE) %>%
  summarize(checklists = length(checklist_id)) %>%
  group_by(project_code) %>%
  slice_max(checklists)

# o Checklists in 0 hr Blocks -------------------------------------------------
## Complete checklists longer than five minutes from blocks that had 0 hours of
## diurnal effort.
baw_0 <- baw[! duplicated(baw$checklist_id), ]

baw_0 <- baw_0 %>%
  group_by(observer_id, project_code) %>%
  filter(is.na(block_effort) &
           duration_minutes >= 5 &
           priority == "yes" &
           all_species_reported == TRUE) %>%
  summarize(checklists = length(checklist_id)) %>%
  group_by(project_code) %>%
  slice_max(checklists)

# o Atlas Checklists ----------------------------------------------------------
## Complete checklist with at least one coded species or nocturnal
baw_atlas <- baw %>%
  filter(breeding_bird_atlas_category %in% c("C2", "C3", "C4") | 
           nocturnal == "nocturnal")

baw_atlas <- baw_atlas[! duplicated(baw_atlas$checklist_id), ]

baw_atlas <- baw_atlas %>%
  group_by(observer_id, project_code) %>%
  filter(all_species_reported == TRUE) %>%
  summarize(checklists = length(checklist_id)) %>%
  group_by(project_code) %>%
  slice_max(checklists)

# o First-time Checklists -----------------------------------------------------
## Any atlaser who contributes their first checklist
baw_first <- anti_join(baw, atlasers, by = c("observer_id" = "user_id"))

baw_first <- baw_first %>%
  group_by(project_code) %>%
  slice_sample(n = 1)

# Inter-state Individual Challenges -------------------------------------------

# o Nocturnal Hours -----------------------------------------------------------
baw_noc_all <- baw_noc %>% 
  ungroup() %>% 
  slice_max(duration)

# o Confirmed Codes -----------------------------------------------------------
baw_c4_all <- baw_c4 %>%
  ungroup() %>%
  slice_max(confirmations)

# o Confirmed Grassland Birds -------------------------------------------------
baw_grass_all <- baw_grass %>%
  ungroup() %>%
  slice_max(confirmations)

# o Checklists in <20 hr Blocks -----------------------------------------------
baw_20_all <- baw_20 %>%
  ungroup() %>%
  slice_max(checklists)

# o Checklists in 0 hr Blocks -------------------------------------------------
baw_0_all <- baw_0 %>%
  ungroup() %>%
  slice_max(checklists)

# o Atlas Checklists ----------------------------------------------------------
baw_atlas_all <- baw_atlas %>%
  ungroup() %>%
  slice_max(checklists)

# o First-time Checklists -----------------------------------------------------
baw_first_all <- baw_first %>%
  ungroup() %>%
  slice_sample(n = 1)

# Inter-state Challenges ------------------------------------------------------