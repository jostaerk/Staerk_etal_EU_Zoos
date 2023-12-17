# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: Decision framework for EU species
# Author: Johanna Staerk
# Description: Initial data cleaning of ZIMS, AZE, & Protected areas

# Note: this script requires data that is publicly available or can be obtained
# upon request (see readme file for data sources). This script is only published
# for transparency and cannot directly be reproduced, unless you obtain the raw
# data files.


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INIT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list = ls())
dev.off()
cat('\014')

# Set working directory
setwd("/Users/johanna_staerk/Dropbox/PROJECTS/EU.Zoos/EU/ANALYSIS/DATA/")
library(tidyverse)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ZIMS DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# ZIMS species holdings can be obtained from Species360 at their data sharing
# site:
# https://conservation.species360.org/data-sharing/.

# read raw ZIMS data (TAG export, 28 Januar 2020)
zims <- read.csv("1_raw/ZOOS/SPECIES360_2021/Species360Data00.csv")

# Exlude records with zero animals
zims <- zims[which(zims$Total.Count > 0), ]

# Only keep taxonomic ranks at species level or below
zims <- zims[which(zims$Rank %in%
                     c("Breed", "Domestic", "Species",
                       "Subspecies", "Variety")),]

# Convert subpecies to species level data
zims <-
  zims %>% separate(
    col = Scientific.Name,
    into = c("Genus", "Epithet"),
    sep = " ",
    remove = F
  )
zims <- zims %>%
  unite(col = "Species", Genus, Epithet, sep = " ")

# Fix obsolete species names
id1 <- grep("OBSOLETE: Use", zims$Common.Name)
a <-
  substr(zims[id1, "Common.Name"], str_locate(zims[id1, "Common.Name"], "Use") +
           4, nchar(zims[id1, "Common.Name"]))
a <- gsub("/", " ", a)
a <-
  a %>% as.data.frame() %>% separate(
    col = ".",
    into = c("Genus", "Epithet"),
    sep = " ",
    remove = T
  ) %>%
  unite(col = "Species", Genus, Epithet, sep = " ")
zims[id1, "Species"] <- a

id2 <- grep("OBSOLETE: use", zims$Common.Name)
aa <-
  substr(zims[id2, "Common.Name"], str_locate(zims[id2, "Common.Name"], "use") +
           4, nchar(zims[id2, "Common.Name"]))
aa <-
  aa %>% as.data.frame() %>% separate(
    col = ".",
    into = c("Genus", "Epithet"),
    sep = " ",
    remove = T
  ) %>%
  unite(col = "Species", Genus, Epithet, sep = " ")
aa$Species <- gsub(",", "", aa$Species)
zims[id2, "Species"] <- aa$Species

# Subset for EU country zoos

# EU countries
EU <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Czech Republic",
  "Cyprus",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Isle Of Man",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Netherlands",
  "Poland",
  "Portugal",
  "Slovakia",
  "Spain",
  "Sweden",
  "United Kingdom"
)

# Add column to indicate if zoo is in EU
zims$EU.zoo <- ifelse((zims$Country %in% EU), 1, 0)
zims <- zims %>% select(Species, everything())

# Save data for taxonomy check
#write.csv(zims, "2_cleanforcolcheck/Species360Data00.csv", row.names = F)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# AZE DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Species data from the Alliance of Zero Extinction was obtained upon request,
# see: https://zeroextinction.org/contact/

aze <-
  read.csv(
    "1_raw/AZE/Update2019/AZE_species_full_list_cleaned.csv",
    stringsAsFactors = F,
    na.strings = c("", " ", "NA"),
    header = T,
    sep = ";"
  )

# wide to long format
aze2 <-
  aze %>%
  pivot_longer(AZE.trigger.species:AZE.trigger.species20,
               names_to = "AZE.Trigger.Species") %>%
  select(Country, value) %>%
  rename(AZE.Trigger.Species = value) %>%
  subset(!(is.na(AZE.Trigger.Species)))

# Trim whitespace, remove double spaces
aze2$AZE.Trigger.Species <-
  str_trim(aze2$AZE.Trigger.Species, side = "both")
aze2$Country <- str_trim(aze2$Country, side = "both")
aze2$Country <- gsub("\\s+", ' ', aze2$Country)
aze2$AZE.Trigger.Species <-
  gsub("\\s+", ' ', aze2$AZE.Trigger.Species)

# Save data for taxonomy check
# write.csv(aze2, "2_cleanforcolcheck/AZE_species_full_list_cleaned02.csv",
#           row.names = F)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# PROTECTED AREA DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# Read in PA data after taxonomic check (GBIF)
PaData <-
  read.csv("4_alldataclean/PADataColCheck00_GBIF_checked.csv",
           stringsAsFactors = F)

# if species get merged due to taxonomy we use the maximum value of the two species
PaData <-
  PaData %>% group_by(gbif.species) %>% summarise(
    perc.target.met.in.PAs.and.N2K = max(perc.target.met.in.PAs.and.N2K, na.rm = T),
    Representation.target.perc = max(Representation.target.perc, na.rm = T),
    Area.in.EU28.km2 = max(Area.in.EU28.km2, na.rm = T),
    db.subspecies = paste(db.subspecies, collapse = ','),
    db.species.merged = paste(db.species, collapse = ', ')
  )

# Calculated the target area in km2 by multiplying the area that is occupied
# with the representation target (see Maiorano et al. or definiton os
# representation target)
PaData$TargetAreakm2    <-
  PaData$Area.in.EU28.km2 * (PaData$Representation.target.perc / 100)

# Calculated the area in km2 that is currently protected (PA and Natura20000) by
# multiplying the % of target met in protected areas and Natura2000  with the
# target area in km2
PaData$ProtectedAreakm2 <-
  PaData$TargetAreakm2 * (PaData$perc.target.met.in.PAs.and.N2K / 100)

# Calculated the gap area (area missing to fulfill the target for PA and N2000)
# by takin difference betweem Target area and currently protected area
PaData$Gap.km2       <-
  PaData$TargetAreakm2 - PaData$ProtectedAreakm2

# Replace negative values with 0
PaData$Gap.km2 <- ifelse(PaData$Gap.km2 < 0, 0, PaData$Gap.km2)

# Save data
write.csv(
  PaData,
  "ANALYSIS/DATA/4_alldataclean/PADataColCheck00_GBIF_checked_cleaned.csv",
  row.names = F
)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# EUROPEAN RL DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Read European Red List assessment data (Taxonomy checked according to GBIF)
Iucn <-
  read.csv(
    "4_alldataclean/IucnEUTetrapods_GBIF_checked_cleaned.csv",
    # European RL
    na.strings = c("", " ", "NA"),
    stringsAsFactors = F,
    sep = ","
  )

# Exclude species not assessed according to EU assessment
table(Iucn$RL.catEU27)
Iucn <- Iucn %>% filter(RL.catEU27 %in% c("CR", "CR (PE)", "DD", "EN", "EX", "LC", "NT", "RE", "VU"))


# Clean endemism columns
table(Iucn$endemic.europe)
Iucn <- Iucn %>%
  mutate(endemic.europe = case_when(endemic.europe == 'N' ~ NA,
                                    endemic.europe == 'Y' ~ 'Yes',
                                    .default = as.character(endemic.europe)))
table(Iucn$endemic.eu)
Iucn[which(Iucn$endemic.eu == "No"), "endemic.eu"] <- NA

# fix taxonomy
Iucn[which(Iucn$gbif.species == "Carduelis chloris"), "gbif.species"] <- "Chloris chloris"
Iucn$EUSpecies <- 1

# add common names
common <- read.csv("4_alldataclean/Common_names.csv")
common <- common %>% select(db.species, common.name)
Iucn <- Iucn %>% left_join(common, by = "db.species")

# Excluded species (hybrids, Cetaceans, and extinct or possibly extinct species)
Iucn <- Iucn %>%
  mutate(
    excluded = case_when(
      RL.catEU27 %in% c("CR (PE)", "EX") ~ "extinct/possibly extinct",
      db.species %in% c(
        "Pelophylax esculentus",
        "Pelophylax grafi",
        "Pelophylax hispanicus"
      ) ~ "hybrid species",
      order == "Cetacea" ~ "cetacean",
      .default = NA
    )
  )

# Check for duplicates due to taxonomic standardization
Iucn[which(duplicated(Iucn$gbif.species)),]
Iucn[which(Iucn$db.species == "Psammodromus manuelae"), "excluded"] <-
  "taxonomic duplicate"

# Add columns "excluded" and "exclusion.reason"
Iucn$exclusion.reason <- Iucn$excluded
Iucn[which(!(is.na(Iucn$excluded))), "excluded"] <- 1
Iucn[which((is.na(Iucn$excluded))), "excluded"] <- 0

# Save cleaned data
write.csv(Iucn,
          "4_alldataclean/IucnEUTetrapods_GBIF_checked_cleaned.csv",
          row.names = F)
