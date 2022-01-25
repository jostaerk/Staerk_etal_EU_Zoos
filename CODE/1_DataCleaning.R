# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: Decision framework for EU species
# Author: Johanna Staerk
# Description: Data cleaning of ZIMS, AZE, & Protected areas
# Note: this script requires data that is publicly available or can be obtained
# upon request
# It is only published for transparency and cannot be directly be reproduced.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INIT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list = ls())
dev.off()
cat('\014')

# Point to data folder
setwd("/Users/johannas/Dropbox/PROJECTS/EU.Zoos/EU/ANALYSIS/DATA/")

if (!"tidyverse" %in% installed.packages())
  install.packages("tidyverse")
suppressWarnings(library(tidyverse))
if (!"stringr" %in% installed.packages())
  install.packages("stringr")
suppressWarnings(library(stringr))


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

# Convert subpecies to species level data (our study only considers species, not
# subspecies)
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

zims$EU.zoo <- ifelse((zims$Country %in% EU), 1, 0)
zims <- zims %>% select(Species, everything())

# Save data for taxonomy check
#write.csv(zims, "2_cleanforcolcheck/Species360Data00.csv", row.names = F)

# Read ZIMS data after taxonomic check (GBIF)
zims2 <-
  read.csv("3_colcheck/Species360Data00_GBIF_checked.csv")

# Add genus column
zims2 <-
  zims2 %>% separate(
    col = Species,
    into = c("Genus", "Epithet"),
    sep = " ",
    remove = F
  )
# For a few species for which taxonomic information is missing, we use the ZIMS
# taxonomy
zims2[which(is.na(zims2$order)), "order"] <-
  zims2[which(is.na(zims2$order)), "Order"]
zims2[which(is.na(zims2$family)), "family"] <-
  zims2[which(is.na(zims2$family)), "Family"]
zims2[which(is.na(zims2$class)), "class"] <-
  zims2[which(is.na(zims2$class)), "Class"]
zims2[which(is.na(zims2$genus)), "genus"] <-
  zims2[which(is.na(zims2$genus)), "Genus"]
zims2[which(is.na(zims2$gbif.species)), "gbif.species"] <-
  zims2[which(is.na(zims2$gbif.species)), "Species"]

# fix gbif mistakes (GBIF sometimes returns extinct species  etc.)
id <- which(zims2$class %in% c("Florideophyceae", "Insecta"))
zims2[id, "order"] <- zims2[id, "Order"]
zims2[id, "family"] <- zims2[id, "Family"]
zims2[id, "class"] <- zims2[id, "Class"]
zims2[id, "genus"] <- zims2[id, "Genus"]
zims2[id, "gbif.species"] <- zims2[id, "Species"]

# save clean data
write.csv(zims2,
          "4_alldataclean/Species360Data00_GBIF_checked.csv",
          row.names = F)

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

# Read AZE data after taxonomic check (GBIF)
aze2 <-
  read.csv("3_colcheck/AZE_species_full_list_cleaned02_GBIF_checked.csv")

# Use original names if not found by gbif
aze2[which(is.na(aze2$species)), "species"] <-
  aze2[which(is.na(aze2$species)), "AZE.Trigger.Species"]
# save clean data
write.csv(aze2,
          "4_alldataclean/AZE_species_full_list_cleaned02_GBIF_checked.csv")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# PROTECTED AREA DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Data on protected area were obtained from the supplemental materials of
# Maiorano, L., Amori, G., Montemaggiori, A., Rondinini, C., Santini, L., Saura,
# S. & Boitani, L. (2015). On how much biodiversity is covered in Europe by
# national protected areas and by the Natura 2000 network: Insights from
# terrestrial vertebrates. Conserv. Biol., 29.

# Read in PA data after taxonomic check (GBIF)
PaData <-
  read.csv("4_alldataclean/PADataColCheck00_GBIF_checked.csv",
           stringsAsFactors = F)

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

# For subspecies, we sum the gap areas and add them on the species level
spec.sum <- aggregate(Gap.km2 ~ gbif.species,
                      data = PaData, FUN = sum)
# Merge revised gap areas back in
PaData <- merge(PaData, spec.sum, by = "gbif.species")

# Remove duplicated species (due to subspecies or revised taxonomy)
PaData    <- PaData[!duplicated(PaData$gbif.species), ]

# delete old column, rename new column
PaData <- subset(PaData, select = -Gap.km2.x)
names(PaData)[names(PaData) == "Gap.km2.y"] <- "Gap.km2"

# Save data
write.csv(
  PaData,
  "ANALYSIS/DATA/4_alldataclean/PADataColCheck00_GBIF_checked_cleaned.csv",
  row.names = F
)
