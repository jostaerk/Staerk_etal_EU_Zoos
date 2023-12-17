# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: Decision framework for EU species
# Author: Johanna Staerk
# Description: Prepares and merges all datasets for the decision analysis

# Note: this script requires data that is publicly available or can be obtained
# upon request (see readme file for data sources). This script is only published
# for transparency and cannot directly be reproduced, unless you obtain the raw
# data files.

# All data were taxonomically standardized prior according to the GBIF taxonomy,
# using the taxize R package (Chamberlain, S.A. & Szöcs, E. (2013). Taxize:
# Taxonomic search and retrieval in R. F1000Research, 2.)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INIT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list = ls())
cat('\014')

# Point to data folder
setwd("/Users/johanna_staerk/Dropbox/PROJECTS/EU.Zoos/EU/ANALYSIS/DATA/")

library(tidyverse)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INPUT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# European Red List
Iucn <-
  read.csv(
    "4_alldataclean/IucnEUTetrapods_GBIF_checked_cleaned.csv",
    na.strings = c("", " ", "NA"),
    stringsAsFactors = F,
    sep = ","
  )
# Species360 ZIMS
Zims    <-
  read.csv("4_alldataclean/Species360Data00_GBIF_checked.csv",
           stringsAsFactors = F)
# Protected areas
Pa      <-
  read.csv("4_alldataclean/PADataColCheck00_GBIF_checked_cleaned.csv",
           stringsAsFactors = F)

# EAZA European breeding programs
EepEsb  <-
  read.csv(
    "4_alldataclean/EEPESB_list_11 December 2019_cleaned_GBIF_checked.csv",
    stringsAsFactors = F
  )
# Bodymass mammals
BwMam   <-
  read.csv(
    "4_alldataclean/BodymassPacificiDataColCheck00_GBIF_checked.csv",
    stringsAsFactors = F
  )
# Bodymass birds
BWAve   <-
  read.csv("4_alldataclean/CRCDataColCheck00_GBIF_checked.csv",
           stringsAsFactors = F)

# Alliance for Zero Extinction
aze     <-
  read.csv(
    "4_alldataclean/AZE_species_full_list_cleaned02_GBIF_checked.csv",
    stringsAsFactors = F
  )
# Climate change vulnerability assessment birds and amphibians
CCh     <-
  read.csv(
    "4_alldataclean/CChAmAvEuropeDataColCheck00_GBIF_checked.csv",
    stringsAsFactors = F
  )
# Climate change vulnerability assessment reptiles
CChRep  <-
  read.csv(
    "4_alldataclean/ClimChReptilesDataColCheck00_GBIF_checked.csv",
    stringsAsFactors = F
  )
# Evolutionary distinctiveness (ED)
ED      <-
  read.csv("4_alldataclean/ED2DataColCheck00_GBIF_checked.csv",
           stringsAsFactors = F)

# Missing ED scores. For some species that missed ED scores we manually checked them
# For most species it was a taxonomy issue, if we could still not find an ED, we used
# the mean ED of genus
missED  <-
  read.csv(
    "4_alldataclean/MissingED2SpeciesDataColCheck00_GBIF_checked.csv",
    stringsAsFactors = F
  )
# Some species with missing body weight data, we used data from:
#
missBW  <-
  read.csv(
    "4_alldataclean/MissingBodyweightDataColCheck00_GBIF_checked.csv",
    stringsAsFactors = F
  )
# Amphibian Ark
AArk <-
  read.csv("4_alldataclean/AArk_exsituDataColCheck00_GBIF_checked.csv",
           stringsAsFactors = F)

# IUCN Red List Ex-situ programmes mentioned on the Global RL
RLExsitu <-
  read.csv(
    "4_alldataclean/ExSituPrograms.EEP.ESB.AARK.IUCN_GBIF_checked.csv",
    stringsAsFactors  = F,
    na.strings = c("", " ")
  )

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA MERGING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Merge all data onto the list of species provided by the European Red List

# 1) ZIMS data ----

# Add column to IUCN data on whether species is in a Species360 zoo, globally (1=yes, 0=no)
Iucn$Species.Zoo.global <-
  ifelse(Iucn$gbif.species %in% Zims$gbif.species, 1, 0)

# Add column whether species is in Species360 zoo in the EU (1=yes, 0=no)
Iucn$Species.Zoo.EU <-
  ifelse(Iucn$gbif.species %in% Zims[Zims$EU.zoo == 1, "gbif.species"], 1, 0)

# If the species is not in a zoo, is the genus represented in zoos globally? (1=yes, 0=no)
Iucn$Genus.Zoo.Global <-
  ifelse(Iucn$Species.Zoo.global == 0 &
           Iucn$genus %in% Zims$genus,
         1,
         0)

# Calculate  number of individuals in zoos globally
NrIndivGlobal  <- aggregate(Total.Count ~ gbif.species,
                            data = Zims, FUN = sum)
colnames(NrIndivGlobal) <- c("gbif.species", "NrIndivGlobal")

# Calculate number of individuals in zoos in the EU
NrIndivEU <- aggregate(Total.Count ~ gbif.species,
                       data = Zims[which(Zims$EU.zoo == 1), ], FUN = sum)
colnames(NrIndivEU)     <- c("gbif.species", "NrIndivEU")

# Merge number of individuals with IUCN data
Iucn <- merge(Iucn, NrIndivGlobal, by = "gbif.species",  all.x = T)
Iucn <- merge(Iucn, NrIndivEU, by = "gbif.species",  all.x = T)
Iucn$NrIndivGlobal[is.na(Iucn$NrIndivGlobal)] <- 0 # replace NA
Iucn$NrIndivEU[is.na(Iucn$NrIndivEU)] <- 0

# Calculate the largest number of animals in a single zoo in the EU
MaxPopSizeEU <- aggregate(Total.Count ~ gbif.species,
                          data = Zims[which(Zims$EU.zoo == 1), ], FUN = max)
colnames(MaxPopSizeEU)     <- c("gbif.species", "MaxPopSizeEU")
Iucn <- merge(Iucn, MaxPopSizeEU, by = "gbif.species",  all.x = T)

# Calculate the second largest number of animals in a single zoo in the EU
SecondMaxPopSizeEU <- aggregate(
  Total.Count ~ gbif.species,
  data = Zims[which(Zims$EU.zoo == 1), ],
  FUN = function(x) {
    max(x[x != max(x)])
  }
)
colnames(SecondMaxPopSizeEU)     <-
  c("gbif.species", "SecondMaxPopSizeEU")
SecondMaxPopSizeEU[which(is.infinite(SecondMaxPopSizeEU$SecondMaxPopSizeEU)), "SecondMaxPopSizeEU"] <-
  0
Iucn <-
  merge(Iucn, SecondMaxPopSizeEU, by = "gbif.species",  all.x = T)

# Calculate number of institutions globally
NrZoosGlobal  <- aggregate(
  Mnemonic ~ gbif.species,
  data = Zims,
  FUN = function(x) {
    length(unique(x))
  }
)
colnames(NrZoosGlobal) <- c("gbif.species", "NrZoosGlobal")

# Calculate number of institutions in EU
NrZoosEU      <- aggregate(
  Mnemonic ~ gbif.species,
  data = Zims[which(Zims$EU.zoo == 1),],
  FUN = function(x) {
    length(unique(x))
  }
)
colnames(NrZoosEU) <- c("gbif.species", "NrZoosEU")

# Merge number of institutions with IUCN dataframe
Iucn <- merge(Iucn, NrZoosGlobal, by = "gbif.species",  all.x = T)
Iucn <- merge(Iucn, NrZoosEU, by = "gbif.species",  all.x = T)
Iucn$NrZoosGlobal[is.na(Iucn$NrZoosGlobal)] <- 0
Iucn$NrZoosEU[is.na(Iucn$NrZoosEU)] <- 0

# Calculate the number of institutions with > 100 individuals
nv <- 100
Zims$SustPop <- ifelse(Zims$Total.Count >= nv, 1, 0)

# Globally:
NrSustPopGlobal  <- aggregate(SustPop ~ gbif.species,
                              data = Zims, FUN = sum)
colnames(NrSustPopGlobal) <- c("gbif.species", "NrSustPopGlobal")
# in EU;
NrSustPopEU  <- aggregate(SustPop ~ gbif.species,
                          data = Zims[which(Zims$EU.zoo == 1), ], FUN = sum)
colnames(NrSustPopEU) <- c("gbif.species", "NrSustPopEU")

# Merge with IUCN dataframe
Iucn <-
  merge(Iucn, NrSustPopGlobal, by = "gbif.species",  all.x = T)
Iucn <- merge(Iucn, NrSustPopEU, by = "gbif.species",  all.x = T)
Iucn$NrSustPopGlobal[is.na(Iucn$NrSustPopGlobal)] <- 0
Iucn$NrSustPopEU[is.na(Iucn$NrSustPopEU)] <- 0

# Indicate if there are >100 individuals as a metapopulation (in all zoos
# combined)
Iucn$SustMetaPopGlobal <- ifelse(Iucn$NrIndivGlobal >= nv, 1, 0)
Iucn$SustMetaPopEU <- ifelse(Iucn$NrIndivEU >= nv, 1, 0)

# 2) Climate change data (Birds/ Amphibians/ Reptiles) ----

# Subset relevant cols
CCh    <- CCh[, c("gbif.species", "FINAL_SCORE")]
CChRep <- CChRep[, c("gbif.species", "Overall.1")]
colnames(CChRep) <- c("gbif.species", "FINAL_SCORE")
# Bind Climate change (Reptiles with Birds and Amphibian)
CChAll <- rbind(CCh, CChRep)
# Remove duplicates
CChAll <- CChAll[which(!(duplicated(CChAll$gbif.species))), ]
# Merge data to IUCN list
Iucn   <- merge(Iucn, CChAll, by = "gbif.species", all.x = T)
# Rename column
colnames(Iucn)[which(colnames(Iucn) == "FINAL_SCORE")] <-
  "VC.category"
table(Iucn$VC.category)

# Fill up data for missing species
# in most cases it was a taxonomic issue, for few cases we used data on

Iucn[which(Iucn$db.species == "Neophron percnopterus"), "VC.category"] <-
  "H"
Iucn[which(Iucn$gbif.species == "Calonectris borealis"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Calonectris diomedea"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Curruca balearica"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Sylvia sarda"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Curruca cantillans"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Sylvia cantillans"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Curruca melanocephala"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Sylvia melanocephala"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Curruca subalpina"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Sylvia melanocephala"), "FINAL_SCORE"] #related
Iucn[which(Iucn$gbif.species == "Cyanistes teneriffae"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Cyanistes caeruleus"), "FINAL_SCORE"] #related
Iucn[which(Iucn$gbif.species == "Cyanopica cooki"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Cyanopica cyanus"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Fringilla polatzeki"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Fringilla teydea"), "FINAL_SCORE"] #related
Iucn[which(Iucn$gbif.species == "Lanius meridionalis"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Lanius excubitor"), "FINAL_SCORE"] #related
Iucn[which(Iucn$gbif.species == "Larus michahellis"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Larus cachinnans"), "FINAL_SCORE"] #related
Iucn[which(Iucn$gbif.species == "Neophron percnopterus"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Neophron perenopterus"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Oceanodroma monteiroi"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Oceanodroma castro"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Onychoprion fuscatus"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Sterna fuscata"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Passer italiae"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Passer domesticus"), "FINAL_SCORE"] #related
Iucn[which(Iucn$gbif.species == "Phalacrocorax pygmaeus"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Microcarbo pygmeus"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Phylloscopus orientalis"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Phylloscopus bonelli"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Pterodroma deserta"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Pterodroma feae"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Sylvia crassirostris"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Sylvia hortensis"), "FINAL_SCORE"]
Iucn[which(Iucn$gbif.species == "Tarentola angustimentalis"), "VC.category"] <-
  CChAll[which(CChAll$gbif.species == "Tarentola mauritanica"), "FINAL_SCORE"]

# 3) Protected areas ----

Iucn <- merge(Iucn, Pa, by = "gbif.species", all.x = TRUE)

# 4) Ex-situ programs ----

# EAZA EEP/ESB

# We exclude Dama dama as it is only managed as its subspecies Dama mesopotamica (outside of EU).
# same for Cervus elaphus (bactrianus)

EepEsb <- EepEsb[which(!(EepEsb$species == "Dama dama")), ]
EepEsb <- EepEsb[which(!(EepEsb$species == "Cervus elaphus")), ]

eep <- EepEsb[which(EepEsb$Program == "EEP"), ]
eepsp <-
  unique(c(eep$Scientific.Name.db, eep$Scientific.Name, eep$species))
eepsp <- eepsp[!(is.na(eepsp))]
esb <- EepEsb[which(EepEsb$Program == "ESB"), ]
esbsp <-
  unique(c(esb$Scientific.Name.db, esb$Scientific.Name, esb$species))
esbsp <- esbsp[!(is.na(esbsp))]

Iucn$EEP <- ifelse(Iucn$gbif.species %in% eepsp, 1, 0)
Iucn$ESB <- ifelse(Iucn$gbif.species %in% esbsp, 1, 0)


# Other Ex Situ programmes mentioned on global RL online assessments:
Iucn$RLExsitu <-
  ifelse(Iucn$gbif.species %in% RLExsitu[which(RLExsitu$Ex.situ.in.place ==
                                                 "Y"), "gbif.species"], 1, 0)
Iucn$RLExsitu[Iucn$EUSpecies == 0] <- NA

Iucn$RLExsituNeeded <-
  ifelse(Iucn$gbif.species %in% RLExsitu[which(RLExsitu$Ex.situ.needed ==
                                                 "Y"), "gbif.species"], 1, 0)
table(Iucn$RLExsituNeeded)
table(Iucn$RLExsitu)


# Amphibian Ark ex-situ monitoring
Iucn$Aark <- ifelse(Iucn$gbif.species %in% AArk$gbif.species, 1, 0)

# 5) Alliance for Zero Extinction ----
azesp <- unique(aze$AZE.Trigger.Species, aze$species)
Iucn$Aze <- ifelse(Iucn$gbif.species %in% azesp, 1, 0)

# 6) Evolutionary Distinctiveness ----

# Due to taxonomic merging based on gbif names we get duplicates, for those we take the
# mean ED value, for EDGE we use the maximum
ED2 <-
  ED %>% group_by(gbif.species) %>% summarise(ED.score = mean(ED.score, na.rm = T),
                                              EDGE = max(EDGE, na.rm = T))
# Merge data
Iucn <- merge(Iucn, ED2, by = "gbif.species", all.x = T)

# For species with no ED we use ED from our list of manually checked data
missED <- missED[, c("gbif.species",
                     "ED.score.related",
                     "related.species",
                     "EDGE.Species",
                     "reason")]
# Merge missing data
Iucn <- merge(Iucn, missED, by = "gbif.species", all.x = T)
Iucn[which(is.na(Iucn$ED.score)), "ED.score"] <-
  Iucn[which(is.na(Iucn$ED.score)), "ED.score.related"]

# For species with no EDGE we use EDGE from our list of manually checked data
Iucn[which(Iucn$reason == "taxonomy"), "EDGE"] <-
  Iucn[which(Iucn$reason == "taxonomy"), "EDGE.Species"]

# 7) Bodyweight data -----

# Calculate mean bodyweight for mammals
BWMam.mean <-
  aggregate(AdultBodyMass_g ~ gbif.species,
            data = BwMam,
            FUN = mean)


# Calculate mean bodyweight for birds
BWAve.mean <-
  aggregate(Mean ~ gbif.species, data = BWAve, FUN = mean)

# Merge bodyweight data onto IUCN species list
Iucn <- merge(Iucn, BWMam.mean, by = "gbif.species", all.x = T)
Iucn <- merge(Iucn, BWAve.mean, by = "gbif.species", all.x = T)

# Combine data in one column
Iucn[which(is.na(Iucn$AdultBodyMass_g)), "AdultBodyMass_g"] <-
  Iucn[which(is.na(Iucn$AdultBodyMass_g)), "Mean"]
Iucn <-
  Iucn %>% rename(Bodymass.g = AdultBodyMass_g) %>% select(-Mean)

# Add missing bodyweights
missBW <- missBW[, c("gbif.species",
                     "Bodymass.g")]
Iucn <- merge(Iucn, missBW, by = "gbif.species", all.x = T)
Iucn[which(is.na(Iucn$Bodymass.g.x)), "Bodymass.g.x"] <-
  Iucn[which(is.na(Iucn$Bodymass.g.x)), "Bodymass.g.y"]
Iucn <-
  Iucn %>% rename(Bodymass.g = Bodymass.g.x) %>% select(-Bodymass.g.y)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# OUTPUT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

write.csv(Iucn,
          "5_finalforanalysis/EUSpeciesDataFinal00.csv",
          row.names = F)

