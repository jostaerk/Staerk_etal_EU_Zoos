# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: Decision framework for EU species
# Author: Johanna Staerk
# Description: Prepares and merges all datasets for the decision analysis

# Note: this script requires data that is publicly available or can be obtained
# upon request. It is only published for transparency and cannot be directly be reproduced.
# For data sources, see readme file

# All data were taxonomically standardized prior according to the GBIF taxonomy,
# using the taxize R package (Chamberlain, S.A. & Szöcs, E. (2013). Taxize:
# Taxonomic search and retrieval in R. F1000Research, 2.)
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

# Calculate global number of individuals in zoos
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

# Merge insititutions with IUCN
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

# Merge with IUCN
Iucn <-
  merge(Iucn, NrSustPopGlobal, by = "gbif.species",  all.x = T)
Iucn <- merge(Iucn, NrSustPopEU, by = "gbif.species",  all.x = T)
Iucn$NrSustPopGlobal[is.na(Iucn$NrSustPopGlobal)] <- 0
Iucn$NrSustPopEU[is.na(Iucn$NrSustPopEU)] <- 0

# Indicate if there are >100 individuals as a metapopulation (in all zoos combined)
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

# 3) Protected areas ----

# subset relevant cols
Pa   <-
  Pa[, c("gbif.species",
         "TargetAreakm2",
         "ProtectedAreakm2",
         "Gap.km2",
         "perc.target.met.in.PAs.and.N2K",
         "Habitats.Directive.Annex2", "Birds.Directive.Annex.1"
         )]
# merege onto IUCN list
Iucn <- merge(Iucn, Pa, by = "gbif.species", all.x = TRUE)

# We define EU species as those that were assessed by Maiorano et al. (2015), i.e.
# those that not NA after merging with the Maiorano data
Iucn$EUSpecies <- ifelse(is.na(Iucn$TargetAreakm2), 0, 1)
table(Iucn$EUSpecies)


# 4) Ex-situ programs ----

# EAZA EEP/ESB

# Dama dama is only managed as its subspecies Dama mesopotamica (outside of EU).
# Therefore we exlude it
EepEsb <- EepEsb[which(!(EepEsb$species == "Dama dama")), ]

eep <- EepEsb[which(EepEsb$Program == "EEP"), ]
eepsp <-
  unique(c(eep$Scientific.Name.db, eep$Scientific.Name, eep$species))
eepsp <- eepsp[!(is.na(eepsp))]
esb <- EepEsb[which(EepEsb$Program == "ESB"), ]
esbsp <-
  unique(c(esb$Scientific.Name.db, esb$Scientific.Name, esb$species))
esbsp <- esbsp[!(is.na(esbsp))]

# Check if IUCN species names appear in any of the synonyms mentioned as EEP/ESB species (1=yes, 0=no)
Iucn$EEP <- ifelse(Iucn$gbif.species %in% eepsp, 1, 0)
Iucn$ESB <- ifelse(Iucn$gbif.species %in% esbsp, 1, 0)

# Other Ex Situ programmes mentioned on global RL online assessments:
Iucn$RLExsitu <-
  ifelse(Iucn$gbif.species %in% RLExsitu[which(RLExsitu$Ex.situ.in.place ==
                                                 "Y"), "gbif.species"], 1, 0)
Iucn$RLExsitu[Iucn$EUSpecies == 0] <- NA

Iucn$RLExsituNeeded <- ifelse(Iucn$gbif.species %in% RLExsitu[which(RLExsitu$Ex.situ.needed ==
                                                                      "Y"), "gbif.species"], 1, 0)
table(Iucn$RLExsituNeeded)

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


# 8) Other data preparations ----

# Add column for species that are either vulnerable to climate change or threated acc. to the EU Red list
Iucn$THR_CCH <-
  ifelse(Iucn$RLcatEUFinal %in% c("CR", "EN", "VU", "RE", "EW") |
           Iucn$VC.category == "H",
         1,
         0)
Iucn$THR_CCH[is.na(Iucn$THR_CCH)] <- 0
Iucn$THR <-
  ifelse(Iucn$RLcatEUFinal %in% c("CR", "EN", "VU", "RE", "EW"), 1, 0)

# Subset data to only include EU species
Iucn.EU <- Iucn[which(Iucn$EUSpecies == 1),]

# Remove species not assessed by the RL
Iucn.EU <- Iucn.EU[which(!(is.na(Iucn.EU$RLcatEUFinal))),]

# Check for duplicates due to taxonomic standardization and correct
Iucn.EU[which(duplicated(Iucn.EU$gbif.species)), ]
Iucn.EU <-
  Iucn.EU[-which(Iucn.EU$db.species == "Chalcides simonyi"),]
Iucn.EU <- Iucn.EU[-which(Iucn.EU$db.species == "Picus sharpei"),]
Iucn.EU <-
  Iucn.EU[-which(Iucn.EU$db.species == "Psammodromus manuelae"),]

# Check mammal species that are not assessed for vulnerability to climate change
# id of species to exclude
id.cchNA <- intersect(which(!(Iucn.EU$class == "Mammalia")),
                      which(is.na(Iucn.EU$VC.category)))
# If speices cannot be found assume Low vulnerability to climate change
# In total 27 could not be found of those 4 were due to taxonomy (3 stayed L, 1 was H)
# so for 23 species we assumed Low status
Iucn.EU[id.cchNA, "VC.category"] <- "L"
# For Neophron percnopterus it is a taxonomic issue (listed as Neophron perenopterus)
Iucn.EU[which(Iucn.EU$db.species == "Neophron perenopterus"), "VC.category"] <-
  "H"

# 9) Level of management expertise (l = 1,2,3,4) --------
# We defined levels of management as Gi1 (no expertise, neither species i nor a
# congeneric species are kept in a zoo), Gi2 (transferrable expertise, a
# congeneric species is kept in a zoo), Gi3 (expertise exists but may be
# difficult to access because species is kept in a zoo outside the EU) and Gi4
# (expertise exists within EU zoos).

CalcLevelManagement <- function(Seu, Sww, Gww){
  # Sww = Species in a zoo worldwide, 1,0 statememt if species is in a zoo global
  # Seu = Species in a zoo EU, 1, 0 statememt if species is in a zoo in the EU
  # Gww = Genus in a zoo worldwide, 1,0 statememt if genus is in a zoo global

  x <- rep(0, length(Sww))

  for(i in 1:length(x)){
    if (Seu[i] == 1){
      x[i] = 4 # Calculates the additional benefit if Species is in EU based in # of individuals present in zoo (N0/Nv)
    } else {
      if (Sww[i] == 1) {
        x[i] <- 3
      } else {
        if (Gww[i] == 1){
          x[i] <- 2
        } else {
          x[i] <- 1
        }
      }
    }
  }

  return(x)
}

Iucn.EU$LevelManagement <- CalcLevelManagement(Iucn.EU$Species.Zoo.EU,
                                          Iucn.EU$Species.Zoo.global,
                                          Iucn.EU$Genus.Zoo.Global)



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# OUTPUT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

write.csv(Iucn.EU,
          "5_finalforanalysis/EUSpeciesDataFinal00.csv",
          row.names = F)

