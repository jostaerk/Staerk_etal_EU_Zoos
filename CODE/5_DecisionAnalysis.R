# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: A decision framework to integrate in-situ and ex-situ management for
# species in the European Union
# Author: Johanna Staerk
# Description: Calculate # expected values, benefits, cost-effectiveness for
# each possible decision (i.e. habitat protection, captive breeding, both or no
# action ) and choose recommended strategy for each species based on maximum
# benefits. Then ranks species based on four different ranking criteria
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Init -------------------------------------------------------------------------
library(tidyverse)
rm(list = ls())
options(scipen = 20)

# Call functions
source("CODE/6_Functions.R")

# Input -----------------------------------------------------------------------

eudat    <-
  read.csv(
    "DATA/EUSpeciesDataFinal01.csv",
    na.strings = c("", " ", "NA"),
    stringsAsFactors = F
  )
# subset relevant columns
eudat <- eudat %>% select(
  gbif.species,
  class,
  excluded,
  exclusion.reason,
  redlist.eu = RL.catEU27,
  endemic.europe.final = endemic,
  vuln.clim.change = VC.category,
  THR_CCH,
  gap.km2 = Gap.km2,
  perc.target.met.in.PAs.and.N2K,
  db.subspecies,
  zoo.global = Species.Zoo.global,
  zoo.eu = Species.Zoo.EU,
  genus.zoo.global = Genus.Zoo.Global,
  pop.zoo.eu = NrIndivEU,
  redlist.exsitu = RLExsitu,
  redlist.exsitu.needed = RLExsituNeeded,
  ed.value = ED.score,
  bodymass.g = Bodymass.g,
  cost.captive,
  overlap.sum,
  cost.wild,
  cost.wild.adjust
)

# Add level of breeding expertise
eudat$breeding.expertise = CalcLevelManagement(eudat$zoo.eu,
                                               eudat$zoo.global,
                                               eudat$genus.zoo.global)

# Subset only red-listed or vulnerable to climate change species or near
# threatened species that are endemic
eudat <-
  eudat[which(
    eudat$redlist.eu %in% c("CR", "EN", "VU") |
      eudat$redlist.eu %in% c("LC", "NT") &
      eudat$vuln.clim.change == "H" |
      eudat$redlist.eu %in% c("NT") &
      eudat$endemic.europe.final == "Yes"
  ), ]

# remove species that do not have data on protected areas or only have PA data at
# subspecies level
eudat$excluded <- ifelse(is.na(eudat$gap.km2), 1, 0)
eudat$exclusion.reason <-
  ifelse(is.na(eudat$gap.km2), "no data on PAs", NA)

# exclude species for which habitat data is only available at subspecies level
eudat[which(!(is.na(eudat$db.subspecies))), "excluded"] <- 1
eudat[which(!(is.na(eudat$db.subspecies))), "exclusion.reason"] <-
  "PA data only available for subspecies"
table(eudat$excluded)

excluded <- eudat[which(eudat$excluded == 1),]
eudat <- eudat[which(eudat$excluded == 0),] # 277 species

# Decision tree ----------------------------------------------------------------
# 1) Data for probabilities wild  ----

# K_i - Climate change vulnerability
# We assign an extinction probability of 0.3 to species vulnerable to climate
# change
eudat$Ki <- eudat$vuln.clim.change
eudat[which(eudat$Ki == "L"), "Ki"] <- 0
eudat[which(eudat$Ki == "H"), "Ki"] <- 0.3
# For missing values we assume the CLimate change probability to be 0
eudat[which(is.na(eudat$Ki)), "Ki"] <- 0
eudat$Ki <- as.numeric(eudat$Ki)

# A_i - Proportion of protected area
eudat$Ai <- eudat$perc.target.met.in.PAs.and.N2K / 100
eudat[eudat$Ai > 1, "Ai"] <- 1

# I_i Iucn extinction probablity
# Extinction probablity transformation
# We used the extinction prob as designated by the IUCN scaled to 100 years
# (see Mooers et al. 2008)

Ii <- rep(0, nrow(eudat))
Ii[which(eudat$redlist.eu == "LC")] <- 0.0001
Ii[which(eudat$redlist.eu == "NT")] <- 0.01
Ii[which(eudat$redlist.eu == "VU")] <- 0.1
Ii[which(eudat$redlist.eu == "EN")] <- 0.667
Ii[which(eudat$redlist.eu == "CR")] <- 0.999
eudat$Ii <- Ii

# 2) Data for probability captivity ----

# Viable population
Nv <- 100

# Breeding success levels
G1 <- 0.05 # when neither species nor genus is in a zoo
G2 <- 0.1  # when genus is in a zoo
G3 <- 0.2  # when species is in any zoo globally

# Calculate level of management expertise based on number of individuals in EU
# zoos
eudat$Gi <- CalcManagementExpertise(
  eudat$zoo.eu,
  eudat$zoo.global,
  eudat$genus.zoo.global,
  eudat$pop.zoo.eu,
  Nv = Nv,
  G1 = G1,
  G2 = G2,
  G3 = G3
)

# 3) Stakeholder values ----
# Decision makers value judgments of each outcome
Vz <- 0.1  # species survives in zoo
Vw <- 1  # species survives in wild
Vb <- 1  # species survives in both
Vx <- 0  # species extinct

# 4) Calculate benefits ----
df.benefits <- CalcBenefits(
  Ai = eudat$Ai,
  Ii = eudat$Ii,
  Ki = eudat$Ki,
  Gi = eudat$Gi,
  pop.zoo.eu = eudat$pop.zoo.eu,
  Nv = Nv,
  cost.wild = eudat$cost.wild.adjust,
  cost.captive = eudat$cost.captive,
  Vz = Vz,
  Vw = Vw,
  Vb = Vb,
  Vx = Vx
)

eudat <- cbind(eudat, df.benefits)

# 5) Choose  strategy ----
# Without budget constraints (B)
eudat$net.strategy <- ChooseCostStrategy(
  Wild = eudat$benefit.wild,
  Zoo = eudat$benefit.zoo,
  Both = eudat$benefit.both
)
# With budget constraints (Benefits/Costs)
eudat$cost.net.strategy <-
  ChooseCostStrategy(
    Wild = eudat$cost.benefit.wild,
    Zoo = eudat$cost.benefit.zoo,
    Both = eudat$cost.benefit.both
  )

# Ranking ----------------------------------------------------------------------

# Replace NA's
eudat[which(is.na(eudat$cost.benefit.wild)), "cost.benefit.wild"]  <- 0
eudat[which(is.na(eudat$cost.benefit.zoo)), "cost.benefit.zoo"]  <- 0
eudat[which(is.na(eudat$cost.benefit.both)), "cost.benefit.both"]  <- 0

# Get maximum benefits / cost benefit values
eudat$benefit.max <-
  apply(eudat[, match("benefit.wild", names(eudat)):match("benefit.both", names(eudat))], 1, max)
eudat$cost.benefit.max <-
  apply(eudat[, match("cost.benefit.wild", names(eudat)):match("cost.benefit.both", names(eudat))], 1, max)

# Get maximum benefits * ED / cost benefit values *ED

# Calculate adjusted ED value by dividing ED scores by max per class
amp.mx = 191
rep.mx = 243
mam.mx = 89.5
ave.mx = 72.8

eudat$ed.value.adjust = NA
eudat[which(eudat$class == "Amphibia"), "ed.value.adjust"] <-
  eudat[which(eudat$class == "Amphibia"), "ed.value"] / amp.mx
eudat[which(eudat$class == "Reptilia"), "ed.value.adjust"] <-
  eudat[which(eudat$class == "Reptilia"), "ed.value"] / rep.mx
eudat[which(eudat$class == "Mammalia"), "ed.value.adjust"] <-
  eudat[which(eudat$class == "Mammalia"), "ed.value"] / mam.mx
eudat[which(eudat$class == "Aves"), "ed.value.adjust"] <-
  eudat[which(eudat$class == "Aves"), "ed.value"] / ave.mx


eudat$cost.benefit.max <-
  apply(eudat[, match("cost.benefit.wild", names(eudat)):match("cost.benefit.both", names(eudat))], 1, max)

# Ranking criterion 1 -
# Benefits only
eudat <- eudat %>% group_by(class) %>%
  mutate(rank1.ben = as.numeric(as.factor(rank(-benefit.max, na.last = T)))) %>%
  arrange(class, (rank1.ben))

# Ranking criterion 2
# Benefits * ED
eudat$benefit.max.ed <- eudat$benefit.max * eudat$ed.value.adjust
eudat <- eudat %>% group_by(class) %>%
  mutate(rank2.ben.ed = as.numeric(as.factor(rank(-benefit.max.ed, na.last = T)))) %>%
  arrange(class, (rank2.ben.ed))

# Ranking criterion 3 - cost benefits
# Benefits / Costs
eudat <- eudat %>% group_by(class) %>%
  mutate(rank3.costben = as.numeric(as.factor(rank(-cost.benefit.max, na.last = T)))) %>%
  arrange(class, (rank3.costben))

# Ranking criterion 3 - cost benefits * ED
# (Benefits / Costs ) * ED
eudat$cost.benefit.max.ed <- eudat$cost.benefit.max * eudat$ed.value.adjust
eudat <- eudat %>% group_by(class) %>%
  mutate(rank4.costben.ed = as.numeric(as.factor(rank(-cost.benefit.max.ed, na.last = T)))) %>%
  arrange(class, (rank4.costben.ed))


eudat <- eudat %>% arrange(class, rank1.ben)

# Summary results Table 1 ------------------------------------------------------

t.n <- table(eudat$net.strategy); t.n
t.n
round((prop.table(t.n))*100, 2)

t.c <- table(eudat$cost.net.strategy)
t.c
round((prop.table(t.c))*100)
# Net Strategy per class
tnc <- table(eudat$net.strategy, eudat$class)
tnc
rowSums(tnc)
rowSums(tnc)/277
colSums(tnc)
round(rowSums(tnc) / sum(rowSums(tnc)), 2)
round(prop.table(tnc, 2), 2) * 100 # Percentages

# Cost Net strategy per class
tcc <- table(eudat$cost.net.strategy, eudat$class)
tcc
rowSums(tcc)
rowSums(tcc) / 277
colSums(tcc)
round(prop.table(tcc, 2), 2) * 100 # Percentages


# Comparison with the IUCN RL
table(eudat$redlist.exsitu.needed, eudat$net.strategy)
table(eudat$redlist.exsitu)

# Number of gap species
nrow(eudat[which(eudat$gap.km2 == 0), ])

# Output -----------------------------------------------------------------------

# add back in the excluded species
eudat <- eudat %>% bind_rows(excluded)

# Save Supplemental Data file tab2 "Decision Analysis"
eudat <- eudat %>% select(gbif.species, class,
                          excluded,
                          exclusion.reason,
                          redlist.eu:vuln.clim.change,
                          perc.target.met.in.PAs.and.N2K,
                          zoo.global:genus.zoo.global, pop.zoo.eu, breeding.expertise,
                          ed.value, ed.value.adjust,
                          cost.captive, cost.wild, overlap.sum, cost.wild.adjust,
                          Ki:Gi, ev.wild:cost.benefit.both,
                          net.strategy, cost.net.strategy,
                          rank1.ben, rank2.ben.ed, rank3.costben, rank4.costben.ed, excluded,
                          exclusion.reason)

#write.csv(eudat, "RESULTS/RankingEUSpecies.csv", row.names = F)



