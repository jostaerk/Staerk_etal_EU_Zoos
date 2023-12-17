# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: A decision framework to integrate in-situ and ex-situ management for
# species in the European Union
# Author: Johanna Staerk
# Description:  This code calculates cost of habitat protection and captive
# management for EU species in a 100-year horizon
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INIT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list = ls())
cat('\014')
library(tidyverse)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INPUT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# read EU species data
eudat  <- read.csv(
  "DATA/EUSpeciesDataFinal00.csv",
  na.strings = c("", " ", "NA"),
  stringsAsFactors = F
)
# Matrices of protected area overlap between species per class
# obtained upon request from Maiorano et al. (2015)
percAA <-
  read.csv("DATA/OverlapData/percAADataColCheck00_GBIF_checked.csv") # Amphibians
percBB <-
  read.csv("DATA/OverlapData/percBBDataColCheck00_GBIF_checked.csv") # Birds
percMM <-
  read.csv("DATA/OverlapData/percMMDataColCheck00_GBIF_checked.csv") # Mammals
percRR <-
  read.csv("DATA/OverlapData/percRRDataColCheck00_GBIF_checked.csv") # Reptiles

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 1.) Costs wild ---------------------------------------------------------------

# Data obtained from: Verburg, R. W., Hennen, W. H. G. J., Puister, L. F.,
# Michels, R., & van Duijvendijk, K. (2017). Estimating costs of nature
# management in the European Union: Exploration modelling for PBL’s Nature
# Outlook (No. 97). Wageningen University & Research, Statutory Research Tasks
# Unit for Nature & the Environment.https://edepot.wur.nl/425041

# Costs are divided in annualised one-off costs and recurrent costs per hectar
# in the EU-28 over a 50-year timeframe

# Take mean land purchase across EU countries (2009)
oneoff.purch = mean(
  c(
    14763,
    31983,
    1138,
    4034,
    2249,
    25919,
    2644,
    6885,
    5130,
    9091,
    4854,
    3413,
    20021,
    16796,
    1014,
    971,
    20000,
    130000,
    47051,
    39494,
    844,
    3005,
    2764,
    11974,
    1256,
    5616,
    7807,
    3747,
    18690,
    9409
  )
)

# Recurring costs (€/ha/year annualised over 50 years, p. 52 in Verburg et al.)
recurr = 58.68

# In order to extrapolate to a 100-year horizon we assume that one-off costs are
# paid off after the 50 years over which the data has been annualised, but
# recurring costs continue

costw.ha = (oneoff.purch) + recurr * 100 # Euros/ha/100 years

# Convert to cost per km2 (100ha = 1km2)
costw.km2 = costw.ha / 100 # Euros/km2/100 years

# Multiply cost per km^2 with gap areas to estimate cost of protecting
# species' gap area
eudat$cost.wild <- costw.km2 * eudat$Gap.km2

# Because species can share their gap areas we divide costs by the sum of their
# overlapping areas with all other species of the same class

# Calculate row sums to calculate overlap sum per species
percAA <- percAA %>% select(gbif.species, a1:a104)
percAA$overlap.sum <- rowSums(percAA[, 2:ncol(percAA)])
percBB <- percBB %>% select(gbif.species, b1:b513)
percBB$overlap.sum <- rowSums(percBB[, 2:ncol(percBB)])
percMM <- percMM %>% select(gbif.species, m2:m295)
percMM$overlap.sum <- rowSums(percMM[, 2:ncol(percMM)])
percRR <- percRR %>% select(gbif.species, r2:r253)
percRR$overlap.sum <- rowSums(percRR[, 2:ncol(percRR)])

overlap.sum <- rbind(percAA[, c("gbif.species", "overlap.sum")],
                     percBB[, c("gbif.species", "overlap.sum")],
                     percMM[, c("gbif.species", "overlap.sum")],
                     percRR[, c("gbif.species", "overlap.sum")])

# For taxonomic duplicates, we use the max of the 2 species
overlap.sum <- aggregate(overlap.sum ~ gbif.species,
                         data = overlap.sum, FUN = max)
# Add overlap to dataframe
eudat <- merge(eudat, overlap.sum, by = "gbif.species", all.x = T)

# Calculate new habitat protection costs by dividing through overlap
eudat$cost.wild.adjust <- eudat$cost.wild / eudat$overlap.sum

# We do not have overlap estimates for all gap species,
# for the ones missing we use total costs
id.na <- which(is.na(eudat$cost.wild.adjust))
eudat$cost.wild.adjust[id.na] <- eudat$cost.wild[id.na]

# 2.) Costs captive ------------------------------------------------------------

# Currency conversion rate (US$ to Euro), avergage conversion rate between Dec.2016-Dec. 2020
# https://www.ofx.com/en-au/forex-news/historical-exchange-rates/yearly-average-rates/
conv <- 0.88

# Number of individuals needed for viable population
nv <-  100

# Number of individuals needed in EU zoos to make a viable population
# Take difference between nv and current number of individuals in EU zoos
diff.nv <- nv - eudat$NrIndivEU
diff.nv <- ifelse(diff.nv < 0, 0, diff.nv)

# Cost calculation Mammals/ Birds
# See Conde, D. A., Colchero, F., Güneralp, B., Gusset, M., Skolnik, B., Parr,
# M., ... & Fa, J. E. (2015). Opportunities and costs for preventing vertebrate
# extinctions. Current Biology, 25(6), R219-R221.
# And regression formula in:
# Balmford, A., Mace, G. M., & Leader‐Williams, N. (1996). Designing the ark:
# setting priorities for captive breeding. Conservation biology, 10(3), 719-727.
cost.captive <- rep(NA, nrow(eudat))

# Mammals
id.mam <- which(eudat$class == "Mammalia")
cost.captive[id.mam] <-
  ((10 ^ (
    0.34 * log10(eudat$Bodymass.g[id.mam] / 1000) + 3.21
  )) * diff.nv[id.mam]) * conv
# Birds
id.ave <- which(eudat$class == "Aves")
cost.captive[id.ave] <-
  (10 ^ (0.01 * log10(eudat$Bodymass.g[id.ave] / 1000) + 2.85) * diff.nv[id.ave]) * conv

# Cost calculation amphibians and reptiles
# estimated by: Gasçon, C., Collins, J.P., Moore, R.D., Church, D.R., McKay,
# J.E., and Mendelson, J.R. III (eds.) (2007). Amphibian Conservation Action
# Plan. (Gland and Cambridge: eudat/SSC Amphibian Specialist Group).
# costs for one species/year in captive facility, in US$), ACAP page 37
# Species preservation in captivity in 2 separate populations
# Amphibian captive facility 25,000 x 2 = 50,000 USD
# Maintenance and overhead, 10,000 x 2 = 20,000
# Amphibian keeper staff 1 per population x 2 = 50,000

# logical vector to indicate reptiles and amphibians
herps <- eudat$class %in% c("Reptilia", "Amphibia")

# One-off costs:
# Ideally we would like at least 2 separate populations with a total number of
# individuals of at least 100. Additional costs to reach this goal depend on what
# is currently present in EU zoos. We differentiate the following scenarios:

# Species with
# 1)  no population in captivity
id.1 <- which(eudat$NrIndivEU == 0 & herps)
# 2) less than 100 individuals in only a single institution
id.2 <-
  which(eudat$NrIndivEU < nv & eudat$NrIndivEU != 0 &
          eudat$NrZoosEU == 1 & herps)
# 3) less than 100 individuals in more than one institution
id.3 <-
  which(eudat$NrIndivEU < nv & eudat$NrIndivEU != 0 &
          eudat$NrZoosEU >= 2 & herps)

# 4) more than 100 individuals in a single institution
id.4 <-
  which(eudat$NrIndivEU >= nv &
          eudat$NrZoosEU == 1 & herps)
# 5) more than 100 individuals in more than one institution
id.5 <-
  which(eudat$NrIndivEU >= nv &
          eudat$NrZoosEU >= 2 & herps)

# We assume one-off costs for facility building etc, need to be repeated every
# 10 years the remaining 90 years  we only have maintenance costs

cost.captive[id.1] <-
  (125000 * 10 +  71250 * 90) * conv # case 1 - full costs
cost.captive[id.2] <-
  (95000 *  10 + 53437 * 90) * conv # case 2 - reduced costs
cost.captive[id.3] <-
  (65000 *  10 + 35625 * 90) * conv  # case 3 - reduced costs
cost.captive[id.4] <-
  (32500 * 10 + 17813 * 90) * conv  # case 4 - reduced costs
cost.captive[id.5] <- 0 # case 5- no addtional costs

eudat <- cbind(eudat, cost.captive)


# Output ------------------------------------------------------------------

# write.csv(eudat,
#           "EUSpeciesDataFinal01.csv",
#           row.names = F)
