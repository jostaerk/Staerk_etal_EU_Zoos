# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: A decision framework to integrate in-situ and ex-situ management for
# species in the European Union
# Author: Johanna Staerk
# Description:

# This code analyses:
# number of species in EU zoos
# number of threatened species in EU zoos
# population sizes of threatened species in EU zoos
# number of Species360 institutions in EU
# Costs to protect all EU species
# Other threat indices (AZE, Climate change etc.)

# Plot 1: Number of species in zoos compared to total
# Plot 2: Number of species per threat category in zoos compared to total
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

# EU species
eudat    <- read.csv(
  "DATA/EUSpeciesDataFinal01.csv",
  na.strings = c("", " ", "NA"),
  stringsAsFactors = F
)
# remove excluded species
eudat <- eudat[which(eudat$excluded == 0),]

# data available upon request
Species360 <-
  read.csv("DATA/Species360Data00_GBIF_checked.csv")

# 1.) Species-------------------------------------------------------------------

# Number of species in EU zoos
t1 <- table(eudat$Species.Zoo.EU)
t1
round(prop.table(t1), 3) * 100

t1.2 <- table(eudat$Species.Zoo.EU, eudat$class)
t1.2
round(prop.table(t1.2, 2), 2) * 100


# Number of EU threatened (only IUCN) species in EU zoos
eudat$THR <-
  ifelse(eudat$RL.catEU27 %in% c("RE", "CR", "EN", "VU"), 1, 0)

t2 <- table(eudat$Species.Zoo.EU, eudat$THR)
t2
round(prop.table(t2, 2), 3) * 100

t2.2 <- table(eudat$class, eudat$Species.Zoo.EU, eudat$THR)
t2.2
prop.table(t2.2, c(1, 3))

# Number of species Thr by Climate change in zoos
table(eudat$VC.category)
t3.1 <-
  table(eudat[which(!(eudat$class == "Mammalia")), "Species.Zoo.EU"],
        eudat[which(!(eudat$class == "Mammalia")), "VC.category"])
t3.1
prop.table(t3.1, 2) * 100

t3.2 <- table(eudat$class, eudat$Species.Zoo.EU, eudat$VC.category)
t3.2
prop.table(t3.2, c(1, 3))

# Number of species that are AZE
t4.1 <- table(eudat$Species.Zoo.EU, eudat$Aze)
t4.1
prop.table(t4.1, 2) * 100

# Number of species that are EDGE
t5.1 <- table(eudat$Species.Zoo.EU, eudat$EDGE)
t5.1
prop.table(t5.1, 2) * 100

# Number of species that are managed
eudat$EAZA <- ifelse(eudat$EEP == 1 | eudat$ESB == 1, 1, 0)
table(eudat$EAZA)
table(eudat$EEP)
table(eudat$ESB)
table(eudat$RLExsitu, eudat$Aark)
table(eudat$EAZA, eudat$THR)

eudat$AllExsitu <- rowSums(eudat[, c("Aark", "RLExsitu", "EAZA")])
eudat$AllExsitu <- ifelse(eudat$AllExsitu >= 1, 1, 0)
table(eudat$AllExsitu)
table(eudat$AllExsitu, eudat$THR)

# Number of endemic species
table(eudat$endemic.europe) # endemic in Europe accord. to IUCN
table(eudat$endemic.eu) # endemic in EU accord. to IUCN
table(eudat$endemic) # endemic in Europe with at least one country in EU (manually checked on IUCN RL European online assessments)
eudat[which(eudat$endemic.europe == "Yes*"), "endemic.europe"] <-
  "Yes"
eudat[which(eudat$endemic.eu == "Yes*"), "endemic.eu"] <- "Yes"
t6.1 <- table(eudat$Species.Zoo.EU, eudat$endemic.eu)
t6.1
prop.table(t6.1, 2) * 100

t6.2 <- table(eudat$Species.Zoo.EU, eudat$endemic)
t6.2
prop.table(t6.2, 2) * 100

t6.3 <- table(eudat$Species.Zoo.EU, eudat$endemic.europe)
t6.3
prop.table(t6.3, 2) * 100

# Number of sepcies threatend or vuln. to climate change in EU zoos
table(eudat$THR_CCH)
t7 <- table(eudat$Species.Zoo.EU, eudat$THR_CCH)
t7
round(prop.table(t7, 2), 3) * 100


#  2.) Institutions ------------------------------------------------------------

# ID for EU zoos
id.eu <- which(Species360$EU.zoo == 1)

# Number of institutions in Species360 worldwide
s3 <- length(unique(Species360$Mnemonic))
s3

# Number of institutions in EU
s4 <- length(unique(Species360[id.eu, "Mnemonic"]))
s4

# Percentage of EU institutions compared to worldwide
(s4 / s3) * 100


# 3.) Total Costs  -------------------------------------------------------------

# Total costs wild
sum(eudat$cost.wild.adjust, na.rm = T) # 4,808,106

# Total costs zoos
sum(eudat$cost.captive, na.rm = T) # 1,056,576,245


# 4.) Poisson regression--------------------------------------------------------

# Summary statistics of population size of threatened species
summary(eudat[which(eudat$THR == 1 &
                      eudat$Species.Zoo.EU == 1), "NrIndivEU"])
# Threatnend species with largest population
eudat %>% filter(THR == 1) %>% arrange(desc(NrIndivEU)) %>% head()

# Summary statistics of population size of non-threatened species
summary(eudat[which(eudat$THR == 0 &
                      eudat$Species.Zoo.EU == 1), "NrIndivEU"])

# Summary statistics of threatened species per class
summary(eudat[which(eudat$class == "Amphibia" &
                      eudat$THR == 1 &
                      eudat$Species.Zoo.EU == 1), "NrIndivEU"])
summary(eudat[which(eudat$class == "Aves" &
                      eudat$THR == 1 &
                      eudat$Species.Zoo.EU == 1), "NrIndivEU"])
summary(eudat[which(eudat$class == "Mammalia" &
                      eudat$THR == 1 &
                      eudat$Species.Zoo.EU == 1), "NrIndivEU"])
summary(eudat[which(eudat$class == "Reptilia" &
                      eudat$THR == 1 &
                      eudat$Species.Zoo.EU == 1), "NrIndivEU"])


# Quasi-Poisson model:
m.tot <-
  glm(eudat$NrIndivEU ~ factor(eudat$THR) + factor(eudat$class),
      family = "quasipoisson")
summary(m.tot)


# Plot 1: Boxplot number of individuals ----------------------------------------

col <- c("#027F7F", "#DB4A7A")

# function for labels (number of observations)
give.n <- function(x) {
  return(c(y = max(x) * 1.04, label = length(x)))
}
eudat$THR <- ifelse(eudat$THR == 1, "Threatened", "Non-threatened")
g1 <- ggplot(eudat[which(eudat$Species.Zoo.EU == 1), ],
             aes(
               x = factor(class, levels = c(
                 "Mammalia", "Aves", "Reptilia", "Amphibia"
               )),
               y = NrIndivEU,
               fill = factor(THR)
             )) +
  geom_boxplot(outlier.size = 1,
               alpha = 1) +
  ylab("Number of individuals in EU zoos") +
  xlab("") +
  scale_y_continuous(trans = "log",
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 250,  500, 1000)) +
  scale_fill_manual(values = col) +
  stat_summary(
    fun.data = give.n,
    geom = "text",
    fun = median,
    position = position_dodge(width = 0.75),
    size = 3
  ) +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_blank()
  )
g1


#  Plot 2 EU species by THR category--------------------------------------------
eudat$thr_cch <- eudat$RL.catEU27
id <-
  which(eudat$RL.catEU27 %in% c("LC", "NT") &
          eudat$VC.category == "H")
eudat$thr_cch[id] <- paste(eudat$thr_cch[id], "+K")


table(eudat$thr_cch)
df2 <- eudat[, c("class", "Species.Zoo.EU", "thr_cch")]

t5 <- table(df2$thr_cch, df2$Species.Zoo.EU, df2$class)
t5
prop.table(t5, c(1, 3))
df2 <- as.data.frame(t5)
colnames(df2) <- c("eudatStatus", "zoo", "Class", "Freq")
df2$zoo <- factor(df2$zoo, levels = c(0, 1))
df2$eudatStatus <-
  factor(df2$eudatStatus, levels = rev(c(
    "RE", "CR", "EN", "VU", "NT +K", "NT", "LC +K", "LC", "DD"
  )))
df2 <- arrange(df2, zoo, eudatStatus)
df2$Class <-
  factor(df2$Class, levels = c("Mammalia", "Aves", "Reptilia", "Amphibia"))
df2 <- df2[which(df2$Freq > 0), ]

# Percent labels
library(plyr)
df2 <-
  ddply(df2,
        .(Class, eudatStatus),
        transform,
        percent = round(Freq / sum(Freq) * 100, 2))
df2$label = paste0(sprintf("%.0f", df2$percent), "%")
df2[which(df2$zoo == "0"), "label"] <- ""

g2 <- ggplot(df2, aes(x = eudatStatus, y = Freq , fill = zoo)) +
  geom_bar(stat = "identity", width = 0.75) +
  geom_text(
    aes(label = label),
    position = "stack",
    hjust = -1,
    size = 3
  ) +
  ylab("Number of species") +
  coord_flip() +
  xlab("") +
  scale_fill_manual(values = c("grey", "#027F7F")) +
  facet_wrap( ~ Class, nrow = 2, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank())

g2
detach(package:plyr, unload = TRUE)

# Numbers for doughnut charts

# % of threatened species in zoos
eudat$THR <- ifelse(eudat$THR == "Threatened", 1, 0)
thr <- eudat[which(eudat$THR == 1),]
t <- table(thr$Species.Zoo.EU, thr$class)
t
round(prop.table(t, 2) * 100, 0)
# % of non-threatened species in zoos
nthr <- eudat[which(eudat$THR == 0),]
t <- table(nthr$Species.Zoo.EU, nthr$class)
t
round(prop.table(t, 2) * 100, 0)


# SUP Data 1 Files ----

# Save supplemental data file tab 1: All EU species
eudat2 <- eudat %>% select(gbif.species,
                           gbif.id = gbif.species.id,
                           iucn.species = db.species,
                           common.name,
                           genus,
                           family,
                           order,
                           class,
                           excluded,
                           exclusion.reason,
                           redlist.europe = RLcat.europe,
                           redlist.eu = RL.catEU27,
                           endemic.europe.iucn = endemic.europe,
                           endemic.eu.iucn = endemic.eu,
                           endemic.europe.final = endemic,
                           vuln.clim.change = VC.category,
                           aze = Aze,
                           target.area.km2 = TargetAreakm2,
                           protected.area.km2 = ProtectedAreakm2,
                           gap.km2 = Gap.km2,
                           perc.target.met.in.PAs.and.N2K,
                           zoo.global = Species.Zoo.global,
                           zoo.eu = Species.Zoo.EU,
                           genus.zoo.global = Genus.Zoo.Global,
                           pop.zoo.global = NrIndivGlobal,
                           pop.zoo.eu = NrIndivEU,
                           max.pop.eu = MaxPopSizeEU,
                           second.max.pop.eu = SecondMaxPopSizeEU,
                           nr.zoos.global = NrZoosGlobal,
                           nr.zoos.eu = NrZoosEU,
                           eep = EEP,
                           esb = ESB,
                           aark = Aark,
                           redlist.exsitu = RLExsitu,
                           redlist.exsitu.needed = RLExsituNeeded,
                           ed.value = ED.score,
                           edge = EDGE,
                           bodymass.g = Bodymass.g,
)
# save as supplemental data file
#write.csv(eudat2, "RESULTS/SupplementalFile1_EUSpeciesData.csv", row.names = F)




