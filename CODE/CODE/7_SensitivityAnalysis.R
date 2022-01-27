# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: A decision framework to integrate in-situ and ex-situ management for
# species in the European Union
# Author: Johanna Staerk
# Description: Sensitivity analysis of judgement values, probability assigned to
# climate change vulnerbility and Breeding success probabilities
# To run this script, read in the data provided in Annex A

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INIT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list = ls())
dev.off()
cat('\014')
options(scipen = 20)

setwd("/Users/johannas/Dropbox/PROJECTS/EU.Zoos/EU/ANALYSIS/")

if (!"tidyverse" %in% installed.packages())
  install.packages("tidyverse")
suppressWarnings(library(tidyverse))

if (!"colorRamps" %in% installed.packages())
  install.packages("colorRamps")
suppressWarnings(library(colorRamps))

if (!"RColorBrewer" %in% installed.packages())
  install.packages("RColorBrewer")
suppressWarnings(library(RColorBrewer))


# Call functions
source("CODE/CODE_JS/ForSubmission/CODE/6_Functions.R")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# INPUT ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

eudat    <-
  read.csv("RESULTS/Ranking/RankingEUSpecies.csv",
           stringsAsFactors = F)


# 1.) Climate change (K)
Kh <- 0.3 # Probability highly vulnerale
Kl <- 0 # # Probability low vulnerale
nv <- 20
KhVec <- seq(0.01, 0.5, length = nv) # different values for sens. analysis
KlVec <- seq(0.0, 0.2, length = nv)
Kgr <- expand.grid(Kh = KhVec, Kl = KlVec) # different combinations (one is kept constant the other is varied)

# 2.) Probability of persistence in captivity with different levels of management expertise (G)
G1 <- 0.05 # when neither species nor genus is in a zoo
G2 <- 0.1  # when genus is in a zoo
G3 <- 0.2  # when species is in any zoo globally

ns <- 20
Gmat <- cbind(G1 = seq(0.01, 0.5, length = ns),
              G2 = seq(0.01, 0.5, length = ns),
              G3 = seq(0.05, 0.5, length = ns))
Ggr <- expand.grid(G1 = Gmat[, 'G1'], G2 = Gmat[, 'G2'], G3 = Gmat[, 'G3'])

# 3.) Decision makers judgement values (V)
Vz <- 0.1  # zoo
Vb <- 1  # both
Vw <- 1  # wild
Vx <- 0     # extinct

nh <- 20
VzVec <- seq(0.01, 0.3, length = nh)
VbVec <- seq(0.75, 1, length = nh)
DeltaVz <- diff(VzVec[1:2])
DeltaVb <- diff(VbVec[1:2])
Vgr <- expand.grid(Vz = VzVec, Vb = VbVec, Vw = Vw, Vx = Vx)

# Number of individuals for a viable population
Nv <- 100

# Output matrices:

  Gbenmat <- matrix(0, nrow(Ggr), 4)
  Vbenmat <- matrix(0, nrow(Vgr), 4)
  Kbenmat <- matrix(0, nrow(Kgr), 4)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# RUN RESULTS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  #
   for (j in 1:3) {
    niter <- ifelse(j == 1, nrow(Ggr), ifelse(j == 2, nrow(Vgr), nrow(Kgr)))
    cat(sprintf("Analysis for %s\n", c("G", "V", "K")[j]))
    progrBar <- txtProgressBar(min = 1, max = niter, style = 3)
    if (j == 1) {
      Vzi <- Vz
      Vbi <- Vb
      Vwi <- Vw
      Vxi <- Vx
      Khi <- Kh
      Kli <- Kl
    } else if (j == 2) {
      G1i <- G1
      G2i <- G2
      G3i <- G3
      Khi <- Kh
      Kli <- Kl
    } else {
      Vzi <- Vz
      Vbi <- Vb
      Vwi <- Vw
      Vxi <- Vx
      G1i <- G1
      G2i <- G2
      G3i <- G3
    }
    for (i in 1:niter) {
      if (j == 1) {
        G1i <- Ggr[i, "G1"]
        G2i <- Ggr[i, "G2"]
        G3i <- Ggr[i, "G3"]
      } else if (j == 2){
        Vzi <- Vgr[i, "Vz"]
        Vbi <- Vgr[i, "Vb"]
        Vwi <- Vgr[i, "Vw"]
        Vxi <- Vgr[i, "Vx"]
      } else {
        Khi <- Kgr[i, "Kh"]
        Kli <- Kgr[i, "Kl"]
      }


      # Replace climate change column with new values
      eudat[which(eudat$vuln.clim.change == "H"),"Ki"] <- Khi
      eudat[which(eudat$vuln.clim.change == "L"),"Ki"] <- Kli
      eudat[which(is.na(eudat$vuln.clim.change)),"Ki"] <- Kli

      # Calculate persistence probability in zoos (P_i2) given different levels of management
      eudat$Gi <- PersistZoo(zoo.eu = eudat$zoo.eu,
                                      zoo.global = eudat$zoo.global,
                                      genus.zoo.global = eudat$genus.zoo.global,
                                      pop.zoo.eu = eudat$pop.zoo.eu, Nv = Nv,
                                      G1 = G1i, G2 = G2i, G3 = G3i)

      # Calculate Expected benefit
      DecisionDf <- CalcBenefits(Ai =  eudat$Ai, Ii = eudat$Ii, Ki = eudat$Ki,
                                   Gi = eudat$Gi, pop.zoo.eu = eudat$pop.zoo.eu, Nv = Nv,
                                   cost.wild = eudat$cost.wild,
                                   cost.captive = eudat$cost.captive,
                                   Vz = Vzi, Vb = Vbi, Vw = Vwi, Vx = Vxi)
      out <- apply(DecisionDf[, 1:4], 2, mean, na.rm = T) # Take mean benefit
      if (j == 1) {
        Gbenmat[i, ] <- out
      } else if (j == 2){
        Vbenmat[i, ] <- out
      } else {
        Kbenmat[i, ] <- out
      }
      setTxtProgressBar(progrBar, i)
    }
  }

  colnames(Gbenmat) <- colnames(Vbenmat) <- colnames(Kbenmat) <- names(out)

#   save(list = c("Vbenmat", "Gbenmat", "Kbenmat", "Vgr", "Ggr", "Kgr"),
#        file = "RESULTS/SensitivityAnalysis/SensAnalysisFC_03.RData")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# RUN RESULTS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Heatmap for breeding success (G) ----

slevs <- c("G1", "G2", "G3")
svals <- c(G1, G2, G3)
strats <- c("nothing", "zoo", "both", "wild")
mlist <- list()
axlist <- list()
for (j in 1:length(slevs)) {
  mlist[[slevs[j]]] <- list()
  idS <- which(abs(Ggr[[slevs[j]]] - svals[j]) ==
                 min(abs(Ggr[[slevs[j]]] - svals[j])))
  axlist[[slevs[j]]] <- list(x = unique(Ggr[idS, -j][, 1]),
                             y = unique(Ggr[idS, -j][, 2]))
  for (i in 1:length(strats)) {
    mlist[[slevs[j]]][[strats[i]]] <-
      matrix(Gbenmat[idS, sprintf("ev.%s", strats[i])], ns, ns)
  }
}

rans <- apply(Gbenmat[, paste("ev.", strats, sep = "")], 2, range, na.rm = T)
rana.palette <- colorRampPalette(brewer.pal(9, "Greens")[-1], space = "rgb")
ranc.palette <- colorRampPalette(brewer.pal(9, "Reds")[-1], space = "rgb")
nc <- 40
cols <- c(rana.palette(nc), ranc.palette(nc))
cols <- rana.palette(nc)
cols <- matlab.like2(nc)
colv <- sapply(1:length(strats),
               function(i) seq(rans[1, i], rans[2, i], length = length(cols)))
# Draw plots:
whr <- 3.5 / 4.25
# pdf(file = "RESULTS/PLOTS/SensToS_2.pdf", width = 8 * whr, height = 8)
layout(cbind(c(0,4:7), c(1, 12:15), c(2, 16:19), c(3, 20:23), c(0, 8:11)),
       widths = c(0.25, rep(1, 3), 0.25), heights = c(0.25, rep(1, 4)))
par(mar = c(0, 4, 0, 1))
for (j in 1:length(slevs)) {
  plot(c(0, 1), c(0, 1), col = NA, axes = F, xlab = "", ylab = "")
  text(0.5, 0.5, sprintf("%s = %s", slevs[j], svals[j]), cex = 2)
}
par(mar = c(4, 0, 1, 0))
for (i in 1:length(strats)) {
  plot(c(0, 1), c(0, 1), col = NA, axes = F, xlab = "", ylab = "")
  text(0.5, 0.5, strats[i], srt = 90, cex = 1.5)
}

for (i in 1:length(strats)) {
  par(mar = c(4, 0, 1, 0))
  plot(c(0, 1), c(0, 1), col = NA, axes = F, xlab = "", ylab = "")
  points(rep(0.05, nc), seq(0.05, 0.95, length = nc), pch = 15, col = cols,
         cex = 1.2)
  text(c(0.5, 0.5), c(0.1, 0.9), signif(rans[, i], 2))
}

par(mar = c(4, 4, 1, 1))
for (j in 1:length(slevs)) {
  for (i in 1:length(strats)) {
    plmat <- mlist[[slevs[j]]][[strats[i]]]
    x <- axlist[[slevs[j]]]$x
    y <- axlist[[slevs[j]]]$y
    ranm <- range(plmat, na.rm = T)
    colij <- cols[which(colv[, i] >= ranm[1] & colv[, i] <= ranm[2])]
    image(x, y, plmat, xlab = "", ylab = "", col = colij)
    contour(x, y, plmat, add = T)
    #if (j == 2 & i == 4) mtext(slevs[-j][1], 1, line = 2.5)
    mtext(slevs[-j][1], 1, line = 2.5)
    #if (i == 3) mtext(slevs[-j][2], 2, las = 2, line = 2.5, at = 0.55)
    mtext(slevs[-j][2], 2, las = 2, line = 2.5)
    points(svals[-j][1], svals[-j][2], pch = 19, col = 'grey40', cex = 3)
    points(svals[-j][1], svals[-j][2], pch = 19, col = 'white')
  }
}
dev.off()
# Heatmap Judgment values (H) ----------------

hlevs <- c("Vw", "Vb", "Vz", "Vx")
hvals <- c(Vw, Vb, Vz, Vx)
hlabs <- c("Wild", "Both", "Zoo", "Extinct")
strats <- c("nothing", "zoo", "both", "wild")
mlist <- list()
axlist <- list()
for (j in 1:length(hlevs)) {
  mlist[[hlevs[j]]] <- list()
  idH <- which(abs(Vgr[[hlevs[j]]] - hvals[j]) ==
                 min(abs(Vgr[[hlevs[j]]] - hvals[j])))
  axlist[[hlevs[j]]] <- list(x = unique(Vgr[idH, -j][, 1]),
                             y = unique(Vgr[idH, -j][, 2]))
  for (i in 1:length(strats)) {
    mlist[[hlevs[j]]][[strats[i]]] <-
      matrix(Vbenmat[idH, sprintf("ev.%s", strats[i])], ns, ns)
  }
}

# Calculate average sensitivities:
j <- 1
mlist[[1]] <- list()
idH <- which(abs(Vgr[[2]] - hvals[2]) ==
               min(abs(Vgr[[1]] - hvals[1])))
axlist[[1]] <- list(x = unique(Vgr[, "Vz"]),
                    y = unique(Vgr[, "Vb"]))
for (i in 1:length(strats)) {
  mlist[[1]][[strats[i]]] <-
    matrix(Vbenmat[, sprintf("ev.%s", strats[i])], ns, ns)
}

ranh <- apply(Vbenmat[, paste("ev.", strats, sep = "")], 2, range, na.rm = T)
nc <- 40
colv <- sapply(1:length(strats),
               function(i) seq(ranh[1, i], ranh[2, i], length = length(cols)))
# Draw plots:
#pdf("RESULTS/PLOTS/SensToH_02.pdf", width = 4, height = 4 * 4.25 / 1.35)
layout(cbind(c(0, 2:5), c(1, 10:13), c(0, 6:9)), widths = c(0.15, 1, 0.25),
       heights = c(0.25, rep(1, 4)))
par(mar = c(0, 4, 0, 1))
plot(c(0, 1), c(0, 1), col = NA, axes = F, xlab = "", ylab = "")
text(0.5, 0.5, sprintf("%s = %s\n%s = %s", hlevs[1], hvals[1],
                       hlevs[4], hvals[4]), cex = 2)

par(mar = c(4, 0, 1, 0))
for (i in 1:length(strats)) {
  plot(c(0, 1), c(0, 1), col = NA, axes = F, xlab = "", ylab = "")
  text(0.5, 0.5, strats[i], srt = 90, cex = 1.5)
}
par(mar = c(4, 0, 1, 0))
for (i in 1:length(strats)) {
  plot(c(0, 1), c(0, 1), col = NA, axes = F, xlab = "", ylab = "")
  points(rep(0.05, nc), seq(0.05, 0.95, length = nc), pch = 15, col = cols,
         cex = 1.2)
  text(c(0.5, 0.5), c(0.1, 0.9), signif(ranh[, i], 2))
}

par(mar = c(4, 4, 1, 1))
for (i in 1:length(strats)) {
  plmat <- mlist[[1]][[strats[i]]]
  x <- axlist[[1]]$x
  y <- axlist[[1]]$y
  ranm <- range(x, y, plmat, na.rm = T)
  colij <- cols[which(colv[, i] >= ranm[1] & colv[, i] <= ranm[2])]
  image(x, y, plmat, xlab = "", ylab = "", col = colij)
  contour(x, y, plmat, add = T)
  if (i == 4) mtext(hlevs[3], 1, line = 2.5)
  if (i == 3) mtext(hlevs[2], 2, las = 2, line = 2.5, at = 1.15)
  points(hvals[3], hvals[2], pch = 19, col = 'grey40', cex = 3)
  points(hvals[3], hvals[2], pch = 19, col = 'white', cex = 1)
}
dev.off()



# Heatmap Vulnerability to climate change (V) ----------------------------------
Klevs <- c("Kh", "Kl")
vvals <- c(Kh, Kl)
strats <- c("nothing", "zoo", "both", "wild")
mlist <- list()
axlist <- list(x = unique(Kgr[, 1]), y = unique(Kgr[, 2]))

for (i in 1:length(strats)) {
  mlist[[strats[i]]] <-
    matrix(Vbenmat[, sprintf("ev.%s", strats[i])], ns, ns)
}

rans <- apply(Vbenmat[, paste("ev.", strats, sep = "")], 2, range, na.rm = T)
rana.palette <- colorRampPalette(brewer.pal(9, "Greens")[-1], space = "rgb")
ranc.palette <- colorRampPalette(brewer.pal(9, "Reds")[-1], space = "rgb")
nc <- 40
cols <- c(rana.palette(nc), ranc.palette(nc))
cols <- rana.palette(nc)
cols <- matlab.like2(nc)
colv <- sapply(1:length(strats),
               function(i) seq(rans[1, i], rans[2, i], length = length(cols)))

# Plot
idv <- which(Kgr[, 2] == 0)
ylim <- range(Kbenmat)
x <- axlist$x
dx <- x[2] - x[1]
iddef <- which(abs(x - 0.3) == min(abs(x - 0.3)))
par(mfrow = c(4, 1), mar = c(4, 4, 1, 1))
for (i in 1:length(strats)) {
  y <- Kbenmat[idv, sprintf("ev.%s", strats[i])]

  ranm <- range(y, na.rm = T)
  colij <- cols[which(colv[, i] >= ranm[1] & colv[, i] <= ranm[2])]
  plot(x, y, col = NA, xlab = "", ylab = "", ylim = ylim)
  for (j in 1:length(x)) {
    xx <- x[j] + dx * c(-0.45, 0.45)
    yy <- c(ylim[1], y[j])
    polygon(c(xx, rev(xx)), yy[c(1, 1, 2, 2)], col = colij[j],
            border = NA)
  }
  # image(x, y, plmat, xlab = "", ylab = "", col = colij)
  # contour(x, y, plmat, add = T)
  #if (j == 2 & i == 4) mtext(slevs[-j][1], 1, line = 2.5)
  if (i == 4) {
    mtext(Klevs[1], 1, line = 2.5)
  }

  #if (i == 3) mtext(slevs[-j][2], 2, las = 2, line = 2.5, at = 0.55)
  mtext(strats[i], 2, las = 3, line = 2.5)
  points(x[iddef], y[iddef], pch = 19, col = 'grey40', cex = 3)
  points(x[iddef], y[iddef], pch = 19, col = 'white')
}

# Sensitivity of changes in strategies ----

# Changes in strategies when varying judgment values (H)
DecisionBaseline <-  CalcBenefits(
  Ai = eudat$Ai,
  Ii = eudat$Ii,
  Ki = eudat$Ki,
  Gi = eudat$Gi,
  pop.zoo.eu = eudat$pop.zoo.eu,
  Nv = Nv,
  cost.wild = eudat$cost.wild,
  cost.captive = eudat$cost.captive,
  Vz = Vz,
  Vw = Vw,
  Vb = Vb,
  Vx = Vx
)

strategyBaseline <- ChooseStrategy(Wild = DecisionBaseline$benefit.wild,
                             Zoo = DecisionBaseline$benefit.zoo,
                             Both = DecisionBaseline$benefit.both)
table(strategyBaseline)

sensOutV <- matrix(NA, nh, 2, dimnames = list(NULL, c("Vz", "Vb")))

for (j in 1:2) {
  Vzi <- Vz
  Vbi <- Vb

  for (i in 1:nh) {
    if (j == 1) {
      Vzi <- VzVec[i]
      DH <- Vz - Vzi
    } else {
      Vbi <- VbVec[i]
      DH <- Vb - Vbi
    }

    # Expected benefit
    DecisionDf <- CalcBenefits(
      Ai = eudat$Ai,
      Ii = eudat$Ii,
      Ki = eudat$Ki,
      Gi = eudat$Gi,
      pop.zoo.eu = eudat$pop.zoo.eu,
      Nv = Nv,
      cost.wild = eudat$cost.wild,
      cost.captive = eudat$cost.captive,
      Vz = Vzi,
      Vw = Vw,
      Vb = Vbi,
      Vx = Vx
    )

    NetStrategy <- ChooseStrategy(Wild = DecisionDf$benefit.wild,
                            Zoo = DecisionDf$benefit.zoo,
                            Both = DecisionDf$benefit.both)

    sensOutV[i, j] <- length(which(NetStrategy != strategyBaseline))
  }
}

# Plot changes in strategies (Vb)
sensOutV
plot(VbVec, sensOutV[, "Vb"], xlab = expression(paste(italic("V"),"(1,1)")),
     ylab = "Change in Strategy", pch=16)

dev.off()
# Plot of % change in strategy (Vb)
plot(VbVec, round((sensOutV[, "Vb"]/nrow(DecisionDf))*100, 3), xlab = expression(paste(italic("V"),"(1,1)")),
     ylab = "% Change in Strategy", pch=16)


# Changes in strategies when varying breeding success probabiliy based on management expertise (Gi)-----------
eudat$Gi <- PersistZoo(
  eudat$zoo.eu,
  eudat$zoo.global,
  eudat$genus.zoo.global,
  eudat$pop.zoo.eu,
  Nv = Nv,
  G1 = G1,
  G2 = G2,
  G3 = G3
)

DecisionBaseline <-  CalcBenefits(
  Ai = eudat$Ai,
  Ii = eudat$Ii,
  Ki = eudat$Ki,
  Gi = eudat$Gi,
  pop.zoo.eu = eudat$pop.zoo.eu,
  Nv = Nv,
  cost.wild = eudat$cost.wild,
  cost.captive = eudat$cost.captive,
  Vz = Vz,
  Vw = Vw,
  Vb = Vb,
  Vx = Vx
)
strategyBaseline <- ChooseStrategy(Wild = DecisionBaseline$benefit.wild,
                                   Zoo = DecisionBaseline$benefit.zoo,
                                   Both = DecisionBaseline$benefit.both)

sensOutG <- matrix(NA, nh, 3, dimnames = list(NULL, c("G1", "G2", "G3")))

for (j in 1:3) {
  G1i <- G1
  G2i <- G2
  G3i <- G3

  for (i in 1:nh) {
    if (j == 1) {
      G1i <- Gmat[, 'G1'][i]
      DH <- G1 - G1i
    } else if (j == 2){
      G2i <- Gmat[, 'G2'][i]
      DH <- G2 - G2i
    } else {
      G3i <- Gmat[, 'G3'][i]
      DH <- G3 - G3i
    }

    # Expected benefit
    eudat$Gi <- PersistZoo(
      eudat$zoo.eu,
      eudat$zoo.global,
      eudat$genus.zoo.global,
      eudat$pop.zoo.eu,
      Nv = Nv,
      G1 = G1i,
      G2 = G2i, G3 = G3i
    )

    DecisionDf2 <-  CalcBenefits(
      Ai = eudat$Ai,
      Ii = eudat$Ii,
      Ki = eudat$Ki,
      Gi = eudat$Gi,
      pop.zoo.eu = eudat$pop.zoo.eu,
      Nv = Nv,
      cost.wild = eudat$cost.wild,
      cost.captive = eudat$cost.captive,
      Vz = Vz,
      Vw = Vw,
      Vb = Vb,
      Vx = Vx
    )

    NetStrategy2 <- ChooseStrategy(Wild = DecisionBaseline$benefit.wild,
                                   Zoo = DecisionBaseline$benefit.zoo,
                                   Both = DecisionBaseline$benefit.both)

    sensOutG[i, j] <- length(which(NetStrategy2 != strategyBaseline))
  }
}

# Plot changes in strategies
sensOutG
plot(Gmat[, 'G1'], sensOutG[, "G1"])
plot(Gmat[, 'G2'], sensOutG[, "G2"])
plot(Gmat[, 'G3'], sensOutG[, "G3"])


# Changes in strategies when varying Climate change extinction probabiliy (Ki)-----------
eudat$Gi <- PersistZoo(
  eudat$zoo.eu,
  eudat$zoo.global,
  eudat$genus.zoo.global,
  eudat$pop.zoo.eu,
  Nv = Nv,
  G1 = G1i,
  G2 = G2i, G3 = G3i
)
DecisionBaseline <-  CalcBenefits(
  Ai = eudat$Ai,
  Ii = eudat$Ii,
  Ki = eudat$Ki,
  Gi = eudat$Gi,
  pop.zoo.eu = eudat$pop.zoo.eu,
  Nv = Nv,
  cost.wild = eudat$cost.wild,
  cost.captive = eudat$cost.captive,
  Vz = Vz,
  Vw = Vw,
  Vb = Vb,
  Vx = Vx
)
strategyBaseline <- ChooseStrategy(Wild = DecisionBaseline$benefit.wild,
                                   Zoo = DecisionBaseline$benefit.zoo,
                                   Both = DecisionBaseline$benefit.both)

sensOutK <- matrix(NA, nh, 1, dimnames = list(NULL, c("Kh")))

  for (i in 1:nh) {

      Khi <- KhVec[i]
      DH <- Kh - Khi

      # Climate change
      eudat[which(eudat$vuln.clim.change == "H"),"Ki"] <- Khi

    # Expected benefit

    DecisionDf2 <-  CalcBenefits(
      Ai = eudat$Ai,
      Ii = eudat$Ii,
      Ki = eudat$Ki,
      Gi = eudat$Gi,
      pop.zoo.eu = eudat$pop.zoo.eu,
      Nv = Nv,
      cost.wild = eudat$cost.wild,
      cost.captive = eudat$cost.captive,
      Vz = Vz,
      Vw = Vw,
      Vb = Vb,
      Vx = Vx
    )

    NetStrategy2 <- ChooseStrategy(Wild = DecisionBaseline$benefit.wild,
                                   Zoo = DecisionBaseline$benefit.zoo,
                                   Both = DecisionBaseline$benefit.both)

    sensOutK[i] <- length(which(NetStrategy2 != strategyBaseline))
  }
sensOutK

