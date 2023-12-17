# Functions --------------------------------------------------------------------

# Determine level of management (1, 2, 3 or 4)
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


# Calculate level of management expertise
CalcManagementExpertise <-
  function(zoo.eu,
           zoo.global,
           genus.zoo.global,
           pop.zoo.eu,
           Nv,
           G1,
           G2,
           G3) {
    # zoo.eu = Species in a zoo EU, (1=yes, 0=no)
    # zoo.global = Species in a global zoo (1=yes, 0=no)
    # genus.zoo.global = Species is not in any zoo, but related species of same
    # genus is in a global zoo (1=yes, 0=no)
    # pop.zoo.eu = Total number of individuals across all EU zoos (metapopulation)
    # Nv = Number of individuals required for viable population
    # G1 breeding success probability when neither species nor genus is in a zoo
    # G2 breeding success probability when genus is in a zoo
    # G3 breeding success probabilitywhen species is in any zoo globally

    x <- rep(0, length(zoo.eu))

    for (i in 1:length(x)) {
      if (zoo.eu[i] == 1) {
        x[i] = G3 + (1 - G3) * min(1, (pop.zoo.eu[i] / Nv))
      } else {
        if (zoo.global[i] == 1) {
          x[i] <- G3
        } else {
          if (genus.zoo.global[i] == 1) {
            x[i] <- G2
          } else {
            x[i] <- G1
          }
        }
      }
    }

    return(x)
  }

CalcBenefits <-
  function(Ai,
           Ii,
           Ki,
           pop.zoo.eu,
           Nv,
           Gi,
           cost.wild,
           cost.captive,
           Vz,
           Vw,
           Vb,
           Vx) {
    # Ai = percent protected area
    # Ii = IUCN Extinction probability
    # Ki = Vulnerability to climate change
    # pop.zoo.eu = Number of individuals in EU zoos
    # Nv = viable population size
    # Gi = Management expertise
    # cost.wild = cost for habitat protection
    # cost.captive = cost for captive breeding
    # Vw = Decision makers judgment value outcome species survives in wild
    # Vz = Decision makers judgment value outcome species survives in zoo
    # Vb = Decision makers judgment value outcome species survives in both
    # Vx = Decision makers judgment value outcome species  extinct

    ### --- Probabilities of decision paths --- ###

    # Invest in wild (W = 1)
    # Probability to survive (x=1)
    W1x1 <-  (1 - Ki * Ai) * (1 - Ii * Ai)
    # Probability to go extinct (x=0)
    W1x0 <- 1 - W1x1

    # Do not invest in wild (W = 0)
    # Probability to survive (x=1)
    W0x1  <-  (1 - Ii) * (1 - Ki)
    # Probability to go extinct (x=0)
    W0x0  <- 1 - W0x1

    # Invest in zoo (Z = 1)
    # Probability to survive (y=1)
    Z1y1  <- Gi
    # Probability to go extinct (y=0)
    Z1y0  <- 1 - Gi

    # Do not invest in zoo (Z = 0)
    # Probability to survive (y=1)
    N0Nv <- pop.zoo.eu / Nv
    N0Nv <- ifelse(N0Nv > 1, 1, N0Nv)
    Z0y1  <- N0Nv
    # Probability to go extinct (y=0)
    Z0y0  <- 1 - Z0y1

    ### --- Calculate Expected Value E_ij --- ###


    # Invest in protecting wild (W = 1, Z = 0)
    ev.wild     <-
      (W1x1 * Z0y1 * Vb) + # x = 1, y = 1 BOTH
      (W1x1 * Z0y0 * Vw) + # x = 1, y = 0 WILD
      (W1x0 * Z0y1 * Vz) + # x = 0, y = 1 ZOO
      (W1x0 * Z0y0 * Vx)   # x = 0, y = 0 EXTINCT
    # Invest in captive population (W = 0, Z = 1)
    ev.zoo     <-
      (W0x1 * Z1y1 * Vb) + # x = 1, y = 1 BOTH
      (W0x1 * Z1y0 * Vw) + # x = 1, y = 0 WILD
      (W0x0 * Z1y1 * Vz) + # x = 0, y = 1 ZOO
      (W0x0 * Z1y0 * Vx)   # x = 0, y = 0 EXTINCT
    # Invest in both (W = 1, Z = 0)
    ev.both     <-
      (W1x1 * Z1y1 * Vb) + # x = 1, y = 1 BOTH
      (W1x1 * Z1y0 * Vw) + # x = 1, y = 0 WILD
      (W1x0 * Z1y1 * Vz) + # x = 0, y = 1 ZOO
      (W1x0 * Z1y0 * Vx)   # x = 0, y = 0 EXTINCT
    # No investment (W = 0, Z = 0)
    ev.nothing <-
      (W0x1 * Z0y1 * Vb) + # x = 1, y = 1 BOTH
      (W0x1 * Z0y0 * Vw) + # x = 1, y = 0 WILD
      (W0x0 * Z0y1 * Vz) + # x = 0, y = 1 ZOO
      (W0x0 * Z0y0 * Vx)   # x = 0, y = 0 EXTINCT


    ### ---  Net Benefit --- ###
    # Net Benefit =  Expected value of intervention minus expected value of
    # doing nothing

    benefit.both     <- ev.both - ev.nothing
    benefit.wild     <- ev.wild - ev.nothing
    benefit.zoo      <- ev.zoo  - ev.nothing

    ### --- Cost Net Benefit --- ###

    # we multiply with 10mio since numbers would be very small, since they are
    # relative to each other it is not a problem
    cost.benefit.wild <- (benefit.wild / cost.wild) * 10000000
    cost.benefit.zoo  <- (benefit.zoo / cost.captive) * 10000000
    cost.benefit.both <-
      (benefit.both / (cost.wild + cost.captive)) * 10000000

    return(
      data.frame(
        ev.wild,
        ev.zoo,
        ev.both,
        ev.nothing,
        benefit.wild,
        benefit.zoo,
        benefit.both,
        cost.benefit.wild,
        cost.benefit.zoo,
        cost.benefit.both
      )
    )

  }
### --- Choose recommended strategy without budget constraints --- ###


ChooseStrategy <- function(Wild, Zoo, Both) {
  # Wild = Net Benefit  of wild strategy
  # Zoo  = Net Benefit of zoo strategy
  # Both = Net Benefit of both
  benf <- cbind(Wild, Zoo, Both)
  strats <- c("W", "Z", "B")
  x <- apply(benf, 1, function(be) {
    idmax <- which(be == max(be))
    st <- strats[idmax]
    if (length(idmax) > 1) {
      if ("W" %in% st & "Z" %in% st & "B" %in% st) {
        st <- "N"
      } else if ("W" %in% st & "B" %in% st) {
        st <- "W"
      } else if ("W" %in% st & "Z" %in% st) {
        st <- "W"
      } else if ((!"W" %in% st) & "Z" %in% st & "B" %in% st) {
        st <- "Z"
      }
    }
    return(st)
  })
  return(x)
}


ChooseCostStrategy <- function(Wild, Zoo, Both) {
  # Wild = Net Benefit  of wild strategy
  # Zoo  = Net Benefit of zoo strategy
  # Both = Net Benefit of both

  Wild[which(is.na(Wild))] <- 0
  Zoo[which(is.na(Zoo))] <- 0
  Both[which(is.na(Both))] <- 0


  cbenf <- cbind(Wild, Zoo, Both)
  strats <- c("W", "Z", "B")
  x <- apply(cbenf, 1, function(be) {
    idmax <- which(be == max(be))
    idn <- which(sum(be) == 0)
    st <- strats[idmax]
    if (length(idmax) > 1) {
      if ("W" %in% st & "B" %in% st) {
        st <- "W"
      } else if ("W" %in% st & "Z" %in% st) {
        st <- "W"
      } else if ((!"W" %in% st) & "Z" %in% st & "B" %in% st) {
        st <- "Z"
      }
    }
    st[idn] <- "N"
    return(st)
  })

  return(x)

}
