

check.rpath2 = function(Rpath.params) {

Type <- Group <- Biomass <- EE <- PB <- QB <- ProdCons <- BioAcc <- Unassim <- DetInput <- NULL
w <- 0
c <- 0
if (nrow(Rpath.params$model[Type == 0, ]) == 0) {
  warning("Model must contain at least 1 consumer")
  w <- w + 1
}
if (nrow(Rpath.params$model[Type == 1, ]) == 0) {
  warning("Model must contain a producer group")
  w <- w + 1
}
if (nrow(Rpath.params$model[Type == 2, ]) == 0) {
  warning("Model must contain at least 1 detrital group")
  w <- w + 1
}
if (nrow(Rpath.params$model[Type == 3, ]) == 0) {
  warning("Model must contain at least 1 fleet")
}
n.groups <- nrow(Rpath.params$model)
n.living <- length(Rpath.params$model[Type <= 1, Group])
n.dead <- length(Rpath.params$model[Type == 2, Group])
n.fleet <- length(Rpath.params$model[Type == 3, Group])
if (ncol(Rpath.params$model) != 10 + n.dead + 2 * n.fleet) {
  warning("Model does not have the correct number of column.  There should be 10\n         columns plus one for each detrital group plus two for each fleet group\n         (landings and discards).  Please double check your columns")
  w <- w + 1
}
if (length(Rpath.params$model[is.na(Biomass) & is.na(EE) & 
                              Type < 2, Group]) > 0) {
  warning(paste(Rpath.params$model[is.na(Biomass) & is.na(EE) & 
                                     Type < 2, Group], "are missing both Biomass and EE...must enter one \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[!is.na(Biomass) & !is.na(EE) & 
                              (!is.na(PB) | (is.na(PB) & !is.na(QB) & !is.na(ProdCons))) & 
                              Type < 2, Group]) > 0) {
  warning(paste(Rpath.params$model[!is.na(Biomass) & !is.na(EE) & 
                                     (!is.na(PB) | (is.na(PB) & !is.na(QB) & !is.na(ProdCons))) & 
                                     Type < 2, Group], "have all of Biomass, EE, and PB(or QB and ProdCons) entered... Note that Rpath does\n        not calculate BA, please enter a value for BA if appropriate \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type == 3 & !is.na(Biomass), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type == 3 & !is.na(Biomass), 
                                   Group], "is a fleet and should not have a biomass...set to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type == 3 & !is.na(PB), Group]) > 
    0) {
  warning(paste(Rpath.params$model[Type == 3 & !is.na(PB), 
                                   Group], "is a fleet and should not have a PB...set to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type > 1 & !is.na(QB), Group]) > 
    0) {
  warning(paste(Rpath.params$model[Type > 1 & !is.na(QB), 
                                   Group], "are not living and should not have a QB... please set to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type > 1 & !is.na(EE), Group]) > 
    0) {
  warning(paste(Rpath.params$model[Type > 1 & !is.na(EE), 
                                   Group], "are not living and should not have a EE... please set to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type > 1 & !is.na(ProdCons), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type > 1 & !is.na(ProdCons), 
                                   Group], "are not living and should not have a ProdCons... please set to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type < 2 & is.na(PB), Group]) > 
    0) {
  no.pb <- Rpath.params$model[Type < 2 & is.na(PB), Group]
  if (length(Rpath.params$model[Group %in% no.pb & (is.na(QB) | 
                                                    is.na(ProdCons)) & (is.na(Biomass) | is.na(EE)), 
                                Group]) > 0) {
    warning(paste(Rpath.params$model[Group %in% no.pb & 
                                       (is.na(QB) | is.na(ProdCons)) & (is.na(Biomass) | 
                                                                          is.na(EE)), Group], "are missing a PB without either a (QB and ProdCons) or (EE and B) to estimate PB... please set to >= 0 \n", 
                  sep = " "))
    w <- w + 1
  }
}
if (length(Rpath.params$model[is.na(QB) & is.na(ProdCons) & 
                              Type < 1, Group]) > 0) {
  warning(paste(Rpath.params$model[is.na(QB) & is.na(ProdCons) & 
                                     Type < 1, Group], "are missing both QB and ProdCons...must enter one \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[!is.na(QB) & !is.na(ProdCons) & 
                              Type < 1, Group]) > 0) {
  both <- Rpath.params$model[!is.na(QB) & !is.na(ProdCons) & 
                               Type < 1, Group]
  if (length(Rpath.params$model[Group %in% both & !is.na(PB), 
                                Group]) > 0) {
    warning(paste(Rpath.params$model[Group %in% both & 
                                       !is.na(PB), Group], "have PB, QB, and ProdCons... ProdCons will be recalculated during balancing \n", 
                  sep = " "))
    w <- w + 1
    c <- c + 1
  }
}
if (length(Rpath.params$model[Type == 3 & !is.na(BioAcc), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type == 3 & !is.na(BioAcc), 
                                   Group], "are fleets and should not have a BioAcc...set to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type == 3 & !is.na(Unassim), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type == 3 & !is.na(Unassim), 
                                   Group], "are fleets and should not have an Unassim...set to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type != 3 & is.na(BioAcc), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type != 3 & is.na(BioAcc), 
                                   Group], "must have a number for BioAcc...set to >= 0 \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type != 3 & is.na(Unassim), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type != 3 & is.na(Unassim), 
                                   Group], "must have a number for Unassim...set to >= 0 \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type != 2 & !is.na(DetInput), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type != 2 & !is.na(DetInput), 
                                   Group], "are not detritus...set DetInput to NA \n", 
                sep = " "))
  w <- w + 1
}
if (length(Rpath.params$model[Type == 2 & is.na(DetInput), 
                              Group]) > 0) {
  warning(paste(Rpath.params$model[Type == 2 & is.na(DetInput), 
                                   Group], "are detritus...set DetInput to 0 \n", sep = " "))
  w <- w + 1
}
det.matrix <- Rpath.params$model[, 11:(10 + n.dead), with = F]
if (length(which(is.na(det.matrix))) > 0) {
  na.group <- which(is.na(det.matrix))
  for (i in 1:length(na.group)) {
    while (na.group[i] > n.groups) {
      na.group[i] <- na.group[i] - n.groups
    }
  }
  na.group <- unique(na.group)
  warning(paste(Rpath.params$model[na.group, Group], "one or more detrital fates are NA...set to >= 0 \n", 
                sep = " "))
  w <- w + 1
}
fleet.matrix <- Rpath.params$model[1:(n.groups - n.fleet), 
                                   (11 + n.dead):ncol(Rpath.params$model), with = F]
if (length(which(is.na(fleet.matrix))) > 0) {
  na.group <- which(is.na(fleet.matrix))
  for (i in 1:length(na.group)) {
    while (na.group[i] > n.groups) {
      na.group[i] <- na.group[i] - n.groups
    }
  }
  na.group <- unique(na.group)
  warning(paste(Rpath.params$model[na.group, Group], "one or more catches are NA...set to >= 0 \n", 
                sep = " "))
  w <- w + 1
}

col.names <- names(Rpath.params$diet)[2:ncol(Rpath.params$diet)]
col.sums <- Rpath.params$diet[, lapply(.SD, sum, na.rm = T), 
                              .SDcols = col.names]
types <- Rpath.params$model[Type < 2, Type]
dctype <- round(col.sums + types, 3)
if (length(which(dctype != 1)) > 0) {
  for (i in 1:length(which(dctype != 1))) {
    warning(paste(col.names[which(dctype != 1)][i], "sum,", 
                  col.sums[, which(dctype != 1)[i], with = F], 
                  "is not 1...check DC or proportion of primary production"))
    w <- w + 1
  }
}
dietcol <- ncol(Rpath.params$diet)
if (dietcol != (n.living + 1)) {
  warning(paste(dietcol, " is the incorrect number of columns in diet matrix.", 
                "There should be", n.living + 1))
  w <- w + 1
}
if (!Rpath.params$diet[nrow(Rpath.params$diet), 1] == "Import" & 
    !Rpath.params$diet[nrow(Rpath.params$diet), 1] == "import") {
  warning("Diet matrix is missing the import row.  Please add \"Import\" as the\n            final row.  All entries can be 0 or NA.")
  w <- w + 1
}
dietDF <- data.frame(Rpath.params$diet)
dietDF[is.na(dietDF)] <- 0
if (any(dietDF < 0)) {
  warning("Found a negative diet value. Please make sure no diet values are negative.")
  w <- w + 1
}
if (w == 0) {
  cat("Rpath parameter file is functional. \n")
}
else {
  if (w == c) {
    cat("Rpath parameters functional, though some may be recalculated during balance. \n")
  }
  else {
    cat("Rpath parameter file needs attention! \n")
  }
}}
