

rpath2 = function (Rpath.params, eco.name = NA, eco.area = 1) 
{
  Type <- Group <- DetInput <- ProdCons <- PB <- QB <- noB <- noEE <- alive <- noPB <- NULL
  BEE <- Biomass <- Q <- BioAcc <- BioQB <- diag.a <- EEa <- B <- M0 <- NULL
  QBloss <- Unassim <- Ex <- NULL
  model <- copy(Rpath.params$model)
  diet <- copy(Rpath.params$diet)
  if (length(which(sapply(model, class) == "logical")) > 0) {
    logic.col <- which(sapply(model, class) == "logical")
    for (i in 1:length(logic.col)) {
      set(model, j = logic.col[i], value = as.numeric(model[[logic.col[i]]]))
    }
  }
  if (sapply(diet, class)[1] == "factor") {
    diet[, `:=`(1, NULL)]
  }
  if (sapply(diet, class)[1] == "character") {
    diet[, `:=`(1, NULL)]
  }
  mixotrophs <- which(model[, Type] > 0 & model[, Type] < 1)
  mix.Q <- 1 - model[mixotrophs, Type]
  for (i in seq_along(mixotrophs)) {
    new.dc <- diet[, mixotrophs[i], with = F] * mix.Q[i]
    diet[, `:=`(mixotrophs[i], new.dc)]
  }
  diet[is.na(diet)] <- 0
  ngroups <- nrow(model)
  nliving <- nrow(model[Type < 2, ])
  ndead <- nrow(model[Type == 2, ])
  ngear <- nrow(model[Type == 3, ])
  nodetrdiet <- diet[1:nliving, ]
  model[is.na(DetInput), `:=`(DetInput, 0)]
  GE <- ifelse(!is.na(model[, QB]) & !is.na(model[, PB]), model[, 
                                                                PB/QB], model[, ProdCons])
  QB.1 <- ifelse(is.na(model[, QB]), model[, PB/GE], model[, 
                                                           QB])
  PB.1 <- ifelse(is.na(model[, PB]), model[, ProdCons * QB], 
                 model[, PB])
  model[, `:=`(QB, QB.1)]
  model[, `:=`(PB, PB.1)]
  landmat <- model[, (10 + ndead + 1):(10 + ndead + ngear), 
                   with = F]
  discardmat <- model[, (10 + ndead + 1 + ngear):(10 + ndead + 
                                                    (2 * ngear)), with = F]
  totcatchmat <- landmat + discardmat
  if (is.data.frame(totcatchmat)) {
    totcatch <- rowSums(totcatchmat)
    landings <- rowSums(landmat)
    discards <- rowSums(discardmat)
    gearland <- colSums(landmat, na.rm = T)
    geardisc <- colSums(discardmat, na.rm = T)
  }
  else {
    totcatch <- totcatchmat
    landings <- landmat
    discards <- discardmat
    gearland <- sum(landmat, na.rm = T)
    geardisc <- sum(discardmat, na.rm = T)
  }
  geartot <- gearland + geardisc
  model[, `:=`(landings, landings)]
  model[, `:=`(discards, discards)]
  model[, `:=`(totcatch, totcatch)]
  model[, `:=`(noB, 0)]
  model[, `:=`(noEE, 0)]
  model[, `:=`(alive, 0)]
  model[, `:=`(BEE, 0)]
  model[, `:=`(noPB, 0)]
  model[is.na(Biomass), `:=`(noB, 1)]
  model[is.na(EE), `:=`(noEE, 1)]
  model[Type < 2, `:=`(alive, 1)]
  model[noB == 0 & noEE == 0, `:=`(BEE, 1)]
  model[BEE == 1 & is.na(PB), `:=`(noPB, 1)]
  if (any(model$Type == 0 & is.na(model$QB) & is.na(model$ProdCons))) {
    stop("A consumer is missing both QB and ProdCons - balance failed. Use check.rpath.params() to diagnose.")
  }
  detfate <- model[, (10 + 1):(10 + ndead), with = F]
  detdetfate <- model[Type == 2, (10 + 1):(10 + ndead), with = F]
  living <- model[alive == 1, ]
  living[, `:=`(Ex, totcatch + BioAcc)]
  living[, `:=`(BioQB, Biomass * QB)]
  cons <- as.matrix(nodetrdiet) * living$BioQB[col(as.matrix(nodetrdiet))]
  living[, `:=`(b, Ex + rowSums(cons, na.rm = T))]
  living[noEE == 1, `:=`(diag.a, Biomass * PB)]
  living[noEE == 0, `:=`(diag.a, PB * EE)]
  living[noPB == 1, `:=`(diag.a, Biomass * EE)]
  A <- matrix(0, nliving, nliving)
  diag(A) <- living[, diag.a]
  QBDC <- as.matrix(nodetrdiet) * living$QB[col(as.matrix(nodetrdiet))]
  dimnames(QBDC) <- list(NULL, NULL)
  QBDC[is.na(QBDC)] <- 0
  QBDCa <- as.matrix(QBDC) * living$noB[col(as.matrix(QBDC))]
  A <- A - QBDCa
  if (any(is.na(A))) {
    stop("Model is missing parameters - can't be balanced. Use check.rpath.params() to diagnose.")
  }
  x <- MASS::ginv(A, tol = .Machine$double.eps) %*% living[, 
                                                           b]
  living[, `:=`(EEa, x * noEE)]
  living[is.na(EE), `:=`(EE, EEa)]
  living[, `:=`(B, x * noB)]
  living[is.na(Biomass), `:=`(Biomass, B)]
  living[, `:=`(PBa, x * noPB)]
  living[is.na(PB), `:=`(PB, PBa)]
  living[, `:=`(M0, PB * (1 - EE))]
  living[, `:=`(QBloss, QB)]
  living[is.na(QBloss), `:=`(QBloss, 0)]
  loss <- c((living[, M0] * living[, Biomass]) + (living[, 
                                                         Biomass] * living[, QBloss] * living[, Unassim]), rep(0, 
                                                                                                               ndead), geardisc)
  detinputs1 <- colSums(loss * detfate + model[, DetInput])
  detdiet <- diet[(nliving + 1):(nliving + ndead), ]
  BQB <- living[, Biomass * QB]
  detcons <- as.matrix(detdiet) * BQB[col(as.matrix(detdiet))]
  detoutputs <- rowSums(detcons, na.rm = T)
  det_unused <- ifelse(detinputs1 > detoutputs, detinputs1 - 
                         detoutputs, 0)
  detinputs <- detinputs1 + colSums(det_unused * detdetfate)
  EE <- c(living[, EE], as.vector(detoutputs/detinputs))
  Default_Detrital_PB <- 0.5
  inDetPB <- model[(nliving + 1):(nliving + ndead), PB]
  inDetB <- model[(nliving + 1):(nliving + ndead), Biomass]
  DetPB <- ifelse(is.na(inDetPB), Default_Detrital_PB, inDetPB)
  DetB <- ifelse(is.na(inDetB), detinputs/DetPB, inDetB)
  DetPB <- as.numeric(detinputs)/DetB
  b <- rep(1, ngroups)
  TLcoeff <- matrix(0, ngroups, ngroups)
  diag(TLcoeff) <- rep(1, ngroups)
  gearcons <- as.matrix(totcatchmat)/geartot[col(as.matrix(totcatchmat))]
  dimnames(gearcons) <- list(NULL, NULL)
  gearcons[is.na(gearcons)] <- 0
  dietplus <- as.matrix(diet)
  dimnames(dietplus) <- list(NULL, NULL)
  import <- which(dietplus[nrow(diet), ] > 0)
  for (i in seq_along(import)) {
    import.denom <- 1 - dietplus[nrow(diet), import[i]]
    dietplus[, import[i]] <- dietplus[, import[i]]/import.denom
  }
  dietplus <- dietplus[1:(nliving + ndead), ]
  dietplus <- rbind(dietplus, matrix(0, ngear, nliving))
  dietplus <- cbind(dietplus, matrix(0, ngroups, ndead), gearcons)
  TLcoeffA <- TLcoeff - dietplus
  TL <- solve(t(TLcoeffA), b)
  Bplus <- c(living[, Biomass], DetB, rep(0, ngear))
  PBplus <- c(living[, PB], DetPB, rep(0, ngear))
  PBplus[is.na(PBplus)] <- 0
  EEplus <- c(EE, rep(0, ngear))
  QBplus <- model[, QB]
  QBplus[is.na(QBplus) & PBplus > 0 & !(is.na(GE) | is.nan(GE) | 
                                          is.infinite(GE))] <- (PBplus/GE)[is.na(QBplus) & PBplus > 
                                                                             0 & !(is.na(GE) | is.nan(GE) | is.infinite(GE))]
  QBplus[is.na(QBplus)] <- 0
  GE <- PBplus/QBplus
  GE[is.na(GE) | is.nan(GE) | is.infinite(GE)] <- 0
  RemPlus <- model[, totcatch]
  RemPlus[is.na(RemPlus)] <- 0
  balanced <- list(Group = model[, Group], TL = TL, Biomass = Bplus, 
                   PB = PBplus, QB = QBplus, EE = EEplus, GE = GE, Removals = RemPlus)
  M0plus <- c(living[, M0], as.vector(detoutputs/detinputs))
  gearF <- as.matrix(totcatchmat)/living[, Biomass][row(as.matrix(totcatchmat))]
  newcons <- as.matrix(nodetrdiet) * BQB[col(as.matrix(nodetrdiet))]
  predM <- as.matrix(newcons)/living[, Biomass][row(as.matrix(newcons))]
  predM <- rbind(predM, detcons)
  morts <- list(Group = model[Type < 3, Group], PB = model[Type < 
                                                             3, PB], M0 = M0plus, F = gearF[1:(nliving + ndead), ], 
                M2 = predM)
  gnames <- as.character(balanced$Group)
  balanced$Biomass[is.na(balanced$Biomass)] <- 0
  balanced$PB[is.na(balanced$PB)] <- 0
  balanced$QB[is.na(balanced$QB)] <- 0
  balanced$EE[is.na(balanced$EE)] <- 0
  balanced$GE[is.na(balanced$GE)] <- 0
  model$BioAcc[is.na(model$BioAcc)] <- 0
  model$Unassim[is.na(model$Unassim)] <- 0
  dietm <- as.matrix(diet)
  dimnames(dietm) <- list(c(gnames[1:(nliving + ndead)], "Import"), 
                          gnames[1:nliving])
  dietm[is.na(dietm)] <- 0
  landmatm <- as.matrix(landmat)
  dimnames(landmatm) <- list(gnames, gnames[(ngroups - ngear + 
                                               1):ngroups])
  landmatm[is.na(landmatm)] <- 0
  discardmatm <- as.matrix(discardmat)
  dimnames(discardmatm) <- list(gnames, gnames[(ngroups - ngear + 
                                                  1):ngroups])
  discardmatm[is.na(discardmatm)] <- 0
  detfatem <- as.matrix(detfate)
  dimnames(detfatem) <- list(gnames, gnames[(nliving + 1):(nliving + 
                                                             ndead)])
  detfatem[is.na(detfatem)] <- 0
  out.Group <- gnames
  names(out.Group) <- gnames
  out.type <- model[, Type]
  names(out.type) <- gnames
  out.TL <- TL
  names(out.TL) <- gnames
  out.Biomass <- balanced$Biomass
  names(out.Biomass) <- gnames
  out.PB <- balanced$PB
  names(out.PB) <- gnames
  out.QB <- balanced$QB
  names(out.QB) <- gnames
  out.EE <- balanced$EE
  names(out.EE) <- gnames
  out.BA <- model[, BioAcc]
  names(out.BA) <- gnames
  out.Unassim <- model[, Unassim]
  names(out.Unassim) <- gnames
  out.GE <- balanced$GE
  names(out.GE) <- gnames
  path.model <- list(NUM_GROUPS = ngroups, NUM_LIVING = nliving, 
                     NUM_DEAD = ndead, NUM_GEARS = ngear, Group = out.Group, 
                     type = out.type, TL = out.TL, Biomass = out.Biomass, 
                     PB = out.PB, QB = out.QB, EE = out.EE, BA = out.BA, Unassim = out.Unassim, 
                     GE = out.GE, DC = dietm, DetFate = detfatem, Landings = landmatm, 
                     Discards = discardmatm)
  class(path.model) <- "Rpath"
  attr(path.model, "eco.name") <- eco.name
  attr(path.model, "eco.area") <- eco.area
  return(path.model)
}