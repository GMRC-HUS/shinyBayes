#' code_sans_dep
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

library(utils)

# setInternet2(TRUE)




descr1 <- function(Y, Tap = FALSE) {
  if (Tap) {
    res <- list(Descriptif = NULL, TestNormalite = NULL, Tap = NULL)
  } else {
    res <- list(Descriptif = NULL, TestNormalite = NULL)
  }
  nomY <- deparse(substitute(Y))
  library(moments)
  aze <- matrix(NA, ncol = 1, nrow = 24)
  rownames(aze) <- c(
    "Effectifs presents", "Proportions de presents %", "Effectifs manquants", "Proportions de manquants %",
    "Moyenne", "Ecart-type", "Variance", "Erreur standard (s.e.m)", "Minimum", "Maximum", "Percentile 2,5", "Percentile 5", "Q1 ",
    "Mediane", "Q3", "Percentile 95", "Percentile 97,5", "Ecart inter-quartiles", "IC valeurs borne inf", "IC valeurs borne sup",
    "IC moyenne borne inf", "IC moyenne borne sup", "coefficient d'asymetrie", "Kurtosis"
  )


  colnames(aze) <- nomY ### ,levels(X))
  qtl <- function(x) {
    quantile(x, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975), na.rm = T)
  }
  nbm <- function(x) {
    sum(is.na(x))
  }
  nbp <- function(x) {
    sum(!is.na(x))
  }
  nbmpct <- function(x) {
    sum(is.na(x)) * 100 / length(x)
  }
  nbppct <- function(x) {
    sum(!is.na(x)) * 100 / length(x)
  }
  nbmqt <- sum(is.na(Y))
  ddl <- nbp(Y) - 1
  errstm <- sd(Y, na.rm = TRUE) / sqrt(nbp(Y))
  liminfy <- mean(Y, na.rm = TRUE) - qt(0.975, ddl) * sd(Y, na.rm = TRUE)
  limsupy <- mean(Y, na.rm = TRUE) + qt(0.975, ddl) * sd(Y, na.rm = TRUE)
  liminfy2 <- mean(Y, na.rm = TRUE) - qt(0.975, ddl) * sd(Y, na.rm = TRUE) / sqrt(ddl + 1)
  limsupy2 <- mean(Y, na.rm = TRUE) + qt(0.975, ddl) * sd(Y, na.rm = TRUE) / sqrt(ddl + 1)
  asymetrie <- skewness(Y, na.rm = TRUE)
  kurt <- kurtosis(Y, na.rm = TRUE)
  aze[1, ] <- nbp(Y)
  aze[2, ] <- nbppct(Y)
  aze[3, ] <- nbm(Y)
  aze[4, ] <- nbmpct(Y)
  aze[5, ] <- mean(Y, na.rm = TRUE)
  aze[6, ] <- sd(Y, na.rm = TRUE)
  aze[7, ] <- var(Y, na.rm = TRUE)
  aze[8, ] <- errstm
  aze[9, ] <- min(Y, na.rm = TRUE)
  aze[10, ] <- max(Y, na.rm = TRUE)
  aze[11:17, 1] <- qtl(Y)
  aze[18, ] <- IQR(Y, na.rm = TRUE)
  aze[19, ] <- liminfy
  aze[20, ] <- limsupy
  aze[21, ] <- liminfy2
  aze[22, ] <- limsupy2
  aze[23, ] <- asymetrie
  aze[24, ] <- kurt
  aze <- round(aze, digits = 4)

  pvalnorm <- matrix(c(NA, NA), ncol = 1)
  rownames(pvalnorm) <- c("Test de normalite de Shapiro-Wilk       : p =", "Test de normalite de Kolmogorov-Smirnov : p =")
  colnames(pvalnorm) <- c("")
  if (length(Y) < 5000) {
    pvalnorm[1] <- shapiro.test(Y)$p.value
  } else {
    pvalnorm[1] <- NA
  }
  pvalnorm[2] <- ks.test(Y, "pnorm", mean(Y, na.rm = T), sd(Y, na.rm = T))$p.value
  pvalnorm <- round(pvalnorm, digits = 4)

  if (Tap) {
    long <- length(table(Y))
    nbval <- sum(!is.na(Y)) # length(Y)
    triap <- matrix(NA, ncol = 4, nrow = long)
    rownames(triap) <- unique(sort(Y))
    colnames(triap) <- c("Eff.", "Eff. cum.", "Prop.", "Prop. cum")
    triap[, 1] <- table(Y)
    triap[, 2] <- cumsum(table(Y))
    triap[, 3] <- round(table(Y) * 100 / nbval, digits = 2)
    triap[, 4] <- round(cumsum(table(Y)) * 100 / nbval, digits = 2)
    res <- list(Descriptif = aze, TestNormalite = pvalnorm, Triaplat = triap)
  }
  if (!Tap) {
    res <- list(Descriptif = aze, TestNormalite = pvalnorm)
  }
  return(res)
}

###########################################################################################################
###########################################################################################################
#********************************************************************************************************* #
#                                                                                                         #
#             DESCR3 :   Croiser une variable qualitative avec une variable quantitative                  #
#                                                                                                         #
#********************************************************************************************************* #
###########################################################################################################
###########################################################################################################

descr3 <- function(Y, X, Tap = FALSE, nom = NULL, nomY = NULL, latex = 0) {
  if (Tap) {
    res <- list(Descriptif = NULL, TestNormalite = NULL, Testpv = NULL, TestsNPv = NULL, Tests_de_Student = NULL, TestsNP = NULL, Triaplat = NULL, CommqT = NULL)
  } else {
    res <- list(Descriptif = NULL, TestNormalite = NULL, Testpv = NULL, TestsNPv = NULL, Tests_de_Student = NULL, TestsNP = NULL)
  }
  if (is.null(nom)) {
    nom <- deparse(substitute(X))
  }
  if (!is.factor(X)) {
    X <- as.factor(X)
  }
  if (is.null(nomY)) {
    nomY <- deparse(substitute(Y))
  }
  nbnv <- nlevels(X)
  library(moments)

  aze <- matrix(NA, ncol = 1 + nbnv, nrow = 27)
  rownames(aze) <- c(
    "Effectifs presents", "Proportions de presents", "Effectifs manquants", "Proportions de manquants",
    "Moyenne", "Ecart-type", "Variance", "Erreur standard (s.e.m)", "Err. Std (basee sur l'ANOVA)", "Minimum", "Maximum", "Percentile 2,5", "Percentile 5", "Q1 ", "Mediane", "Q3",
    "Percentile 95", "Percentile 97,5", "Ecart inter-quartiles", "IC valeurs borne inf", "IC valeurs borne sup", "IC moyenne borne inf",
    "IC moyenne borne sup", "IC moyenne borne inf (ANOVA)", "IC moyenne borne sup (ANOVA)", "coefficient d'asymetrie", "Kurtosis"
  )


  colnames(aze) <- c(nomY, paste(" ", nom, "=", levels(X)))

  qtl <- function(x) {
    quantile(x, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975), na.rm = T)
  }
  nbm <- function(x) {
    sum(is.na(x))
  }
  nbp <- function(x) {
    sum(!is.na(x))
  }
  nbmpct <- function(x) {
    sum(is.na(x)) * 100 / length(x)
  }
  nbppct <- function(x) {
    sum(!is.na(x)) * 100 / length(x)
  }
  nbmqt <- sum(is.na(Y))
  ddl <- length(Y) - 1
  ddlg <- tapply(Y, X, nbp) - 1
  aa <- summary(aov(Y ~ X))
  carmoy <- aa[[1]][2, 3]
  ddlaov <- aa[[1]][2, 1]
  nbvalg <- tapply(Y, X, nbp)
  aze[1, ] <- c(nbp(Y), nbvalg)
  aze[2, ] <- c(nbppct(Y), tapply(Y, X, nbppct))
  aze[3, ] <- c(nbm(Y), tapply(Y, X, nbm))
  aze[4, ] <- c(nbmpct(Y), tapply(Y, X, nbmpct))
  aze[5, ] <- c(mean(Y, na.rm = TRUE), tapply(Y, X, mean, na.rm = TRUE))
  aze[6, ] <- c(sd(Y, na.rm = TRUE), tapply(Y, X, sd, na.rm = T))
  aze[7, ] <- c(var(Y, na.rm = TRUE), tapply(Y, X, var, na.rm = T))

  errstm <- sd(Y, na.rm = TRUE) / sqrt(ddl)
  errstmg <- tapply(Y, X, sd, na.rm = T) / sqrt(ddlg + 1)
  aze[8, ] <- c(errstm, errstmg)
  errstgr <- (carmoy / nbvalg)^0.5
  aze[9, ] <- c(NA, errstgr)
  aze[10, ] <- c(min(Y, na.rm = TRUE), tapply(Y, X, min, na.rm = T))
  aze[11, ] <- c(max(Y, na.rm = TRUE), tapply(Y, X, max, na.rm = T))
  aze[12:18, 1] <- qtl(Y)
  stock <- tapply(Y, X, qtl)
  for (i in 1:nbnv) {
    aze[12:18, 1 + i] <- stock[[i]]
  }

  aze[19, ] <- c(IQR(Y, na.rm = TRUE), tapply(Y, X, IQR, na.rm = T))

  liminfy <- mean(Y, na.rm = TRUE) - qt(0.975, ddl) * sd(Y, na.rm = TRUE)
  liminfyx <- tapply(Y, X, mean, na.rm = T) - tapply(Y, X, sd, na.rm = T) * qt(0.975, ddlg)
  limsupy <- mean(Y, na.rm = TRUE) + qt(0.975, ddl) * sd(Y, na.rm = TRUE)
  limsupyx <- tapply(Y, X, mean, na.rm = T) + tapply(Y, X, sd, na.rm = T) * qt(0.975, ddlg)
  aze[20, ] <- c(liminfy, liminfyx)
  aze[21, ] <- c(limsupy, limsupyx)

  liminfy2 <- mean(Y, na.rm = TRUE) - qt(0.975, ddl) * sd(Y, na.rm = TRUE) / sqrt(ddl + 1)
  liminfyx2 <- tapply(Y, X, mean, na.rm = T) - tapply(Y, X, sd, na.rm = T) * qt(0.975, ddlg) / sqrt(ddlg + 1)
  limsupy2 <- mean(Y, na.rm = TRUE) + qt(0.975, ddl) * sd(Y, na.rm = TRUE) / sqrt(ddl + 1)
  limsupyx2 <- tapply(Y, X, mean, na.rm = T) + tapply(Y, X, sd, na.rm = T) * qt(0.975, ddlg) / sqrt(ddlg + 1)
  aze[22, ] <- c(liminfy2, liminfyx2)
  aze[23, ] <- c(limsupy2, limsupyx2)

  liminfg <- tapply(Y, X, mean, na.rm = TRUE) - qt(0.975, df = ddlaov) * errstgr
  limsupg <- tapply(Y, X, mean, na.rm = TRUE) + qt(0.975, df = ddlaov) * errstgr

  aze[24, ] <- c(NA, liminfg)
  aze[25, ] <- c(NA, limsupg)

  aze[26, ] <- c(skewness(Y, na.rm = TRUE), tapply(Y, X, skewness, na.rm = T))
  aze[27, ] <- c(kurtosis(Y, na.rm = TRUE), tapply(Y, X, kurtosis, na.rm = T))


  if (nlevels(X) == 2) {
    pvaleur <- format.pval(wilcox.test(Y ~ X)$p.value, digits = 4)
  } else {
    if (nlevels(X) > 2) {
      pvaleur <- format.pval(kruskal.test(Y ~ X)$p.value, digits = 4)
    }
  }

  if (nlevels(X) == 2) {
    testnp <- paste("Test de Mann & Whitney : p =", pvaleur)
  } else {
    if (nlevels(X) > 2) {
      testnp <- paste("Test de Kruskal & Wallis : p =", pvaleur)
    }
  }

  pvalstud <- matrix(c(NA, NA), ncol = 1)
  rownames(pvalstud) <- c("Test de Student, variances egales   : p =", "Test de Student, variances inegales : p =")
  colnames(pvalstud) <- c("")

  if (nlevels(X) == 2) {
    pvalstud[1] <- t.test(Y ~ X, var.equal = TRUE)$p.value
  } else {
    if (nlevels(X) > 2) {
      pval <- format.pval(summary(aov(Y ~ X))[[1]][1, 5], digits = 4)
    }
  }

  if (nlevels(X) == 2) {
    pvalstud[2] <- t.test(Y ~ X, var.equal = FALSE)$p.value
  } else {
    if (nlevels(X) > 2) {
      pval <- format.pval(summary(aov(Y ~ X))[[1]][1, 5], digits = 4)
    }
  }


  if (nlevels(X) == 2) {
    testp <- round(pvalstud, digits = 4)
  } else {
    if (nlevels(X) > 2) {
      testp <- paste("Analyse de la Variance : p =", pval)
    }
  }

  if (nlevels(X) == 2) {
    pvartest <- format.pval(var.test(Y ~ X)$p.value, digits = 4)
  } else {
    if (nlevels(X) > 2) {
      pvartestpg <- format.pval(bartlett.test(Y ~ X)$p.value, digits = 4)
    }
  }

  if (nlevels(X) == 2) {
    testpv <- paste(list(paste("Test parametrique d'egalite de deux variances (Fisher): p =", pvartest)))
  } else {
    if (nlevels(X) > 2) {
      testpv <- paste("Test parametrique d'egalite de plus de deux variances (Bartlett) : p =", pvartestpg)
    }
  }

  pvalnorm <- matrix(c(NA, NA), ncol = 1)
  rownames(pvalnorm) <- c("Test de normalite de Shapiro-Wilk       : p =", "Test de normalite de Kolmogorov-Smirnov : p =")
  colnames(pvalnorm) <- c("")
  if (length(Y) < 5000) {
    pvalnorm[1] <- shapiro.test(Y)$p.value
  } else {
    pvalnorm[1] <- NA
  }
  pvalnorm[2] <- ks.test(Y, "pnorm", mean(Y, na.rm = T), sd(Y, na.rm = T))$p.value
  pvalnorm <- round(pvalnorm, digits = 4)

  if (nlevels(X) == 2) {
    pvalfl2g <- format.pval(ansari.test(Y ~ X)$p.value, digits = 4)
  } else {
    if (nlevels(X) > 2) {
      pvalfl3g <- format.pval(fligner.test(Y ~ X)$p.value, digits = 4)
    }
  }

  if (nlevels(X) == 2) {
    testnpv <- paste(list(paste("Test non param. d'egalite de deux variances (Ansari) : p =", pvalfl2g)))
  } else {
    if (nlevels(X) > 2) {
      testnpv <- paste("Test non param. d'egalite de plus de deux variances (Fligner) : p =", pvalfl3g)
    }
  }

  if (Tap) {
    long <- length(table(Y))
    nbval <- sum(!is.na(Y)) # length(Y)
    triap <- matrix(NA, ncol = 5 + nbnv, nrow = long)
    rownames(triap) <- unique(sort(Y))
    colnames(triap) <- c("Eff.", "Eff. cum.", "Prop.", "Prop. cum", paste(" ", nom, "=", levels(X)), "Tot.Compl")
    triap[, 1] <- table(Y)
    triap[, 2] <- cumsum(table(Y))
    triap[, 3] <- round(table(Y) * 100 / nbval, digits = 2)
    triap[, 4] <- round(cumsum(table(Y)) * 100 / nbval, digits = 2)
    triap[, 5:(5 + nbnv - 1)] <- matrix(table(Y, X))
    triap[, (5 + nbnv)] <- matrix(apply(table(Y, X), 1, sum))
  }



  if (Tap) {
    res<- aze

    # res$CommqT<-commqt
  }

  if (!Tap) {
    res<-aze
 
  }

  if (latex == 1) {
    library(xtable)
    if (pvalnorm[1] >= 0.05) {
      pvalTEX <- testp[1]
    } else {
      pvalTEX <- as.numeric(gsub(".* ([0-9.]+).*", "\\1", testnp[1]))
    }
    # cat(paste("Croisement de la variable", nomY, "en fonction de", nom))
    # cat("\n")
    # print(xtable(res$Descriptif[-c(8, 9, 19, 20, 21, 24, 25, 26, 27), ], align = "|r|rrr|"), table.placement = "H", size = "small")
    # cat(paste("La p.valeur associee aux croisement de ces variables est de: "))
    # cat(pvalTEX)
    # cat("\n")
  }
  if (latex == 0) {
    res<-res[1:19,]
    return(res)
  }
}
###########################################################################################################
###########################################################################################################
#********************************************************************************************************* #
#                                                                                                         #
#                       CROISEMENTS DE VARIABLES DANS UN JEU DE DONNEES                                   #
#                                                                                                         #
#********************************************************************************************************* #
###########################################################################################################
###########################################################################################################


###########################################################################################################
###########################################################################################################
#********************************************************************************************************* #
#                                                                                                         #
#                       FONCTION DE DESCRIPTION d'UNE SEULE VARIABLE                                      #
#                                                                                                         #
#********************************************************************************************************* #
###########################################################################################################
###########################################################################################################

desc1 <- function(x, modmax = 7) {
  # modmax<-7  # passe en argument
  if (length(table(as.factor(x))) == 1) {
    res <- paste("Variable n'ayant qu'une seule modalite", levels(as.factor(x)), sep = "  :  ")
  } else {
    if (is.character(x)) {
      nature.variable <- 0
      res <- "Variable textuelle"
    }
    if (is.factor(x)) {
      if (nlevels(x) < modmax) {
        nature.variable <- 2
      } else {
        nature.variable <- 3
        res <- "Facteur nombreuses modalites, a verifier manuellement"
      }
    } else {
      if (is.numeric(x)) {
        if (nlevels(as.factor(x)) < modmax) {
          nature.variable <- 2
        } else {
          nature.variable <- 1
        }
      } else {
        nature.variable <- 0

      }
    }

    nature <- ifelse(nature.variable == 1, "quantitative", "qualitative")


    if (nature.variable == 1) # Dans le cas d'une variable quantitative
      {
        effectif <- noquote(c(length(x), length(x[!is.na(x)]), 100 * round(length(x[!is.na(x)]) / length(x), 2), round(sum(is.na(x)), 2), 100 * round(sum(is.na(x)) / length(x), 2)))
        names(effectif) <- c("N", "N.presents", "%presents", "NA", "%NA")
        x1 <- x[!is.na(x)]
        stats <- round(c(mean(x1, na.rm = TRUE), var(x1, na.rm = TRUE), sd(x1, na.rm = TRUE), median(x1, na.rm = TRUE)), 3)
        names(stats) <- c("Moyenne", "Variance", "Ecart-type", "Mediane")
        quantiles <- round(quantile(x, probs = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5), na.rm = TRUE), 3)
        quantiles2 <- round(quantile(x, probs = c(0.75, 0.9, 0.95, 0.975, 0.99), na.rm = TRUE), 3)
        min.max <- round(c(min(x1, na.rm = TRUE), max(x1, na.rm = TRUE), max(x1, na.rm = TRUE) - min(x1, na.rm = TRUE)), 3)
        names(min.max) <- c("min", "max", "etendue")
        p.val.shapiro <- round(shapiro.test(x)$p.value, 4)
        norm <- ifelse((p.val.shapiro > 0.05), "Non Rejet Normalite", "Rejet Normalite")
        Shap <- c(p.val.shapiro, norm)
        names(Shap) <- c("p.valeur", "Hypothese")
        res <- list(Effectifs = effectif, Type = nature, Stats = stats, Quantiles = quantiles, Quantiles = quantiles2, Extremes = min.max, Test_de_Shapiro_Wilk = Shap)
        # cat("\n")                                                                                                       # Affichage
        # cat("Variable",nature,"\n")
        # cat("\n")
        # cat("Effectifs","\n")
        # cat("\n")
        # cat("\t","N","\t","N.presents","\t","%presents","\t","NA","\t","%NA");cat("\n")
        # cat("\t",effectif[1],"\t",effectif[2],"\t","\t",effectif[3],"\t","\t",effectif[4],"\t",effectif[5],"\n")
        # cat("\n")
        # cat("Statistiques","\n")
        # cat("\n")
        # cat("\t","Moyenne","\t","Ecart-type","\t","Variance","\t","Mediane","\t");cat("\n")
        # cat("\t",stats[1],"\t",stats[2],"\t",stats[3],"\t","\t",stats[4],"\t","\n")
        # cat("\n")
        # cat("\t","2.5%","5.0%","10 %","25 %","50 %","75 %","90 %","95 %","97.5%");cat("\n")
        # cat("\t",quantiles[1],quantiles[2],quantiles[3],quantiles[4],quantiles[5],quantiles[6],quantiles[7],quantiles[8],quantiles[9])
        # cat("\n")
      }

    if (nature.variable == 2) # Dans le cas d'une variable qualitative
      {
        effectif <- noquote(c(length(x), length(x[!is.na(x)]), 100 * round(length(x[!is.na(x)]) / length(x), 2), round(sum(is.na(x)), 2), 100 * round(sum(is.na(x)) / length(x), 2)))
        names(effectif) <- c("N", "Npres", "%pres", "NA", "%NA")
        modalites <- nlevels(as.factor(x))
        type <- paste(nature, "a", modalites, "modalites")
        tabl <- table(x)
        pour <- round(table(x) / length(x[!is.na(x)]), 3)
        pourcumul <- round(table(x) / length(x[!is.na(x)]), 3)
        for (i in 2:length(pourcumul)) {
          pourcumul[i] <- pourcumul[i - 1] + pour[i]
        }
        IC.2.5 <- IC.97.5 <- rep(0, modalites)
        for (i in 1:modalites) {
          IC.2.5[i] <- round(binom.test(tabl[i], sum(tabl))$conf.int[1], 3)
          IC.97.5[i] <- round(binom.test(tabl[i], sum(tabl))$conf.int[2], 3)
        }
        T <- cbind(tabl, pour, pourcumul, IC.2.5, IC.97.5)

        colnames(T) <- c("N", "%", "%cumul", "IC_2.5", "IC_97.5")
        res <- list(Effectifs = effectif, Type = type, Tableaux = T)
        # cat("\n")                                                                                                       # Affichage
        # cat("Variable",type,"\n")
        # cat("\n")
        # cat("Effectifs","\n")
        # cat("\n")
        # cat("\t","N","\t","N.presents","\t","%presents","\t","NA","\t","%NA");cat("\n")
        # cat("\t",effectif[1],"\t",effectif[2],"\t","\t",effectif[3],"\t","\t",effectif[4],"\t",effectif[5],"\n")
        # cat("\n")
        # cat("Tableaux","\n")
        # cat("\n")
        # cat("\t","N","\t","%","\t","%cumules","\t","ICinf","\t","ICsup","\n")
        # for(i in 1:length(T[,1])){
        # AF<-T[i,]
        # cat("\t",AF[1]);cat("\t");cat(AF[2]);cat("\t");cat(AF[3]);cat("\t","\t");cat(AF[4]);cat("\t");cat(AF[5]);cat("\n")
        # }
      }
  }
  return(res)
  # return(noquote(""))
}



###
descd <- function(D) {
  if (is.data.frame(D)) {
    situation <- 2
  } else {
    if (is.matrix(D)) {
      situation <- 1
    } else {
      situation <- 0
    }
  }
  if (situation == 2 | situation == 1) {
    n.lignes <- dim(D)[1]
    n.col <- dim(D)[2]
    N <- n.lignes * n.col
    #---------------------------------------------
    # Calcul du nombre de variables quanti et quali
    n.quanti <- n.quali <- n.txt <- n.autres <- 0
    for (i in 1:n.col) {
      x <- D[, i]
      modmax <- 7
      if (length(table(as.factor(x))) == 1) {
        nature.variable <- -1
      } else {
        if (is.character(x)) {
          nature.variable <- 0
          res <- "Variable textuelle"
        }
        if (is.factor(x)) {
          if (nlevels(x) < modmax) {
            nature.variable <- 2
          } else {
            nature.variable <- 3
            res <- "Facteur nombreuses modalites, a verifier manuellement"
          }
        } else {
          if (is.numeric(x)) {
            if (nlevels(as.factor(x)) < modmax) {
              nature.variable <- 2
            } else {
              nature.variable <- 1
            }
          } else {
            nature.variable <- 0
          }
        }
      }
      if (nature.variable == 1) {
        n.quanti <- n.quanti + 1
      }
      if (nature.variable == 2) {
        n.quali <- n.quali + 1
      }
    }
    if (nature.variable == 0) {
      n.txt <- n.txt + 1
    }
    if (nature.variable == -1 | nature.variable == 3) {
      n.autres <- n.autres + 1
    }
  }

  #----------------------------------------------------
  C.vide <- apply(D, 2, function(x) all(is.na(x))) # colonnes vides
  lig.vides <- sum(apply(D, 1, function(x) all(is.na(x)))) # nombre de lignes vides
  col.vides <- sum(C.vide) # nombre de colonnes vides
  dat.vides <- paste(sum(is.na(D)), "(", round(sum(is.na(D)) / N * 100, 0), "%", ")")
  sub.comp.lig <- dim(D[complete.cases(D[, -which(C.vide)]), ])[1] # nombre de lignes completes (sans les colonnes vides)
  sub.comp <- paste(sub.comp.lig, " lignes, dans ", sum(!C.vide), " colonnes", sep = "")

  #----------------------------------------------------

  RES1 <- matrix(NA, nrow = 3, ncol = 1)
  RES1[1, 1] <- n.col
  RES1[2, 1] <- n.lignes
  RES1[3, 1] <- N
  colnames(RES1) <- ""
  rownames(RES1) <- c("Nombre de colonnes", "Nombre de lignes", "Nombre de donnees")

  RES2 <- matrix(NA, nrow = 4, ncol = 1)
  RES2[1, 1] <- col.vides
  RES2[2, 1] <- lig.vides
  RES2[3, 1] <- dat.vides
  RES2[4, 1] <- sub.comp

  colnames(RES2) <- ""
  rownames(RES2) <- c("Nombre de colonnes vides", "Nombre de lignes vides", "Nombre de donnees manquantes", "Dimensions ss-groupe complet")

  RES3 <- matrix(NA, nrow = 4, ncol = 1)
  RES3[1, 1] <- n.quanti
  RES3[2, 1] <- n.quali
  RES3[3, 1] <- n.txt
  RES3[4, 1] <- n.autres
  colnames(RES3) <- ""
  rownames(RES3) <- c("Nombre de variables Quantitatives", "Nombre de variables Qualitatives", "Nombre de variables Textuelles", "Nombre de variables d’un autre type ")

  RES <- list("Dim" = RES1, "Données manquantes" = RES2, Nature = RES3)

  if (situation == 0) {
    RES <- "1 seule variable a decrire, utilisez desc()"
  }
  return(RES)
}

desql <- function(X) {
  nomX <- deparse(substitute(X))
  cat("\nDescriptif de la variable : ", nomX, "  \n\n")
  if (!is.factor(X)) {
    X <- as.factor(X)
  }
  nbl <- nlevels(X)
  aze <- matrix(NA, ncol = 2, nrow = nbl + 3)
  rownames(aze) <- c(levels(X), "Total", "Non Manquants", "MANQUANTS")
  colnames(aze) <- c("Effectifs", "Proportions")
  tcr <- table(X)
  aze[1:(nbl), 1] <- table(X, exclude = NULL)[1:(nbl)]
  aze[1:nbl, 2] <- round(tcr * 100 / sum(tcr), digits = 3)
  aze[nbl + 1, 1] <- sum(tcr)
  aze[nbl + 1, 2] <- 100
  nbmq <- table(X, exclude = NULL)[nbl + 1]
  nbmq <- ifelse(is.na(nbmq), 0, nbmq)
  aze[nbl + 3, 1] <- nbmq
  aze[nbl + 3, 2] <- nbmq * 100 / length(X)
  aze[nbl + 2, 1] <- length(X) - aze[nbl + 3, 1]
  aze[nbl + 2, 2] <- 100 - aze[nbl + 3, 2]

  return(aze)
}


plot.na <- function(data_in, title = NULL) {
  temp_df <- as.data.frame(ifelse(is.na(data_in), 1, 0))
  temp_df <- temp_df[, order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(
    x = unlist(data_temp$x),
    y = unlist(data_temp$y),
    m = unlist(data_temp$m)
  )

  ggplot(data_temp) +
    geom_tile(aes(x = x, y = y, fill = factor(m))) +
    scale_fill_manual(
      values = c("lightblue", "red"),
      name = "Missing\n(1=Yes, 0=No)"
    ) +
    theme_light() +
    ylab("Variable") +
    xlab("Numero du sujet") +
    ggtitle(title)
}






ggcompar <- function(x, y, DDD = NULL) {
  if (is.null(DDD)) {
    DDD <- as.data.frame(cbind(x, y))
  } else {
    DDD <- DDD[, c(x, y)]
  }
  titre <- paste("Distributions de", x, "en fonction de", y)
  DDD[, 2] <- as.factor(DDD[, 2])
  return(
    ggplot(DDD, aes(x = DDD[, 1], fill = DDD[, 2])) +
      geom_density(alpha = .3) +
      ggtitle(titre) +
      xlab(x) +
      guides(fill = guide_legend(title = y))
  )
}


ggpoints <- function(x, y, droite = 0, nomx = NULL, nomy = NULL) {
  options(warn = -1)
  if (is.null(nomx)) {
    nomx <- deparse(substitute(x))
  }
  if (is.null(nomy)) {
    nomy <- deparse(substitute(y))
  }
  if (!require(ggplot2)) install.packages("ggplot2")
  library(ggplot2)
  if (length(x) == length(y)) {
    DDD <- data.frame(cbind(y, x))
    c <- ggplot(DDD, aes(x, y))
    # x11()
    if (droite == 1) {
      return(c + stat_smooth(method = "lm") + geom_point() + xlab(nomx) + ylab(nomy))
    } else {
      if (droite == 0) {
        return(c + geom_point() + xlab(nomx) + ylab(nomy))
      }
    }
    if (droite != 0 & droite != 1) {
      return("Mauvaise valeur pour l'option droite")
    }
  } else {
    return("Les longueurs des vecteurs ne sont pas egales")
  }
  options(warn = 0)
}







ggpie <- function(Valeur, Groupe = "Graphique") {
  library(reshape)
  library(plyr)
  library(ggplot2)
  y <- data.frame(
    category = Groupe,
    value = Valeur
  )
  # get counts and melt it
  data.m <- melt(table(y))
  names(data.m)[3] <- "count"
  # calculate percentage:
  m1 <- ddply(data.m, .(category), summarize, ratio = count / sum(count))
  # order data frame (needed to comply with percentage column):
  m2 <- data.m[order(data.m$category), ]
  # combine them:
  mydf <- data.frame(m2, ratio = m1$ratio)
  # get positions of percentage labels:
  mydf <- ddply(mydf, .(category), transform, position = cumsum(ratio) - 0.5 * ratio)
  # create bar plot
  colnames(mydf)[2] <- "Legende"
  mydf$Legende <- as.factor(mydf$Legende)
  pie <- ggplot(mydf, aes(x = factor(1), y = ratio, fill = Legende)) +
    theme_void() +
    xlab("") +
    ylab("") +
    geom_bar(stat = "identity", width = 1) +
    facet_wrap(~category)
  # make a pie
  pie <- pie + coord_polar(theta = "y")
  # add labels
  return(
    pie +
      geom_text(aes(label = sprintf("%1.2f%%", ratio * 100), y = 1 - position))
  )
}

pie <- function(x, nomx, na.rm = FALSE) {
  if (missing(nomx)) {
    nomx <- deparse(substitute(x))
  }
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }


  if (sum(is.na(x)) != 0) {
    levels(x) <- c(levels(x), "NA")
    x[is.na(x)] <- "NA"
  }

  effec <- as.numeric(table(factor(as.character(x))))
  labls <- names(table(factor(as.character(x))))
  zz <- data.frame(labls, effec)

  myTitle <- paste("Proportions : Variable", nomx)
  percent <- paste(as.character(round(effec / sum(effec) * 100, 1)), "%")
  coord <- effec / 2 + c(0, cumsum(effec)[-length(effec)])

  g <- ggplot(data.frame(labls, effec, coord, percent), aes(x = "", y = effec, fill = labls)) +
    geom_bar(width = 1) +
    coord_polar("y") +
    labs(x = "") +
    labs(y = "") +
    geom_text(aes(y = coord, label = percent), size = 7) +
    guides(fill = guide_legend(label.position = "bottom", keywidth = 3, keyheight = 3, title = "", title.theme = element_text(size = rel(2), angle = 0))) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(colour = "black", angle = 0, size = 13, hjust = 2, vjust = 2, face = "bold"),
      plot.title = element_text(size = 20)
    ) +
    labs(title = myTitle, size = 15)


  return(g)
}

