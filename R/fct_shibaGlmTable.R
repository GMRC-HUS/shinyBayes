#' shibaGlmTable
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



shibaGlmTable <- function(fit, type_glm, seuilTwoIt = NULL, ...) {
  print(seuilTwoIt)

  res <- fit$stan_summary %>%
    as.data.frame() %>%
    dplyr::select(Médiane = `50%`, `2.5%`, `97.5%`)
  print(res)

  ligne_mean_PPD <- which(rownames(res) == "mean_PPD")
  if (is.null(seuilTwoIt)) {
    print("type_glm")

    if (type_glm %in% c("poiss", "binom")) {
      res[1:ligne_mean_PPD - 1, 1:3] <- exp(res[1:ligne_mean_PPD - 1, 1:3])
      names(res)[1] <- ifelse(type_glm == "poiss", "RR", "OR")
    }

    return(res)
  }




  nomsModel <- fit$coefficients %>% names()

  if (seuilTwoIt$type == "seuil") {
    if (type_glm %in% c("poiss", "binom")) {
      seuils <- log(seuilTwoIt$val)
      seuil_init <- seuilTwoIt$val
    } else {
      seuil_init <- seuils <- (seuilTwoIt$val)
    }


    if (seuilTwoIt$plusieur_seuils) {
      seuils_df <- data.frame(
        seuils = c(NA, seuils),
        parameter = nomsModel
      )


      res_s <- sapply(
        nomsModel[-1],
        function(i) length(which(as.array(fit)[, , i] > seuils_df$seuils[seuils_df$parameter == i])) / length(as.array(fit)[, , i])
      )
    } else {
      seuils <- rep(seuils, length(nomsModel))
      seuil_init <- rep(seuil_init, length(nomsModel))
      seuils[1] <- NA
      seuil_init <- seuil_init[-1]
      seuils_df <- data.frame(
        seuils = seuils,
        parameter = nomsModel
      )
      res_s <- sapply(
        nomsModel[-1],
        function(i) length(which(as.array(fit)[, , i] > seuils_df$seuils[seuils_df$parameter == i])) / length(as.array(fit)[, , i])
      )
    }

    res_seuil <- data.frame(Seuil = c("", seuil_init), Prob = c("", res_s))
    res_seuil[(nrow(res_seuil) + 1):(nrow(res_seuil) + (nrow(res) - nrow(res_seuil))), ] <- ""
    res <- cbind(res, res_seuil)

    ligne_mean_PPD <- which(rownames(res) == "mean_PPD")
    if (type_glm %in% c("poiss", "binom")) {
      res[1:ligne_mean_PPD - 1, 1:3] <- exp(res[1:ligne_mean_PPD - 1, 1:3])
      names(res)[1] <- ifelse(type_glm == "poiss", "RR", "OR")
    }

    return(res)
  } else if (!is.null(seuilTwoIt$val$var)) {
    if (!seuilTwoIt$val$var %in% nomsModel) {
      if (type_glm %in% c("poiss", "binom")) {
        res[1:ligne_mean_PPD - 1, 1:3] <- exp(res[1:ligne_mean_PPD - 1, 1:3])
        names(res)[1] <- ifelse(type_glm == "poiss", "RR", "OR")
      }

      return(res)
    }




    list_param <- seuilTwoIt$val
    if (type_glm %in% c("poiss", "binom")) {
      twoIt <- twoItStanGlm(fit, list_param$var,
        HA_diff_l = log(list_param$theta_A_min), HA_diff_u = log(list_param$theta_A_max),
        HP_diff_l = log(list_param$theta_P_min), HP_diff_u = log(list_param$theta_P_max)
      )

      twoIt$names[1:2] <- c(
        paste0("PR(", list_param$theta_A_min, " < diff < ", list_param$theta_A_max, ")"),
        paste0("PR(", list_param$theta_P_min, " < diff < ", list_param$theta_P_max, ")")
      )
    } else {
      twoIt <- twoItStanGlm(fit, list_param$var,
        HA_diff_l = list_param$theta_A_min, HA_diff_u = list_param$theta_A_max,
        HP_diff_l = list_param$theta_P_min, HP_diff_u = list_param$theta_P_max
      )
    }

    Pr <- data.frame(c("H|Prior", "H|Données"), twoIt$values[1:2], twoIt$values[3:4])
    names(Pr) <- c("Two It", twoIt$names[1:2])
    ligne_para<- which(rownames(res) == list_param$var)
    matrice_vide <- as.data.frame(matrix("",nrow = ligne_para-1,ncol = 3))
    names(matrice_vide)<- names(Pr)
    Pr<- rbind(matrice_vide, Pr)
    
    
    ligne_vide <- as.data.frame(matrix("",nrow = 1,ncol = ncol(res)))
    rownames(ligne_vide)<-""
    names(ligne_vide) <- names(res)
    
    
    
    print(ligne_para)
    Pr[(nrow(Pr) + 1):(nrow(Pr) + (nrow(res) - nrow(Pr)+1)), ] <- ""
    n_ligne<- nrow(res)
    print(Pr)
    print(n_ligne)
    res<- rbind(res[1:ligne_para,],ligne_vide, res[(ligne_para+1):n_ligne,])
    print(res)
    res <- cbind(res, Pr)
    ligne_mean_PPD <- which(rownames(res) == "mean_PPD")


    if (type_glm %in% c("poiss", "binom")) {
      res[1:ligne_mean_PPD - 1, 1:3] <- exp(res[1:ligne_mean_PPD - 1, 1:3])
      names(res)[1] <- ifelse(type_glm == "poiss", "RR", "OR")
    }

    return(res)
  }
}
