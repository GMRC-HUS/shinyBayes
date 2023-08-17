#' fonctions
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import gridExtra
#' @import coda
#' @import bayesplot
#' @importFrom plyr alply

theme_ShiBA <- theme_light

pasDeBase_ui<- function() {
  fluidPage(
  h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible."),
  p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
)
}
size_box <- "150px"

css <- "
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #ffffff;
  color: black;
  border: 1px solid lightgray;
  padding: 10px;
  font-size: 25px;
  font-style: italic;
  text-align: justify;
  margin-left: 0;
  max-width: 1000px;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}

.fas{font-size: 20px;}

"

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"

bold <- function(x){
  return(paste0("<b>",x,"</b>"))
}


diagrammeBarre <- function(base) {
  data <- tablePourcent(base)
  bp <- ggplot(data = data, aes(x = nom, y = pourcent * 100, fill = reorder(nom, 1 / pourcent)))
  
  maxPourcent <- max(data$pourcent, na.rm = T)
  label <- paste(round(data$pourcent, 3) * 100, "%")
  vjust <- unlist(as.list(ifelse(data$pourcent < maxPourcent / 5, -1.6, 1.6)), use.names = F)
  
  barre <- bp +
    labs(
      title = "Diagramme en barre",
      x = "", y = "pourcentage"
    ) +
    geom_bar(stat = "identity", color = "black") +
    guides(fill = guide_legend(override.aes = list(colour = NULL))) +
    
    
    geom_text(aes(label = label), vjust = vjust, color = "black", size = 5) +
    theme(plot.title = element_text(lineheight = 3, face = "bold", color = "black", size = 17))
  
  barre$labels$fill <- ""
  return(barre)
}

tablePourcent <- function(base) {
  pourcent <- prop.table(table(base))
  pourcent <- pourcent[order(pourcent)]
  
  data <- data.frame(pourcent = as.numeric(pourcent), nom = names(pourcent))
  
  
  
  return(data)
}


sd_quali <- function(list_var, BDD) {
  unlist(sapply(list_var, function(x) {
    sapply(levels(BDD[, x])[-1], function(i) sd(BDD[, x] == i, na.rm = T))
  }))
}



text_aide <- function(text) {
  return(span(
    `data-toggle` = "tooltip", `data-placement` = "right",
    title = text,
    icon("info-circle")
  ))
}
formule <- function(x) {
  return(as.formula(paste("~", x)))
}

formule_default <- function(y, X_quanti, X_quali) {
  if (length(c(X_quanti, X_quali)) > 0) {
    var_interet <- paste(c(X_quanti, X_quali), collapse = " + ")
  } else {
    var_interet <- 1
  }

  paste(isolate(y), "~", var_interet)
}

df_prior<- function(fit){
  
  loc_var  <- prior_summary(fit)$prior$location
  scale_var <- prior_summary(fit)$prior$adjusted_scale
  if (is.null(scale_var)) {
    scale_var <- prior_summary(fit)$prior$scale
  }
  
  
  loc_inter  <- prior_summary(fit)$prior_intercept$location
  scale_inter <- prior_summary(fit)$prior_intercept$adjusted_scale
  if (is.null(scale_inter)) {
    scale_inter <- prior_summary(fit)$prior_intercept$scale
  }
  
  return(data.frame(Var = fit$coefficients%>%names, 
                     Position =c(loc_inter, loc_var),
                     Dispersion = c(scale_inter,scale_var)))
}


barplot_croise <- function(base, var1, var2) {
  BDD <- base[, c(var1, var2)]
  pourcent <- prop.table(table(BDD), 1)
  data <- as.data.frame(pourcent)
  names(data) <- c("var1", "var2", "Freq")
  data[, 1] <- as.factor(data[, 1])
  data[, 2] <- as.factor(data[, 2])
  maxPourcent <- max(data$Freq, na.rm = T)
  label <- paste(round(data$Freq, 3) * 100, "%")
  vjust <- unlist(as.list(ifelse(data$Freq < maxPourcent / 5, -1.6, 1.6)), use.names = F)

  barplotCroise <- ggplot(data = data, aes(x = var2, y = Freq)) +
    geom_col(position = "dodge", color = "black", aes(fill = var2)) +
    facet_wrap(~var1) +
    geom_text(data = data, aes(label = paste(round(Freq, 3) * 100, "%")), vjust = vjust, color = "black", size = 5) +
    theme(plot.title = element_text(lineheight = 3, face = "bold", color = "black", size = 17)) +
    ggtitle(paste("En fonction de ", var1, sep = "")) +
    xlab(var2) +
    labs(fill = var2)
  return(barplotCroise)
}

tablePourcent <- function(base) {
  pourcent <- prop.table(table(base))
  pourcent <- pourcent[order(pourcent)]

  data <- data.frame(pourcent = as.numeric(pourcent), nom = names(pourcent))



  return(data)
}

diag_convergence <- function(fit, rhat = 1.05, autocorr = 0.2, lags = c(1:50)) {
  s <- as.array(fit)
  mcmc <- as.mcmc.list(alply(s[, , -dim(s)[3]], 2, function(x) as.mcmc(x)))

  AC <- autocorr.diag(mcmc, lags = lags)


  rhats <- rhat(fit)
  check <- is.element(T, rhats < 1.05) | is.element("TRUE", AC < autocorr)
  return(check)
}
verif_conv <- function(fit, shiny = F, nb_repeat = 3) {
  for (i in 1:nb_repeat) {
    print(i)
    checks <- diag_convergence(fit)
    if (checks) {
      return(fit)
    }
    fit <-
      update(
        fit,
        iter = (fit$stanfit@sim$iter * 2),
        keep_every = fit$stanfit@sim$thin * 2,
        seed = 42
      )
    print(fit$stanfit@sim$iter)
    checks <- diag_convergence(fit)
    if (checks) {
      return(fit)
    }

    if (shiny) {
      shinyalert(
        "Warning!",
        paste0(
          "Problème de convergence\n\nLe nombre d'itération a été augmenté à ",
          fit$stanfit@sim$iter,
          "\n Thining augmenté à ",
          fit$stanfit@sim$thin,
          "\n\n"
        ),
        type = "warning"
      )
    } else {
      message(
        paste0(
          "Problème de convergence\n\nLe nombre d'itération a été augmenté à ",
          fit$stanfit@sim$iter,
          "\n\n Thining augmenté à ",
          fit$stanfit@sim$thin,
          "\n\n"
        )
      )
    }
  }
  if (shiny) {
    shinyalert("Warning!",
      paste0("Problème de convergence, vérifier le modèle"),
      type = "error"
    )
  } else {
    message("Problème de convergence, vérifier le modèle")
  }
  return(fit)
}


pieChart <- function(base) {
  data <- tablePourcent(base)
  bp <- ggplot(data = data, aes(x = 0, y = pourcent, fill = reorder(nom, 1 / pourcent))) +
    coord_polar(theta = "y")
  df <- try(data %>% mutate(pos = cumsum(sort(data$pourcent)) - sort(data$pourcent) / 2))

  if (length(df) > 0) {
    label <- (sort(round(data$pourcent * 100, 1)))
    label <- as.character(ifelse(label < 4, "", paste(label, "%")))

    nom <- data$nom[order(data$pourcent, decreasing = F)]

    y <- df$pos

    pie <- bp +
      labs(
        title = "Diagramme circulaire",
        x = "", y = ""
      ) +
      theme(axis.text.x = element_blank()) +
      geom_bar(stat = "identity", color = "black") +
      guides(fill = guide_legend(override.aes = list(colour = "black"))) +
      theme(
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank()
      ) +

      theme_void() +
      geom_text(aes(x = 0.2, y = df$pos, label = label), size = 6) +
      theme(plot.title = element_text(lineheight = 3, face = "bold", color = "black", size = 17))


    pie$labels$fill <- ""
    pie$theme$legend.title$size <- 15
    return(pie)
  } else {
    return("une erreur c'est produite")
  }
}


diagrammeBarre <- function(base) {
  data <- tablePourcent(base)
  bp <- ggplot(data = data, aes(x = nom, y = pourcent * 100, fill = reorder(nom, 1 / pourcent)))

  maxPourcent <- max(data$pourcent, na.rm = T)
  label <- paste(round(data$pourcent, 3) * 100, "%")
  vjust <- unlist(as.list(ifelse(data$pourcent < maxPourcent / 5, -1.6, 1.6)), use.names = F)

  barre <- bp +
    labs(
      title = "Diagramme en barre",
      x = "", y = "pourcentage"
    ) +
    geom_bar(stat = "identity", color = "black") +
    guides(fill = guide_legend(override.aes = list(colour = NULL))) +


    geom_text(aes(label = label), vjust = vjust, color = "black", size = 5) +
    theme(plot.title = element_text(lineheight = 3, face = "bold", color = "black", size = 17))

  barre$labels$fill <- ""
  return(barre)
}

t.testVarEgal <- function(x, ...) {
  t.test(x, var.equal = T, ...)
}

tests_autoGMRC <- function(var, grp) {
  grp <- grp %>% factor()
  if (nlevels(grp) < 2) {
    ~no.test
  } else if (var %>% is.factor()) {
    if (tryCatch(stats::chisq.test(var, grp)$p.value >= 0, warning = function(e) F, error = function(e) F)) {
      ~chisq.test
    } else {
      ~fisher.test
    }
  } else {
    all_normal <- all(var %>% tapply(grp, desctable::is.normal))
    if (nlevels(grp) == 2) {
      if (all_normal) {
        if (tryCatch(stats::var.test(var ~ grp)$p.value >
          0.1, warning = function(e) F, error = function(e) F)) {
          ~t.testVarEgal
        } else {
          ~ . %>% t.test(var.equal = F)
        }
      } else {
        ~wilcox.test
      }
    } else if (all_normal) {
      if (tryCatch(stats::bartlett.test(var ~ grp)$p.value >
        0.1, warning = function(e) F, error = function(e) F)) {
        ~ . %>% oneway.test(var.equal = T)
      } else {
        ~ . %>% oneway.test(var.equal = F)
      }
    } else {
      ~kruskal.test
    }
  }
}

file.choose2 <- function(...) {
  pathname <- NULL
  tryCatch(
    {
      pathname <- file.choose(T)
    },
    error = function(ex) {
    }
  )
  pathname
}


plot_diag <- function(fit, var) {
  chains <- mcmc_trace(fit, pars = var) + theme_ShiBA()

  autocorrelation <- mcmc_acf_bar(fit, pars = var) + facet_grid(Parameter ~ Chain) + theme_ShiBA()


  grid <- grid.arrange(chains, autocorrelation, nrow = 2)
  return(grid)
}

min_max_exp_to_norm <- function(min, max, prob = 0.95){
  
  min_log = log(min)
  max_log = log(max)
  
  moy  = (min_log+max_log)/2
  
  sd = abs(moy-min_log)/abs(qnorm((1-prob)/2))
  
  return(c(moy=moy, sd=sd))
  
}


norm_tomin_max_exp <- function(moy, sd, prob=0.95){
  



res<-  exp(qnorm(c((1-prob)/2, prob+(1-prob)/2), mean = moy, sd=sd))

  return(c(min=res[1], max=res[2]))
  
}



arrondi_echelle_sup <- function(x){
  ceiling(x/10**(floor(log(x,10))))*10**floor(log(x,10))
}
arrondi_echelle_inf <- function(x){
  floor(x/10**(floor(log(x,10))))*10**floor(log(x,10))
}

