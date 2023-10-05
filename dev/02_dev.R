# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module(name = "chargement", with_test = FALSE) # Name of the module
golem::add_module(name = "Accueil", with_test = FALSE) # Name of the module

golem::add_module(name = "inferenceUni", with_test = T) # Name of the module

golem::add_module(name = "Descriptifs", with_test = FALSE) # Name of the module
golem::add_module(name = "Croisements", with_test = FALSE) # Name of the module
golem::add_module(name = "Survie", with_test = FALSE) # Name of the module
golem::add_module(name = "Tests", with_test = FALSE) # Name of the module
golem::add_module(name = "Concordance", with_test = FALSE) # Name of the module
golem::add_module(name = "SaisieManuelle", with_test = FALSE) # Name of the module
golem::add_module(name = "Redaction", with_test = FALSE) # Name of the module

golem::add_module(name = "Multivarie", with_test = T)
golem::add_module(name = "Info_base", with_test = F)
golem::add_module(name = "table1", with_test = F)

golem::add_module(name = "comp_moy", with_test = F)
golem::add_module(name = "comp_pour", with_test = F)

golem::add_module(name = "infe_moy", with_test = F)
golem::add_module(name = "infe_pour", with_test = F)


golem::add_fct("ifelse_perso")
golem::add_fct("glm_shiba")
golem::add_fct( "fonctions" ) 
golem::add_utils( "fonctions" )
golem::add_utils("UI")
golem::add_fct( "code_sans_dep" ) 

golem::add_fct("modal_seuil2It")
golem::add_fct("twoIt")
golem::add_fct("shibaGlmPlot")
golem::add_fct("shibaGlmTable")

golem::add_fct("prop_comp")

golem::add_fct("infe_moy")




golem::add_fct("table_interact")

usethis::use_build_ignore("r/exemple.R")

getSession <- function() {
  session <- shiny::getDefaultReactiveDomain()

  if (is.null(session)) {
    stop("Could not find the Shiny session object. This usually happens if you're trying to call a function outside of a Shiny app.", call. = FALSE)
  }

  session
}

# insert content into the <head> tag of the document if this is a proper
# Shiny app, but if it's inside an interactive Rmarkdown document then don't
# use <head> as it won't work
insertHead <- function(...) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    runtime <- knitr::opts_knit$get("rmarkdown.runtime")
    if (!is.null(runtime) && runtime == "shiny") {
      # we're inside an Rmd document
      shiny::tagList(...)
    } else {
      # we're in a shiny app
      shiny::tags$head(...)
    }
  } else {
    # we're in a shiny app
    shiny::tags$head(...)
  }

}

getDependencies <- function() {
  list(
    htmltools::htmlDependency(
      name = "shinycssloaders-binding",
      version = as.character(utils::packageVersion("shinycssloaders")),
      package = "shinycssloaders",
      src = "assets",
      script = "spinner.js",
      stylesheet = "spinner.css"
    )
  )
}

get_proxy_element <- function(ui_element, proxy.height, hide.ui) {
  if (!hide.ui) {
    return(shiny::tagList())
  }

  if (is.null(proxy.height)) {
    if (!grepl("height:\\s*\\d", ui_element)) {
      proxy.height <- "400px"
    }
  } else {
    if (is.numeric(proxy.height)) {
      proxy.height <- paste0(proxy.height, "px")
    }
  }

  if (is.null(proxy.height)) {
    proxy_element <- shiny::tagList()
  } else {
    proxy_element <- shiny::div(style=glue::glue("height:{proxy.height}"),
                                class="shiny-spinner-placeholder")
  }
}

get_spinner_css_tag <- function(type, color, size, color.background, custom.css, id, image, caption, output_spinner) {
  base_css <- ""
  add_default_style <- (is.null(image) && !custom.css && type != 0)
  if (add_default_style) {
    if (type %in% c(2, 3) && is.null(color.background)) {
      stop("shinycssloaders: For spinner types 2 & 3 you need to specify `color.background`.")
    }

    color.rgb <- paste(grDevices::col2rgb(color), collapse = ",")
    color.alpha0 <- sprintf("rgba(%s, 0)", color.rgb)
    color.alpha2 <- sprintf("rgba(%s, 0.2)", color.rgb)

    css_file <- system.file(glue::glue("loaders-templates/load{type}.css"), package="shinycssloaders")
    if (file.exists(css_file)) {
      base_css <- paste(readLines(css_file), collapse = " ")
      base_css <- glue::glue(base_css, .open = "{{", .close = "}}")
    }

    # get default font-size from css, and cut it by 25%, as for outputs we usually need something smaller
    size_px <- round(c(11, 11, 10, 20, 25, 90, 10, 10)[type] * size * 0.75)
    base_css <- paste(base_css, glue::glue("#{id} {{ font-size: {size_px}px; }}"))
  }

  if (!is.null(caption)) {
    base_css <- paste(base_css, glue::glue("#{id}__caption {{ color: {color}; font-size: {size + 0.5}em; }}"))
  }

  css_rules_tag <- NULL
  if (nzchar(base_css)) {
    css_rules_tag <- insertHead(shiny::tags$style(
      class = if (!output_spinner) "global-spinner-css",
      shiny::HTML(base_css)
    ))
  }
  css_rules_tag
}
## 2.2 Add dependencies

usethis::use_package( "shiny" ) 
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinyFiles" )
usethis::use_package( "irr" )
usethis::use_package( "gdata" )
usethis::use_package( "boot" )
usethis::use_package( "xtable" )
usethis::use_package( "dplyr" )
usethis::use_package( "ggplot2" )
#usethis::use_package( "DataExplorer" )
usethis::use_package( "ggthemes" )
usethis::use_package( "pROC" )
usethis::use_package( "dashboardthemes" )
usethis::use_package( "shinydashboardPlus")
usethis::use_package("utils" )
usethis::use_package("desctable" )
usethis::use_package("moments" )
usethis::use_package("sortable" )
usethis::use_package("broom.mixed" )
usethis::use_package("rstanarm" )
usethis::use_package("bslib" )
usethis::use_package("ggdist" )
usethis::use_package("coda" )
usethis::use_package("bayesplot" )
usethis::use_package("gridExtra" )
usethis::use_package("shinyalert" )
usethis::use_package("waiter" )
usethis::use_package("shinycssloaders" )
usethis::use_package("colourpicker" )

usethis::use_package("data.table" )
usethis::use_package("plotly" )

usethis::use_package("coda" )
usethis::use_package("colourpicker" )
usethis::use_package("ggridges" )
usethis::use_package("plotly" )
usethis::use_package("plyr" )



attachment::att_from_rscripts()

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()


## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
# golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("GmrcShinyBayes")
devtools::build_vignettes()


## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()


## 3.3 format r script
styler::style_file("R/mod_Multivarie.R")

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")


golem::browser_button()
golem::add_css_file("mise_en_forme")
usethis::use_package("shinyWidgets")
golem::add_fct("sd_prec_to_alpha_beta")


usethis::use_pkgdown_github_pages()
pkgdown::build_site()




