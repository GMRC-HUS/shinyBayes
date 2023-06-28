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

golem::add_fct("ifelse_perso")
golem::add_fct("glm_shiba")
golem::add_fct( "fonctions" ) 
golem::add_utils( "fonctions" )
golem::add_fct( "code_sans_dep" ) 
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

