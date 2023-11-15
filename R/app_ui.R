#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @noRd
#' @dashboardthemes

# source("./theme.R", local = TRUE)


couleur_shiba <-"#428bca"
couleur_mddle = "#bed9ed"
couleur_back_logo <- "rgb(64,64,64)"
### creating custom theme object
customTheme <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Arial",
  appFontColor = "rgb(0,0,0)",
  primaryFontColor = "rgb(0,0,0)",
  infoFontColor = "rgb(0,0,0)",
  successFontColor = "rgb(0,0,0)",
  warningFontColor = "rgb(0,0,0)",
  dangerFontColor = "rgb(0,0,0)",
  bodyBackColor = "rgb(248,248,248)"

  ### header
  , logoBackColor = couleur_back_logo,
  headerButtonBackColor = couleur_shiba,
  headerButtonIconColor = "rgb(75,75,75)",
  headerButtonBackColorHover = "rgb(210,210,210)",
  headerButtonIconColorHover = "rgb(0,0,0)",
  headerBackColor = couleur_shiba,
  headerBoxShadowColor = "#aaaaaa",
  headerBoxShadowSize = "2px 2px 2px"

  ### sidebar
  , sidebarBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = couleur_back_logo,
    colorMiddle = "rgb(96,96,96)",
    colorEnd ="rgb(64,64,64)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarPadding = 0,
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 0,
  sidebarMenuBorderRadius = 0,
  sidebarShadowRadius = "3px 5px 5px",
  sidebarShadowColor = "#aaaaaa",
  sidebarUserTextColor = "rgb(255,255,255)",
  sidebarSearchBackColor = "rgb(55,72,80)",
  sidebarSearchIconColor = "rgb(153,153,153)",
  sidebarSearchBorderColor = "rgb(55,72,80)",
  sidebarTabTextColor = "rgb(255,255,255)",
  sidebarTabTextSize = 13,
  sidebarTabBorderStyle = "none none solid none",
  sidebarTabBorderColor = "rgb(96,96,96)",
  sidebarTabBorderWidth = 1,
  sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right",
    colorStart = couleur_shiba,
    colorMiddle = couleur_mddle,
    colorEnd = "rgba(240,240,255,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorSelected = "rgb(0,0,0)",
  sidebarTabRadiusSelected = "0px 20px 20px 0px",
  sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right",
    colorStart = couleur_shiba,
    colorMiddle = couleur_mddle,
    colorEnd = "rgba(240,240,255,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorHover = "rgb(50,50,50)",
  sidebarTabBorderStyleHover = "none none solid none",
  sidebarTabBorderColorHover = "rgb(75,126,151)",
  sidebarTabBorderWidthHover = 1,
  sidebarTabRadiusHover = "0px 20px 20px 0px"

  ### boxes
  , boxBackColor = "rgb(255,255,255)",
  boxBorderRadius = 5,
  boxShadowSize = "0px 1px 1px",
  boxShadowColor = "rgba(0,0,0,.1)",
  boxTitleSize = 16,
  boxDefaultColor = "rgb(210,214,220)",
  boxPrimaryColor = couleur_shiba,
  boxInfoColor = "rgb(210,214,220)",
  boxSuccessColor = "rgba(60,179,113,1)",
  boxWarningColor = "rgb(244,156,104)",
  boxDangerColor = "rgb(255,88,55)",
  tabBoxTabColor = "rgb(255,255,255)",
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = "rgb(0,0,0)",
  tabBoxTabTextColorSelected = "rgb(0,0,0)",
  tabBoxBackColor = "rgb(255,255,255)",
  tabBoxHighlightColor = "rgba(44,222,235,1)",
  tabBoxBorderRadius = 5

  ### inputs
  , buttonBackColor = "rgb(245,245,245)",
  buttonTextColor = "rgb(0,0,0)",
  buttonBorderColor = "rgb(200,200,200)",
  buttonBorderRadius = 5,
  buttonBackColorHover = "rgb(235,235,235)",
  buttonTextColorHover = "rgb(100,100,100)",
  buttonBorderColorHover = "rgb(200,200,200)",
  textboxBackColor = "rgb(255,255,255)",
  textboxBorderColor = "rgb(200,200,200)",
  textboxBorderRadius = 5,
  textboxBackColorSelect = "rgb(245,245,245)",
  textboxBorderColorSelect = "rgb(200,200,200)"

  ### tables
  , tableBackColor = "rgb(255,255,255)",
  tableBorderColor = "rgb(240,240,240)",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1
)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(
      # tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css"),
       tags$style(HTML(css_perso)),
       tags$script(HTML(js))
    ),
  customTheme,
    # addResourcePath("www", tempdir),

    # Dev button
    # actionButton("browser", "browser"),
    # tags$script("$('#browser').hide();"),
    # By default, this button will be hidden.
    # To show it, open your web browser JavaScript console
    # And run $('#browser').show();

    # Your application UI logic

        dashboardPage(
          dashboardHeader(title =  span(img(src="www/shiba_def.png", height = "100%" ))),
          dashboardSidebar(
            sidebarMenu(
      
         
              menuItem("Accueil", tabName = "accueil", icon = icon("far fa-star", verify_fa = FALSE)),
              menuItem("Base de données", tabName = "base", icon = icon("fas fa-database", verify_fa = FALSE),
                       startExpanded = F,
                       menuSubItem("Chargement des données",
                                   tabName = "chargement"),
                       menuSubItem("Visualisation de la base",
                                   tabName = "info_base")
                       
                       ),
              menuItem("Descriptif", tabName = "descriptif", icon = icon("fal fa-percent", verify_fa = FALSE)),
              menuItem("Croisement", tabName = "crois", icon = icon("far fa-chart-bar", verify_fa = FALSE),
                       startExpanded = F,
                       menuSubItem("Deux à deux",
                                   tabName = "croisements"),
                       menuSubItem("Table 1",
                                   tabName = "table_1")
                       
              ),
                       
                       
       
              menuItem("Inférence univariée",
                       tabName = "infUni" , icon = icon("fas fa-calculator",verify_fa = FALSE),
                       startExpanded = F,
                       menuSubItem("Moyennes",
                                   tabName = "infe_moy"),
                       menuSubItem("Pourcentages",
                                   tabName = "infe_pour")
              ),
              menuItem("Comparaison", tabName = "comp", icon = icon("fas fa-people-arrows", verify_fa = FALSE),
                       startExpanded = F,
                       menuSubItem("Moyennes",
                                   tabName = "comp_moy"),
                       menuSubItem("Pourcentages",
                                   tabName = "comp_pour")
                       
              ),
              
          menuItem("Multivarié",
                   tabName = "multivarie_Uni"  , icon = icon("fas fa-table",verify_fa = FALSE)
          ),
          menuItem("Crédits",
                   tabName = "credit_Uni"  , icon = icon("fas fa-table",verify_fa = FALSE)
          )
              # menuItem("Croisements/Inférence", tabName = "croisements", icon = icon("fa-light fa-dice", verify_fa = FALSE)),
              # menuItem("Analyse de survie", tabName = "survie", icon = icon("fa-light fa-skull-crossbones", verify_fa = FALSE)),
              # menuItem("Tests diagnostiques", tabName = "tests", icon = icon("fa-light fa-vial", verify_fa = FALSE)),
              # menuItem("Concordance", tabName = "concordance", icon = icon("fa-light fa-equals", verify_fa = FALSE))
            )
          ),
          dashboardBody(

            # dashboardthemes::shinyDashboardThemes(
            #   theme = "custom_theme"
            # ),
            # customTheme,
            tabItems(
              # First tab content
              tabItem(
                tabName = "accueil",
                mod_Accueil_ui("Accueil_1")
              ),

              # Second tab content
              tabItem(
                tabName = "chargement",
                mod_chargement_ui("chargement_1")
              ),
              tabItem(
                tabName = "info_base",
                mod_Info_base_ui("info_base_1")
              ),

              # Third tab content
              tabItem(
                tabName = "descriptif",
                mod_Descriptifs_ui("Descriptifs_1")
              ),
              # tabItem(
              #   tabName = "infUni",
              #   mod_inferenceUni_ui("inferenceUni_1")
              # ),
              tabItem(
                tabName = "multivarie_Uni",
                mod_Multivarie_ui("multivarie_1")
              ),
              tabitem(tabName = "credit_Uni",
                      mod_credit_ui("credit_1")),
              
              tabItem(
                tabName = "croisements",
                mod_Croisements_ui("Croisements_1")
              ),
              tabItem(
                tabName = "table_1",
                mod_table1_ui("table_1")
              ),
              
              
              tabItem(
                tabName = "infe_moy",
                mod_infe_moy_ui("infe_moy_1")
              ),
              tabItem(
                tabName = "infe_pour",
                mod_infe_pour_ui("infe_pour_1")
              ),
              tabItem(
                tabName = "comp_moy",
                mod_comp_moy_ui("comp_moy_1")
              ),
              tabItem(
                tabName = "comp_pour",
                mod_comp_pour_ui("comp_pour_1")
              )
              
              
              
              


              
              
              
              
              
              # tabItem(
              #   tabName = "survie",
              #   mod_Survie_ui("Survie_1")
              # ),
              # tabItem(
              #   tabName = "tests",
              #   mod_Tests_ui("Tests_1")
              # ),
              # tabItem(
              #   tabName = "concordance",
              #   mod_Concordance_ui("Concordance_1")
              )
            )
          )
        
      
    
    # ,
    #   tabPanel(
    #     "Saisie manuelle",
    #     mod_SaisieManuelle_ui("SaisieManuelle_1")
    #   ),
    #   tabPanel(
    #     "Rédaction",
    #     mod_Redaction_ui("Redaction_1")
    #   )
    )
  
}



# tagList(
#   # Leave this function for adding external resources
#   golem_add_external_resources(),
#   # Your application UI logic
#   tabsetPanel(id="Panneau",
#               tabPanel("Accueil",
#                        mod_chargement_ui("chargement_1")
#               ))
#
# )




#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    'www', app_sys('app/www')
  )


  tags$head(
    favicon(),
    # bundle_resources(
    #   path =  system.file('app/www', package = 'GmrcShinyStats'),
    #   app_title = "GmrcShinyStats"
    # )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
