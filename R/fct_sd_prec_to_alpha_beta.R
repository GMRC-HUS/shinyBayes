#' sd_prec_to_alpha_beta
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


#' inferenceUni UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import gmrc.bayes

mod_sd_prec_to_alph_beta_ui <- function(id) {
  ns <- NS(id)
  showModal(
    modalDialog(uiOutput(ns("alpha_beta_ui")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ok"), "OK")
      )
    )
  )
}

#' inferenceUni Server Functions
#'
#' @noRd
mod_sd_prec_to_alph_beta_server <- function(id, alpha, beta, parent, alpha_name, beta_name) {
  moduleServer(id, function(input, output, session) {
    # alpha=nu_0
    # beta=nu_0*sigma_02






    output$alpha_beta_ui <- renderUI({
      ns <- session$ns
      tagList(
        plotOutput(width = 200, height = 100, ns("ellicitationcurve")),
        numericInput(ns("sigma_0"), "A priori % : ",
          min = 0, max = 100, value = isolate(alpha)
        ),
        numericInput(ns("n_sigma_0"), "Taille pseudo population % : ",
          min = 0, max = 2000, value = (1 / (alpha / isolate(beta)))
        )
      )
    })





    output$ellicitationcurve <- renderPlot({
      # print(input)
      alpha_0 <- input$sigma_0
      beta_0 <- alpha_0 * input$n_sigma_0

      ggplot(data = data.frame(x = c(0, input$sigma_0 * 4)), aes(x)) +
        stat_function(fun = dgamma, args = list(shape = alpha_0, rate = 1 / beta_0)) +
        theme_light() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        ylab("") +
        xlab("")
    })



    observeEvent(input$ok, {
      # print(session)
      updateNumericInput(parent, alpha_name, value = input$sigma_0)
      updateNumericInput(parent, beta_name, value = input$sigma_0 * input$n_sigma_0)
      print(input$sigma_0)
    })

    # return(list(
    #   level3_select = reactive({input$sigma_0})
    # ))
    #
  })
}

## To be copied in the UI
# mod_inferenceUni_ui("inferenceUni_1")

## To be copied in the server
# mod_inferenceUni_server("inferenceUni_1")
