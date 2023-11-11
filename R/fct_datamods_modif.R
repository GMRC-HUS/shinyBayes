#' datamods_modif 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import htmltools
#' @import rlang

import_server2<-function (id, validation_opts = NULL, allowed_status = c("OK", 
                                                         "Failed", "Error"), return_class = c("data.frame", 
                                                                                              "data.table", "tbl_df", "raw"), read_fns = list()) 
{
  allowed_status <- match.arg(allowed_status, several.ok = TRUE)
  return_class <- match.arg(return_class)
  if (length(read_fns) > 0) {
    if (!is_named(read_fns)) 
      stop("import_file_server: `read_fns` must be a named list.", 
           call. = FALSE)
    if (!all(vapply(read_fns, is_function, logical(1)))) 
      stop("import_file_server: `read_fns` must be list of function(s).", 
           call. = FALSE)
  }
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_rv <- reactiveValues(data = NULL)
    imported_rv <- reactiveValues(data = NULL)
    observeEvent(input$hidden, {
      data_rv$data <- NULL
      data_rv$name <- NULL
      if (length(validation_opts) < 1) {
        hideTab(inputId = "tabs-mode", target = "validate")
      }
    })
    observeEvent(input$from, {
      updateTabsetPanel(session = session, inputId = "tabs-import", 
                        selected = input$from)
    })
    from_env <- import_globalenv_server(id = "env", 
                                        trigger_return = "change", btn_show_data = FALSE, 
                                        reset = reactive(input$hidden))
    from_file <- import_file_server2(id = "file", trigger_return = "change", 
                                     btn_show_data = FALSE, reset = reactive(input$hidden), 
                                     read_fns = read_fns)
    from_copypaste <- import_copypaste_server(id = "copypaste", 
                                              trigger_return = "change", btn_show_data = FALSE, 
                                              reset = reactive(input$hidden))
    from_googlesheets <- import_googlesheets_server(id = "googlesheets", 
                                                    trigger_return = "change", btn_show_data = FALSE, 
                                                    reset = reactive(input$hidden))
    from_url <- import_url_server(id = "url", trigger_return = "change", 
                                  btn_show_data = FALSE, reset = reactive(input$hidden))
    observeEvent(from_env$data(), {
      data_rv$data <- from_env$data()
      data_rv$name <- from_env$name()
    })
    observeEvent(from_file$data(), {
      data_rv$data <- from_file$data()
      data_rv$name <- from_file$name()
    })
    observeEvent(from_copypaste$data(), {
      data_rv$data <- from_copypaste$data()
      data_rv$name <- from_copypaste$name()
    })
    observeEvent(from_googlesheets$data(), {
      data_rv$data <- from_googlesheets$data()
      data_rv$name <- from_googlesheets$name()
    })
    observeEvent(from_url$data(), {
      data_rv$data <- from_url$data()
      data_rv$name <- from_url$name()
    })
    observeEvent(data_rv$data, {
      req(data_rv$data)
      if (is.data.frame(data_rv$data)) {
        if (length(validation_opts) < 1) {
          datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
        }
        else {
          status <- validation_results$status()
          if (isTRUE(status %in% allowed_status)) {
            datamods:::toggle_widget(inputId = "confirm", 
                          enable = TRUE)
          }
          else {
            datamods:::toggle_widget(inputId = "confirm", 
                          enable = FALSE)
          }
        }
        datamods:::enable_tab("tabs-mode", "view")
        datamods:::enable_tab("tabs-mode", "update")
        datamods:::enable_tab("tabs-mode", "validate")
      }
      else {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
      }
    })
    output$view <- reactable::renderReactable({
      data <- req(data_rv$data)
      reactable::reactable(data, defaultColDef = reactable::colDef(header = datamods:::header_with_classes(data)), 
                           columns = list(), bordered = TRUE, compact = TRUE, 
                           striped = TRUE)
    })
    updated_data <- update_variables_server(id = "update", 
                                            data = reactive(data_rv$data), height = "300px")
    validation_results <- validation_server(id = "validation", 
                                            data = reactive({
                                              data_rv$data
                                            }), n_row = validation_opts$n_row, n_col = validation_opts$n_col, 
                                            n_row_label = validation_opts$n_row_label %||% "Valid number of rows", 
                                            n_col_label = validation_opts$n_col_label %||% "Valid number of columns", 
                                            btn_label = validation_opts$btn_label, rules = validation_opts$rules)
    observeEvent(validation_results$status(), {
      status <- validation_results$status()
      req(status)
      if (status %in% c("Error", "Failed")) {
        datamods:::update_tab_label("tabs-mode", "validate", 
                         tagList(phosphoricons::ph("warning-circle", 
                                                   weight = "fill", fill = "firebrick"), 
                                 i18n("Validate")))
      }
      else {
        datamods:::update_tab_label("tabs-mode", "validate", 
                         i18n("Validate"))
      }
      if (status %in% allowed_status) {
        datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
      }
      else {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
      }
    })
    observeEvent(updated_data(), {
      data_rv$data <- updated_data()
    })
    observeEvent(input$confirm, {
      removeModal()
      imported_rv$data <- data_rv$data
      imported_rv$name <- data_rv$name %||% "imported_data"
    })
    return(list(data = reactive(datamods:::as_out(imported_rv$data, 
                                       return_class)), name = reactive(imported_rv$name)))
  })
}



import_file_ui2 <- function(id,
                           title = TRUE,
                           preview_data = TRUE,
                           file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav")) {
  
  ns <- NS(id)
  
  if (isTRUE(title)) {
    title <- tags$h4(
      i18n("Import a file"),
      class = "datamods-title"
    )
  }
  
  tags$div(
    class = "datamods-import",
    datamods:::html_dependency_datamods(),
    title,
    tags$div(
      class = "datamods-file-import",
      tags$div(
        fileInput(
          inputId = ns("file"),
          label = i18n("Upload a file:"),
          buttonLabel = i18n("Browse..."),
          placeholder = i18n("No file selected"),
          accept = file_extensions,
          width = "100%"
        )
      ),
      tags$div(
        tags$label(
          class = "control-label",
          style = htmltools::css(visibility = "hidden", width = "100%", marginBottom = "0.5rem"),
          "Parameters",
          `for` = ns("settings")
        ),
        dropMenu(
          placement = "bottom-end",
          actionButton(
            inputId = ns("settings"),
            label = phosphoricons::ph("gear", title = "parameters"),
            class = "btn-block"
          ),
          numericInputIcon(
            inputId = ns("skip_rows"),
            label = i18n("Number of rows to skip before reading data:"),
            value = 0,
            min = 0,
            icon = list("n =")
          ),
          textInputIcon(
            inputId = ns("na_label"),
            label = i18n("Na label:"),
            value = "",
            icon = list("NAs : ")
          ),
          textInputIcon(
            inputId = ns("dec"),
            label = i18n("Decimal separator:"),
            value = ".",
            icon = list("0.00")
          ),
          textInputIcon(
            inputId = ns("encoding"),
            label = i18n("Encoding:"),
            value = "UTF-8",
            icon = phosphoricons::ph("text-aa")
          )
        )
      )
    ),
    tags$div(
      class = "hidden",
      id = ns("sheet-container"),
      pickerInput(
        inputId = ns("sheet"),
        label = i18n("Select sheet to import:"),
        choices = NULL,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b(i18n("No file selected:")),
        sprintf(i18n("You can import %s files"), paste(file_extensions, collapse = ", ")),
        dismissible = TRUE
      )
    ),
    if (isTRUE(preview_data)) {
      tagAppendAttributes(
        tableOutput(outputId = ns("table")),
        class = "datamods-table-container"
      )
    },
    uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    )
  )
}


#' @inheritParams import_globalenv_server
#' @param read_fns Named list with custom function(s) to read data:
#'  * the name must be the extension of the files to which the function will be applied
#'  * the value must be a function that can have 5 arguments (you can ignore some of them, but you have to use the same names),
#'    passed by user through the interface:
#'    + `file`: path to the file
#'    + `sheet`: for Excel files, sheet to read
#'    + `skip`: number of row to skip
#'    + `dec`: decimal separator
#'    + `encoding`: file encoding
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom htmltools tags tagList
#' @importFrom shiny reactiveValues reactive observeEvent removeUI req renderTable
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom readxl excel_sheets
#' @importFrom rio import
#' @importFrom rlang exec fn_fmls_names is_named is_function
#' @importFrom tools file_ext
#' @importFrom utils head
#'
#' @rdname import-file
import_file_server2 <- function(id,
                               btn_show_data = TRUE,
                               show_data_in = c("popup", "modal"),
                               trigger_return = c("button", "change"),
                               return_class = c("data.frame", "data.table", "tbl_df", "raw"),
                               reset = reactive(NULL),
                               read_fns = list()) {
  
  if (length(read_fns) > 0) {
    if (!is_named(read_fns))
      stop("import_file_server: `read_fns` must be a named list.", call. = FALSE)
    if (!all(vapply(read_fns, is_function, logical(1))))
      stop("import_file_server: `read_fns` must be list of function(s).", call. = FALSE)
  }
  
  trigger_return <- match.arg(trigger_return)
  return_class <- match.arg(return_class)
  
  module <- function(input, output, session) {
    
    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, status = NULL)
    
    observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
    })
    
    output$container_confirm_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        datamods:::button_import()
      }
    })
    
    observeEvent(input$file, {
      if (isTRUE(is_excel(input$file$datapath))) {
        updatePickerInput(
          session = session,
          inputId = "sheet",
          choices = readxl::excel_sheets(input$file$datapath)
        )
        datamods:::showUI(paste0("#", ns("sheet-container")))
      } else {
        datamods:::hideUI(paste0("#", ns("sheet-container")))
      }
    })
    
    observeEvent(list(
      input$file,
      input$sheet,
      input$skip_rows,
      input$dec,
      input$encoding,
      input$na_label
    ), {
      req(input$file)
      req(input$skip_rows)
      extension <- tools::file_ext(input$file$datapath)
      if (isTRUE(extension %in% names(read_fns))) {
        parameters <- list(
          file = input$file$datapath,
          sheet = input$sheet,
          skip = input$skip_rows,
          dec = input$dec,
          encoding = input$encoding,
          na.strings = input$na_label
        )
        parameters <- parameters[which(names(parameters) %in% fn_fmls_names(read_fns[[extension]]))]
        imported <- try(rlang::exec(read_fns[[extension]], !!!parameters), silent = TRUE)
      } else {
        if (is_excel(input$file$datapath)) {
          req(input$sheet)
          parameters <- list(
            file = input$file$datapath,
            which = input$sheet,
            skip = input$skip_rows,
            na = input$na_label
          )
        } else if (is_sas(input$file$datapath)) {
          parameters <- list(
            file = input$file$datapath,
            skip = input$skip_rows,
            encoding = input$encoding,
            na.strings = input$na_label
          )
        } else {
          parameters <- list(
            file = input$file$datapath,
            skip = input$skip_rows,
            dec = input$dec,
            encoding = input$encoding,
            na.strings = c("NA", "")
          )
        }
        imported <- try(rlang::exec(rio::import, !!!parameters), silent = TRUE)
      }
      
      if (inherits(imported, "try-error"))
        imported <- try(rlang::exec(rio::import, !!!parameters[1]), silent = TRUE)
      
      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error(mssg = i18n(attr(imported, "condition")$message))
        temporary_rv$status <- "error"
        temporary_rv$data <- NULL
        temporary_rv$name <- NULL
        
      } else {
        
        datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
        
        datamods:::insert_alert(
          selector = ns("import"),
          status = "success",
          datamods:::make_success_alert(
            imported,
            trigger_return = trigger_return,
            btn_show_data = btn_show_data,
            extra = i18n("First five rows are shown below:")
          )
        )
        temporary_rv$status <- "success"
        temporary_rv$data <- imported
        temporary_rv$name <- input$file$name
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$see_data, {
      show_data(temporary_rv$data, title = i18n("Imported data"), type = show_data_in)
    })
    
    output$table <- renderTable({
      req(temporary_rv$data)
      data <- head(temporary_rv$data, 5)
      classes <- datamods:::get_classes(data)
      classes <- sprintf("<span style='font-style: italic; font-weight: normal; font-size: small;'>%s</span>", classes)
      names(data) <- paste(names(data), classes, sep = "<br>")
      data
    }, striped = TRUE, bordered = TRUE, sanitize.colnames.function = identity, spacing = "xs")
    
    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
      imported_rv$name <- temporary_rv$name
    })
    
    if (identical(trigger_return, "button")) {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(imported_rv$name),
        data = reactive(datamods:::as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(temporary_rv$name),
        data = reactive(datamods:::as_out(temporary_rv$data, return_class))
      ))
    }
  }
  
  moduleServer(
    id = id,
    module = module
  )
}

# utils -------------------------------------------------------------------

is_excel <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("xls", "xlsx"))
}

is_sas <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("sas7bdat"))
}


import_ui2<-function (id, from = c("env", "file", "copypaste", 
                       "googlesheets", "url"), file_extensions = c(".csv", 
                                                                   ".txt", ".xls", ".xlsx", ".rds", 
                                                                   ".fst", ".sas7bdat", ".sav")) 
{
  ns <- NS(id)
  from <- match.arg(from, several.ok = TRUE)
  env <- if ("env" %in% from) {
    tabPanelBody(value = "env", tags$br(), import_globalenv_ui(id = ns("env"), 
                                                               title = NULL))
  }
  file <- if ("file" %in% from) {
    tabPanelBody(value = "file", tags$br(), import_file_ui2(id = ns("file"), 
                                                           title = NULL, file_extensions = file_extensions))
  }
  copypaste <- if ("copypaste" %in% from) {
    tabPanelBody(value = "copypaste", tags$br(), import_copypaste_ui(id = ns("copypaste"), 
                                                                     title = NULL))
  }
  googlesheets <- if ("googlesheets" %in% from) {
    tabPanelBody(value = "googlesheets", tags$br(), 
                 import_googlesheets_ui(id = ns("googlesheets"), 
                                        title = NULL))
  }
  url <- if ("url" %in% from) {
    tabPanelBody(value = "url", tags$br(), import_url_ui(id = ns("url"), 
                                                         title = NULL))
  }
  labsImport <- list(env = i18n("Environment"), file = i18n("External file"), 
                     copypaste = i18n("Copy / Paste"), googlesheets = i18n("Googlesheets"), 
                     url = i18n("URL"))
  iconsImport <- list(env = phosphoricons::ph("code", 
                                              title = labsImport$env), file = phosphoricons::ph("file-arrow-down", 
                                                                                                title = labsImport$file), copypaste = phosphoricons::ph("clipboard-text", 
                                                                                                                                                        title = labsImport$copypaste), googlesheets = phosphoricons::ph("cloud-arrow-down", 
                                                                                                                                                                                                                        title = labsImport$googlesheets), url = phosphoricons::ph("link", 
                                                                                                                                                                                                                                                                                  title = labsImport$url))
  if (identical(length(from), 1L)) {
    importTab <- switch(from, env = import_globalenv_ui(id = ns("env")), 
                        file = import_file_ui2(id = ns("file"), file_extensions = file_extensions), 
                        copypaste = import_copypaste_ui(id = ns("copypaste")), 
                        googlesheets = import_googlesheets_ui(id = ns("googlesheets")), 
                        url = import_url_ui(id = ns("url")), )
  }
  else {
    tabsetPanelArgs <- datamods:::dropNulls(list(env, file, copypaste, 
                                      googlesheets, url, id = ns("tabs-import"), 
                                      type = "hidden"))
    importTab <- do.call(what = tabsetPanel, args = tabsetPanelArgs)
    importTab <- fluidRow(column(width = 3, tags$br(), tags$style(HTML(sprintf("#%s>.btn-group-vertical {width: 100%%;}", 
                                                                               ns("from"))), HTML(sprintf(".btn-group-vertical>.btn-group>.btn {text-align: left;}"))), 
                                 radioGroupButtons(inputId = ns("from"), label = i18n("How to import data?"), 
                                                   choiceValues = from, choiceNames = lapply(X = from, 
                                                                                             FUN = function(x) {
                                                                                               tagList(iconsImport[[x]], labsImport[[x]])
                                                                                             }), direction = "vertical", width = "100%")), 
                          column(width = 9, importTab))
  }
  tags$div(class = "datamods-imports", datamods:::html_dependency_datamods(), 
           tabsetPanel(type = "tabs", id = ns("tabs-mode"), 
                       tabPanel(title = tagList(phosphoricons::ph("download-simple", 
                                                                  title = i18n("Import")), i18n("Import")), 
                                value = "import", importTab), tabPanel(title = tagList(phosphoricons::ph("table", 
                                                                                                         title = i18n("View")), i18n("View")), 
                                                                       value = "view", tags$br(), reactable::reactableOutput(outputId = ns("view"))), 
                       tabPanel(title = tagList(phosphoricons::ph("gear-six", 
                                                                  title = i18n("Update")), i18n("Update")), 
                                value = "update", tags$br(), update_variables_ui(id = ns("update"), 
                                                                                 title = NULL)), tabPanel(title = tagList(phosphoricons::ph("shield-check", 
                                                                                                                                            title = i18n("Validate")), i18n("Validate")), 
                                                                                                          value = "validate", tags$br(), validation_ui(id = ns("validation"), 
                                                                                                                                                       display = "inline", max_height = "400px"))), 
           tags$div(id = ns("confirm-button"), style = "margin-top: 20px;", 
                    datamods:::button_import(list(ns = ns))), tags$div(style = "display: none;", 
                                                            textInput(inputId = ns("hidden"), label = NULL, 
                                                                      value = datamods:::genId())), tags$script(sprintf("$('#%s').addClass('nav-justified');", 
                                                                                                             ns("tabs-mode")), sprintf("fadeTab({id: '%s'});", 
                                                                                                                                       ns("tabs-mode")), sprintf("disableTab({id: '%s', value: '%s'});", 
                                                                                                                                                                 ns("tabs-mode"), "view"), sprintf("disableTab({id: '%s', value: '%s'});", 
                                                                                                                                                                                                   ns("tabs-mode"), "update"), sprintf("disableTab({id: '%s', value: '%s'});", 
                                                                                                                                                                                                                                       ns("tabs-mode"), "validate")))
}
