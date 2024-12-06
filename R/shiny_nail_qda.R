library(shiny)
library(dplyr)
library(shinycssloaders)

#' @importFrom NaileR nail_qda
nail_qda_polish <- function(data, formula, custom_text_1, custom_text_2, param1, param2, param3, comprises, model = "llama3") {
  res_nail_qda <- NaileR::nail_qda(
    data,
    formul = formula,
    firstvar = 3,
    proba = comprises,
    model = model,
    introduction = custom_text_1,
    request = custom_text_2,
    isolate.groups = param1,
    drop.negative = param2,
    generate = param3
  )

  if (param1 && param3) {
    results <- paste(sapply(res_nail_qda, function(x) x$response), collapse = "\n***************\n")
  } else if (param3) {
    results <- res_nail_qda$response
  } else {
    results <- res_nail_qda
  }

  return(results)
}

#' Launch a Shiny app for analyzing Quantitative Descriptive Analysis data (QDA)
#'
#' This function launches a Shiny app for analyzing QDA data with the 'Nailer' package.
#' The app allows users to explore and analyze their QDA dataset.
#'
#' @param dataset A data frame containing the data to be analyzed.
#' @return This function does not return a value; it launches a Shiny app.
#' @export
#' @import shiny
#' @importFrom dplyr %>%
#' @importFrom dplyr all_of
#'
#' @examples
#' if(interactive()){
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#'library(SensoMineR)
#'data(chocolates)
#'shiny_nail_qda(sensochoc)
#'
#' }

shiny_nail_qda <- function(dataset) {
  # Validate dataset
  if (!is.data.frame(dataset)) {
    stop("The argument must be a data frame.")
  }

  # Identify factor and numeric columns
  factor_cols <- names(dataset)[sapply(dataset, is.factor)]
  continuous_cols <- names(dataset)[sapply(dataset, is.numeric)]

  if (length(factor_cols) < 2 || length(continuous_cols) < 1) {
    stop("The dataset must contain at least two factors and one continuous variable.")
  }

  # UI
  ui <- fluidPage(
    titlePanel("Automatic Interpretation of QDA Data with NaileR"),
    sidebarLayout(
      sidebarPanel(
        h4("Input Parameters"),
        selectInput("factor_1", "Stimulus Effect:", choices = factor_cols, selected = factor_cols[1]),
        selectInput("factor_2", "Panelist Effect:", choices = factor_cols[-1], selected = factor_cols[2]),
        selectInput("dependent_vars", "Perceptual Attributes:",
                    choices = continuous_cols, selected = continuous_cols, multiple = TRUE),
        textAreaInput("custom_text_1", "Prompt Introduction:",
                      value = "For this study, a set of stimuli has been evaluated by panelists that used a common list of perceptual or sensory attributes.", rows = 3),
        textAreaInput("custom_text_2", "Prompt Task:",
                      value = "Based on the results, please describe what characterize the stimuli and what set them apart. Then, based on these characteristics, give each stimulus a new name.", rows = 3),
        checkboxInput("param1", "Stimuli interpreted individually", value = FALSE),
        checkboxInput("param2", "Attributes above average only", value = FALSE),
        textInput("model", "Model (llama3 by Default):", value = "llama3"),
        sliderInput("comprises", "Significance Threshold:", min = 0, max = 1, value = 0.05, step = 0.05),
        checkboxInput("param3", "Run the LLM (return the prompt if FALSE)", value = FALSE),
        actionButton("run_qda", "Run Analysis")
      ),
      mainPanel(
        h4("nail_qda results: a prompt or the result of the task"),
        verbatimTextOutput("qda_results") %>% shinycssloaders::withSpinner(),
        tags$style(
          "#qda_results {
            height: 400px;
            overflow-y: scroll;
            overflow-x: auto;
            white-space: pre-wrap;  /* Ensures long text wraps */
            border: 1px solid #ccc;
            padding: 10px;
            font-family: 'Courier New', Courier, monospace;
          }"
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    observeEvent(input$factor_1, {
      remaining_choices <- setdiff(factor_cols, input$factor_1)
      updateSelectInput(session, "factor_2", choices = remaining_choices, selected = remaining_choices[1])
    })

    # Update text area based on `param1`
    observeEvent(input$param1, {
      text <- if (input$param1) {
        "For this study, a stimulus has been evaluated by panelists that used a common list of perceptual or sensory attributes."
      } else {
        "For this study, a set of stimuli has been evaluated by panelists that used a common list of perceptual or sensory attributes."
      }
      updateTextAreaInput(session, "custom_text_1", value = text)
    })

    observeEvent(input$param1, {
      text <- if (input$param1) {
        "Based on the results, please describe what characterize the stimulus. Then, based on these characteristics, give the stimulus a new name."
      } else {
        "Based on the results, please describe what characterize the stimuli and what set them apart. Then, based on these characteristics, give each stimulus a new name."
      }
      updateTextAreaInput(session, "custom_text_2", value = text)
    })

    # Reactive dataset selection
    selected_data <- eventReactive(input$run_qda, {
      req(input$factor_1, input$factor_2, input$dependent_vars)

      validate(
        need(input$factor_1 != input$factor_2, "The two factors must be different."),
        need(length(input$dependent_vars) > 0, "Select at least one dependent variable.")
      )

      dataset %>% dplyr::select(all_of(c(input$factor_1, input$factor_2, input$dependent_vars)))
    })

    # Run QDA Analysis
    qda_results <- eventReactive(input$run_qda, {
      req(selected_data())

      formula <- paste("~", input$factor_1, "*", input$factor_2)

      tryCatch(
        nail_qda_polish(
          data = selected_data(),
          formula = formula,
          custom_text_1 = input$custom_text_1,
          custom_text_2 = input$custom_text_2,
          param1 = input$param1,
          param2 = input$param2,
          param3 = input$param3,
          comprises = input$comprises,
          model = input$model
        ),
        error = function(e) paste("Error:", e$message)
      )
    })

    # Display Results
    output$qda_results <- renderText({
      req(qda_results())
      qda_results()
    })
  }

  # Launch the app
  shinyApp(ui = ui, server = server)
}
