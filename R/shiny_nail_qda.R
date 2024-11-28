library(shiny)
library(dplyr)


#' @importFrom NaileR nail_qda
nail_qda_polish <- function(data, formula, custom_text_1, custom_text_2, param1, param2, param3, comprises) {

  res_nail_qda <- NaileR::nail_qda(data, formul = formula, firstvar = 3,
                                   proba = comprises,
                                   introduction = custom_text_1,
                                   request = custom_text_2,
                                   isolate.groups = param1,
                                   drop.negative = param2,
                                   generate = param3)

  if (param1 == FALSE){
    if (param3) {
      results <- paste(res_nail_qda$response)
    } else {
      results <- paste(res_nail_qda)
    }
  } else {
    if (param3) {
      results <- paste(sapply(res_nail_qda, function(x) x$response), collapse = "\n***************\n")
    } else {
      results <- paste(res_nail_qda)
    }
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
  # Ensure the dataset is valid
  if (!is.data.frame(dataset)) {
    stop("The argument must be a data frame.")
  }

  # Identify factor and continuous columns
  factor_cols <- names(dataset)[sapply(dataset, is.factor)]
  continuous_cols <- names(dataset)[sapply(dataset, is.numeric)]

  if (length(factor_cols) < 2 || length(continuous_cols) < 1) {
    stop("The dataset must contain at least two factors and one continuous variable.")
  }

  # Define UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("An automatic interpretation of QDA data with NaileR"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("nail_qda Input Parameters"),

        # Select two factors
        shiny::selectInput("factor_1", "Select the stimulus effect:",
                    choices = factor_cols,
                    selected = factor_cols[1]),
        shiny::selectInput("factor_2", "Select the panelist effect:",
                    choices = factor_cols,
                    selected = factor_cols[2]),

        # Select dependent variables
        shiny::selectInput("dependent_vars", "Select the perceptual attributes:",
                    choices = continuous_cols,
                    #selected = continuous_cols[1],
                    selected = continuous_cols,
                    multiple = TRUE),

        # Boolean parameters as checkboxes
        shiny::checkboxInput("param1", "Stimuli are interpreted individually", value = FALSE),
        shiny::checkboxInput("param2", "Attributes are considered when above average (only)", value = FALSE),
        shiny::checkboxInput("param3", "Run the LLM (otherwise check the prompt first)", value = FALSE),

        # Text input for additional information
        shiny::textAreaInput("custom_text_1", "The introduction of the prompt:",
                      value = "For this study, a set of stimuli have been evaluated by panelists that used a common list of perceptual or sensory attributes.",
                      rows = 3),

        shiny::textAreaInput("custom_text_2", "The task of the prompt:",
                      value = "Based on the results, please describe what characterize the stimuli and what set them apart. Then, based on these characteristics, give each stimulus a new name.",
                      rows = 3),

        # Numeric parameter for comprises (threshold between 0 and 1)
        shiny::sliderInput("comprises", "Set significance threshold:",
                    min = 0, max = 1, value = 0.05),

        shiny::actionButton("run_anova", "Run nail_qda")
      ),

      shiny::mainPanel(
        shiny::h4("nail_qda results: a prompt or the result of the request"),

        # Scrollable text output area
        shiny::tags$style(
          "#anova_results { height: 400px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; }"
        ),
        shiny::verbatimTextOutput("anova_results")  # Display the combined result as a large text block
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {

    # Reactive data subset based on user inputs
    selected_data <- shiny::eventReactive(input$run_anova, {
      req(input$factor_1, input$factor_2, input$dependent_vars)

      # Ensure factors are distinct
      if (input$factor_1 == input$factor_2) {
        stop("The two factors must be different.")
      }

      # Subset the data
      dataset %>%
        dplyr::select(all_of(c(input$factor_1, input$factor_2, input$dependent_vars))) %>%
        as.data.frame()
    })

    observeEvent(input$param1, {
      if (input$param1) {
        shiny::updateTextAreaInput(session, "custom_text_1",
                            value = "For this study, a stimulus has been evaluated by panelists that used a common list of perceptual or sensory attributes.")
      } else {
        shiny::updateTextAreaInput(session, "custom_text_1",
                            value = "For this study, a set of stimuli have been evaluated by panelists that used a common list of perceptual or sensory attributes.")
      }
    })


    # Run ANOVA with the selected data
    anova_results <- shiny::eventReactive(input$run_anova, {
      req(selected_data())

      # Dynamically construct the formula
      formula <- paste("~", input$factor_1, "*", input$factor_2)

      # Get both custom text inputs and other parameters
      custom_text_1 <- input$custom_text_1
      custom_text_2 <- input$custom_text_2
      param1 <- input$param1
      param2 <- input$param2
      param3 <- input$param3
      comprises <- input$comprises

      # Run your function with the updated parameters
      nail_qda_polish(selected_data(), formula, custom_text_1, custom_text_2, param1, param2, param3, comprises)
    })

    # Display the combined text result
    output$anova_results <- renderText({
      req(anova_results())

      # Display the full text result from ANOVA
      return(anova_results())
    })
  }

  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}
