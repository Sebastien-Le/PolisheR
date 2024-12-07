library(shiny)
library(NaileR)
library(shinycssloaders)

#' @importFrom NaileR nail_catdes
nail_catdes_polish <- function(data_modif, introduction, request, proba, generate, model = "llama3") {
result <- nail_catdes(
  data_modif,
  num.var = 1,
  introduction = introduction,
  request = request,
  model = model,
  proba = proba,
  generate = generate
)
if (generate) result$response else result
}

#' Launch a Shiny app for interpreting a (latent) categorical variable
#'
#' This function launches a Shiny app for interpreting a (latent)
#' categorical variable with the 'Nailer' package.
#'
#' @param dataset A data frame containing the categorical variable to be analyzed.
#' @return This function does not return a value; it launches a Shiny app.
#' @export
#' @import shiny
#' @importFrom dplyr %>%
#' @importFrom dplyr all_of
#' @import FactoMineR
#' @import stringr
#'
#' @examples
#' if(interactive()){
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' data(iris)
#' intro_iris <- "A study measured various parts of iris flowers
#' from 3 different species: setosa, versicolor and virginica.
#' I will give you the results from this study.
#' You will have to identify what sets these flowers apart."
#' intro_iris <- gsub('\n', ' ', intro_iris) |>
#' stringr::str_squish()
#' intro_iris
#'
#' req_iris <- "Please explain what makes each species distinct.
#' Also, tell me which species has the biggest flowers,
#' and which species has the smallest."
#' req_iris <- gsub('\n', ' ', req_iris) |>
#' stringr::str_squish()
#' req_iris
#'
#' shiny_nail_catdes(iris)
#' }

shiny_nail_catdes <- function(dataset) {
  ui <- fluidPage(
    titlePanel("Interpret a Categorical (Latent) Variable"),
    sidebarLayout(
      sidebarPanel(
        selectInput("selected_var", "Select a Categorical Variable:",
                    choices = names(dataset)[sapply(dataset, is.factor)],
                    selected = names(dataset)[1]),
        textAreaInput("introduction", "Prompt Introduction:", placeholder = "Enter introduction here..."),
        textAreaInput("request", "Prompt Task:", placeholder = "Enter request here..."),
        textInput("model", "Model (llama3 by Default):", value = "llama3"),  # Changed to textInput
        sliderInput("proba", "Significance Threshold:", min = 0, max = 1, value = 0.05, step = 0.05),
        checkboxInput("generate", "Run the LLM (return the prompt if FALSE)", value = FALSE),
        actionButton("run", "Run Analysis")
      ),
      mainPanel(
        h4("nail_catdes results: a prompt or the result of the request"),
        verbatimTextOutput("function_output") %>% shinycssloaders::withSpinner(),
        tags$style(
          "#function_output {
            height: 300px;
            width: 100%;
            overflow-y: scroll;
            overflow-x: auto;
            white-space: pre-wrap;
            font-family: 'Courier New', Courier, monospace;
            padding: 10px;
            border: 1px solid #ccc;
            background-color: #f9f9f9;
          }"
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Helper function for debugging
    debug_input <- function() {
      list(
        selected_var = input$selected_var,
        introduction = input$introduction,
        request = input$request,
        proba = input$proba,
        generate = input$generate,
        model = input$model
      )
    }

    # Reactive to rearrange dataset
    modified_data <- eventReactive(input$run, {
      req(input$selected_var)
      selected_var <- input$selected_var
      dataset[, c(selected_var, setdiff(names(dataset), selected_var))]
    })

    # Reactive analysis results
    analysis_results <- eventReactive(input$run, {
      req(modified_data())
      params <- debug_input()  # Consolidated debug data
      tryCatch({
        nail_catdes_polish(
          data_modif = modified_data(),
          introduction = params$introduction,
          request = params$request,
          proba = params$proba,
          generate = params$generate,
          model = params$model  # Pass the model input
        )
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })

    # Render the analysis results
    output$function_output <- renderPrint({
      req(analysis_results())
      cat(analysis_results())
    })
  }

  shinyApp(ui, server)
}
