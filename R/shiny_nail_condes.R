library(shiny)
library(dplyr)
library(shinycssloaders)

#' @importFrom NaileR nail_condes
nail_condes_polish <- function(data_modif, introduction, request, proba, generate,
                               quanti_threshold, quanti_cat, sample_pct, model = "llama3") {
  result <- NaileR::nail_condes(
    data_modif,
    num.var = 1,
    introduction = introduction,
    request = request,
    model = model,
    quanti.threshold = quanti_threshold,
    quanti.cat = quanti_cat,
    weights = NULL,
    proba = proba,
    sample.pct = sample_pct,
    generate = generate
  )
  if (generate) result$response else result
}

#' Launch a Shiny app for interpreting a (latent) continuous variable
#'
#' This function launches a Shiny app for interpreting a (latent)
#' continuous variable with the 'Nailer' package.
#'
#' @param dataset A data frame containing the continuous variable to be analyzed.
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
#'library(FactoMineR)
#'library(stringr)
#'library(NaileR)
#'data(beard_cont)
#'
#'res_ca_beard <- FactoMineR::CA(beard_cont, graph = FALSE)
#'
#'beard_work <- res_ca_beard$row$coord |> as.data.frame()
#'beard_work <- beard_work[,1] |> cbind(beard_cont)
#'
#'intro_beard <- "These data refer to 8 types of beards.
#'Each beard was evaluated by 62 assessors."
#'intro_beard <- gsub('\n', ' ', intro_beard) |>
#'stringr::str_squish()
#'
#'req_beard <- "Please explain what differentiates beards
#'on both sides of the scale.
#'Then, give the scale a name."
#'req_beard <- gsub('\n', ' ', req_beard) |>
#'stringr::str_squish()
#'
#'shiny_nail_condes(beard_work)
#'
#' }

shiny_nail_condes <- function(dataset) {

  clean_text <- function(x) stringr::str_squish(gsub('\n', ' ', x))

  ui <- fluidPage(
    titlePanel("Interpret a Continuous (Latent) Variable"),
    sidebarLayout(
      sidebarPanel(
        selectInput("selected_var", "Select a Continuous Variable:",
                    choices = names(dataset)[sapply(dataset, is.numeric)],
                    selected = names(dataset)[1]),
        textAreaInput("introduction", "Prompt Introduction:", placeholder = "Enter introduction here..."),
        textAreaInput("request", "Prompt Task:", placeholder = "Enter request here..."),
        numericInput("quanti_threshold", "Quantitative Threshold:", value = 0, step = 0.5),
        textInput("quanti_cat_1", "Category for Above Average:", value = "Significantly above average"),
        textInput("quanti_cat_2", "Category for Below Average:", value = "Significantly below average"),
        textInput("quanti_cat_3", "Category for Average:", value = "Average"),
        sliderInput("sample_pct", "Proportion of Significant Variables (sample.pct):", min = 0, max = 1, value = 1, step = 0.05),
        textInput("model", "Model (llama3 by Default):", value = "llama3"),
        sliderInput("proba", "Significance Threshold:", min = 0, max = 1, value = 0.05, step = 0.05),
        checkboxInput("generate", "Run the LLM (return the prompt if FALSE)", value = FALSE),
        actionButton("run", "Run Analysis")
      ),
      mainPanel(
        h4("nail_condes results: a prompt or the result of the request"),
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
    debug_input <- function() {
      list(
        selected_var = input$selected_var,
        introduction = clean_text(input$introduction),
        request = clean_text(input$request),
        proba = input$proba,
        generate = input$generate,
        quanti_threshold = input$quanti_threshold,
        quanti_cat = c(input$quanti_cat_1, input$quanti_cat_2, input$quanti_cat_3),
        sample_pct = input$sample_pct,
        model = input$model
      )
    }

    modified_data <- eventReactive(input$run, {
      req(input$selected_var)
      selected_var <- input$selected_var
      dataset[, c(selected_var, setdiff(names(dataset), selected_var))]
    })

    analysis_results <- eventReactive(input$run, {
      req(modified_data())
      params <- debug_input()
      tryCatch({
        nail_condes_polish(
          data_modif = modified_data(),
          introduction = params$introduction,
          request = params$request,
          proba = params$proba,
          generate = params$generate,
          quanti_threshold = params$quanti_threshold,
          quanti_cat = params$quanti_cat,
          sample_pct = params$sample_pct,
          model = params$model
        )
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })

    output$function_output <- renderPrint({
      req(analysis_results())
      cat(analysis_results())
    })
  }

  shinyApp(ui, server)
}
