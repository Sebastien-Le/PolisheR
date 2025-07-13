library(shiny)
library(NaileR)
library(shinycssloaders)
library(stringr)

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
#' @param dataset A data frame containing the categorical variable to be analyzed.
#' @export
shiny_nail_catdes <- function(dataset) {
  # Vérification préalable
  qual_vars <- names(dataset)[sapply(dataset, is.factor)]
  if (length(qual_vars) == 0) {
    stop("The dataset must contain at least one categorical (factor) variable.")
  }

  # Fonction utilitaire de nettoyage
  clean_text <- function(x) {
    str_squish(gsub('\n', ' ', x))
  }

  ui <- fluidPage(
    titlePanel("Interpret a Categorical (Latent) Variable"),
    sidebarLayout(
      sidebarPanel(
        helpText("Only variables of type 'factor' are shown below.",
                 "Please make sure your categorical variable is declared as a factor."),
        selectInput("selected_var", "Select a Categorical Variable:",
                    choices = qual_vars,
                    selected = qual_vars[1]),
        textAreaInput("introduction", "Prompt Introduction:", placeholder = "Enter introduction here..."),
        textAreaInput("request", "Prompt Task:", placeholder = "Enter request here..."),
        textInput("model", "Model (llama3 by Default):", value = "llama3"),
        sliderInput("proba", "Significance Threshold:", min = 0, max = 1, value = 0.05, step = 0.05),
        checkboxInput("generate", "Run the LLM (return the prompt if FALSE)", value = FALSE),
        actionButton("run", "Run Analysis")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Prompt or Result",
                   h4("nail_catdes result"),
                   verbatimTextOutput("function_output") %>% shinycssloaders::withSpinner()
          ),
          tabPanel("Levels",
                   h4("Levels of selected variable"),
                   verbatimTextOutput("factor_levels") %>% shinycssloaders::withSpinner(),
                   h4("Counts per level"),
                   verbatimTextOutput("factor_counts") %>% shinycssloaders::withSpinner()
          )
        ),
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
    # Nettoyage et collecte des entrées
    debug_input <- function() {
      list(
        selected_var = input$selected_var,
        introduction = clean_text(input$introduction),
        request = clean_text(input$request),
        proba = input$proba,
        generate = input$generate,
        model = input$model
      )
    }

    # Affichage des levels
    output$factor_levels <- renderPrint({
      req(input$selected_var)
      levels(dataset[[input$selected_var]])
    })

    # Affichage des effectifs
    output$factor_counts <- renderPrint({
      req(input$selected_var)
      table(dataset[[input$selected_var]])
    })

    # Données modifiées : la variable choisie est placée en première colonne
    modified_data <- eventReactive(input$run, {
      req(input$selected_var)
      selected_var <- input$selected_var
      dataset[, c(selected_var, setdiff(names(dataset), selected_var))]
    })

    # Appel à nail_catdes
    analysis_results <- eventReactive(input$run, {
      req(modified_data())
      params <- debug_input()
      tryCatch({
        nail_catdes_polish(
          data_modif = modified_data(),
          introduction = params$introduction,
          request = params$request,
          proba = params$proba,
          generate = params$generate,
          model = params$model
        )
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })

    # Affichage du résultat
    output$function_output <- renderPrint({
      req(analysis_results())
      cat(analysis_results())
    })
  }

  shinyApp(ui, server)
}
