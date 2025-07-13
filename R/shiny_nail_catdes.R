library(shiny)
library(NaileR)
library(shinycssloaders)
library(stringr)

#' @importFrom NaileR nail_catdes
nail_catdes_polish <- function(data_modif, introduction, request, proba, generate,
                               model = "llama3", quali.sample = 1, quanti.sample = 1, isolate.groups = FALSE) {
  result <- nail_catdes(
    data_modif,
    num.var = 1,
    introduction = introduction,
    request = request,
    model = model,
    proba = proba,
    generate = generate,
    quali.sample = quali.sample,
    quanti.sample = quanti.sample,
    isolate.groups = isolate.groups
  )

  if (generate) {
    if (is.list(result) && all(sapply(result, function(x) "response" %in% names(x)))) {
      # Si plusieurs résultats (un par groupe), concatène les réponses
      return(paste(sapply(result, function(x) x$response), collapse = "\n\n---\n\n"))
    } else {
      return(result$response)
    }
  } else {
    if (is.list(result) && is.null(names(result))) {
      return(paste(result, collapse = "\n\n---\n\n"))
    } else {
      return(result)
    }
  }
}

#' Shiny app principale
#' @param dataset A data frame containing at least one categorical variable (factor).
#'
#' @return Launches a Shiny web application.
#' @export
shiny_nail_catdes <- function(dataset) {
  qual_vars <- names(dataset)[sapply(dataset, is.factor)]
  if (length(qual_vars) == 0) {
    stop("The dataset must contain at least one categorical (factor) variable.")
  }

  clean_text <- function(x) str_squish(gsub('\n', ' ', x))

  ui <- fluidPage(
    titlePanel("Interpret a Categorical (Latent) Variable"),
    sidebarLayout(
      sidebarPanel(
        helpText("Only variables of type 'factor' are shown below.",
                 "Please make sure your categorical variable is declared as a factor."),
        selectInput("selected_var", "Select a Categorical Variable:",
                    choices = qual_vars,
                    selected = qual_vars[1]),
        textAreaInput("introduction", "Prompt Introduction:",
                      value = "For this study, observations were grouped according to their similarities."),
        textAreaInput("request", "Prompt Task:",
                      value = "Based on the results, please describe what characterize the observations of each group and what set them apart from the other groups. Then, based on these characteristics, give each group a new name."),
        sliderInput("proba", "Significance Threshold:", min = 0, max = 1, value = 0.05, step = 0.05),
        sliderInput("quali_sample", "Qualitative Sampling (quali.sample):", min = 0.0, max = 1, value = 1, step = 0.05),
        sliderInput("quanti_sample", "Quantitative Sampling (quanti.sample):", min = 0.0, max = 1, value = 1, step = 0.05),
        checkboxInput("isolate_groups", "Describe Each Group Separately (isolate.groups)", value = FALSE),
        textInput("model", "Model (llama3 by Default):", value = "llama3"),
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
            height: 400px;
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
        model = input$model,
        quali.sample = input$quali_sample,
        quanti.sample = input$quanti_sample,
        isolate.groups = input$isolate_groups
      )
    }

    output$factor_levels <- renderPrint({
      req(input$selected_var)
      levels(dataset[[input$selected_var]])
    })

    output$factor_counts <- renderPrint({
      req(input$selected_var)
      table(dataset[[input$selected_var]])
    })

    modified_data <- eventReactive(input$run, {
      req(input$selected_var)
      selected_var <- input$selected_var
      dataset[, c(selected_var, setdiff(names(dataset), selected_var))]
    })

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
          model = params$model,
          quali.sample = params$quali.sample,
          quanti.sample = params$quanti.sample,
          isolate.groups = params$isolate.groups
        )
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })

  #   output$function_output <- renderPrint({
  #     req(analysis_results())
  #     cat(analysis_results())
  #   })
  # }

    output$function_output <- renderPrint({
      req(analysis_results())
      result <- analysis_results()

      if (is.list(result)) {
        cat(paste(unlist(result), collapse = "\n\n---\n\n"))
      } else {
        cat(result)
      }
    })
}
  shinyApp(ui, server)
}
