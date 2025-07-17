library(shiny)
library(NaileR)
library(shinycssloaders)
library(stringr)
library(rmarkdown)

#' @importFrom utils capture.output
#' @importFrom NaileR nail_catdes
nail_catdes_polish <- function(data_modif, introduction, request, proba, generate,
                               model = "llama3", quali.sample = 1, quanti.sample = 1, isolate.groups = FALSE) {
  result <- NaileR::nail_catdes(
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
      raw_merged <- paste(sapply(result, function(x) x$response), collapse = "\n***************\n")

      meta_prompt <- paste0(
        "You are now tasked with editing the following collection of descriptions.\n\n",
        "Each section is separated by a line with ***************.\n",
        "Your goal is to produce a clear and well-structured report in **Quarto Markdown** format.\n\n",
        "Use headings, bullets and avoid repeating the separator.\n\n",
        "Here is the raw content:\n\n",
        raw_merged
      )

      final_response <- ollamar::generate(
        prompt = meta_prompt,
        model = model,
        output = "text"
      )

      return(final_response)
    } else {
      return(result$response)
    }
  }
  else {
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
                   verbatimTextOutput("function_output") %>% shinycssloaders::withSpinner(),
                   br(),
                   conditionalPanel(
                     condition = "input.generate == true",
                     actionButton("generate_docx", "Generate Word Document (.docx)")
                   )
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
        if (all(sapply(result, is.character))) {
          cat(paste(unlist(result), collapse = "\n\n---\n\n"))
        } else if (all(sapply(result, function(x) "response" %in% names(x)))) {
          cat(paste(sapply(result, function(x) x$response), collapse = "\n\n---\n\n"))
        } else {
          print(result)
        }
      } else {
        cat(as.character(result))
      }
    })

    observeEvent(input$generate_docx, {
      req(analysis_results(), input$generate)

      # 0. Récupérer le résultat depuis la reactive
      result <- analysis_results()

      # 1. S'assurer que c'est bien un texte (capture si nécessaire)
      if (!is.character(result)) {
        result <- capture.output(print(result))
      }

      # 2. Créer un fichier temporaire .Rmd
      rmd_file <- tempfile(fileext = ".Rmd")
      writeLines(result, con = rmd_file)

      # 3. Définir le nom de sortie sur le bureau
      desktop_dir <- file.path(path.expand("~"), "Desktop")
      file_name <- paste0("nail_catdes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      output_file <- file.path(desktop_dir, file_name)

      # 4. Générer le .docx avec rmarkdown
      rmarkdown::render(
        input = rmd_file,
        output_format = "word_document",
        output_file = output_file,
        quiet = TRUE
      )

      # 5. Ouvrir le document (sur macOS)
      system2("open", shQuote(output_file))

      # 6. Notification utilisateur
      showNotification("Word document generated and opened.", type = "message")
    })

  }
  shinyApp(ui, server)
}
