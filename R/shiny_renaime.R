#' Launch a Shiny App to Rename Data Frame Columns via LLM
#'
#' This function launches a Shiny app to generate and validate short variable names using a local LLM.
#'
#' @param data Optional data frame or tibble. If NULL, a CSV upload is prompted.
#' @param model Character string specifying the LLM model name. Default is "llama3".
#' @param prompt_prefix String used before each input to instruct the LLM. Default is a general renaming instruction.
#' @param prompt_suffix String used after each input to enforce format. Default includes a clear example.
#' @param max_length Maximum number of characters allowed for the new label. Default is 20.
#'
#' @return A list with two data frames returned after clicking "Apply and return results" in the Shiny app:
#' \describe{
#'   \item{data_recoded}{The data frame with renamed columns. Each original column name is replaced with its corresponding short label.}
#'   \item{labels}{A data frame with three columns:}
#'     \describe{
#'       \item{varname}{The original column names.}
#'       \item{label_short}{The generated or manually edited short labels.}
#'       \item{prompt}{The full prompt used to generate each label (NA if not generated).}
#'     }
#' }
#' @export
#'
#' @import shiny
#' @importFrom utils read.csv write.csv
shiny_renaime <- function(data = NULL,
                          model = "llama3",
                          prompt_prefix = NULL,
                          prompt_suffix = NULL,
                          max_length = 20) {

  if (is.null(prompt_prefix)) {
    prompt_prefix <- "Rename each variable with a short, clear name (less than 20 characters) that preserves its meaning. Keep verbs when present - they carry essential meaning."
  }

  if (is.null(prompt_suffix)) {
    prompt_suffix <- paste(
      "Respond ONLY with the new name - no explanation, no punctuation, no formatting.",
      "Do NOT include quotes.",
      "Example:",
      'Input: "How often do you buy local food?"',
      "Output: BuyLocalFoodFreq",
      sep = "\n"
    )
  }

  generate_labels_short <- function(varnames,
                                    model = "llama3",
                                    prompt_prefix,
                                    prompt_suffix,
                                    max_length) {
    if (!requireNamespace("ollamar", quietly = TRUE)) {
      stop("The 'ollamar' package must be installed.")
    }

    labels <- character(length(varnames))
    prompts <- character(length(varnames))
    failed <- character(0)

    for (i in seq_along(varnames)) {
      v <- varnames[i]

      if (nchar(v) <= max_length) {
        labels[i] <- v
        prompts[i] <- NA
        next
      }

      full_prompt <- paste0(
        prompt_prefix, "\n",
        prompt_suffix, "\n\n",
        "Input: \"", v, "\"\n",
        "Output:"
      )

      prompts[i] <- full_prompt
      success <- FALSE

      try({
        response <- ollamar::generate(model = model, prompt = full_prompt, output = "text")
        response_text <- trimws(strsplit(response, "\n")[[1]][1])
        response_text <- gsub("^Output:\\s*", "", response_text)
        response_text <- gsub('[\"\'""]', "", response_text)  # clean quotes
        labels[i] <- response_text
        success <- TRUE
      }, silent = TRUE)

      if (!success || labels[i] == "" || is.na(labels[i])) {
        labels[i] <- NA
        failed <- c(failed, v)
      }
    }

    duplicated_labels <- labels[duplicated(labels) & !is.na(labels)]
    if (length(duplicated_labels) > 0) {
      warning("Some labels are not unique: ", paste(unique(duplicated_labels), collapse = ", "))
    }

    if (length(failed) > 0) {
      warning("Label generation failed for: ", paste(failed, collapse = "\n"))
    }

    data.frame(
      varname = varnames,
      label_short = labels,
      prompt = prompts,
      stringsAsFactors = FALSE
    )
  }

  app_values <- reactiveValues(data_recoded = NULL, labels = NULL)

  ui <- fluidPage(
    titlePanel("Rename Variable Names via LLM"),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "output.allow_upload",
          fileInput("file", "Upload a .csv file", accept = ".csv")
        ),
        actionButton("generate", "Generate short names"),
        actionButton("apply_labels", "Apply and return results"),
        downloadButton("download", "Download label mapping"),
        br(), verbatimTextOutput("action_status"),
        tags$p(
          style = "color: #888; font-size: 90%; margin-top: 10px;",
          "Note: The 'Generate' button uses the original column names only. ",
          "It will not reapply to already edited names. To make changes, edit the fields manually."
        )
      )
      ,
      mainPanel(
        uiOutput("labels_ui")
      )
    )
  )

  server <- function(input, output, session) {
    data_input <- reactiveVal(NULL)
    labels_df <- reactiveVal(NULL)

    if (!is.null(data)) {
      data_input(data)
      output$allow_upload <- reactive(FALSE)
    } else {
      output$allow_upload <- reactive(TRUE)
    }
    outputOptions(output, "allow_upload", suspendWhenHidden = FALSE)

    observeEvent(input$file, {
      req(input$file)
      df <- read.csv(input$file$datapath, stringsAsFactors = TRUE, sep = ";")
      data_input(df)
      labels_df(NULL)
    })

    observeEvent(input$generate, {
      req(data_input())
      vars <- names(data_input())
      labels_df(NULL)

      withProgress(message = "Generating labels...", value = 0, {
        result <- generate_labels_short(
          varnames = vars,
          model = model,
          prompt_prefix = prompt_prefix,
          prompt_suffix = prompt_suffix,
          max_length = max_length
        )
        labels_df(result)
        if (any(is.na(result$label_short))) {
          showNotification(
            paste("Label generation failed for some variables."),
            type = "warning",
            duration = 5
          )
        }
        if (any(duplicated(result$label_short[!is.na(result$label_short)]))) {
          showNotification(
            paste("Some generated labels are not unique."),
            type = "warning",
            duration = 5
          )
        }
      })
    })

    output$labels_ui <- renderUI({
      req(labels_df())
      df <- labels_df()
      lapply(1:nrow(df), function(i) {
        tagList(
          strong(df$varname[i]),
          textInput(
            inputId = paste0("label_", i),
            label = NULL,
            value = df$label_short[i],
            width = "100%"
          ),
          tags$hr()
        )
      })
    })

    observe({
      req(labels_df())
      df <- labels_df()
      for (i in 1:nrow(df)) {
        val <- input[[paste0("label_", i)]]
        if (!is.null(val)) {
          df$label_short[i] <- val
        }
      }
      labels_df(df)
    })

    output$download <- downloadHandler(
      filename = function() paste0("labels_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- labels_df()
        df$prompt <- gsub("\n", " ", df$prompt)  #  Remove newlines for clean CSV
        write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    observeEvent(input$apply_labels, {
      req(data_input(), labels_df())
      df <- data_input()
      label_map <- labels_df()

      if (nrow(label_map) != ncol(df)) {
        output$action_status <- renderText("Error: Number of labels doesn't match number of columns.")
        return()
      }
      if (any(duplicated(label_map$label_short))) {
        output$action_status <- renderText("Error: Some new names are duplicated.")
        return()
      }

      names(df) <- label_map$label_short
      app_values$data_recoded <- df
      app_values$labels <- label_map

      stopApp(invisible(list(
        data_recoded = df,
        labels = label_map
      )))
    })
  }

  runApp(shinyApp(ui, server))
}
