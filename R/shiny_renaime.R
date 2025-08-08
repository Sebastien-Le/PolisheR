library(shinycssloaders)
#' Launch a Shiny App to Rename Data Frame Columns via LLM
#'
#' This function launches a Shiny app to generate and validate short variable names using a local LLM.
#'
#' @param data A data frame or tibble.
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
shiny_renaime <- function(data,
                          model = "llama3",
                          prompt_prefix = NULL,
                          prompt_suffix = NULL,
                          max_length = 20) {

  # --- Guards ---------------------------------------------------------------
  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    stop("`data` must be a non-NULL data frame.", call. = FALSE)
  }

  # --- Defaults -------------------------------------------------------------
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

  # --- Helper: call LLM per variable ---------------------------------------
  generate_labels_short <- function(varnames,
                                    model = "llama3",
                                    prompt_prefix,
                                    prompt_suffix,
                                    max_length) {
    if (!requireNamespace("ollamar", quietly = TRUE)) {
      stop("The 'ollamar' package must be installed.")
    }

    labels  <- character(length(varnames))
    prompts <- character(length(varnames))
    failed  <- character(0)

    for (i in seq_along(varnames)) {
      v <- varnames[i]

      # Keep original if already short enough
      if (nchar(v) <= max_length) {
        labels[i]  <- v
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
        resp <- ollamar::generate(model = model, prompt = full_prompt, output = "text")
        out  <- trimws(strsplit(resp, "\n")[[1]][1])
        out  <- gsub("^Output:\\s*", "", out)
        out  <- gsub('[\"\'"]', "", out)
        labels[i] <- out
        success <- TRUE
      }, silent = TRUE)

      if (!success || labels[i] == "" || is.na(labels[i])) {
        labels[i] <- NA
        failed <- c(failed, v)
      }
    }

    data.frame(
      varname     = varnames,
      label_short = labels,
      prompt      = prompts,
      stringsAsFactors = FALSE
    )
  }

  # --- App state ------------------------------------------------------------
  app_values <- reactiveValues(data_recoded = NULL, labels = NULL)

  ui <- fluidPage(
    titlePanel("Rename Variable Names via LLM"),
    sidebarLayout(
      sidebarPanel(
        #p(strong("Columns to rename:"), paste(names(data), collapse = ", ")),
        actionButton("generate", "Generate short names"),
        actionButton("apply_labels", "Apply and return results"),
        downloadButton("download", "Download label mapping"),
        br(), verbatimTextOutput("action_status"),
        tags$p(
          style = "color: #888; font-size: 90%; margin-top: 10px;",
          "Note: The 'Generate' button uses the original column names only. ",
          "It will not reapply to already edited names. To make changes, edit the fields manually."
        )
      ),
      mainPanel(
        p(strong("Columns to rename:"), paste(names(data), collapse = ", ")),
        # Spinner appears while labels_ui waits for labels_generated()
        shinycssloaders::withSpinner(uiOutput("labels_ui"))
      )
    )
  )

  server <- function(input, output, session) {
    data_input <- reactiveVal(data)

    # Trigger LLM generation on click
    labels_generated <- eventReactive(input$generate, {
      vars <- names(data_input())
      generate_labels_short(
        varnames      = vars,
        model         = model,
        prompt_prefix = prompt_prefix,
        prompt_suffix = prompt_suffix,
        max_length    = max_length
      )
    }, ignoreInit = TRUE)

    # Warn after generation
    observeEvent(labels_generated(), {
      res <- labels_generated()
      if (any(is.na(res$label_short))) {
        showNotification("Label generation failed for some variables.", type = "warning", duration = 5)
      }
      if (any(duplicated(res$label_short[!is.na(res$label_short)]))) {
        showNotification("Some generated labels are not unique.", type = "warning", duration = 5)
      }
    })

    # UI for editing labels
    output$labels_ui <- renderUI({
      res <- labels_generated()
      req(res)
      tagList(
        lapply(seq_len(nrow(res)), function(i) {
          tagList(
            strong(res$varname[i]),
            textInput(
              inputId = paste0("label_", i),
              label   = NULL,
              value   = res$label_short[i],
              width   = "100%"
            ),
            tags$hr()
          )
        })
      )
    })

    # Download mapping (current edits included)
    output$download <- downloadHandler(
      filename = function() paste0("labels_", Sys.Date(), ".csv"),
      content  = function(file) {
        res <- labels_generated()
        req(res)
        edited <- vapply(seq_len(nrow(res)), function(i) {
          val <- input[[paste0("label_", i)]]
          if (is.null(val)) res$label_short[i] else val
        }, FUN.VALUE = character(1))
        out <- data.frame(
          varname     = res$varname,
          label_short = edited,
          prompt      = gsub("\n", " ", res$prompt),
          stringsAsFactors = FALSE
        )
        write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # Apply labels and return
    observeEvent(input$apply_labels, {
      req(labels_generated())
      df  <- data_input()
      res <- labels_generated()

      edited <- vapply(seq_len(nrow(res)), function(i) {
        val <- input[[paste0("label_", i)]]
        if (is.null(val)) res$label_short[i] else val
      }, FUN.VALUE = character(1))

      if (length(edited) != ncol(df)) {
        output$action_status <- renderText("Error: Number of labels doesn't match number of columns.")
        return()
      }
      if (any(duplicated(edited))) {
        output$action_status <- renderText("Error: Some new names are duplicated.")
        return()
      }

      names(df) <- edited
      app_values$data_recoded <- df
      app_values$labels       <- data.frame(
        varname     = res$varname,
        label_short = edited,
        prompt      = res$prompt,
        stringsAsFactors = FALSE
      )

      stopApp(invisible(list(
        data_recoded = df,
        labels       = app_values$labels
      )))
    })
  }

  runApp(shinyApp(ui, server))
}
