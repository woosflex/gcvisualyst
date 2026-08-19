library(shiny)
library(gcvisualyst)


ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(bootswatch = "spacelab"),
  title = "GC Visualyst - Analyze and Visualize GC Content, Skew & CpG Islands",

  sidebar = bslib::sidebar(
    textInput(
      "sequences",
      "Enter DNA sequences (comma-separated):",
      placeholder = "AGCTGCGCGT, GGCTAGCTCG"
    ),
    fileInput("file", "Upload a FASTA file:", accept = ".fasta"),
    h5("Thresholds"),
    numericInput("window", "Window size (bp):", value = 100, min = 1),
    numericInput(
      "gc_threshold", "CpG island GC threshold:",
      value = 0.5, min = 0, max = 1, step = 0.05
    ),
    numericInput(
      "cpg_threshold", "Observed/expected CpG threshold:",
      value = 0.6, min = 0, max = 5, step = 0.05
    ),
    numericInput("min_length", "Minimum island length (bp):", value = 200, min = 1),
    radioButtons(
      "plot_type",
      "Plot layout:",
      choices = list("Combined" = "combined", "Facet-wrapped" = "facet"),
      selected = "combined"
    ),
    actionButton("analyze", "Run Analysis", class = "btn-primary")
  ),

  bslib::navset_card_tab(
    bslib::nav_panel(
      "GC Content",
      plotOutput("gc_plot")
    ),
    bslib::nav_panel(
      "GC Skew",
      plotOutput("skew_plot")
    ),
    bslib::nav_panel(
      "CpG Islands",
      div(
        downloadButton("download_csv", "Download CSV"),
        downloadButton("download_bed", "Download BED"),
        actionButton("clear_results", "Clear Results", class = "btn-outline-secondary")
      ),
      br(),
      h5("Detected CpG Islands"),
      tableOutput("islands_table"),
      p("If no islands are detected the table will be empty. Use the buttons above to export the island coordinates.")
    ),
    bslib::nav_panel(
      "Documentation",
      h3("How to Use GC Visualyst Webapp"),
      p("1. Enter your DNA sequences manually or upload a FASTA file."),
      p("2. Set a window size (in bp) for calculating GC content and GC skew."),
      p("3. Set the CpG island thresholds: minimum GC content, observed/expected CpG ratio, and minimum island length."),
      p("4. Choose a combined or facet-wrapped plot layout."),
      p("5. Click 'Run Analysis' to compute GC content, GC skew and CpG islands for the same input."),
      p("6. View the GC content and GC skew plots in their tabs, and the detected CpG islands in the results table."),
      p("7. Export the CpG island results as CSV, or as a BED file for use with the UCSC Genome Browser."),
      h4("Error handling"),
      p("If the window size is larger than a provided sequence, an informative message is shown instead of the app crashing."),
      h4("For more functionality, try using GC Visualyst's R package available at ",
        tags$a(href = "https://www.github.com/woosflex/gcvisualyst", "GitHub"), ".")
    ),
    bslib::nav_panel(
      "About",
      h3("About GC Visualyst"),
      p("GC Visualyst is an R package designed for bioinformatics researchers to calculate and visualize the GC content of DNA sequences across sliding windows, compute GC skew, and detect CpG islands."),
      p("The goal of gcvisualyst is to analyze and visualize the GC content of DNA sequences across sliding windows. This tool provides a simple and effective way to calculate GC content and generate plots that illustrate GC content variations along sequences."),
      p("Features:"),
      tags$ul(
        tags$li("GC Content Calculation: Computes the GC content of DNA sequences across sliding windows of user-defined size."),
        tags$li("GC Skew Calculation: Computes GC skew (G - C)/(G + C) across sliding windows."),
        tags$li("CpG Island Detection: Finds high-GC, CpG-enriched regions with customizable thresholds."),
        tags$li("Data Visualization: Combined or facet-wrapped line plots for GC content and GC skew, plus a summary CpG island table."),
        tags$li("Data Export: Download CpG islands as CSV or as a BED track."),
        tags$li("Efficient Processing: Utilizes dplyr, ggplot2, stringr, and purrr for efficient data manipulation and plotting.")
      ),
      p("Kindly contact through email at adnanraza3435(at)gmail(dot)com for suggestions and feedback.")
    )
  ),

  # Message area rendered below the tabs so errors never crash the app.
  div(
    style = "margin-top: 15px; color: #b00020;",
    textOutput("error_message")
  )
)


server <- function(input, output, session) {

  # Holds the results of the last successful run.
  results <- reactiveVal(NULL)

  # Reset the error message whenever the inputs change so stale errors vanish.
  observe({
    input$sequences
    input$file
    input$window
    input$gc_threshold
    input$cpg_threshold
    input$min_length
    output$error_message <- renderText("")
  })

  # Clear downloaded/plotted results.
  observeEvent(input$clear_results, {
    results(NULL)
    output$error_message <- renderText("")
  })

  observeEvent(input$analyze, {
    # Clear any prior message and results.
    output$error_message <- renderText("")
    results(NULL)

    # Validate that at least one source of input was provided.
    has_text <- nzchar(trimws(input$sequences))
    if (!has_text && is.null(input$file)) {
      output$error_message <- renderText(
        "Please provide DNA sequences or upload a FASTA file."
      )
      return(NULL)
    }

    # Build a data.frame of sequences from the file or the typed input.
    sequences_df <- NULL
    tryCatch(
      {
        if (!is.null(input$file)) {
          sequences_df <- read_fasta(input$file$datapath)
        } else {
          sequences <- unlist(strsplit(input$sequences, ",\\s*"))
          sequences <- sequences[nzchar(trimws(sequences))]
          if (length(sequences) == 0) {
            stop("No non-empty DNA sequences were entered.")
          }
          header_names <- paste0("sequence_", seq_along(sequences))
          sequences_df <- data.frame(
            headers = header_names,
            sequences = sequences,
            stringsAsFactors = FALSE
          )
        }

        # Run the three analyses on the SAME input using package functions.
        gc_df    <- gc_content(sequences_df, window = input$window)
        skew_df  <- gc_skew(sequences_df, window = input$window)
        islands  <- cpg_islands(
          sequences_df,
          window = input$window,
          gc_threshold = input$gc_threshold,
          cpg_threshold = input$cpg_threshold,
          min_length = input$min_length
        )

        results(list(gc = gc_df, skew = skew_df, islands = islands))
      },
      error = function(e) {
        # gc_content()/gc_skew() raise errors e.g. when the window exceeds the
        # sequence length; surface the message as text instead of crashing.
        output$error_message <- renderText(
          paste("Analysis failed:", conditionMessage(e))
        )
      }
    )
  })

  # --- Outputs ------------------------------------------------------------

  output$gc_plot <- renderPlot({
    req(results())
    combined <- input$plot_type == "combined"
    gc_visualize(results()$gc, combined = combined)
  })

  output$skew_plot <- renderPlot({
    req(results())
    combined <- input$plot_type == "combined"
    gc_skew_visualize(results()$skew, combined = combined)
  })

  # DT is not a hard dependency, so a plain renderTable keeps the app
  # dependency-light and fully self-contained.
  output$islands_table <- renderTable({
    req(results())
    res <- results()$islands
    if (nrow(res) == 0) {
      return(data.frame(
        header = "none", start = NA_integer_, end = NA_integer_,
        length = NA_integer_, mean_gc = NA_real_
      )[0, ])
    }
    res
  })

  output$download_csv <- downloadHandler(
    filename = function() "cpg_islands.csv",
    content = function(file) {
      res <- results()
      if (is.null(res)) stop("Run an analysis first.")
      write.csv(res$islands, file, row.names = FALSE)
    }
  )

  output$download_bed <- downloadHandler(
    filename = function() "cpg_islands.bed",
    content = function(file) {
      res <- results()
      if (is.null(res)) stop("Run an analysis first.")
      write_bed(res$islands, path = file, track = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
