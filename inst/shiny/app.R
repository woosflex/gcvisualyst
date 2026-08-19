library(shiny)
library(gcvisualyst)


ui <- fixedPage(
  theme = bslib::bs_theme(bootswatch = "spacelab"),
  # Application title
  titlePanel("GC Visualyst - Analyze and Visualize GC Content"),
  
  # Tab panel for organizing different sections
  tabsetPanel(
    
    # Tab for using the main GC Visualyst functionality
    tabPanel("Use GC Visualyst", 
             sidebarLayout(
               sidebarPanel(
                 textInput("sequences", "Enter DNA sequences (comma-separated):", 
                           placeholder = "AGCTGCGCGT, GGCTAGCTCG"),
                 fileInput("file", "Upload a FASTA file:", accept = ".fasta"),
                 numericInput("window", "Window size for GC content analysis:", value = 100, min = 1),
                 radioButtons("plot_type", "Select plot type:",
                              choices = list("Combined", "Facet-wrapped"),
                              selected = "Combined"),
                 actionButton("analyze", "Visualize GC Content")
               ),
               mainPanel(
                 plotOutput("gc_plot"),
                 textOutput("error_message")
               )
             )),
    
    tabPanel("Documentation", 
             h3("How to Use GC Visualyst Webapp"),
             p("1. Enter your DNA sequences manually or upload a FASTA file."),
             p("2. Select a window size for calculating GC content."),
             p("3. Choose whether to combine the plots for all sequences or display them separately."),
             p("4. Click 'Visualize GC Content' to generate the plot."),
             h4("For more functionality, try using GC Visualyst's R package available at ", tags$a(href="https://www.github.com/woosflex/gcvisualyst", "GitHub"), ".")),
    
    tabPanel("About", 
             h3("About GC Visualyst"),
             p("GC Visualyst is an R package designed for bioinformatics researchers to calculate and visualize the GC content of DNA sequences across sliding windows."),
             p("The goal of gcvisualyst is to analyze and visualize the GC content of DNA sequences across sliding windows. This tool provides a simple and effective way to calculate GC content and generate plots that illustrate GC content variations along sequences."),
             p("Features:"),
             tags$ul(tags$li("GC Content Calculation: Computes the GC content of DNA sequences across sliding windows of user-defined size."), 
                     tags$li("Data Visualization: Generates visualizations of GC content for multiple sequences, either combined in a single plot or as separate facets for each sequence."),
                     tags$li("Customizable Plots: Provides options for combined or facet-wrapped layouts for easy comparison of multiple sequences."),
                     tags$li("Efficient Processing: Utilizes dplyr, ggplot2, stringr, and purrr for efficient data manipulation and plotting.")
             ),
             p("Kindly contact through email at adnanraza3435(at)gmail(dot)com for suggestions and feedback.")
             )
    )
)

server <- function(input, output, session) {
  
  observeEvent(input$analyze, {
    # Validate if input is provided
    if (input$sequences == "" && is.null(input$file)) {
      output$error_message <- renderText("Please provide DNA sequences or upload a file.")
      return(NULL)
    }
    
    # Create a sequences dataframe
    sequences_df <- NULL
    if (!is.null(input$file)) {
      sequences_df <- read_fasta(input$file$datapath)
    } else {
      sequences <- unlist(strsplit(input$sequences, ",\\s*"))
      num_sequences <- length(sequences)
      header_names <- paste0("sequence_", 1:num_sequences)
      sequences_df <- data.frame(headers = header_names, sequences = sequences, stringsAsFactors = FALSE)
    }
    
    # Perform GC content calculation and plot visualization
    if (!is.null(sequences_df)) {
      gc_content_df <- gc_content(sequences_df, window = input$window)
      
      output$gc_plot <- renderPlot({
        combined <- ifelse(input$plot_type == "Combined", TRUE, FALSE)
        gc_visualize(gc_content_df, combined = combined)
        
      })
      
      output$error_message <- renderText("")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)