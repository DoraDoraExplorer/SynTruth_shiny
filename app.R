library(shiny)
library(shinyjs)
library(SynTruthPkg)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "SynTruth"),
  dashboardSidebar(
    tags$head(tags$style(HTML('padding-top: 10px;
                            padding-left: 20px;
                            display: inline-block;
                            border-color: transparent'))),
    p('Synthetic data generator for assessing the performance of algorithms for predicting treatment response')
  ),
  dashboardBody(
    column(6,
           box(width = 12, height = '430px', solidHeader = TRUE, status = "primary",
               title = "Overview of synthetic gene expression components",
               tags$figure(
                 class = "centerFigure",
                 tags$img(
                   src = "overview.png",
                   width = 500,
                   alt = "Overview of synthetic gene expression components"
                 )
             )
           ),
           box(width = 12, height = '550px', solidHeader = TRUE, status = "primary",
               title = "Survival plot",
               plotOutput("survplot"),
               br(),
               uiOutput("download")
           )
    ),
    column(width = 6,
           fluidRow(
             box(width = 6, solidHeader = TRUE, status = "primary",
                 title = "Gene expression",
                 column(6,
                        numericInput('n_pts', "Number of patients", 50, min = 10, max = 2000)
                        ),
                 column(6,
                        numericInput('n_genes', HTML("Number of genes"), 100, min = 50, max = 20000)
                        ),
                 numericInput('fraction_pts_benefit', "Fraction of B patients", 0.5, min = 0, max = 1),
                 numericInput('fraction_tx_1', "Fraction of treatment1", 0.5, min = 0.2, max = 0.8),
                 numericInput('fraction_censored', "Fraction of censored patients", 0, min = 0, max = 1),
                 column(6,
                        numericInput('noisemean', HTML("Noise mean"), 0, min = -10,  max = 10)
                        ),
                 column(6,
                        numericInput('noisesd', HTML("Noise sd"), 0.2, min = 0,  max = 10)
                        )
             ),
             box(width = 6, solidHeader = TRUE, status = "primary",
                 title = "Survival",
                 br(),
                 numericInput('HR_B0_NB0', "HR B0/NB0", 0.7, min = 0.1, max = 10),
                 numericInput('HR_NB1_NB0', "HR NB1/NB0", 0.9, min = 0, max = 10),
                 numericInput('HR_B1_NB0', "HR B1/NB0", 0.5, min = 0, max = 10),
                 numericInput('scale_NB0', "NB0 scale", 10, min = 0, max = 10),
                 numericInput('shape', "Weibull distribution shape", 1.4, min = -5, max = 5)
             )
           ),
           fluidRow(
             box(width = 6, solidHeader = TRUE, status = "primary",
                 title = "Nonmarker bloks",
                 numericInput('mu_nonmarker', "Mu NM", 0, min = -5, max = 5),
                 textInput('sds_nonmarker', "SD NM", "0.4,0.3"),
                 textInput('rhos_nonmarker', "Correlation NM", "0.8,0.8")
             ),
             box(width = 6, solidHeader = TRUE, status = "primary",
                 title = "Marker blocks",
                 column(6,
                        textInput('marker_blocksizes', HTML("Marker blocksize"), "5,10"),
                        h4('NB'),
                        textInput('mus_NB', "Mu NB", "0,1"),
                        textInput('sds_NB', "SD NB", "0.4,0.5"),
                        textInput('rhos_NB', "Correlation NB", "0.8,0.7")
                        ),
                 column(6,
                        textInput('gene_effects', HTML("Gene <br/> effects"), "AND,OR"),
                        h4('B'),
                        textInput('mu_diffs', "Mu diff", "4,2"),
                        textInput('sds_B', "SD B", "0.4,0.3"),
                        textInput('rhos_B', "Correlation B", "0.8,0.4")
                        )
             ),
             br(),
             actionButton("go", "GO", class = "btn-lg btn-success")
           )
    )
  )
)

  server <- function(input, output, session){
  
  # Parse input
  input_list <- eventReactive(input$go, {
    parsed_num_vec_inputs <- list(marker_blocksizes = input$marker_blocksizes, mus_NB = input$mus_NB, sds_NB = input$sds_NB, 
                           rhos_NB = input$rhos_NB, mu_diffs = input$mu_diffs, sds_B = input$sds_B, 
                           rhos_B = input$rhos_B, sds_nonmarker = input$sds_nonmarker, 
                           rhos_nonmarker = input$rhos_nonmarker)
    for (i in 1:length(parsed_num_vec_inputs)){
      parsed_num_vec_inputs[[i]] <- as.numeric(unlist(strsplit(parsed_num_vec_inputs[[i]], ",")))
    }
    
    parsed_char_vec_input <- list(gene_effects = unlist(strsplit(input$gene_effects, ",")))
    
    non_parsed_input_names <- setdiff(names(input), c(names(parsed_num_vec_inputs), 'gene_effects'))
    non_parsed_input <- reactiveValuesToList(input)[non_parsed_input_names]
    input_list <- as.list(c(parsed_num_vec_inputs, parsed_char_vec_input, non_parsed_input))
    input_list
  })
  
  # output$debug_print <- renderPrint(input_list())
  
  # Make gep and output HRs
  syndata <- reactive(SynTruthPkg::get_best_gep(params = input_list(), n_repeats = 100, threshold = 0.05))
  gep <- reactive(syndata()$best_gep)
  output_hrs <- reactive(syndata()$best_output_hs)

  # output$debug_table <- renderTable(gep())

  # Make survplot
  output$survplot <- renderPlot({
    SynTruthPkg::plot_surv_curve(gep = gep(), output_hrs(),
                                 mytitle = "Survival plot")
  })
  
  observe({
    if (!is.null(gep())){
      output$download <- renderUI(downloadButton('download_data', "Download synthetic data" ))
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      "synthetic_data.txt"
    },
    content = function(file) {
      write.csv(gep(), file, row.names = F, sep = "\t")
    }
  )
  
}

shinyApp(ui, server)
