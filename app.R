library(shiny)
library(shinyjs)
library(SynTruthPkg)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "SynTruth"),
  dashboardSidebar(width = 300,
    # tags$head(tags$style(HTML('padding-top: 10px;
    #                         padding-left: 20px;
    #                         display: inline-block;
    #                         border-color: transparent'))),
    br(),
    h4('Synthetic data generator for assessing the performance of algorithms for predicting treatment response'),
    br(),
    tags$div(
      tags$p("SynTruth generates synthetic datasets of"),
      tags$ol(
        tags$li('gene expression'),
        tags$li('patient benefit labels'),
        tags$li('treatment'),
        tags$li('survival')
      ),
      tags$p("Each patient belongs to one of four groups:"),
      tags$ol(
        tags$li("benefit and treatment 1 (B1)"),
        tags$li("benefit and treatment 0 (B0)"),
        tags$li("non-benefit and treatment 1 (NB1)"),
        tags$li("non-benefit and treatment 0 (NB0)")
      ),
      tags$p("The performance of machine learning algorithms can be assessed by comparing predictions to 
             ground truth datasets generated by SynTruth."),
      p("More information:"),
      a(href = "https://github.com/DoraDoraExplorer/SynTruth_pkg", "SynTruth package")
    )
    # tags$div(
    #   br(),
    #   tags$p("SynTruth generates synthetic datasets of"),
    #   tags$ol(
    #     tags$li('gene expression (marker, M and non-marker, NM genes)'),
    #     tags$li('patient benefit labels (benefit, B or non-benefit, NB)'),
    #     tags$li('treatment (0, 1-treatment of interest)'),
    #     tags$li('survival (time and event indicator)')
    #   ),
    #   tags$p("Each patient belongs to one of four groups: benefit and treatment 1 (B1); benefit and treatment 0 (B0); non-benefit and treatment 1 (NB1); non-benefit and treatment 0 (NB0)."),
    #   tags$p("The performance of machine learning algorithms can be assessed by comparing predictions to the ground truth generated by SynTruth."),
    # )
  ),
  dashboardBody(
    column(6,
           box(width = 12, height = '450px', solidHeader = T, status = "primary",
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
           box(width = 12, height = '550px', solidHeader = T, status = "primary",
               title = "Survival plot",
               plotOutput("survplot"),
               br(),
               uiOutput("download")
           )
    ),
    column(width = 6,
           fluidRow(
             box(width = 6, solidHeader = T, status = "primary",
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
             box(width = 6, solidHeader = T, status = "primary",
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
             box(width = 6, solidHeader = T, status = "primary",
                 title = "Nonmarker blocks",
                 numericInput('mu_nonmarker', "Mu NM", 0, min = -5, max = 5),
                 textInput('sds_nonmarker', "SDs NM", "0.4,0.3"),
                 textInput('rhos_nonmarker', "Correlations NM", "0.8,0.8"),
                 br(),br(),br(),
                 br(),br(),br(),
                 p(" ")
             ),
             box(width = 6, solidHeader = T, status = "primary",
                 title = "Marker blocks",
                 column(6,
                        textInput('marker_blocksizes', HTML("Marker blocksizes"), "5,10"),
                        h4('NB'),
                        textInput('mus_NB', "Mus NB", "0,1"),
                        textInput('sds_NB', "SDs NB", "0.4,0.5"),
                        textInput('rhos_NB', "Correlations NB", "0.8,0.7")
                        ),
                 column(6,
                        textInput('gene_effects', HTML("Gene <br/> effects"), "AND,OR"),
                        h4('B'),
                        textInput('mu_diffs', "Mu diffs", "4,2"),
                        textInput('sds_B', "SDs B", "0.4,0.3"),
                        textInput('rhos_B', HTML("Correlations <br/> B"), "0.8,0.4")
                        )
             )
           ),
           fluidRow(align = "center",
                    br(),
                    actionButton("go", "GO", class = "btn-lg btn-success")
           )
    )
  )
)

  server <- function(input, output, session){
  
  # Parse input
  parse_inputs <- function(num_vec_inputs, input){
    for (i in 1:length(num_vec_inputs)){
      num_vec_inputs[[i]] <- as.numeric(unlist(strsplit(num_vec_inputs[[i]], ",")))
    }
    char_vec_input <- list(gene_effects = unlist(strsplit(input$gene_effects, ",")))
    non_parsed_input_names <- setdiff(names(input), c(names(num_vec_inputs), 'gene_effects'))
    non_parsed_input <- as.list(input)[non_parsed_input_names]
    input_list <- as.list(c(num_vec_inputs, char_vec_input, non_parsed_input))
    input_list
  }
    
    
  input_list <- eventReactive(input$go, {
    parse_inputs(num_vec_inputs = list(marker_blocksizes = input$marker_blocksizes, mus_NB = input$mus_NB, sds_NB = input$sds_NB, 
                                       rhos_NB = input$rhos_NB, mu_diffs = input$mu_diffs, sds_B = input$sds_B, 
                                       rhos_B = input$rhos_B, sds_nonmarker = input$sds_nonmarker, 
                                       rhos_nonmarker = input$rhos_nonmarker),
                 input = input)
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
