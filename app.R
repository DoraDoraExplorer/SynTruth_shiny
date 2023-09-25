library(shiny)
library(shinyjs)
library(SynTruthPkg)


ui <- fluidPage(
  useShinyjs(),
  fluidPage(
    tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
             tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 
                     'SynTruth'),
             p('Synthetic data generator for assessing the performance of algorithms for predicting treatment response'),
    ),
    sidebarLayout(
      sidebarPanel(
        h1("Gene expression"),
        fixedRow(
          column(4, 
                 numericInput('n_pts', "Number of patients", 50, min = 10, max = 2000),
                 numericInput('fraction_censored', "Fraction of censored patients", 0, min = 0, max = 1),
                 numericInput('n_genes', "Number of genes", 100, min = 50, max = 20000)
          ),
          column(4,
                 numericInput('fraction_pts_benefit', "Fraction of B patients", 0.5, min = 0, max = 1),
                 numericInput('fraction_tx_1', "Fraction of\n treatment1", 0.5, min = 0.2, max = 0.8)
          ),
          column(4,
                 numericInput('noise mean', HTML("Noise <br/> mean"), 0, min = 0, max = 1),
                 numericInput('noise sd', HTML("Noise <br/> SD"), 1, min = 0, max = 1)
          )
        ), 
        fixedRow(
          h3('Marker and nonmarker blocks'),
          column(8,
                 h3("M"),
                 numericInput('blocksize_m', HTML("Marker <br/> blocksize"), 5, min = 1, max = 100),
                 selectInput('gene_effect', "Gene effect", choices = list("AND" = "AND", 
                                                                          "OR" = "OR", 
                                                                          "none" = "none")),
                 column(5, 
                        h4("NB"),
                        numericInput('mus_NB', "Mu NB", 0, min = -5, max = 5),
                        numericInput('sds_NB', "SD NB", 0.4, min = 0, max = 1),
                        numericInput('rhos_NB', "Correlation NB", 0.8, min = 0, max = 1)
                 ),
                 column(5, 
                        h4("B"),
                        numericInput('mu_diffs', "Mu diff", 0, min = -5, max = 5),
                        numericInput('sds_B', "SD B", 0.4, min = 0, max = 1),
                        numericInput('rhos_B', "Correlation B", 0.8, min = 0, max = 1))
                 
                 
          ),
          column(4,
                 h3('NM'),
                 numericInput('nm_blocksize', "Non-marker blocksize", 50, min = 1, max = 100),
                 numericInput('mu_nonmarker', "Mu NM", 0, min = -5, max = 5),
                 numericInput('sds_nonmarker', "SD NM", 0.4, min = 0, max = 1),
                 numericInput('rhos_nonmarker', "Correlation NM", 0.8, min = 0, max = 1)
          )
        ),
        h1("Survival"),
        fixedRow(
          column(12, 
                 numericInput('shape', "Weibull distribution shape", 1, min = -5, max = 5),
                 column(6,
                        numericInput('scale_NB0', "NB0 scale", 10, min = 0, max = 10),
                        numericInput('HR_B0_NB0', "HR B0/NB0", 0.7, min = 0.1, max = 10)
                 ),
                 column(6,
                        numericInput('HR_NB1_NB0', "HR NB1/NB0", 0.9, min = 0, max = 10),
                        numericInput('HR_B1_NB0', "HR B1/NB0", 0.5, min = 0, max = 10)
                        
                 )
          )
        ),
        fluidRow(
          actionButton("go", "GO", class = "btn-lg btn-success")
        )
      ),
      mainPanel(
        br(),
        h1("Results"),
        fluidRow(
          br(),
          verbatimTextOutput('debug_print'),
          uiOutput("download_gep"),
          br(),
          h2("Survival curves"),
          plotOutput("survplot"),
          uiOutput("download_survplot"),
          verbatimTextOutput("This text explains Survival plot"),
          h2('HRs calculated from data'),
          tableOutput("dataHRs"),
          verbatimTextOutput("This text explains data HRs")
          
        )
      )
    )
  )
)

server <- function(input, output, session){
  output$debug_print <- renderPrint(lapply(input, function(x) x))
  
  
  output$survplot <- renderPlot({
    input_list <- lapply(input, function(x) x)
    
    gep <- SynTruthPkg::get_best_gep(params = input_list, n_repeats = 100, threshold = 0.05)$best_gep
    print(head(gep))
    output_hrs <- SynTruthPkg::get_best_gep(params = input, n_repeats = 100, threshold = 0.05)$best_output_hs
    
    SynTruthPkg::plot_surv_curve(gep = gep, output_hrs = output_hrs, 
                                 mytitle = "Survival plot")
  })
    
}

shinyApp(ui, server)
