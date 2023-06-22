# Install required packages if not already installed
install.packages(c("shiny", "PLNmodels", "shinyjs","DT","corrplot", "patchwork","cowplot","ggplot2","gridExtra","reshape2", "ape", "stats", "xtable", "plotly", "vegan", "tidyverse", "imputeTS", "imputeMissings", "compositions", "zCompositions"))

library(shiny)
library(ape)
library(stats)
library(plotly)
library(vegan)
library(tidyverse)
library(reshape2)
library(imputeTS)
library(imputeMissings)
library(compositions)
library(DT)
library(xtable)
library(zCompositions)
library(corrplot)
library(cowplot)
library(shinyjs)
library(ggplot2)
library(gridExtra)
library(PLNmodels)
library(patchwork)
source(file = 'R_func.R')



tmpvalues <- reactiveValues(matrixList = list())
values <- reactiveValues(matrixList = list())

matrixList <- reactiveVal(list())

# Define the UI
ui <- navbarPage(
  title = "Similarity Matrix Visualization",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  shinyjs::useShinyjs(),
  
  # Page 1: Method Justifications
  tabPanel(
    "Method Justifications",
    fluidPage(
      useShinyjs(),  # Add useShinyjs()
      div(
        style="text-align:center;",
        h1(tags$span("Method justification",
                     style = "font-family: Times New Roman ; font-size: 42px; color: #FF0000;font-weight: 700;"))
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Linear Kernel (LK)",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            tags$p("Also known as the cross product or Ross matrix, the linear kernel is a suitable choice when the data exhibits a linear pattern. It works well when the data change proportionally.",
                   tags$br(),
                   tags$br(),
                   tags$strong("Ross et al., 2013:"), " ", tags$em("Metagenomic Predictions: From Microbiome to Complex Health and Environmental Phenotypes in Humans and Cattle"),
                   tags$br(),
                   tags$strong(tags$a("https://doi.org/10.1371/journal.pone.0073056", href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0073056")),
                   tags$br(),
                   tags$strong("Montesinos-Lopez et al., 2021:"), " ", tags$em( "A guide for kernel generalized regression methods for genomic-enabled prediction"),
                   tags$br(),
                   tags$strong(tags$a("https://doi.org/10.1038/s41437-021-00412-1", href = "https://www.nature.com/articles/s41437-021-00412-1")),
                   id = "linearJustification",
                   style = "font-size:20px;")
          ),
          actionButton("toggleLinearButton", "Toggle Linear Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Polynomial Kernel (PK)",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            tags$p("The polynomial kernel is preferable when the data shows nonlinear relationships. It allows us to capture intricate interactions and higher-order relationships among features.",
                   tags$br(),
                   tags$br(),
                   tags$strong("Montesinos-Lopez et al., 2021:")," " , tags$em("A guide for kernel generalized regression methods for genomic-enabled prediction"),
                   tags$br(),
                   tags$strong(tags$a("https://doi.org/10.1038/s41437-021-00412-1", href = "https://www.nature.com/articles/s41437-021-00412-1")),
                   tags$br(),
                   tags$strong("He et al., 2022:"), " ", tags$em("Exploring methods to summarize gut microbiota composition for microbiability estimation and phenotypic prediction in swine"),
                   tags$br(),
                   tags$strong(tags$a("https://doi.org/10.1093/jas/skac231", href = "https://academic.oup.com/jas/article/100/9/skac231/6623959?login=true")),
                   id = "polynomialJustification",
                   style = "font-size:20px;")
          ),
          actionButton("togglePolynomialButton", "Toggle Polynomial Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Gaussian Kernel (GK)",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            tags$p("The Gaussian kernel is a suitable choice when the data exhibits a Gaussian-like distribution or when there are local interactions between features. It can capture subtle variations and provide a more nuanced assessment of heritability by giving more importance to samples with similar composition.",
                   tags$br(),
                   tags$br(),
                   tags$strong("Montesinos-Lopez et al., 2021:")," " , tags$em("A guide for kernel generalized regression methods for genomic-enabled prediction"),
                   tags$br(),
                   tags$strong(tags$a("https://doi.org/10.1038/s41437-021-00412-1", href = "https://www.nature.com/articles/s41437-021-00412-1")),
                   tags$br(),
                   tags$strong("He et al., 2022:"), " ", tags$em("Exploring methods to summarize gut microbiota composition for microbiability estimation and phenotypic prediction in swine"),
                   tags$br(),
                   tags$strong(tags$a("https://doi.org/10.1093/jas/skac231", href = "https://academic.oup.com/jas/article/100/9/skac231/6623959?login=true")),
                   id = "gaussianJustification",
                   style = "font-size:20px;")
          ),
          actionButton("toggleGaussianButton", "Toggle Gaussian Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Arc-Cosine Kernel (AK1)",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            p("The arc-cosine kernel is preferred when the data represents relative abundances or compositional data. It effectively captures the compositional nature of the data by handling the inherent constraints of proportions.",
              tags$br(),
              tags$br(),
              tags$strong("Montesinos-Lopez et al., 2021:")," " , tags$em("A guide for kernel generalized regression methods for genomic-enabled prediction"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.1038/s41437-021-00412-1", href = "https://www.nature.com/articles/s41437-021-00412-1")),
              tags$br(),
              tags$strong("He et al., 2022:"), " ", tags$em("Exploring methods to summarize gut microbiota composition for microbiability estimation and phenotypic prediction in swine"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.1093/jas/skac231", href = "https://academic.oup.com/jas/article/100/9/skac231/6623959?login=true")),
              id = "arcCosineJustification",
              style = "font-size:20px;")
          ),
          actionButton("toggleArcCosineButton", "Toggle Arc-Cosine Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Poisson Log Normal method (PLN)",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            tags$p("The Poisson Log Normal (PLN) method can be used to analyze data, whether it is relative or not, due to its ability to address two key characteristics of such data: count-based nature and overdispersion.",
                   tags$br(),
                   tags$br(),
                   tags$strong("Chiquet et al., 2021:"), " ", tags$em("The Poisson-Lognormal Model as a Versatile Framework for the Joint Analysis of Species Abundances"),
                   tags$br(),
                   tags$strong(tags$a("https://doi.org/10.3389/fevo.2021.588292", href = "https://www.frontiersin.org/articles/10.3389/fevo.2021.588292/full")),
                   id = "PLNJustification",
                   style = "font-size:20px;")
          ),
          actionButton("togglePLNButton", "Toggle PLN Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Jaccard",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            p("The Jaccard distance is commonly used to analyze data, whether it's relative or absolute, due to its simplicity and robustness. It compares the presence or absence of features between samples, disregarding abundance levels, making it suitable for diverse datasets. Not recommended to treat compositional data.",
              tags$br(),
              tags$br(),
              tags$strong("Dixon et al., 2003:"), " ", tags$em("VEGAN, a package of R functions for community ecology"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.1016/S0377-8401(03)00204-9", href = "https://www.sciencedirect.com/science/article/pii/S0377840103002049?via%3Dihub")),
              tags$br(),
              tags$strong("Gloor et al., 2017:"), " ", tags$em("Microbiome Datasets Are Compositional: And This Is Not Optional"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.3389/fmicb.2017.02224", href = "https://www.frontiersin.org/articles/10.3389/fmicb.2017.02224/full")),
              id = "jaccardJustification",
              style = "font-size:20px;")
          ),
          actionButton("toggleJaccardButton", "Toggle Jaccard Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Bray-Curtis",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            p("The Bray-Curtis distance is a valuable metric to treat and analyze data due to its ability to capture both the presence/absence and relative abundance of features. Not recommended to treat compositional data.",
              tags$br(),
              tags$br(),
              tags$strong("Dixon et al., 2003:"), " ", tags$em("VEGAN, a package of R functions for community ecology"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.1016/S0377-8401(03)00204-9", href = "https://www.sciencedirect.com/science/article/pii/S0377840103002049?via%3Dihub")),
              tags$br(),
              tags$strong("Gloor et al., 2017:"), " ", tags$em("Microbiome Datasets Are Compositional: And This Is Not Optional"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.3389/fmicb.2017.02224", href = "https://www.frontiersin.org/articles/10.3389/fmicb.2017.02224/full")),
              id = "bcJustification",
              style = "font-size:20px;")
          ),
          actionButton("toggleBCButton", "Toggle Bray-Curtis Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Euclidean/Aitchison",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            p("The Euclidean distance is the most basic distance. When used on CLR transformed data, it is called Aitchison distance. The Aitchison distance is known to handle compositional data, meaning data in which the abundance of one species is dependent on others, and the Aitchison distance accounts for this constraint. It also enables meaningful comparisons and dissimilarity analysis between microbial samples, allowing for the exploration of genetic factors and heritability in microbial abundance variations.",
              tags$br(),
              tags$br(),
              tags$strong("Gloor et al., 2017:"), " ", tags$em("Microbiome Datasets Are Compositional: And This Is Not Optional"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.3389/fmicb.2017.02224", href = "https://www.frontiersin.org/articles/10.3389/fmicb.2017.02224/full")),
              tags$br(),
              tags$strong("Greenacre et al., 2022:")," ", tags$em("Aitchison's Compositional Data Analysis 40 Years On: A Reappraisal"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.48550/arXiv.2201.05197", href = "https://arxiv.org/abs/2201.05197")),
              id = "aitJustification",
              style = "font-size:20px;")
          ),
          actionButton("toggleAitButton", "Toggle Aitchison Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("MultiDimensionalScaling (MDS)",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            p("MDS (Multidimensional Scaling) is used to treat and analyze data because it allows for the visualization of similarities or dissimilarities between samples based on their microbial composition, providing a comprehensive overview of the dataset. MDS reduces the dimensionality of the data while preserving pairwise distances, it exacerbates discrimination between samples and/or group of samples.",
              tags$br(),
              tags$br(),
              tags$strong("He et al., 2022:"), " ", tags$em("Exploring methods to summarize gut microbiota composition for microbiability estimation and phenotypic prediction in swine"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.1093/jas/skac231", href = "https://academic.oup.com/jas/article/100/9/skac231/6623959?login=true")),
              id = "mdsJustification",
              style = "font-size:20px;")
          ),
          actionButton("toggleMDSButton", "Toggle MDS Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Detrended Correpondence Analysis (DCA)",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            p("DCA (Detrended Correspondence Analysis) is used to analyze data due to its ability to capture complex nonlinear relationships or gradients in the dataset. By decomposing the variance in the data, DCA reveals the main trends or gradients present in the microbial communities, allowing for the interpretation of underlying patterns. It also exacerbates discrimination between samples and/or group of samples.",
              tags$br(),
              tags$br(),
              tags$strong("He et al., 2022:"), " ", tags$em("Exploring methods to summarize gut microbiota composition for microbiability estimation and phenotypic prediction in swine"),
              tags$br(),
              tags$strong(tags$a("https://doi.org/10.1093/jas/skac231", href = "https://academic.oup.com/jas/article/100/9/skac231/6623959?login=true")),
              id = "dcaJustification",
              style = "font-size:20px;")
          ),
          actionButton("toggleDCAButton", "Toggle DCA Justification")
        )
      )
    )
  ),
  
  # Page 2: Recapitulative Table
  tabPanel(
    "Recapitulative Table",
    mainPanel(
      div(
        style = "text-align: center;",
        h1(tags$span("Recapitulative Table",
                     style = "font-family: Times New Roman; font-size: 42px; color: #FF0000; font-weight: 700;"))
      ),
      div(
        style = "text-align: center;",
        img(src = "Recap1.PNG", width = "100%")
      ),
      div(
        style = "text-align: center;",
        img(src = "Recap2.PNG", width = "100%")
      ),
      div(
        style = "text-align: center;",
        h2(tags$span("Legend",
                     style = "font-family: Times New Roman; font-size: 42px; color: #FF0000; font-weight: 700;"))
      ),
      div(
        style = "text-align: center;",
        img(src = "Capture.PNG", width = "30%")
      )
    )
  ),
  
  # Page 3: Upload Data and Generate Similarity Matrix
  
  tabPanel(
    "Generate Matrix",
    sidebarLayout(
      sidebarPanel(
        div(
          tags$span("Upload Data File (CSV format)", style = "font-size: 18px;font-weight: 700;"),
          tags$p("n*p dataframe needed as input file (n=sample, p=OTUs/taxa)", style = "font-size: 14px;")
        ),
        tags$style(HTML("
      .custom-file-upload {
        display: inline-block;
        padding: 6px 12px;
        cursor: pointer;
        font-size: 16px;
        line-height: 1.42857143;
        text-align: center;
        white-space: nowrap;
        vertical-align: middle;
        border: 0px solid #ccc;
        border-radius: 4px;
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
        background-color: #f5f5f5;
        color: #333;
      }
      .custom-file-upload input[type='file'] {
        display: none;
      }
    ")),
        div(
          class = "custom-file-upload",
          fileInput("datafile", label = NULL, accept = ".csv")
        ),
        selectInput("method", label = tags$span("Similarity Method", style = "font-size: 17px;"), choices = c("Linear Kernel", "Polynomial Kernel", "Gaussian Kernel", "Arc-Cosine Kernel", "Bray-Curtis", "Jaccard", "Euclidean", "MDS", "DCA", "PLN")),
        conditionalPanel(
          condition = "input.method == 'Polynomial Kernel' || input.method == 'Gaussian Kernel'",
          numericInput("gamma", label = tags$span("Gamma", style = "font-size: 17px;"), value = 0),
          actionButton("generateGamma", "Generate 1/p Gamma value")
        ),
        
        # Preprocessing options
        div(
          style = "border: 1px solid #ddd; padding: 10px; margin-top: 10px;",
          tags$h4("Preprocessing Steps"),
          selectInput("type", label = tags$span("Data type", style = "font-size: 17px;"), choices = c("Raw", "Relative"), selected = "Relative"),
          selectInput("imputation", label = tags$span("Data Imputation", style = "font-size: 17px;"), choices = c("None", "Constant", "GBM"), selected = "None"),
          conditionalPanel(
            condition = "input.imputation != 'None' && input.imputation != 'GBM'",
            numericInput("imputation_value", label = tags$span("Imputation Value", style = "font-size: 17px;"), value = 1)
          ),
          selectInput("transformation", label = tags$span("Data Transformation", style = "font-size: 17px;"), choices = c("None", "Log", "CLR", "ILR", "ALR"), selected = "None")
        ),
        
        # Generate Matrix button and other buttons
        actionButton("generateButton", "Generate Similarity Matrix"),
        downloadButton("exportButton", "Export Matrix", class = "download-button"),
        actionButton("updateListButton", "Update Matrix List"),
        textInput("matrixName", label = "Matrix Name"),
        uiOutput('background_change')
      ),
      mainPanel(
        tableOutput("similarityTable"),
        textOutput("text"),
        verbatimTextOutput("matrixListOutput"),
        verbatimTextOutput("NmatrixListOutput"),
        verbatimTextOutput("GmatrixListOutput")
      )
    )
  ),
  
  # Page 4: Comparison of Similarity Matrices
  tabPanel(
    "Comparison",
    tags$style("
      .checkbox { /* checkbox is a div class*/
        line-height: 20px;
        margin-bottom: 20px; /*set the margin, so boxes don't overlap*/
        margin-top: 8px;
      }
      input[type='checkbox']{ /* style for checkboxes */
        width: 16px; /*Desired width*/
        height: 16px; /*Desired height*/
        line-height: 20px;
      }
      span {
          margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
          line-height: 20px;
      }
      label{
        font-size: 18px;
        font-weight: 700 !important;
        margin-bottom: 20px; /*set the margin, so boxes don't overlap*/
        margin-top: 8px;
      }
      input {
        vertical-align: middle;
      }
         label span {
  vertical-align: middle;
         }
      p {
      font-size: 14px;
      }
  }
"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("selectedMethods", label = tags$span("Select Methods", style = "font-family: Times New Roman ; font-size: 36px; color: #FF0000;"), choices = c(
          "Linear Kernel", 
          "Polynomial Kernel", 
          "Gaussian Kernel", 
          "Arc-Cosine Kernel", 
          "Bray-Curtis", 
          "Jaccard", 
          "Euclidean", 
          "MDS", 
          "DCA", 
          "PLN")),
        selectizeInput(
            "selectedMatrices",
            label = tags$span("Matrices Selected", style = "font-size: 17px;"),
            choices = NULL,
            multiple = TRUE,
            options = list(maxOptions = 10, plugins = list("remove_button"))
          ),
        actionButton("compareButtonCorrPlot", "Compare Similarity Matrices as CorrPlot"),
        actionButton("compareButtonPlot", "Compare Similarity Matrices as Plot"),
        actionButton("compareButtonTable", "Compare Similarity Matrices as Table")
      ),
      mainPanel(
        fluidRow(
          class = "myRow1",
          column(
            width = 6,
            plotOutput("comparisonCorrPlot", width = "200%", height = "800px")
          ),
          tags$head(tags$style("
      .myRow1{height:800px;}"))
        ),
        fluidRow(
          column(
            class = "myRow2",
            width = 6,
            plotOutput("comparisonPlot", width = "200%", height = "1000px")
          ),
          tags$head(tags$style("
      .myRow2{height:1000px;}"))
        ),
        fluidRow(
          column(
            width = 6,
            tableOutput("correlationTable")
          )
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Read the uploaded data file
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath, dec = ",", sep = ";", row.names = 1)
  })
  
  Rdata <- reactive({
    if (input$type == "Raw") {
      data()
    } else if (input$type == "Relative") {
      dataR<-as.data.frame(sapply(data(), prop.table) * 100)
      rownames(dataR)<- rownames(data())
      colnames(dataR)<- colnames(data())
      dataR
    }
    
  })
  
  # Perform data imputation based on the selected method
  imputedData <- reactive({
    if (is.null(Rdata()))
      return(NULL)
    
    if (input$imputation == "None") {
      Rdata()
    } else if (input$imputation == "Constant") {
      Rdata() + input$imputation_value
    } else if (input$imputation == "GBM") {
      cmultRepl(Rdata())
    }
  })
  
  # Perform data transformation based on the selected method
  transformedData <- reactive({
    if (input$transformation == "None") {
      imputedData()
    } else if (input$transformation == "Log") {
      as.data.frame(log(imputedData()))
    } else if (input$transformation == "CLR") {
      as.data.frame(clr(imputedData()))
    } else if (input$transformation == "ILR") {
      as.data.frame(ilr(imputedData()))
    } else if (input$transformation == "ALR") {
      as.data.frame(alr(imputedData()))
    }
  })
  
  # Calculate similarity matrix based on the selected method
  similarityMatrix <- eventReactive(input$generateButton, {
    if (is.null(transformedData()))
      return(NULL)

    transformedDataValue <- transformedData()

    
    method <- input$method
    similarityMatrix <- switch(
      method,
      "Linear Kernel" = K.linear(transformedDataValue),
      "Polynomial Kernel" = {
        if (is.null(input$gamma)) {
          gamma_val <- 1 / data_dims$cols
        } else {
          gamma_val <- input$gamma
        }
        K.Polynomial(transformedDataValue, gamma = gamma_val)
      },
      "Gaussian Kernel" = {
        if (is.null(input$gamma)) {
          gamma_val <- 1 / data_dims$cols
        } else {
          gamma_val <- input$gamma
        }
        K.Gaussian(transformedDataValue, gamma = gamma_val)
      },
      "Arc-Cosine Kernel" = K.AK1_Final(transformedDataValue),
      "Bray-Curtis" = BC_fnc(transformedDataValue),
      "Jaccard" = JC_fnc(transformedDataValue),
      "Euclidean" = Euc_fnc(transformedDataValue),
      "MDS" = MDS_fnc(transformedDataValue),
      "DCA" = DCA_fnc(transformedDataValue),
      "PLN" = PLN_fnc(transformedDataValue),
      stop("Invalid method selected.")
    )
    
    rownames(similarityMatrix)<-colnames(similarityMatrix)
    
    similarityMatrix
  })
  
  # Subset of similarity matrix
  subsetMatrix <- reactive({
    subset(similarityMatrix(), select = c(1:3))
  })
  
  # Generate similarity table
  output$similarityTable <- renderTable({
    if (!is.null(similarityMatrix())) {
      subsetMatrixDF <- subsetMatrix()
      subsetMatrixDF <- as.data.frame(subsetMatrixDF)  # Convert to data frame
      subsetMatrixDF$RowNames <- rownames(subsetMatrixDF)  # Add row names as a column
      rownames(subsetMatrixDF) <- NULL  # Remove row names from the data frame
      subsetMatrixDF <- subsetMatrixDF[, c("RowNames", colnames(subsetMatrixDF))]  # Reorder columns to have row names as the first column
      subsetMatrixDF<-subsetMatrixDF[,-length(subsetMatrixDF)]
      subsetMatrixDF
    }
  })
  
  
  # Generate correlation table
  output$correlationTable <- renderTable({
    req(input$compareButtonTable)
    
    methods <- input$selectedMethods
    numMethods <- length(methods)
    
    if (numMethods >= 2) {
      similarityMatrices <- lapply(methods, function(method) {
        similarityMatrix <- switch(
          method,
          "Linear Kernel" = K.linear(transformedData()),
          "Polynomial Kernel" = {
            if (!exists("input$gamma") || is.null(input$gamma)) {
              gamma_val <- 1 / data_dims$cols
            } else {
              gamma_val <- input$gamma
            }
            K.Polynomial(transformedData(), gamma = gamma_val)
          },
          "Gaussian Kernel" = {
            if (!exists("input$gamma") || is.null(input$gamma)) {
              gamma_val <- 1 / data_dims$cols
            } else {
              gamma_val <- input$gamma
            }
            K.Gaussian(transformedData(), gamma = gamma_val)
          },
          "Arc-Cosine Kernel" = K.AK1_Final(transformedData()),
          "Bray-Curtis" = BC_fnc(transformedData()),
          "Jaccard" = JC_fnc(transformedData()),
          "Euclidean" = Euc_fnc(transformedData()),
          "MDS" = MDS_fnc(transformedData()),
          "DCA" = DCA_fnc(transformedData()),
          "PLN" = PLN_fnc(transformedData()),
          stop("Invalid method selected.")
        )
        
        rownames(similarityMatrix) <- colnames(similarityMatrix)
        similarityMatrix
      })
      
      
      correlationTable <- matrix(nrow = numMethods, ncol = numMethods)
      
      for (i in 1:numMethods) {
        for (j in 1:numMethods) {
          correlationValue <- cor(c(similarityMatrices[[i]]), c(similarityMatrices[[j]]))
          correlationTable[i, j] <- correlationValue
        }
      }
      
      # Perform Mantel test
      pvalues <- matrix(nrow = numMethods, ncol = numMethods)
      
      for (i in 1:numMethods) {
        for (j in 1:numMethods) {
          if (i == j) {
            pvalues[i, j] <- 1  # Set diagonal elements to 1
          } else {
            mantelTest <- mantel(similarityMatrices[[i]], similarityMatrices[[j]], method = "pearson", permutations = 999)
            pvalues[i, j] <- mantelTest$signif
            print(pvalues)
          }
        }
      }
      
      colnames(correlationTable) <- methods
      rownames(correlationTable) <- methods
    }
    
    selectedMatrices <- input$selectedMatrices
    SnumMethods <- length(selectedMatrices)
    
    if (SnumMethods >= 2) {
      correlationTable <- matrix(nrow = SnumMethods, ncol = SnumMethods)
      
      for (i in 1:SnumMethods) {
        for (j in 1:SnumMethods) {
          correlationValue <- cor(
            as.numeric(unlist(values$matrixList[[selectedMatrices[i]]])),
            as.numeric(unlist(values$matrixList[[selectedMatrices[j]]]))
          )
          correlationTable[i, j] <- correlationValue
        }
      }
        # Perform Mantel test
        pvalues <- matrix(nrow = SnumMethods, ncol = SnumMethods)
        
        for (i in 1:SnumMethods) {
          for (j in 1:SnumMethods) {
            if (i == j) {
              pvalues[i, j] <- 1  # Set correlation value to 1 for the same matrix comparison
            } else {
              matrixFF1<- values$matrixList[[selectedMatrices[i]]]
              matrix1 <- matrixFF1[[1]]
              matrixFF2 <- values$matrixList[[selectedMatrices[j]]]
              matrix2 <- matrixFF2[[1]]
              
              # Extract numeric values from the matrices
              matrix1Numeric <- as.numeric(matrix1)
              matrix2Numeric <- as.numeric(matrix2)
              
              # Create new matrices with the original dimensions
              dim(matrix1Numeric) <- dim(matrix1)
              dim(matrix2Numeric) <- dim(matrix2)
              
              mantelResult <- mantel(matrix1Numeric, matrix2Numeric, method = "pearson", permutations = 999)
              pvalues[i, j] <- mantelResult$signif
            }
          }
        }
      
      colnames(correlationTable) <- selectedMatrices
      rownames(correlationTable) <- selectedMatrices
      
    }
    
    # Create a new correlation table with p-values
    correlationTableWithP <- correlationTable
    
    # Add "*" to indicate significance if p-value is below threshold (e.g., 0.05)
    correlationTableWithP[pvalues < 0.05] <- paste0(correlationTable[pvalues < 0.05], "*")
    
    
    correlationTableDF <- as.data.frame(correlationTableWithP)  # Convert to data frame
    correlationTableDF$RowNames <- rownames(correlationTableDF)  # Add row names as a column
    rownames(correlationTableDF) <- NULL  # Remove row names from the data frame
    correlationTableDF <- correlationTableDF[, c("RowNames", colnames(correlationTableDF))]  # Reorder columns to have row names as the first column
    
    correlationTableDF <- correlationTableDF[, !grepl("RowNames.1", colnames(correlationTableDF))]

    correlationTableDF
    
  })
  
  
  # Generate text output
  output$text <- renderText({
    if (!is.null(similarityMatrix())) {
      "Similarity matrix generated."
    }
  })
  
  # Export similarity matrix as CSV file
  output$exportButton <- downloadHandler(
    filename = function() {
      paste("similarity_matrix_", input$method, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(similarityMatrix(), file, row.names = TRUE)
    }
  )
  
  output$comparisonPlot <- renderPlot({
    req(input$compareButtonPlot)
    
    methods <- input$selectedMethods
    selectedMatrices <- input$selectedMatrices
    
    if (length(methods) >= 2) {
      comparisonData <- lapply(methods, function(method) {
        similarityMatrix <- switch(
          method,
          "Linear Kernel" = K.linear(transformedData()),
          "Polynomial Kernel" = {
            if (!exists("input$gamma") || is.null(input$gamma)) {
              gamma_val <- 1 / data_dims$cols
            } else {
              gamma_val <- input$gamma
            }
            K.Polynomial(transformedData(), gamma = gamma_val)
          },
          "Gaussian Kernel" = {
            if (!exists("input$gamma") || is.null(input$gamma)) {
              gamma_val <- 1 / data_dims$cols
            } else {
              gamma_val <- input$gamma
            }
            K.Gaussian(transformedData(), gamma = gamma_val)
          },
          "Arc-Cosine Kernel" = K.AK1_Final(transformedData()),
          "Bray-Curtis" = BC_fnc(transformedData()),
          "Jaccard" = JC_fnc(transformedData()),
          "Euclidean" = Euc_fnc(transformedData()),
          "MDS" = MDS_fnc(transformedData()),
          "DCA" = DCA_fnc(transformedData()),
          "PLN" = PLN_fnc(transformedData()),
          stop("Invalid method selected.")
        )
        
        rownames(similarityMatrix) <- colnames(similarityMatrix)
        
        similarityMatrix
      })
      
      numMethods <- length(methods)
      numPlots <- numMethods * (numMethods - 1) / 2  # Calculate the total number of plots
      
      # Create a grid of plots
      plots <- vector("list", numPlots)
      counter <- 1
      
      for (i in 1:(numMethods - 1)) {
        for (j in (i + 1):numMethods) {
          method1 <- methods[i]
          method2 <- methods[j]
          
          plot_data <- data.frame(
            X = as.vector(comparisonData[[i]]),
            Y = as.vector(comparisonData[[j]])
          )
          
          # Convert data to numeric
          plot_data$X <- as.numeric(plot_data$X)
          plot_data$Y <- as.numeric(plot_data$Y)
          
          # Remove rows with non-numeric values
          plot_data <- plot_data[complete.cases(plot_data), ]
          
          if (nrow(plot_data) > 0) {
            p <- ggplot(plot_data, aes(x = X, y = Y)) +
              geom_hex() +
              labs(x = method1, y = method2) +
              theme_bw() +
              theme(plot.margin = margin(5, 5, 5, 5, "pt"),
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white"),
                    aspect.ratio = 1,
                    plot.title = element_text(size = 14),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))+
              scale_fill_gradient(low = "lightblue", high = "darkblue")
            
            plots[[counter]] <- p
            counter <- counter + 1
          }
        }
      }
      
      # Arrange and display the plots
      grid.arrange(grobs = plots, ncol = 2)
      
      # Remove NULL elements
      
      plots <- plots[!sapply(plots, is.null)]
      
      # Print the grid of plots
      gridExtra::grid.arrange(grobs = plots, ncol = 3)
    }
      


    if (length(selectedMatrices) >= 2) {
      numMatrices <- length(selectedMatrices)
      numPlots <- numMatrices * (numMatrices - 1) / 2  # Calculate the total number of plots
      
      # Create a grid of plots
      plots <- vector("list", numPlots)
      counter <- 1
      
      
      for (i in 1:(numMatrices - 1)) {
        for (j in (i + 1):numMatrices) {
          method1 <- selectedMatrices[i]
          method2 <- selectedMatrices[j]
          
          plot_data <- data.frame(
            X = as.vector(as.numeric(unlist(values$matrixList[[selectedMatrices[i]]]))),
            Y = as.vector(as.numeric(unlist(values$matrixList[[selectedMatrices[j]]])))
          )
          
          # Convert data to numeric
          plot_data$X <- as.numeric(plot_data$X)
          plot_data$Y <- as.numeric(plot_data$Y)
          
          # Remove rows with non-numeric values
          plot_data <- plot_data[complete.cases(plot_data), ]
          
          if (nrow(plot_data) > 0) {
            p <- ggplot(plot_data, aes(x = X, y = Y)) +
              geom_hex() +
              labs(x = method1, y = method2) +
              theme_bw() +
              theme(plot.margin = margin(5, 5, 5, 5, "pt"),
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white"),
                    aspect.ratio = 1,
                    plot.title = element_text(size = 14),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))+
              scale_fill_gradient(low = "lightblue", high = "darkblue")
            
            plots[[counter]] <- p
            counter <- counter + 1
          }
        }
      }
      
      # Remove NULL elements
      plots <- plots[!sapply(plots, is.null)]
      
      if (length(plots) > 0) {
        # Print the grid of plots
        gridExtra::grid.arrange(grobs = plots, ncol = 3)
      } else {
        # Handle case when there are no valid plots
        print("No valid plots to display.")
      }
    }
  })

  # Comparison of similarity matrices
  output$comparisonCorrPlot <- renderPlot({
    req(input$compareButtonCorrPlot)
    
    methods <- input$selectedMethods
    
    if (length(methods) >= 1) {
    
      
      plots <- lapply(methods, function(method) {
        similarityMatrix <- switch(
          method,
          "Linear Kernel" = K.linear(transformedData()),
          "Polynomial Kernel" = {
            if (!exists("input$gamma") || is.null(input$gamma)) {
              gamma_val <- 1 / data_dims$cols
            } else {
              gamma_val <- input$gamma
            }
            K.Polynomial(transformedData(), gamma = gamma_val)
          },
          "Gaussian Kernel" = {
            if (!exists("input$gamma") || is.null(input$gamma)) {
              gamma_val <- 1 / data_dims$cols
            } else {
              gamma_val <- input$gamma
            }
            K.Gaussian(transformedData(), gamma = gamma_val)
          },
          "Arc-Cosine Kernel" = K.AK1_Final(transformedData()),
          "Bray-Curtis" = BC_fnc(transformedData()),
          "Jaccard" = JC_fnc(transformedData()),
          "Euclidean" = Euc_fnc(transformedData()),
          "MDS" = MDS_fnc(transformedData()),
          "DCA" = DCA_fnc(transformedData()),
          "PLN" = PLN_fnc(transformedData()),
          stop("Invalid method selected.")
        )
        
        correlationMatrix <- cor(similarityMatrix)
        melted_data <- melt(correlationMatrix)
        ggplot(data = melted_data, aes(x = as.numeric(Var1), y = as.numeric(Var2), fill = value)) +
          geom_tile() +
          labs(x = "", y = "") +
          theme_bw() +
          scale_fill_gradientn(colours = c("blue", "white", "red"),
                               values = scales::rescale(c(0, 0.45, 0.5, 0.55, 1)))+
          ggtitle(method)+  # Add the method name as the plot title 
          theme(plot.margin = margin(5, 5, 5, 5, "pt"),
                plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white"),
                aspect.ratio = 1,
                plot.title = element_text(size = 14),
                axis.text = element_text(size = 10),
                axis.title = element_text(size = 12))
      })
      
      grid <- do.call(grid.arrange, c(plots, ncol = 3))  # Adjust the number of columns as desired
      
      # Display the grid
      grid
    }
    
    selectedMatrices <- input$selectedMatrices
    
    if (length(selectedMatrices) >= 1) {
      
      
      plots <- lapply(selectedMatrices, function(matrixName) {
        h <- unlist(values$matrixList[[matrixName]])
        p <- matrix(h,nrow = sqrt(length(h)),ncol = sqrt(length(h)))
        correlationMatrix <- cor(p)
        
        melted_data <- melt(correlationMatrix)
        
        ggplot(data = melted_data, aes(x = as.numeric(Var1), y = as.numeric(Var2), fill = value)) +
          geom_tile() +
          labs(x = "", y = "") +
          theme_bw() +
          scale_fill_gradientn(colours = c("blue", "white", "red"),
                               values = scales::rescale(c(0, 0.45, 0.5, 0.55, 1)))+
          ggtitle(matrixName)+  # Add the method name as the plot title 
          theme(plot.margin = margin(5, 5, 5, 5, "pt"),
                plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white"),
                aspect.ratio = 1,
                plot.title = element_text(size = 14),
                axis.text = element_text(size = 10),
                axis.title = element_text(size = 12))
      })
      
      grid <- do.call(grid.arrange, c(plots, ncol = 3))  # Adjust the number of columns as desired
      
      # Display the grid
      grid
    }
  })
  
  observeEvent(input$updateListButton, {
    if (!is.null(similarityMatrix())) {
      tmpvalues$matrixList[[input$matrixName]] <- list(similarityMatrix())
      values$matrixList<-NULL
      values$matrixList <- c(values$matrixList, tmpvalues$matrixList)
    }
  })
  
  observeEvent(values$matrixList, {
    updateSelectizeInput(
      session,
      "selectedMatrices",
      choices = names(values$matrixList)
    )
  })
  
  # Reactive value to store the dimensions of uploaded data
  data_dims <- reactiveValues(rows = 0, cols = 0)

  # Update data dimensions when file is uploaded
  observeEvent(input$datafile, {
    req(input$datafile)
    data <- read.csv(input$datafile$datapath)
    data_dims$rows <- nrow(data())
    data_dims$cols <- ncol(data())

  })
  
  # Set the initial value of gamma when generateGamma button is pressed
  observeEvent(input$generateGamma, {
    # Set the initial value of gamma
    updateNumericInput(session, "gamma", value = 1/data_dims$cols)
  })
  
  # Print matrix list
  output$matrixListOutput <- renderPrint({
    values$matrixList
    # names(values$matrixList)
  })

  # Print matrix list
  output$NmatrixListOutput <- renderPrint({
    names(values$matrixList)
  })
  
  
  # Print matrix list
  output$GmatrixListOutput <- renderPrint({
   str(values$matrixList)
  })

  
  # Toggle visibility of linear kernel justification
  observeEvent(input$toggleLinearButton, {
    shinyjs::toggle("linearJustification")
  })
  
  # Toggle visibility of polynomial kernel justification
  observeEvent(input$togglePolynomialButton, {
    shinyjs::toggle("polynomialJustification")
  })
  
  # Toggle visibility of gaussian kernel justification
  observeEvent(input$toggleGaussianButton, {
    shinyjs::toggle("gaussianJustification")
  })
  
  # Toggle visibility of arc-cosine kernel justification
  observeEvent(input$toggleArcCosineButton, {
    shinyjs::toggle("arcCosineJustification")
  })
  
  # Toggle visibility of PLN justification
  observeEvent(input$togglePLNButton, {
    shinyjs::toggle("PLNJustification")
  })
  
  # Toggle visibility of Jaccard justification
  observeEvent(input$toggleJaccardButton, {
    shinyjs::toggle("jaccardJustification")
  })
  
  # Toggle visibility of Bray-Curtis justification
  observeEvent(input$toggleBCButton, {
    shinyjs::toggle("bcJustification")
  })
  
  # Toggle visibility of Aitchison justification
  observeEvent(input$toggleAitButton, {
    shinyjs::toggle("aitJustification")
  })
  
  # Toggle visibility of MDS justification
  observeEvent(input$toggleMDSButton, {
    shinyjs::toggle("mdsJustification")
  })
  
  # Toggle visibility of DCA justification
  observeEvent(input$toggleDCAButton, {
    shinyjs::toggle("dcaJustification")
  })
}

# Run the application
shinyApp(ui = ui, server = server)