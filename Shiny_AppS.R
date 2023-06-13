# Install required packages if not already installed
install.packages(c("shiny", "PLNmodels", "shinyjs","DT","corrplot", "ape", "stats", "xtable", "plotly", "vegan", "tidyverse", "imputeTS", "imputeMissings", "compositions", "zCompositions"))

library(shiny)
library(ape)
library(stats)
library(plotly)
library(vegan)
library(tidyverse)
library(imputeTS)
library(imputeMissings)
library(compositions)
library(DT)
library(xtable)
library(zCompositions)
library(corrplot)
library(shinyjs)
library(PLNmodels)
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
            tags$p("The linear kernel is a suitable choice when the microbial abundance data exhibits a linear pattern. It works well when the abundances change proportionally, allowing us to assess the heritability of relative abundance variations in a straightforward manner.",
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
            tags$p("The polynomial kernel is preferable when the microbial abundance data shows nonlinear relationships. It allows us to capture intricate interactions and higher-order relationships among microbial taxa, enabling a more accurate assessment of heritability values.",
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
            tags$p("The Gaussian kernel is a suitable choice when the microbial abundance data exhibits a Gaussian-like distribution or when there are local interactions between microbial taxa. It can capture subtle variations and provide a more nuanced assessment of heritability by giving more importance to samples with similar microbial composition.",
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
            p("The arc-cosine kernel is preferred when the microbial abundance data represents relative abundances or compositional data. It effectively captures the compositional nature of the data by handling the inherent constraints of proportions. This kernel is suitable for assessing heritability values in cases where the relative changes in abundance are of interest.",
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
            tags$p("The Poisson Log Normal (PLN) method can be used to analyze microbial abundance data, whether it is relative or not, due to its ability to address two key characteristics of such data: count-based nature and overdispersion.",
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
            p("The Jaccard distance is commonly used to analyze microbial abundance data, whether it's relative or absolute, due to its simplicity and robustness. It compares the presence or absence of microbial species between samples, disregarding abundance levels, making it suitable for diverse datasets. By utilizing the Jaccard distance, researchers can assess similarities, explore community structure, and investigate factors like heritability that influence microbial abundance patterns.",
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
            p("The Bray-Curtis distance is a valuable metric for treating and analyzing microbial abundance data due to its ability to capture both the presence/absence and relative abundance of microbial species. It provides a quantitative measure of compositional dissimilarity between samples, allowing for meaningful comparisons and clustering analysis. By utilizing the Bray-Curtis distance, researchers can gain insights into the genetic influence and heritability of microbial communities, aiding in the understanding of their dynamics and ecological relationships.",
              id = "bcJustification",
              style = "font-size:20px;")
          ),
          actionButton("toggleBCButton", "Toggle Bray-Curtis Justification")
        )
      ),
      fluidRow(
        column(
          width = 10,
          tags$h2("Aitchison",
                  style = "font-size:26px; text-decoration:underline;"),
          shinyjs::hidden(
            p("The Aitchison distance should be used to treat and analyze microbial abundance data for three key reasons. First, microbial abundance data is compositional, meaning the abundance of one species is dependent on others, and the Aitchison distance accounts for this constraint. Second, traditional distance measures like Euclidean distance are unsuitable for compositional data as they ignore the closure constraint. Finally, the Aitchison distance enables meaningful comparisons and dissimilarity analysis between microbial samples, allowing for the exploration of genetic factors and heritability in microbial abundance variations.",
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
            p("MDS (Multidimensional Scaling) should be used to treat and analyze microbial abundance data because it allows for the visualization of similarities or dissimilarities between samples based on their microbial composition, providing a comprehensive overview of the dataset. MDS reduces the dimensionality of the data while preserving pairwise distances, enabling the identification of patterns, clusters, and gradients in the microbial communities. This approach helps uncover potential factors driving microbial composition differences and facilitates further analysis, including the assessment of heritability values.",
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
            p("DCA (Detrended Correspondence Analysis) is valuable for analyzing microbial abundance data due to its ability to capture complex nonlinear relationships or gradients in the dataset. By decomposing the variance in the data, DCA reveals the main trends or gradients present in the microbial communities, allowing for the interpretation of underlying patterns. DCA provides a visual representation of the structure of microbial composition, helping identify associations between taxa and specific environmental or biological factors and aiding in the assessment of heritability values.",
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
        textInput("matrixName", label = "Matrix Name")
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
        actionButton("compareButtonPlot", "Compare Similarity Matrices as Plot"),
        actionButton("compareButtonTable", "Compare Similarity Matrices as Table"),
        actionButton("MantelTest", "Mantel Test")
      ),
      mainPanel(
        plotOutput("comparisonPlot"),
        tableOutput("correlationTable"),
        plotOutput("MantelTest")
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
      "Polynomial Kernel" = K.Polynomial(transformedDataValue,gamma=input$gamma),
      "Gaussian Kernel" = K.Gaussian(transformedDataValue,gamma=input$gamma),
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
    
    if(input$method=="Linear Kernel" || input$method=="Polynomial Kernel" || input$method=="Gaussian Kernel" || input$method=="Arc-Cosine Kernel" || input$method=="MDS" || input$method=="DCA"){
      # Assuming similarityMatrix is a numeric matrix
      diag_vals <- diag(similarityMatrix)
      
      # Ensure diagonal values are non-negative
      diag_vals <- pmax(diag_vals, 0)
      
      # Ensure similarityMatrix values are non-negative
      similarityMatrix <- pmax(similarityMatrix, 0)
      
      # Calculate the outer product of the diagonal values
      outer_product <- outer(diag_vals, diag_vals, pmax)
      
      # Perform normalization by dividing similarityMatrix by the outer product
      similarityMatrix <- similarityMatrix / outer_product
      
      similarityMatrix <- ifelse(similarityMatrix<0.0000001, 0, (similarityMatrix - min(similarityMatrix)) / (max(similarityMatrix) - min(similarityMatrix)))
      
    }else if (input$method=="Jaccard" || input$method=="Bray-Curtis") {
      
      similarityMatrix <- similarityMatrix
      
    }else if (input$method=="Euclidean"){
      
      # Calculate the interquartile range for each column
      iqr <- apply(similarityMatrix, 2, IQR)
      
      # Define a threshold to determine outliers (e.g., 1.5 times the IQR)
      threshold <- 1.5
      
      # Identify the outliers in each column
      outliers <- similarityMatrix > (quantile(similarityMatrix, 0.75) + threshold * iqr) | similarityMatrix < (quantile(similarityMatrix, 0.25) - threshold * iqr)
      
      # Replace the outliers with appropriate values (e.g., the maximum distance)
      max_distance <- max(similarityMatrix[!outliers], na.rm = TRUE)
      similarityMatrix[outliers] <- max_distance
      similarityMatrix <- (similarityMatrix - min(similarityMatrix)) / (max(similarityMatrix) - min(similarityMatrix))
      
    }else if(input$method=="PLN")
    {
      similarityMatrix <- ifelse(similarityMatrix<0.0000001, 0, (similarityMatrix - min(similarityMatrix)) / (max(similarityMatrix) - min(similarityMatrix)))
    }    
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
          "Polynomial Kernel" = K.Polynomial(transformedData(),gamma=input$gamma),
          "Gaussian Kernel" = K.Gaussian(transformedData(),gamma=input$gamma),
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
      
      colnames(correlationTable) <- methods
      rownames(correlationTable) <- methods
      correlationTableDF <- as.data.frame(correlationTable)  # Convert to data frame
      correlationTableDF$RowNames <- rownames(correlationTableDF)  # Add row names as a column
      rownames(correlationTableDF) <- NULL  # Remove row names from the data frame
      correlationTableDF <- correlationTableDF[, c("RowNames", colnames(correlationTableDF))]  # Reorder columns to have row names as the first column
      correlationTableDF<-correlationTableDF[,-length(correlationTableDF)]
      correlationTableDF
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
      
      colnames(correlationTable) <- selectedMatrices
      rownames(correlationTable) <- selectedMatrices
      correlationTableDF <- as.data.frame(correlationTable)
      correlationTableDF$RowNames <- rownames(correlationTableDF)
      rownames(correlationTableDF) <- NULL
      correlationTableDF <- correlationTableDF[, c("RowNames", colnames(correlationTableDF))]
      correlationTableDF <- correlationTableDF[, -length(correlationTableDF)]
    }
    
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
  
  # Comparison of similarity matrices
  output$comparisonPlot <- renderPlot({
    req(input$compareButtonPlot)
    
    methods <- input$selectedMethods
    
    if (length(methods) >= 2) {
      comparisonData <- lapply(methods, function(method) {
        similarityMatrix <- switch(
          method,
          "Linear Kernel" = K.linear(transformedData()),
          "Polynomial Kernel" = K.Polynomial(transformedData(),gamma=input$gamma),
          "Gaussian Kernel" = K.Gaussian(transformedData(),gamma=input$gamma),
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
      
      par(mfrow = c(length(methods), length(methods)))
      
      for (i in 1:length(methods)) {
        for (j in 1:length(methods)) {
          if (i != j) {
            # Create scatterplot
            plot(as.vector(comparisonData[[i]]), as.vector(comparisonData[[j]]),
                 col = "blue", pch = 16,
                 xlab = methods[i], ylab = methods[j],
                 main = paste("Comparison:", methods[i], "vs", methods[j]))
          }
        }
      }
    }
    
    selectedMatrices <- input$selectedMatrices
    
    if (length(selectedMatrices) >= 2) {
            
      
      par(mfrow = c(length(selectedMatrices), length(selectedMatrices)))
      
      for (i in 1:length(selectedMatrices)) {
        for (j in 1:length(selectedMatrices)) {
          if (i != j) {
                  # Create scatterplot
                  plot(as.vector(as.numeric(unlist(values$matrixList[[selectedMatrices[i]]]))), as.vector(as.numeric(unlist(values$matrixList[[selectedMatrices[i]]]))),
                       col = "blue", pch = 16,
                       xlab = selectedMatrices[i], ylab = selectedMatrices[j],
                       main = paste("Comparison:", selectedMatrices[i], "vs", selectedMatrices[j]))
          }
        }
      }
    }
  })
  
  # Generate correlation table with Mantel test results and scatter plot
  output$MantelTest <- renderPlot({
    req(input$MantelTest)
    
    methods <- input$selectedMethods
    numMethods <- length(methods)
    
    transformedDataValue <- transformedData()
    
    if (numMethods >= 2) {
      similarityMatrices <- lapply(methods, function(method) {
        similarityMatrix <- switch(
          method,
          "Linear Kernel" = K.linear(transformedDataValue),
          "Polynomial Kernel" = K.Polynomial(transformedDataValue,gamma=input$gamma),
          "Gaussian Kernel" = K.Gaussian(transformedDataValue,gamma=input$gamma),
          "Arc-Cosine Kernel" = K.AK1_Final(transformedDataValue),
          "Bray-Curtis" = BC_fnc(transformedDataValue),
          "Jaccard" = JC_fnc(transformedDataValue),
          "Euclidean" = Euc_fnc(transformedDataValue),
          "MDS" = MDS_fnc(transformedDataValue),
          "DCA" = DCA_fnc(transformedDataValue),
          "PLN" = PLN_fnc(transformedDataValue),
          stop("Invalid method selected.")
        )
        
        rownames(similarityMatrix) <- colnames(similarityMatrix)
        similarityMatrix
      })
      
      MantelTest<- matrix(nrow = numMethods, ncol = numMethods)
      
      for (i in 1:numMethods) {
        for (j in 1:numMethods) {
          if (i == j) {
            correlationValue <- 1  # Set correlation value to 1 for same matrix comparison
          } else {
            mantelResult <- mantel(similarityMatrices[[i]], similarityMatrices[[j]], method = "pearson", permutations = 999)
            correlationValue <- mantelResult$statistic
          }
          
          MantelTest[i, j] <- correlationValue
        }
      }
      
      colnames(MantelTest) <- methods
      rownames(MantelTest) <- methods
      
      # Normalize correlation values to range [0, 1]
      normalizedCorrelation <- scale(c(MantelTest), center = FALSE, scale = max(MantelTest))
      MantelTest <- matrix(normalizedCorrelation, nrow = numMethods, ncol = numMethods, byrow = TRUE)
      
      # Create scatter plot
      correlationValues <- c(MantelTest)
      methodPairs <- expand.grid(methods, methods)
      
      plot(1:numMethods, 1:numMethods, 
           pch = 16, col = "blue", cex = correlationValues * 3,
           xlim = c(0.5, numMethods + 0.5),
           ylim = c(0.5, numMethods + 0.5),
           xlab = "Method 1", ylab = "Method 2", main = "Mantel Test Results",
           xaxt = "n", yaxt = "n")
      
      # Add method names as labels
      axis(1, at = 1:numMethods, labels = methods)
      axis(2, at = 1:numMethods, labels = methods, las = 2)
      
      # Add correlation values as labels
      text(methodPairs$Var1, methodPairs$Var2, 
           labels = round(correlationValues, 2),
           pos = 3, cex = 0.8)
    }
    
    
    # selectedMatrices <- input$selectedMatrices
    # SnumMethods <- length(selectedMatrices)
    # 
    # if (SnumMethods >= 2) {
    #   
    #   MantelTest<- matrix(nrow = SnumMethods, ncol = SnumMethods)
    #   
    #   for (i in 1:SnumMethods) {
    #     for (j in 1:SnumMethods) {
    #       if (i == j) {
    #         correlationValue <- 1  # Set correlation value to 1 for same matrix comparison
    #       } else {
    #         mantelResult <- mantel(as.numeric(unlist(values$matrixList[[selectedMatrices[i]]])),
    #                                as.numeric(unlist(values$matrixList[[selectedMatrices[j]]])), method = "pearson", permutations = 999)
    #         correlationValue <- mantelResult$statistic
    #       }
    #       
    #       MantelTest[i, j] <- correlationValue
    #     }
    #   }
    #   
    #   colnames(MantelTest) <- selectedMatrices
    #   rownames(MantelTest) <- selectedMatrices
    #   
    #   # Normalize correlation values to range [0, 1]
    #   normalizedCorrelation <- scale(c(MantelTest), center = FALSE, scale = max(MantelTest))
    #   MantelTest <- matrix(normalizedCorrelation, nrow = SnumMethods, ncol = SnumMethods, byrow = TRUE)
    #   
    #   # Create scatter plot
    #   correlationValues <- c(MantelTest)
    #   methodPairs <- expand.grid(selectedMatrices, selectedMatrices)
    #   
    #   plot(1:SnumMethods, 1:SnumMethods, 
    #        pch = 16, col = "blue", cex = correlationValues * 3,
    #        xlim = c(0.5, SnumMethods + 0.5),
    #        ylim = c(0.5, SnumMethods + 0.5),
    #        xlab = "Method 1", ylab = "Method 2", main = "Mantel Test Results",
    #        xaxt = "n", yaxt = "n")
    #   
    #   # Add method names as labels
    #   axis(1, at = 1:SnumMethods, labels = selectedMatrices)
    #   axis(2, at = 1:SnumMethods, labels = selectedMatrices, las = 2)
    #   
    #   # Add correlation values as labels
    #   text(methodPairs$Var1, methodPairs$Var2, 
    #        labels = round(correlationValues, 2),
    #        pos = 3, cex = 0.8)
    # }
    
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