library("shiny")
library("shinythemes")


uploadSize <- round(unlist(options("shiny.maxRequestSize"))/(1024^2))

shinyUI(pageWithSidebar(
  
  headerPanel("HABase: Web-based Spectra Analysis"),
  
  sidebarPanel(
    shinythemes::themeSelector(),
    conditionalPanel(condition='input.tab == "home"',
                     p(strong("Input")),
                     radioButtons(inputId="file", label="Dataset:",
                                  choices=c("Fiedler", "Upload Dataset"),
                                  selected="Fiedler"),
                     conditionalPanel(condition='input.file != "Fiedler"',
                                      fileInput(inputId="uploadFile",
                                                label=paste0("Choose Dataset ",
                                                             "(max: ", uploadSize,
                                                             " MB; supported filetypes: ",
                                                             "zip, tar.gz, tar.bz2, csv):"),
                                                accept=c("application/zip", "application/x-gtar",
                                                         "text/csv", "text/plain")))),
    
    conditionalPanel(condition='input.tab == "smoothing"',
                     p(strong("Preprocessing")),
                     selectInput(inputId="vs", label="Variance:",
                                 choices=c("sqrt", "log"), selected="sqrt"),
                     selectInput(inputId="sm", label="Smoothing:",
                                 choices=c("Savitzky-Golay"="SavitzkyGolay",
                                           "Moving-Average"="MovingAverage"),
                                 selected="Savitzky-Golay"),
                     sliderInput(inputId="smHws", label="Half-Window Size:",
                                 min=1, max=100, value=10)),
    
    
    conditionalPanel(condition='input.tab == "baseline"',
                     p(strong("Baseline Correction")),
                     selectInput(inputId="bc", label="Baseline:",
                                 choices=c("SNIP"="SNIP", "TopHat"="TopHat"),
                                 selected="SNIP"),
                     conditionalPanel(condition='input.bc != "ConvexHull"',
                                      sliderInput(inputId="bcHws", label="Half-Window Size:",
                                                  min=1, max=500, value=100)),
                     tags$hr(),
                     checkboxInput(inputId="bcBC", label="Show Corrected Spectrum",
                                   value=TRUE),
                     checkboxInput(inputId="bcUS", label="Show Uncorrected Spectrum",
                                   value=TRUE)),
                     
    
    conditionalPanel(condition='input.tab == "peaks"',
                     p(strong("Peak Detection")),
                     sliderInput(inputId="pdHws", label="Half-Window Size:",
                                 min=1, max=500, value=20),
                     sliderInput(inputId="pdSNR", label="SNR (signal-to-noise-ration):",
                                 min=1, max=100, value=2),
                     selectInput(inputId="pdNoise", label="Noise Estimator:",
                                 choices=c("MAD", "Friedman's SuperSmoother"="SuperSmoother"),
                                 selected="MAD"),
                     tags$hr(),
                     p(strong("Label Peaks")),
                     sliderInput(inputId="plTopN", label="Label Top N Peaks:",
                                 min=0, max=100, value=5),
                     checkboxInput(inputId="plRotate", label="Rotate Peak Labels",
                                   value=TRUE)
    ),
    
    conditionalPanel(condition='input.tab == "datadownload"',
                    #downloadButton("downloadData", label = "Download")
                    print("Click on button to download"),
                    br(),br(),
                    print("Once the Download button is clicked your spectra data will be uploaded to Database")
    ),
    conditionalPanel(condition='input.tab == "Matching"',
                     #downloadButton("downloadData", label = "Download")
                     print("Please wait while the matching is performed...")
    ),
    
    conditionalPanel(condition='input.tab == "clusterImage"',
                     p(strong("Clustering")),
                     selectInput(inputId="cm1", label="Distance Method:",
                                 choices=c("euclidean"="euclidean"),
                                 selected="euclidean"),
                     selectInput(inputId="cm2", label="Hierarachical Method:",
                                 choices=c("ward.D2"="ward.D2","ward.D"="ward.D"),
                                 selected="ward.D2"),
                     print("Please be patient, this will take a while")
    ),
    
    
    
    br(),
    
    ## zoom
    conditionalPanel(condition="input.tab != 'home' && input.tab != 'clusterImage' && input.tab != 'Matching' && input.tab != 'datadownload'",
                     wellPanel(
                       p(strong("Zoom:")),
                       br(),
                       uiOutput("ySlider"),
                       br(),
                       uiOutput("xSlider"),
                       p(strong("Plot:")),
                       uiOutput("getSpectra")))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Home", verbatimTextOutput("spectraFilterSummary"),value="home"),
      # tabPanel("Raw", uiOutput("rawPlot"), value="raw"),
      tabPanel("Smoothing", uiOutput("sPlots"), value="smoothing"),
      tabPanel("Baseline", uiOutput("bPlots"), value="baseline"),
      tabPanel("Peaks", uiOutput("pPlots"), value="peaks"),
      tabPanel("Clustering", imageOutput("clusterImage"), value="clusterImage"),
      tabPanel("Matching",uiOutput("tabb1"),downloadButton("downloadData1",label = "Save Results"),br(),br(),uiOutput("text1"),value="Matching"),
      tabPanel("FeatureMatrix", br(),br(), br(),br(),downloadButton("downloadData", label = "Download FeatureMatrix"),value="datadownload"),
      id="tab")
  )
))
