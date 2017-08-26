library("shiny")
library("MALDIquant")
library("MALDIquantForeign")
library("pvclust")
library("caret")
library("ggplot2")
library("aws.s3")
library("lsa")
library("SnowballC")
library("dplyr")

options(shiny.maxRequestSize=200*1024^2)

data("fiedler2009subset")

s3BucketName <- "hunterhawks"
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAJRVLXNHWIV3HRC6A",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-east-2")

yLimFunction <- function(lim, fun) {
  fun <- match.fun(fun)
  lim <- fun(lim)
  lim[is.infinite(lim)] <- 0
  return(lim)
}

createList <- function(x) {
  fn <- MALDIquantForeign:::.composeFilename(x, fileExtension="")
  fn <- sub(pattern="[[:punct:]]+$", replacement="", x=fn)
  return(setNames(x, fn))
}

massLim <- function(x) {
  mL <- range(unlist(lapply(x, function(y)range(mass(y)))))
  mL <- trunc(mL)+c(0, 1)
  return(mL)
}

intensityLim <- function(x) {
  return(c(0, ceiling(max(unlist(lapply(x, function(y)max(intensity(y))))))))
}

s <<- list()
s <<- createList(fiedler2009subset)
xlim <- massLim(s)
ylim <- intensityLim(s)

shinyServer(function(input, output, session) {
  
  output$getSpectra <- renderUI({
    input$file
    input$uploadFile
    
    selectInput(inputId="selectdata",
                label="Plot Spectrum:",
                choices=names(s), selected=names(s)[1], multiple=TRUE)
  })
  
  output$xSlider <- renderUI({
    input$file
    input$uploadFile
    
    xlim <- massLim(s)
    sliderInput(inputId="xlim", label="Mass Range:",
                min=xlim[1], max=xlim[2], value=xlim,
                ticks=TRUE)
  })
  
  output$ySlider <- renderUI({
    input$file
    input$uploadFile
    
    ylim <- intensityLim(s)
    sliderInput(inputId="ylim", label="Intensity Range:",
                min=ylim[1], max=ylim[2],
                value=ylim, ticks=TRUE)
  })
  
  dataset <- reactive({
    if (is.null(input$uploadFile) || input$file == "fiedler2009subset") {
      s <- fiedler2009subset
    } else {
      originalSize <- input$uploadFile$size
      uploadedSize <- file.info(input$uploadFile$datapath)$size
      filename <- file.path(dirname(input$uploadFile$datapath), input$uploadFile$name)
      file.rename(input$uploadFile$datapath, filename)
      s <- import(filename)
      file.rename(filename, input$uploadFile$datapath)
    }
    
    s <<- createList(s)
    return(s)
  })
  
  spect <- reactive({
    dataset()
    
    if (is.null(input$selectdata)) {
      return(s[[1]])
    } else {
      return(s[input$selectdata])
    }
  })
  
  sqrtSpectra <- reactive({
    if (is.null(input$vs)) {
      method <- "sqrt"
    } else {
      method <- input$vs
    }
    return(transformIntensity(spect(), method=method))
  })
  
  smooth <- reactive({
    if (is.null(input$sm)) {
      method <- "SavitzkyGolay"
      hws <- 10
    } else {
      method <- input$sm
      hws <- input$smHws
    }
    return(smoothIntensity(sqrtSpectra(), method=method, halfWindowSize=hws))
  })
  
  baseline <- reactive({
    if (is.null(input$bc)) {
      method <- "SNIP"
      hws <- 100
    } else {
      method <- input$bc
      hws <- input$bcHws
    }
    
    return(lapply(smooth(), function(y) {
      bl <- estimateBaseline(y, method=method, hws)
      intensity(y) <- intensity(y)-bl[, 2]
      return(y)
    }))
  })
  
  dp <- reactive({
    return(detectPeaks(baseline(), method=input$pdNoise,
                       halfWindowSize=input$pdHws, SNR=input$pdSNR))
  })
  
  
  spectraFilter <- reactive({
    s <- dataset()
    
    emptySpect <- sapply(s, isEmpty)
    regularSpect <- sapply(s, isRegular)
    lengthSpect <- sapply(s, length)
    
    ifEmpty <- any(emptySpect)
    ifIrregular <- any(!regularSpect)
    ifLenght <- any(length(s[[1]]) != lengthSpect)
    
    return(list(ifEmpty=ifEmpty, ifIrregular=ifIrregular,
                ifLenght=ifLenght,
                table=data.frame(empty=emptySpect, irregular=!regularSpect,
                                 length=lengthSpect)))
  })
  
  output$spectraFilterSummary <- renderPrint({
    cat("Dataset Filter", "\n","\n","\n" )
    cat("Spectrum irregular? : ", spectraFilter()$ifIrregular, "\n","\n")
    cat("Spectrum a different length? : ",
        spectraFilter()$ifLenght, "\n","\n")
    cat("Spectrum Empty? : ", spectraFilter()$ifEmpty, "\n","\n")
  })
  
  listPlot <- function(x, additonalPlotFunction=NULL, prefix, xlim, ylim,
                       type="l") {
    plotOutputList <- lapply(seq_along(s), function(i) {
      plotname <- paste0(prefix, "plot", i)
      plotOutput(plotname)
    })
    
    for (i in seq_along(x)) {
      local({
        my_i <- i
        my_xlim <- xlim
        my_ylim <- ylim
        my_type <- type
        my_x <- x
        plotname <- paste0(prefix, "plot", my_i, sep="")
        output[[plotname]] <<- renderPlot({
          plot(my_x[[my_i]], xlim=my_xlim, ylim=my_ylim, type=my_type)
          if (!is.null(additonalPlotFunction)) {
            fun <- as.function(additonalPlotFunction)
            fun(my_x[[my_i]])
          }
        })
      })
    }
    
    return(plotOutputList)
  }

  
  output$sPlots <- renderUI({
    
    dat <- data.frame(x = numeric(0), y = numeric(0))
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Smoothing", value = 0)
    n <- 10
    
    for (i in 1:n) {
      dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
      progress$inc(1/n, detail = paste("In Progress"))
      Sys.sleep(0.1)
    }
    
    do.call(tagList,
            listPlot(sqrtSpectra(), function(y) {
              lines(smoothIntensity(y, method=input$sm,
                                    halfWindowSize=input$smHws), col=2)},
              prefix="smoothed", xlim=input$xlim,
              ylim=yLimFunction(input$ylim, input$vs)))
  })
  
  output$bPlots <- renderUI({
    
    dat <- data.frame(x = numeric(0), y = numeric(0))
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Baseline Correction", value = 0)
    n <- 10
    
    for (i in 1:n) {
      dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
      progress$inc(1/n, detail = paste("In Progress"))
      Sys.sleep(0.1)
    }
    
    do.call(tagList,
            listPlot(smooth(), function(y) {
              bl <- estimateBaseline(y, method=input$bc, input$bcHws)
              if (input$bcUS) {
                lines(bl, col=2, lwd=2)
              }
              if (input$bcBC) {
                lines(mass(y), intensity(y)-bl[,2], col=4)
              }
            },
            prefix="baseline", xlim=input$xlim,
            ylim=yLimFunction(input$ylim, input$vs),
            type=ifelse(input$bcUS, "l", "n")))
  })
  
  output$pPlots <- renderUI({
    
    dat <- data.frame(x = numeric(0), y = numeric(0))
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Generating Peaks", value = 0)
    n <- 10
    
    for (i in 1:n) {
      dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
      progress$inc(1/n, detail = paste("In Progress"))
      Sys.sleep(0.1)
    }
    
    do.call(tagList,
            listPlot(baseline(), function(y) {
              n <- estimateNoise(y, method=input$pdNoise)
              lines(n[, 1], input$pdSNR*n[, 2], col=2, lwd=2)
              p <- detectPeaks(y, method=input$pdNoise,
                               halfWindowSize=input$pdHws,
                               SNR=input$pdSNR)
              points(p, col=4, pch=4, lwd=2)
              
              if (input$plTopN) {
                top <- sort(intensity(p), decreasing=TRUE,
                            index.return=TRUE,
                            method="quick")$ix[1:input$plTopN]
                if (input$plRotate) {
                  srt <- 90
                  adj <- c(-0.1, 0.5)
                } else {
                  srt <- 0
                  adj <- c(0.5, 0)
                }
                labelPeaks(p[top], srt=srt, adj=adj)
              }
            },
            prefix="peaks", xlim=input$xlim,
            ylim=yLimFunction(input$ylim, input$vs)))
  })
  
  
  output$clusterImage <- renderImage({

    if (is.null(input$cm1)) {
      method1 <- "euclidean"
      
    } else {
      method1 <- input$cm1
    }
    if (is.null(input$cm2)) {
      method2 <- "ward.D2"
      
    } else {
      method2 <- input$cm2
    }


    dat <- data.frame(x = numeric(0), y = numeric(0))
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating Cluster", value = 0)
    n <- 10
    
    for (i in 1:n) {
      dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
      progress$inc(1/n, detail = paste("In Progress"))
      Sys.sleep(0.1)
    }
    
    pd <- list(createMassPeaks(mass=1:4,
                                 intensity=11:14,
                                 metaData=list(name="test mass peaks 1")),
                 createMassPeaks(mass=2:5,
                                 intensity=22:25,
                                 metaData=list(name="test mass peaks 2")))
    s <- dataset()
    spect <- transformIntensity(s, method="sqrt")
    spect <- smoothIntensity(spect, method="SavitzkyGolay")
    spect <- removeBaseline(spect)
    peak <- detectPeaks(spect)
    peak <- binPeaks(peak)
    intensityMatrix(peak)
    fm <- intensityMatrix(peak,spect)
    outfile <- tempfile(fileext='.png')
    png(outfile, width=900, height=600)
    pv <- pvclust(t(fm),method.hclust=input$cm2, method.dist=input$cm1, nboot=100)
    plot(pv)
    dev.off()
    list(src = outfile,
         contentType = 'image/png',
         width = 900,
         height = 600,
         alt = "This is alternate text")}, deleteFile = TRUE)

  
   output$downloadData <- downloadHandler(
      filename = function() {
        paste("FeatureMatrix-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(dataFM(), file)
        #Uploading User data to AWS
        put_object(file = file,bucket = s3BucketName, object = paste("FeatureMatrix-", Sys.time(), ".csv", sep="") ) 
      }
    )
   
   output$downloadData1 <- downloadHandler(
     filename = function() {
       paste("FeatureMatrix-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(dataFM(), file)
       #Uploading User data to AWS
       put_object(file = file,bucket = s3BucketName, object = paste("FeatureMatrix-", Sys.time(), ".csv", sep="") ) 
     }
   )
   
   
  
   output$downloadPeaks <- downloadHandler(
     filename = function() {
       paste("Peaks-", Sys.Date(), ".csv", sep="")

     },
     content = function(file) {
       exportCsv(dataPeaks(), path="~/Downloads/Peaks", sep=",", force=TRUE)

     }
   )
   
   #To display the best matching spectrum
   
   matching<-function(){
     
     dat <- data.frame(x = numeric(0), y = numeric(0))
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     
     progress$set(message = "Matching", value = 0)
     n <- 10
     
     for (i in 1:n) {
       dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
       progress$inc(1/n, detail = paste("In Progress"))
       Sys.sleep(0.1)
     }
     
     #Processing User data
     
     s <- dataset()
     spect <- transformIntensity(s, method=input$vs)
     spect <- smoothIntensity(spect, method=input$sm)
     spect <- removeBaseline(spect)
     peak <- detectPeaks(spect)
     peak <- binPeaks(peak)
     intensityMatrix(peak)
     fm <- intensityMatrix(peak,spect)
     
     # Loading data from AWS
     
     getbket <- get_bucket(s3BucketName)
     con1<-capture.output(print.simple.list(getbket))
     con2 <- textConnection(con1)
     data1 <- read.csv(con2,sep=" ")
     close(con2)
     filter1<-filter(data1,X == 'Contents.Key',grepl('.csv',X.11) | grepl('.csv',X.10))
     filter1$object_list=paste(filter1$X.10,filter1$X.11)
     obj_list<-subset(filter1,select=c(object_list))
     conf <- 0
     obj_Name <- " "
     
     for( val1 in obj_list$object_list){
      getcsvobj <-get_object(object = val1, bucket = s3BucketName )  
      csvobj <- rawToChar(getcsvobj)  
      con <- textConnection(csvobj)  
      aws_csv_df <- read.csv(con)
      k<- 0
      for (i in 1:379 ) {
        x1 <- aws_csv_df[2:16, 2:380]
        y1 <- fm[2:16, 1:379]
        res <- lsa::cosine(x1[,i], y1[,i])
        k <- k + res
      }
      p<-k/379
      if(p >= conf){
         conf <- p
        obj_Name <- val1
      }
     }
     max <- paste(obj_Name,round(conf,digits = 3))
   }
   
   output$tabb1<- renderTable({
     out1<- capture.output(print.simple.list(matching()))
     out2<- textConnection(out1)
     dataa1<-read.csv(out2,sep=' ')
     if(is.na(dataa1$X.2)){
       dataa1$Best_match=paste(dataa1$X.1)
     }
     else{dataa1$Best_match=paste(dataa1$X.1,dataa1$X.2)}
     dataa1$Confidence = paste(dataa1$X.3)
     outt1<-subset(dataa1,select=c(Best_match,Confidence))
   })
   
   #To display all the files and their confidence values
   
   matching1<-function(){
     
     dat <- data.frame(x = numeric(0), y = numeric(0))
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     
     progress$set(message = "Matching", value = 0)
     n <- 10
     
     for (i in 1:n) {
       dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
       progress$inc(1/n, detail = paste("In Progress"))
       Sys.sleep(0.1)
     }
     
     df <- data.frame(Bacteria_Name = " ", Confidence_value = 0)
     #Processing User data
     
     s <- dataset()
     spect <- transformIntensity(s, method=input$vs)
     spect <- smoothIntensity(spect, method=input$sm)
     spect <- removeBaseline(spect)
     peak <- detectPeaks(spect)
     peak <- binPeaks(peak)
     intensityMatrix(peak)
     fm <- intensityMatrix(peak,spect)
     
     # Loading data from AWS
     
     getbket <- get_bucket(s3BucketName)
     con1<-capture.output(print.simple.list(getbket))
     con2 <- textConnection(con1)
     data1 <- read.csv(con2,sep=" ")
     close(con2)
     filter1<-filter(data1,X == 'Contents.Key',grepl('.csv',X.11) | grepl('.csv',X.10))
     filter1$object_list=paste(filter1$X.10,filter1$X.11)
     obj_list<-subset(filter1,select=c(object_list))
     conf <- 0
     obj_Name <- " "
     j<-1
     for( val1 in obj_list$object_list){
       getcsvobj <-get_object(object = val1, bucket = s3BucketName )  
       csvobj <- rawToChar(getcsvobj)  
       con <- textConnection(csvobj)  
       aws_csv_df <- read.csv(con)
       k<- 0
       for (i in 1:379 ) {
         x1 <- aws_csv_df[2:16, 2:380]
         y1 <- fm[2:16, 1:379]
         res <- lsa::cosine(x1[,i], y1[,i])
         k <- k + res
       }
       p<-k/379
       p<-round(p,digits = 3)
       df1<-data.frame(Bacteria_Name = val1,Confidence_value = p)
       df <- rbind(df,df1)
       j <- j + 1
     }
     df<-df[-1,]
   }
   
   output$text1 <- renderTable({
     matching1()
   })
    
   dataFM <- reactive({
     s <- dataset()
     spect <- transformIntensity(s, method=input$vs)
     spect <- smoothIntensity(spect, method=input$sm)
     spect <- removeBaseline(spect)
     peak <- detectPeaks(spect)
     peak <- binPeaks(peak)
     intensityMatrix(peak)
     fm <- intensityMatrix(peak,spect)
     return(fm)
   })
  
   dataPeaks <- reactive({
     s <- dataset()
     spect <- transformIntensity(s, method=input$vs)
     spect <- smoothIntensity(spect, method=input$sm)
     spect <- removeBaseline(spect)
     peak <- detectPeaks(spect)
     peak <- binPeaks(peak)
     return(peak)
   })
})
