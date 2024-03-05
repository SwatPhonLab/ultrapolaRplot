# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

loadTraces <- function(directory_name, categories = c()){

  metaDataFile <- paste(directory_name, "metadata.json", sep = "/") #is this an ok approach?
  #is it guaranteed that the meta file will be called metadata, and is in the same directory level

  if (!file.exists(metaDataFile)){
    stop("metadata file does not exist in given directory")
  }

  metaData <- fromJSON(file = metaDataFile)

  traces_raw <- metaData$traces["Tongue"]
  filesAll <- metaData$files

  myXY_data <- data.frame()
  column_names <- c('file_number', 'itemNumber_inFile', 'segment', 'x', 'y')
  myXY_data <- rbind(myXY_data, column_names)
  allRowsTextGrids <- list()

  for (individualFile in 1:length(filesAll)){
    #accessing text grid files
    recording_name <- filesAll[[individualFile]]$.TextGrid
    plainTextname <- filesAll[[individualFile]]$name
    #used to access text grid files
    fullFilePath <- file.path(directory_name, recording_name)
    #print(fullFilePath)

    cur_recording <- metaData$traces$tongue$files[[(filesAll[[individualFile]]$name)]]

    if (is.null(cur_recording)){
      listExists <- 0
    }else{
      listExists <- max(unlist(lapply(metaData$traces$tongue$files[[(filesAll[[individualFile]]$name)]], length)))
    }

    #read text grid if it is not empty
    if (listExists > 1){
      #read in text grid data but only if recording > 0
      #Reading Text Grid
      textGridDataFile <- read_textgrid(fullFilePath)

      #time to parse
      intervalData <- textGridDataFile[textGridDataFile$tier_type == "IntervalTier", ]
      #intervalData <- intervalData[intervalData$tier_name == "orthographic vowel",] #limits to the tier that actually has annotations, for speed
      #doesn't work actually for large datasets...

      if (length(categories) == 0){ #read all segments
        intervalData <- intervalData[nchar(intervalData$text) == 1 | nchar(intervalData$text) == 2, ] #just gets everything, n^j is two characters
      } else { #specific categories specified
        intervalData <- intervalData[intervalData$text %in% categories, ]
      }

      fileNumber <- (intervalData$file)[1]

      df <- intervalData

      #attaching midpoint and plainTextname for later textgrid access
      df <- df %>% mutate(mid_point = (xmin + xmax)/2) #THIS IS FINE
      df <- df %>% mutate(plainTextName = plainTextname)

      textTiers <-  textGridDataFile[textGridDataFile$tier_type == "TextTier",]

      if (nrow(df) > 0){
        #1) using df min and max, isolate textTiers fragment
        #2) out of fragment, find textTier closest to midpoint
        frameNumberList <- list()
        for (midpoint in 1:length(df$mid_point)){
          min <- (df$xmin)[[midpoint]]
          max <- df$xmax[[midpoint]]
          textTierSection <- textTiers[textTiers$xmin >= min & textTiers$xmin <= max, ]

          if (nrow(textTierSection)!=0){
            frameNumber <- (textTierSection[which.min(abs(textTierSection$xmin - df$mid_point[[midpoint]])), ])$text
          }else{ #special case
            frameNumber <- (textTiers[which.min(abs(textTiers$xmin - df$mid_point[[midpoint]])), ])$text
          }
          frameNumberList = append(frameNumberList, frameNumber)
        }

        frameNumberList <- unlist(frameNumberList)
        df <- df %>% mutate(frame = frameNumberList)

        for (midpoint in 1:length(df$mid_point)){
          allRowsTextGrids <- rbind(allRowsTextGrids, data.frame(df[midpoint,])) #necessary in case multiple text grid files
        }
      }
    }else{
      #print("skipping this textgrid file")
    }

  }

  #extract xy data separately once we have all the data

  #allRowsTextGrids
  #textTiers

  for(frame in 1:length(allRowsTextGrids$frame)){
    frameNumber = (allRowsTextGrids$frame)[[frame]]
    plainTextname <- allRowsTextGrids$plainTextName[[frame]]
    xyFileData <- (metaData$traces)$tongue$files[[plainTextname]][[frameNumber]]

    myFileAndFrameName <- paste(plainTextname, "_", frameNumber, sep = "")
    myVowelType <- (allRowsTextGrids$text)[[frame]]

    if (length(xyFileData)>0){
      for (mark in 1:length(xyFileData)){
        itemNumber <- mark
        xCoor <- xyFileData[[mark]]$x
        yCoor <- xyFileData[[mark]]$y

        #build data.frame
        appendedXYFrame <- c(myFileAndFrameName, itemNumber, myVowelType, yCoor, xCoor)
        myXY_data <- rbind(myXY_data, appendedXYFrame)
      }
    }
  }
  colnames(myXY_data) <- myXY_data[1, ] #replace auto generated heading
  myXY_data <- myXY_data[-1, ] #delete the heading that is in row 1
  myXY_data[ ,4] <- as.numeric(myXY_data[ ,4]) #x, y are ints for graphing
  myXY_data[ ,5] <- as.numeric(myXY_data[ ,5])
  return(myXY_data)
}

sortedOrder <- function(listX){ #Sort by angle
  order_x <- order(listX)
  return(order_x)
}

calculateIntersection2 <- function(radiusPrevious, radiusNext, anglePrevious, angleRay, angleNext){
  
  differenceInTheta = angleNext - anglePrevious
  differenceInRay = angleRay - anglePrevious
  differenceInRadius = radiusNext - radiusPrevious
  
  return ( radiusPrevious + differenceInRay/differenceInTheta * differenceInRadius  )
}

read_in_data <- function(extractedData){
  dataR <- extractedData

  #split by vowel type
  split_data <- split(dataR, dataR$segment)
  uniqueSegments <- unique(dataR$segment)

  dataOfEachCurveNNj <- list()
  for (segment in 1:length(uniqueSegments)){
    dataOfEachCurveNNj[[uniqueSegments[[segment]]]] <- split(split_data[[segment]], split_data[[segment]]$file_number)
  }

  return(dataOfEachCurveNNj)
}


get_unique_segments <- function(extractedData){
  dataR <- extractedData
  uniqueSegments <- unique(dataR$segment)

  return(uniqueSegments)
}

identifyXAverage <- function(myXY_data){ #the x y axis are rotate left 90 degrees, so go based off x axis
  xmin =  min(myXY_data[c(5)])
  xmax =  max(myXY_data[c(5)])

  return((xmin + xmax)/2)
}

identifyPlotBounds <- function(compiledList){
  xmin = 2
  xmax = -2
  ymin = 2
  ymax = -2

  for (segment in 1:length(compiledList)){
    for (trace in 1:length(compiledList[[segment]])){
      tempxmin <- min(compiledList[[segment]][[trace]][1,])
      tempxmax <- max(compiledList[[segment]][[trace]][1,])
      tempymin <- min(compiledList[[segment]][[trace]][2,])
      tempymax <- max(compiledList[[segment]][[trace]][2,])

      if (tempxmin < xmin){
        xmin <- tempxmin
      }

      if (tempxmax > xmax){
        xmax <- tempxmax
      }

      if (tempymin < ymin){
        ymin <- tempymin
      }

      if (tempymax > ymax){
        ymax <- tempymax
      }
    }

  }

  return(c(xmin, xmax, ymin, ymax))
}

formating_data <- function(dataOfEachCurveNNj, uniqueSegments, origin.x = .5, scaling.factor = 800/600){
  compiledList <- list()
  
  for (segment in 1:length(dataOfEachCurveNNj)){
    listofarrays <- list()
    for (i in 1:length(dataOfEachCurveNNj[[segment]])){ #for each curve
      n_rows <- 4 # no need for slope 
      n_cols <- length(dataOfEachCurveNNj[[segment]][[i]][,1])
      myCurveMatrix <- matrix(0, nrow = n_rows, ncol = n_cols) #each individual 2D array
      
      #adjusted x and y values around the center
      xvalues <- (dataOfEachCurveNNj[[segment]][[i]][c(5)][[1]] * scaling.factor) - (origin.x*scaling.factor) #include parameter for aspect ratio
      yvalues <- 1 - dataOfEachCurveNNj[[segment]][[i]][c(4)][[1]]
      
      
      anglevalues <- list()
      
      for (j in 1: n_cols){
        anglevalues <- append(anglevalues, atan2(yvalues[[j]], xvalues[[j]]) )
      }
      
      #sort by angle
      anglevalues <- unlist(anglevalues)
      
      theOrderForSorting <- sortedOrder(anglevalues)
      xvaluesSorted <- xvalues[theOrderForSorting]
      yvaluesSorted <- yvalues[theOrderForSorting]
      anglevalues <- anglevalues[theOrderForSorting]
      
      
      for (j in 1:n_cols){
        myCurveMatrix[1,j] <- xvaluesSorted[[j]]
        myCurveMatrix[2,j] <- yvaluesSorted[[j]]
        myCurveMatrix[3,j] <- anglevalues[[j]] #angle in relation to (0,0)
        myCurveMatrix[4,j] <- (yvaluesSorted[[j]]^2 + xvaluesSorted[[j]]^2)^.5 #radius in relation to (0,0)
        
      }
      
      listofarrays[[i]] <-myCurveMatrix
    }
    compiledList[[uniqueSegments[[segment]]]] <- listofarrays
  }
  return(compiledList)
}

find_intersection_with_ray <- function(formatedData, dataOfEachCurveNNj, uniqueSegments, rayIncrement){ #ie compiledList

  matrixIntersection <- list()

  #start set up unique segment
  for (segment in 1:length(uniqueSegments)){
    matrixIntersection[[uniqueSegments[[segment]]]] <- matrix(NA, nrow = length(formatedData[[segment]]), ncol = (3.14/rayIncrement))
  }
  #end set up

  #for each extending radius
  for (angleRay in seq(from=0, to= 3.14, by= rayIncrement)){

    for (segment in 1:length(uniqueSegments)){ #and for each segment

      for (individualTrace in seq(length(dataOfEachCurveNNj[[segment]]))){ #for each trace in the segment (alphabetical??)

        for (individualPoint in 1:(length(formatedData[[segment]][[individualTrace]][1, all()])-1)){ #for each point on the trace, find intersection
          #find the right two points the angle falls between for each individual trace

          if (angleRay <= formatedData[[segment]][[individualTrace]][3, individualPoint +1]){
            if (angleRay >= formatedData[[segment]][[individualTrace]][3, individualPoint]){

              #print("found intersection with the following ray angle")

              myIntersection <- calculateIntersection2(

                (formatedData[[segment]][[individualTrace]][4,individualPoint]),
                (formatedData[[segment]][[individualTrace]][4,individualPoint + 1]),
                (formatedData[[segment]][[individualTrace]][3,individualPoint]),
                angleRay,
                (formatedData[[segment]][[individualTrace]][3,individualPoint + 1])

              )

              #store value
              matrixIntersection[[segment]][[individualTrace, angleRay/rayIncrement]] <- myIntersection
              break
            }
          }

        }

      } #end individual trace for loop

    } #end segment for loop

  } #end angle ray for loop
  return(matrixIntersection) # columns for rays
}

plotStyleTraces <- function(matrixIntersection, compiledList, dataOfEachCurveNNj, uniqueSegments, myPalette = c(), rayIncrement, points.display = FALSE, mean.lines = FALSE, means.styles = "l", bands.fill = TRUE, bands.lines = FALSE, legend.position = "center", standard.deviation.styles = "l", pdf.filename = "PolarTracePlot.pdf"){
  
  plotbounds <- identifyPlotBounds(compiledList)
  
  standardDeviation <- list()
  averagedRX <- list()
  averagedRY <- list()
  
  for (segment in 1:length(uniqueSegments)){
    averagedRX[[uniqueSegments[[segment]]]] <- list()
    averagedRY[[uniqueSegments[[segment]]]] <- list()
    
    standardDeviation[[uniqueSegments[[segment]]]] <- list()
    
    for (ray in 1:length(matrixIntersection[[segment]][1, all()])){
      if (!is.na(colMeans(matrixIntersection[[segment]], na.rm = TRUE)[[ray]])){ #as long as there is a mean for each column, store the value
        averagedRX[[segment]] <- append(averagedRX[[segment]], cos(rayIncrement*ray) * colMeans(matrixIntersection[[segment]], na.rm = TRUE)[[ray]] )
        averagedRY[[segment]] <- append(averagedRY[[segment]], sin(rayIncrement*ray) * colMeans(matrixIntersection[[segment]], na.rm = TRUE)[[ray]] )
        standardDeviation[[segment]] <- append(standardDeviation[[segment]], sd(matrixIntersection[[segment]][all(), ray], na.rm = TRUE))
      } 
      else{
        averagedRX[[segment]] <- append(averagedRX[[segment]], NA)
        averagedRY[[segment]] <- append(averagedRY[[segment]], NA)
        standardDeviation[[segment]] <- append(standardDeviation[[segment]], NA)
      }
    }
    averagedRX[[segment]] <- unlist(averagedRX[[segment]])
    averagedRY[[segment]] <- unlist(averagedRY[[segment]])
    standardDeviation[[segment]] <- unlist(standardDeviation[[segment]])
  }
  
  
  #we now have the standard deviation
  xSDHigh <- list()
  xSDLow <- list()
  ySDHigh <- list()
  ySDLow <- list()  
  for (segment in 1:length(uniqueSegments)){
    
    xSDHigh[[uniqueSegments[[segment]]]] <- list()
    xSDLow[[uniqueSegments[[segment]]]] <- list()
    ySDHigh[[uniqueSegments[[segment]]]] <- list()
    ySDLow[[uniqueSegments[[segment]]]] <- list()
    
    for (i in 1:length(standardDeviation[[segment]])){
      
      if (!is.na(standardDeviation[[segment]][[i]])){
        xSDHigh[[segment]]<- append(xSDHigh[[segment]], cos(i*rayIncrement) *standardDeviation[[segment]][[i]] + averagedRX[[segment]][[i]])
        xSDLow[[segment]] <-  append( xSDLow[[segment]], -1*cos(i*rayIncrement) *standardDeviation[[segment]][[i]] + averagedRX[[segment]][[i]])
        
        ySDHigh[[segment]] <- append(ySDHigh[[segment]], sin(i*rayIncrement) *standardDeviation[[segment]][[i]] + averagedRY[[segment]][[i]])
        ySDLow[[segment]] <-  append(ySDLow[[segment]], -1*sin(i*rayIncrement) *standardDeviation[[segment]][[i]] + averagedRY[[segment]][[i]])
        
      }
      # else{
      #   xSDHigh[[segment]]<- append(xSDHigh[[segment]], NA)
      #   xSDLow[[segment]] <-  append( xSDLow[[segment]], NA)
      #   
      #   ySDHigh[[segment]] <- append(ySDHigh[[segment]], NA)
      #   ySDLow[[segment]] <-  append(ySDLow[[segment]], NA)
      # }
      
    }
  }
  
  
  xPlotAverage <- (plotbounds[[1]] + plotbounds[[2]])/2
  yPlotAverage <- (plotbounds[[3]] + plotbounds[[4]])/2
  x_ticks <- c(round(plotbounds[[1]],2), round(xPlotAverage,2), round(plotbounds[[2]],2))
  y_ticks <- c(round(plotbounds[[3]],2), round(yPlotAverage,2), round(plotbounds[[4]],2))
  
  #pdf(pdf.filename)
  par(pty = "s")
  
  plot(1, type = "n", xlab = "", ylab = "", ylim = c(plotbounds[[3]], plotbounds[[4]]), xlim = c(plotbounds[[1]], plotbounds[[2]]), xaxt = "n", yaxt = "n", asp = 1)
  
  axis(1, at = x_ticks)
  axis(2, at = y_ticks)
  
  
  if (length(myPalette) == 0){
    numberOfColors <- length(uniqueSegments) + 2
    paletteColors <- (brewer.pal(numberOfColors, "RdBu"))[2:numberOfColors] #PuRd nice
    
  }else{
    paletteColors <- myPalette
  }
  
  
  for (segment in 1:length(uniqueSegments)){
    #Shading
    x1 <- rev(unlist(lapply(xSDLow[[segment]], function(x) na.omit(x)), recursive = TRUE))
    y1 <- rev(unlist(lapply(ySDLow[[segment]], function(x) na.omit(x)), recursive = TRUE))
    
    
    x2 <- rev(unlist(lapply(xSDHigh[[segment]], function(x) na.omit(x)), recursive = TRUE))
    y2 <- rev(unlist(lapply(ySDHigh[[segment]], function(x) na.omit(x)), recursive = TRUE))
    
    
    #print(xSDHigh[[segment]])
    
    #Plot the upper lower standard devation lines
    if (bands.lines == TRUE){
      lines(x1, y1, type = standard.deviation.styles, col = paletteColors[[segment]], lwd = 1.4)#, ylim = c(ymin, ymax), xlim = c(xmin, xmax))
      lines(x2, y2, type = standard.deviation.styles, col = paletteColors[[segment]], lwd = 1.4)
    }
    
    if (bands.fill == TRUE){
      # Create a polygon to shade the region between the arches
      polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = adjustcolor(paletteColors[[segment]], alpha.f = 0.37), border = NA)
    }
    
    
    if (mean.lines){
      if (!all(is.na(standardDeviation[[segment]]))){
        lines(averagedRX[[segment]], averagedRY[[segment]], type = means.styles, col = paletteColors[[segment]] , lwd = 1) #you could also have black...
      }else{
        lines(averagedRX[[segment]], averagedRY[[segment]], type = means.styles, col = paletteColors[[segment]] , lwd = 1)
      }
    }
    
    if (points.display){
      for (trace in 1:length(compiledList[[segment]])){
        points(compiledList[[segment]][[trace]][1, all()], compiledList[[segment]][[trace]][2, all()], type = "p", col = paletteColors[[segment]], asp = 1)
      }
    }
    
    if (legend.position == "center"){
      legend(xPlotAverage, yPlotAverage, legend = uniqueSegments, fill = paletteColors, cex = .9)
    } else if (legend.position == "topleft"){
      legend(plotbounds[[1]], plotbounds[[4]], legend = uniqueSegments, fill = paletteColors, cex = .6)
    }
    
  }
}

makeTracesPolar <- function(myXY_data, origin.algorithm = "BottomMiddle", origin.x = NA, scaling.factor = 800/600){
  
  uniqueSegments <- get_unique_segments(myXY_data)
  dataOfEachCurveNNj <- read_in_data(myXY_data)
  xaverage <- identifyXAverage(myXY_data)
  
  
  if (origin.algorithm == "BottomMiddle"){
    if (is.na(origin.x)){
      origin.x = .5
    }
  }else if (origin.algorithm == "BottomMean"){
    origin.x = xaverage
  }
  
  compiledList <- formating_data(dataOfEachCurveNNj, uniqueSegments, origin.x = origin.x, scaling.factor = scaling.factor)
  return(compiledList)
}

plotTraces <- function(myXY_data, compiledList, interval = 1, mean.lines = TRUE, points.display = FALSE, myPalette = c(), bands.lines = FALSE, bands.fill = TRUE, legend.position = "center", means.styles = "l", standard.deviation.styles = "l"){
  
  rayIncrement = 3.14159/180 * interval
  
  uniqueSegments <- get_unique_segments(myXY_data)
  dataOfEachCurveNNj <- read_in_data(myXY_data)
  
  matrixIntersection <- find_intersection_with_ray(compiledList, dataOfEachCurveNNj, uniqueSegments, rayIncrement)
  
  plotStyleTraces(matrixIntersection = matrixIntersection, compiledList = compiledList, dataOfEachCurve = dataOfEachCurveNNj, uniqueSegments = uniqueSegments, rayIncrement = rayIncrement, mean.lines = mean.lines, points.display = points.display, myPalette = myPalette, bands.lines = bands.lines, legend.position = legend.position, bands.fill = bands.fill, means.styles = means.styles, standard.deviation.styles = standard.deviation.styles)
}
