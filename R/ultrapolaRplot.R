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

loadTraces <- function(directory_name, tiernameAll = c(""), categoriesAll = list(c()), layersAll = c(""),
                       mergeCategories = c(FALSE)){
  
  metaDataFile <- paste(directory_name, "metadata.json", sep = "/") #is this an ok approach?
  #is it guaranteed that the meta file will be called metadata, and is in the same directory level
  
  if (!file.exists(metaDataFile)){
    stop("metadata file does not exist in given directory")
  }
  
  metaData <- fromJSON(file = metaDataFile)
  filesAll <- metaData$files
  
  #type checking
  #c("[]") already acts like a string "", so only need to check categoriesAll
  
  if (inherits(categoriesAll, "character") || inherits(categoriesAll, "NULL")){
    categoriesAll <- list(categoriesAll)
  }
  
  #some initial set-up
  rawTraces <- data.frame()
  column_names <- c('file_number', 'itemNumber_inFile', 'segment', 'x', 'y')
  rawTraces <- rbind(rawTraces, column_names)
  allRowsTextGrids <- list()
  
  #padding
  for (i in 1:length(categoriesAll)){
    if (length(tiernameAll) < length(categoriesAll)){
      tiernameAll <- append(tiernameAll, tiernameAll[[1]])
    }
    if (length(layersAll) < length(categoriesAll)){
      layersAll <- append(layersAll, layersAll[[1]]) #maybe edit later to layersAll[]
    }
    if (length(mergeCategories) < length(categoriesAll)){
      mergeCategories <- append(mergeCategories, mergeCategories[[1]])
    }
  }
  #beginning for loop
  for (item in 1:length(categoriesAll)){
    
    #selecting layer level: future work be able to detect all, rather than setting tongue as default
    if(layersAll[[item]]==""){
      traces_raw <- metaData$traces[['tongue']]
    }else{
      traces_raw <- metaData$traces[[layersAll[[item]]]]
    }
    
    #traces_raw <- metaData$traces[['palate']]
    #traces_raw <- metaData$traces$tongue
    
    tiername = tiernameAll[[item]]
    categories = categoriesAll[[item]]
    
    
    
    #if only metadata xy data
    if (tiernameAll[[item]] == "" && length(categoriesAll[[item]]) == 1 && categoriesAll[[item]] == ""){
      #order of logic matters
      rawTraces <- rbind(rawTraces, tracesWithoutTier(metaDataFile, layersAll[[item]]))
      
    }else{ #actually open textgrids and check categories
      for (individualFile in 1:length(filesAll)){
        #accessing text grid files
        recording_name <- filesAll[[individualFile]]$.TextGrid
        plainTextname <- filesAll[[individualFile]]$name
        #used to access text grid files
        fullFilePath <- file.path(directory_name, recording_name)
        #print(fullFilePath)
        
        #cur_recording <- metaData$traces$tongue$files[[(filesAll[[individualFile]]$name)]]
        cur_recording <- traces_raw$files[[(filesAll[[individualFile]]$name)]]
        
        if (is.null(cur_recording)){
          listExists <- 0
        }else{
          # listExists <- max(unlist(lapply(metaData$traces$tongue$files[[(filesAll[[individualFile]]$name)]], length)))
          listExists <- max(unlist(lapply(traces_raw$files[[(filesAll[[individualFile]]$name)]], length)))
        }
        
        #read text grid if it is not empty
        if (listExists > 1){
          #read in text grid data but only if recording > 0
          #Reading Text Grid
          errorCode = 0
          tryCatch({
            textGridDataFile <- read_textgrid(fullFilePath)
          }, error = function(e){
            message("cannot open textgrid")
            errorCode = 1
          })
          if(errorCode==1){ #skip this file and move onto next
            next
          }
          
          #time to parse TIERS
          intervalData <- textGridDataFile[textGridDataFile$tier_type == "IntervalTier", ]
          if (nchar(tiername)!=0){
            if (tiername %in% intervalData$tier_name){
              intervalData <- intervalData[intervalData$tier_name == tiername,]
            }else{ #you have annotation, but not in the proper tier
              message("skipping this textgrid file, as annotations are not in the proper tier")
              message(plainTextname)
              next
            }
          }
          
          #CATEGORIES
          if (length(categories) == 0){ #read all segments
            #intervalData <- intervalData[nchar(intervalData$text) == 1 | nchar(intervalData$text) == 2, ] 
            #just gets everything, n^j is two characters
            intervalData <- intervalData[nchar(intervalData$text) !=0, ] 
            #absolutely gets everything, that is not a blank space
          } else { #specific categories specified
            intervalData <- intervalData[intervalData$text %in% categories, ]
          }
          
          fileNumber <- (intervalData$file)[1]
          
          
          df <- intervalData
          
          #attaching midpoint and plainTextname for later textgrid access
          df <- df %>% mutate(mid_point = (df$xmin + df$xmax)/2) #THIS IS FINE
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
      #return(allRowsTextGrids)
      #extract xy data separately once we have all the data
      
      for(frame in 1:length(allRowsTextGrids$frame)){
        frameNumber = (allRowsTextGrids$frame)[[frame]]
        plainTextname <- allRowsTextGrids$plainTextName[[frame]]
        #xyFileData <- (metaData$traces)$tongue$files[[plainTextname]][[frameNumber]]
        xyFileData <- traces_raw$files[[plainTextname]][[frameNumber]]
        
        myFileAndFrameName <- paste(plainTextname, "_", frameNumber, sep = "")
        myVowelType <- (allRowsTextGrids$text)[[frame]]
        if (mergeCategories[[item]] == TRUE){
          if (length(unlist(categories))!=0){
            myVowelType <- paste(categories, collapse = "")
          }else{
            myVowelType <- "N/A" #or something else
          }
          
        }
        
        if (length(xyFileData)>0){
          for (mark in 1:length(xyFileData)){
            itemNumber <- mark
            xCoor <- xyFileData[[mark]]$x
            yCoor <- xyFileData[[mark]]$y
            
            #build data.frame
            appendedXYFrame <- c(myFileAndFrameName, itemNumber, myVowelType, yCoor, xCoor)
            rawTraces <- rbind(rawTraces, appendedXYFrame)
          }
        }
      }
      allRowsTextGrids <- list()
    }  
  }
  
  #allRowsTextGrids extract x,y
  #textTiers
  
  #CLEAN UP
  if (unlist(rawTraces[1,3]) == "segment"){
    rawTraces <- rawTraces[-1, ] #delete the heading that is in row 1
  }
  colnames(rawTraces) <- column_names #replace auto generated heading
  rawTraces[ ,4] <- as.numeric(rawTraces[ ,4]) #x, y are ints for graphing
  rawTraces[ ,5] <- as.numeric(rawTraces[ ,5])
  
  #a[order(factor(a$x, levels = reference)),] #sorting given user input
  # if (length(unlist(categoriesAll)) > 0 && max(lapply(categoriesAll, nchar)!=0)){
  #     rawTraces <- rawTraces[order(factor(rawTraces$segment, levels = unlist(categoriesAll))), ]
  # }
  return(rawTraces)
}

tracesWithoutTier <- function(metaDataFilePath, layerName){
  #length(unique(rawTraces$file_number))
  metaData <- fromJSON(file = metaDataFilePath)
  #fileName <- metaData$files[[38]]$name
  #9
  #getting #2 
  filenamexy <- list()
  xvalues <- list()
  yvalues <- list()
  rawTraces <- data.frame()
  column_names <- c('file_number', 'itemNumber_inFile', 'segment', 'x', 'y')
  rawTraces <- rbind(rawTraces, column_names)
  for (file in 1:length(metaData$files)){
    fileName <- fileName <- metaData$files[[file]]$name
    for (fileTrace in 1:length(metaData$traces[[layerName]]$files[fileName][[1]])){
      if (length(metaData$traces[[layerName]]$files[fileName][[1]][[fileTrace]])>0){
        filenamexy <- append(filenamexy, metaData$traces[[layerName]]$files[fileName][[1]][[fileTrace]])
        #names(metaData$traces[['palate']]$files[[1]][1])
        traceNumber <- names(metaData$traces[[layerName]]$files[fileName][[1]][fileTrace])
        myFileAndFrameName <- paste(fileName, "_", traceNumber, sep = "")
        
        for (mark in 1:length(filenamexy)){
          itemNumber <- mark
          xCoor <- filenamexy[[mark]]$x
          yCoor <- filenamexy[[mark]]$y
          xvalues <- append(xvalues, xCoor)
          yvalues <- append(yvalues, yCoor)
          #build data.frame
          #appendedXYFrame <- c(myFileAndFrameName, mark, layerName, yCoor, xCoor)
          appendedXYFrame <- c(myFileAndFrameName, mark, layerName, yCoor, xCoor)
          rawTraces <- rbind(rawTraces, appendedXYFrame)
        }
        filenamexy <- list()
        xvalues <- list()
        yvalues <- list()
      }
    }
    #done getting xy in file
  }
  
  # colnames(rawTraces) <- rawTraces[1, ] #replace auto generated heading
  rawTraces <- rawTraces[-1, ] #delete the heading that is in row 1
  # rawTraces[ ,4] <- as.numeric(rawTraces[ ,4]) #x, y are ints for graphing
  # rawTraces[ ,5] <- as.numeric(rawTraces[ ,5])
  return(rawTraces)
}

loadAllTraces <- function(directory_name){
  
  metaDataFile <- paste(directory_name, "metadata.json", sep = "/") 
  if (!file.exists(metaDataFile)){
    stop("metadata file does not exist in given directory")
  }
  
  metaData <- fromJSON(file = metaDataFile)
  filesAll <- metaData$files
  
  #some initial set-up
  rawTraces <- data.frame()
  column_names <- c('file_number', 'itemNumber_inFile', 'segment', 'x', 'y', 'tiername', 'layername')
  rawTraces <- rbind(rawTraces, column_names)
  allRowsTextGrids <- list()
  
  #beginning for loop
  #but now, you don't know categoriesAll!
  #replace with layer loop
  
  totalRows <- 1
  
  for (item in 1:length(metaData$traces)){
    #item, instead of 'tongue' or 'palate'
    traces_raw <- metaData$traces[[item]]
    
    for (individualFile in 1:length(filesAll)){
      #accessing text grid files
      recording_name <- filesAll[[individualFile]]$.TextGrid
      plainTextname <- filesAll[[individualFile]]$name
      #used to access text grid files
      fullFilePath <- file.path(directory_name, recording_name)
      
      cur_recording <- traces_raw$files[[(filesAll[[individualFile]]$name)]]
      
      if (is.null(cur_recording)){
        listExists <- 0
      }else{
        listExists <- max(unlist(lapply(traces_raw$files[[(filesAll[[individualFile]]$name)]], length)))
      }
      
      #read text grid if, there are some annotations
      #ok, and then if text grid crashes... still get it, but label it as layer name?? only. 
      if (listExists > 1){
        errorCode = 0
        #read in text grid data but only if recording > 0
        #Reading Text Grid
        tryCatch({
          textGridDataFile <- read_textgrid(fullFilePath)
        }, error = function(e){
          errorCode = 1
          message("cannot open textgrid")
          message(plainTextname)
          
          if(errorCode==1){ 
            #there is an error opening the TextGrid. #therefore, get the xy data from this file, and label with layer name...
            #just add xy data immediately to rawTraces?
            filenamexy <- list()
            xvalues <- list()
            yvalues <- list()
            for (fileTrace in 1:length(cur_recording)){
              if (length(cur_recording[[fileTrace]])>0){
                filenamexy <- append(filenamexy, cur_recording[[fileTrace]])
                traceNumber <- names(cur_recording[fileTrace])
                layer <- names(metaData$traces[item])
                myFileAndFrameName <- paste(plainTextname, "_", traceNumber, sep = "")
                
                for (mark in 1:length(filenamexy)){
                  totalRows <<- totalRows + 1
                  itemNumber <- mark
                  xCoor <- filenamexy[[mark]]$x
                  yCoor <- filenamexy[[mark]]$y
                  xvalues <- append(xvalues, xCoor)
                  yvalues <- append(yvalues, yCoor)
                  
                  appendedFrame <- c(myFileAndFrameName, mark, layer, yCoor, xCoor, "", layer)
                  if (length(rawTraces[1,all()]) - length(column_names) > 0){
                    for (anExtraSpace in 1:length(rawTraces[1,all()]) - length(column_names)){
                      appendedFrame <- append(appendedFrame, NA) 
                      #randomly looping around to add file name and item number
                      #replace with NA, to make it clear
                    }
                  }
                  
                  rawTraces <<- rbind(rawTraces, appendedFrame)
                  #layer as segment name, used for plotting.
                }
                filenamexy <- list()
                xvalues <- list()
                yvalues <- list()
              }
            }
            
          }
          #next section is actually looking at TextGrid
        })
        
        if(errorCode==1){ #skip this file and move onto next
          #doesn't even do anything
          next
        }
        
        #time to parse TIERS
        intervalData <- textGridDataFile[textGridDataFile$tier_type == "IntervalTier", ]
        intervalData <- intervalData[nchar(intervalData$text) !=0, ] 
        
        fileNumber <- (intervalData$file)[1]
        
        
        df <- intervalData
        
        #attaching midpoint and plainTextname for later textgrid access
        df <- df %>% mutate(mid_point = (df$xmin + df$xmax)/2) #THIS IS FINE
        df <- df %>% mutate(plainTextName = plainTextname)
        df <- df %>% mutate(layerName =  names(metaData$traces[item]))
        
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
        
        
      }
    }
    #return(allRowsTextGrids)
    #extract xy data separately once we have all the data
    
    #this should be fine, not hard coded to 'tongue' as layer name
    #print(names(metaData$traces[item]))
    #print(length(allRowsTextGrids$frame))
    
    if (length(allRowsTextGrids$frame) > 0){ #R should already be able to do this. 
      for(frame in 1:length(allRowsTextGrids$frame)){
        frameNumber = (allRowsTextGrids$frame)[[frame]]
        layerName = (allRowsTextGrids$layerName)[[frame]]
        tierName = (allRowsTextGrids$tier_name)[[frame]]
        plainTextname <- allRowsTextGrids$plainTextName[[frame]]
        
        #indices of tiers it is nested in. only used if there is an actual annotation
        #length(xyFileData > 0)
        timeMin <- (allRowsTextGrids$xmin)[[frame]]
        
        timeMax <- (allRowsTextGrids$xmax)[[frame]]
        
        
        filteringAllRowsTextGrids <- allRowsTextGrids %>% filter(xmin <= timeMin)
        #crashes if you try to do &&
        filteringAllRowsTextGrids <- filteringAllRowsTextGrids %>% filter(xmax >= timeMax)
        
        #filtering by, of course, text grid file 
        filteringAllRowsTextGrids <- filteringAllRowsTextGrids %>% filter(filteringAllRowsTextGrids$plainTextName == plainTextname)
        
        
        #xyFileData <- (metaData$traces)$tongue$files[[plainTextname]][[frameNumber]]
        xyFileData <- traces_raw$files[[plainTextname]][[frameNumber]]
        
        myFileAndFrameName <- paste(plainTextname, "_", frameNumber, sep = "")
        myVowelType <- (allRowsTextGrids$text)[[frame]]
        
        
        if (length(xyFileData)>0){
          for (mark in 1:length(xyFileData)){
            totalRows <- totalRows + 1
            itemNumber <- mark
            xCoor <- xyFileData[[mark]]$x
            yCoor <- xyFileData[[mark]]$y
            #print(filteringAllRowsTextGrids)
            
            #build data.frame
            appendedXYFrame <- c(myFileAndFrameName, itemNumber, myVowelType, yCoor, xCoor, tierName, layerName)
            rawTraces <- rbind(rawTraces, appendedXYFrame)
            
            
            #add/use layered tier columns
            
            if (length(filteringAllRowsTextGrids[all(),1]) > 0){
              #print(length(filteringAllRowsTextGrids[all(),1]))
              for (tier in 1:length(filteringAllRowsTextGrids[all(),1])){
                #print(totalRows)
                #print(filteringAllRowsTextGrids$text[[tier]])
                
                rawTraces[[(filteringAllRowsTextGrids$tier_name[[tier]])]][totalRows] <- filteringAllRowsTextGrids$text[[tier]]
                
              }
            }
            
            
          }
        }
      } 
    }
    allRowsTextGrids <- list()
  }
  
  #allRowsTextGrids extract x,y
  #textTiers
  
  #CLEAN UP
  if (unlist(rawTraces[1,3]) == "segment"){
    rawTraces <- rawTraces[-1, ] #delete the heading that is in row 1
  }
  
  #replace auto generated heading with initial 7 garanteed columns
  for (columnReplacement in 1:length(column_names)){
    colnames(rawTraces)[[columnReplacement]] <- column_names[[columnReplacement]]
  }
  
  # colnames(rawTraces) <- column_names 
  rawTraces[ ,4] <- as.numeric(rawTraces[ ,4]) #x, y are ints for graphing
  rawTraces[ ,5] <- as.numeric(rawTraces[ ,5])
  
  #a[order(factor(a$x, levels = reference)),] #sorting given user input
  # if (length(unlist(categoriesAll)) > 0 && max(lapply(categoriesAll, nchar)!=0)){
  #     rawTraces <- rawTraces[order(factor(rawTraces$segment, levels = unlist(categoriesAll))), ]
  # }
  return(rawTraces)
}

loadAllTracesMidPoint <- function(directory_name){
  
  metaDataFile <- paste(directory_name, "metadata.json", sep = "/") 
  if (!file.exists(metaDataFile)){
    stop("metadata file does not exist in given directory")
  }
  
  metaData <- fromJSON(file = metaDataFile)
  filesAll <- metaData$files
  
  rawTraces <- data.frame()
  #insert annotation later
  column_names <- c('file_number', 'itemNumber_inFile', I(list(NA)), 'x', 'y', I(list(NA)), 'layer')
  rawTraces <- rbind(rawTraces, column_names)
  colnames(rawTraces) <- c('file_number', 'itemNumber_inFile', 'segment', 'x', 'y', 'tiers_list', 'layer')
  
  
  
  allTiersLabelling <- c()
  allRowsTextGrids <- list()
  
  totalRows <- 1
  
  for (item in 1:length(metaData$traces)){
    #item, instead of 'tongue' or 'palate'
    traces_raw <- metaData$traces[[item]]
    
    for (individualFile in 1:length(filesAll)){
      #accessing text grid files
      recording_name <- filesAll[[individualFile]]$.TextGrid
      plainTextname <- filesAll[[individualFile]]$name
      #used to access text grid files
      fullFilePath <- file.path(directory_name, recording_name)
      
      cur_recording <- traces_raw$files[[(filesAll[[individualFile]]$name)]]
      
      if (is.null(cur_recording)){
        listExists <- 0
      }else{
        listExists <- max(unlist(lapply(traces_raw$files[[(filesAll[[individualFile]]$name)]], length)))
      }
      
      #read text grid if, there are some annotations
      #ok, and then if text grid crashes... still get it, but label it as layer name?? only. 
      
      #will restructure to match rest of allRowsTextGrids
      if (listExists > 1){
        errorCode = 0
        #read in text grid data but only if recording > 0
        #Reading Text Grid
        tryCatch({
          textGridDataFile <- read_textgrid(fullFilePath)
        }, error = function(e){
          errorCode = 1
          print("cannot open textgrid")
          print(plainTextname)
          message("cannot open textgrid")
          message(plainTextname)
          
          #remove if statement afterwards i think 
          if(errorCode==1){ 
            #there is an error opening the TextGrid. #therefore, get the xy data from this file, and label with layer name...
            #just add xy data immediately to rawTraces?
            filenamexy <- list()
            xvalues <- list()
            yvalues <- list()
            for (fileTrace in 1:length(cur_recording)){
              if (length(cur_recording[[fileTrace]])>0){
                filenamexy <- append(filenamexy, cur_recording[[fileTrace]])
                traceNumber <- names(cur_recording[fileTrace])
                layer <- names(metaData$traces[item])
                myFileAndFrameName <- paste(plainTextname, "_", traceNumber, sep = "")
                
                for (mark in 1:length(filenamexy)){
                  totalRows <<- totalRows + 1
                  itemNumber <- mark
                  xCoor <- filenamexy[[mark]]$x
                  yCoor <- filenamexy[[mark]]$y
                  xvalues <- append(xvalues, xCoor)
                  yvalues <- append(yvalues, yCoor)
                  
                  #lack of segment and tiers, for plotting layer name is used for segment
                  appendedFrame <- data.frame(myFileAndFrameName, mark, layer, yCoor, xCoor, I(list(NA)), layer)
                  colnames(appendedFrame) <- c('file_number', 'itemNumber_inFile', 'segment', 'x', 'y', 'tiers_list', 'layer')
                  rawTraces <<- rbind(rawTraces, appendedFrame)
                  #layer as segment name, used for plotting.
                }
                filenamexy <- list()
                xvalues <- list()
                yvalues <- list()
              }
            }
            
          }
          #next section is actually looking at TextGrid
        })
        
        if(errorCode==1){ #skip this file and move onto next
          #doesn't even do anything
          next
        }
        
        #time to parse TIERS
        intervalData <- textGridDataFile[textGridDataFile$tier_type == "IntervalTier", ]
        intervalData <- intervalData[nchar(intervalData$text) !=0, ] 
        
        fileNumber <- (intervalData$file)[1]
        
        df <- intervalData
        
        #attaching midpoint and plainTextname for later textgrid access
        df <- df %>% mutate(mid_point = (df$xmin + df$xmax)/2) #THIS IS FINE
        df <- df %>% mutate(plainTextName = plainTextname)
        df <- df %>% mutate(layerName =  names(metaData$traces[item]))
        
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
          
          #create new column for intersecting items
          df <- df %>% mutate(overlappingSegments = list(NA))
          df <- df %>% mutate(overlappingTiers = list(NA))
          
          #now will keep less rows, and check if there is annotation on that midpoint
          
          #df rows be appended once df is additionally processed below
        }
        
        #at this point df should only contain individual file entries.
        #will be cleared for each loop
        #modifying what's going into allRowsTextGrids
        
        #going through annotated frames in each file
        #finding nested intervals
        if (nrow(df) > 0){
          annotedTrueFalse <- list()
          for(frame in 1:length(df$frame)){
            overlapping <- list()
            additionalTiers <- list()
            timeMin <- (df$xmin)[[frame]]
            timeMax <- (df$xmax)[[frame]]
            
            plainTextname <- (df$plainTextName)[[frame]]
            frameNumber <- (df$frame)[[frame]]
            
            currentLayer <- df$layerName[[frame]]
            
            xyFileData <- (metaData$traces)[[currentLayer]]$files[[plainTextname]][[frameNumber]]
            #need to check if there is actually annotation here]
            if (length(xyFileData) > 0) {
              annotedTrueFalse <- append(annotedTrueFalse, 1)
              #finding smaller intervals
              #intersect
              filteringAllRowsTextGrids <- df[which(df$xmin <= timeMin),] #%>% filter(timeMin >= xmin)
              filteringAllRowsTextGrids <- filteringAllRowsTextGrids[which(filteringAllRowsTextGrids$xmax >= timeMax),]#df %>% filter(timeMax <= xmax)
              
              for (smallerInterval in 1:length(filteringAllRowsTextGrids$xmax)){
                frameNumber <- (filteringAllRowsTextGrids$frame)[[smallerInterval]]
                textOnTier <- (filteringAllRowsTextGrids$text)[[smallerInterval]]
                tierOverlap <- (filteringAllRowsTextGrids$tier_name)[[smallerInterval]]
                #plainTextname should be same as it is same file still
                xyFileData <- traces_raw$files[[plainTextname]][[frameNumber]]
                
                overlapping <- append(overlapping, textOnTier)
                additionalTiers <- append(additionalTiers, tierOverlap)
                #df[[(filteringAllRowsTextGrids$tier_name[[smallerInterval]])]][frame] <- c(filteringAllRowsTextGrids$text[[smallerInterval]], annotated)
                
              }
              df$overlappingSegments[[frame]] <- unlist(overlapping)
              df$overlappingTiers[[frame]] <- unlist(additionalTiers)
            }else{
              annotedTrueFalse <- append(annotedTrueFalse, 0)
            }
          }
          df <- df %>% mutate(annotated = unlist(annotedTrueFalse))
          #can still keep unnannoted segments by removing if statement
          for (midpoint in 1:length(df$mid_point)){
            if (df$annotated[[midpoint]] == 1){ 
              allRowsTextGrids <- rbind(allRowsTextGrids, data.frame(df[midpoint,]))
            }
          }
          
          
        }
        
      }
      
    }
    
    #return(allRowsTextGrids)
    #extract xy data separately
    if (length(allRowsTextGrids$frame) > 0){ 
      for(frame in 1:length(allRowsTextGrids$frame)){
        frameNumber = (allRowsTextGrids$frame)[[frame]]
        plainTextname <- allRowsTextGrids$plainTextName[[frame]]
        myFileAndFrameName <- paste(plainTextname, "_", frameNumber, sep = "")
        xyFileData <- traces_raw$files[[plainTextname]][[frameNumber]]
        
        segments <- list(unlist(allRowsTextGrids$overlappingSegments[[frame]]))
        tiers <- list(unlist(allRowsTextGrids$overlappingTiers[[frame]]))
        layerOf <- allRowsTextGrids$layerName[[frame]]
        
        if (length(xyFileData)>0){
          for (mark in 1:length(xyFileData)){
            itemNumber <- mark
            xCoor <- xyFileData[[mark]]$x
            yCoor <- xyFileData[[mark]]$y
            
            allFrame <- data.frame(myFileAndFrameName, itemNumber, I(segments), yCoor, xCoor, I(tiers), layerOf)
            colnames(allFrame) <- c('file_number', 'itemNumber_inFile', 'segment', 'x', 'y', 'tiers_list', 'layer')
            #print(data.frame(myFileAndFrameName, itemNumber, I(segments), xCoor, yCoor, I(tiers), layerOf))
            rawTraces <- rbind(rawTraces, allFrame)
          }
        }
      } 
    }
    allRowsTextGrids <- list()
  }
  
  rawTraces <- rawTraces[-1, ]
  #colnames(rawTraces) <- c('file_number', 'itemNumber_inFile', 'segment_list', 'x', 'y', 'tiers_list', 'layer')
  rawTraces[ ,4] <- as.numeric(rawTraces[ ,4]) #x, y are ints for graphing
  rawTraces[ ,5] <- as.numeric(rawTraces[ ,5])
  
  return(rawTraces)
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
    dataOfEachCurveNNj[[uniqueSegments[[segment]]]] <- split(split_data[[uniqueSegments[[segment]]]], split_data[[uniqueSegments[[segment]]]]$file_number)
  }
  
  return(dataOfEachCurveNNj)
}


get_unique_segments <- function(extractedData){
  dataR <- extractedData
  uniqueSegments <- unique(dataR$segment)
  
  return(uniqueSegments)
}

identifyXAverage <- function(rawTraces){ #the x y axis are rotate left 90 degrees, so go based off x axis
  xmin =  min(rawTraces[c(5)])
  xmax =  max(rawTraces[c(5)])
  
  return((xmin + xmax)/2)
}

identifyPlotBounds <- function(polarTraces){
  xmin = 2
  xmax = -2
  ymin = 2
  ymax = -2
  
  for (segment in 1:length(polarTraces)){
    for (trace in 1:length(polarTraces[[segment]])){
      tempxmin <- min(polarTraces[[segment]][[trace]][1,])
      tempxmax <- max(polarTraces[[segment]][[trace]][1,])
      tempymin <- min(polarTraces[[segment]][[trace]][2,])
      tempymax <- max(polarTraces[[segment]][[trace]][2,])
      
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

formating_data <- function(dataOfEachCurveNNj, uniqueSegments, origin.x = .5, scaling.factor = 800/600, x_coor = 0, y_coor = 0){
  polarTraces <- list()
  
  for (segment in 1:length(dataOfEachCurveNNj)){
    listofarrays <- list()
    for (i in 1:length(dataOfEachCurveNNj[[segment]])){ #for each curve
      n_rows <- 4 # no need for slope 
      n_cols <- length(dataOfEachCurveNNj[[segment]][[i]][,1])
      myCurveMatrix <- matrix(0, nrow = n_rows, ncol = n_cols) #each individual 2D array
      
      #adjusted x and y values around the center
      xvalues <- (dataOfEachCurveNNj[[segment]][[i]][c(5)][[1]] * scaling.factor) - (origin.x*scaling.factor) 
      #include parameter for aspect ratio
      yvalues <- 1 - dataOfEachCurveNNj[[segment]][[i]][c(4)][[1]]
      
      
      anglevalues <- list()
      
      #angles in relation to origin (x_coor, y_coor) default (0, 0)
      for (j in 1: n_cols){
        new_angle = atan2((yvalues[[j]] - y_coor), (xvalues[[j]] - x_coor))
        if (new_angle < 0){
          new_angle = new_angle + 2*pi
        }
        anglevalues <- append(anglevalues, new_angle)
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
        myCurveMatrix[3,j] <- anglevalues[[j]] 
        myCurveMatrix[4,j] <- ((yvaluesSorted[[j]] - y_coor)^2 + (xvaluesSorted[[j]] - x_coor)^2)^.5 #radius in relation to (0,0)
        
      }
      
      listofarrays[[i]] <-myCurveMatrix
    }
    polarTraces[[uniqueSegments[[segment]]]] <- listofarrays
  }
  # print("polar traces")
  # print(polarTraces)
  return(polarTraces)
}

find_intersection_with_ray <- function(formatedData, dataOfEachCurveNNj, uniqueSegments, rayIncrement){ #ie compiledList
  
  matrixIntersection <- list()
  
  #start set up unique segment 
  for (segment in 1:length(uniqueSegments)){
    matrixIntersection[[uniqueSegments[[segment]]]] <- matrix(NA, nrow = length(formatedData[[segment]]), ncol = (3.14/rayIncrement))
  }
  #end set up
  count <- 0
   #for each extending radius
    for (angleRay in seq(from=0, to= 3.14, by= rayIncrement)){
      count <- count + 1
      for (segment in 1:length(uniqueSegments)){ #and for each segment
      
       for (individualTrace in seq(length(dataOfEachCurveNNj[[segment]]))){ #for each trace in the segment (alphabetical??)
         if (length(formatedData[[segment]][[individualTrace]][1, all()])-1 > 1){
             for (individualPoint in 1:(length(formatedData[[segment]][[individualTrace]][1, all()])-1)){ #for each point on the trace, find intersection
            #find the right two points the angle falls between for each individual trace
             
             if (angleRay <= formatedData[[segment]][[individualTrace]][3, individualPoint +1]){
               if (angleRay >= formatedData[[segment]][[individualTrace]][3, individualPoint]){ 
                 
                 myIntersection <- calculateIntersection2(
                    (formatedData[[segment]][[individualTrace]][4,individualPoint]), 
                    (formatedData[[segment]][[individualTrace]][4,individualPoint + 1]), 
                    (formatedData[[segment]][[individualTrace]][3,individualPoint]), 
                    angleRay, 
                    (formatedData[[segment]][[individualTrace]][3,individualPoint + 1]) 
                 )
                 
                 matrixIntersection[[segment]][[individualTrace, count]] <- myIntersection
                 break
               }
             }
                 
           }
         }
         
         
     } #end individual trace for loop 
      
    } #end segment for loop
  
  } #end angle ray for loop 
  return(matrixIntersection) # columns for rays
}

find_intersection_with_ray_difference_plot <- function(formatedData, dataOfEachCurveNNj, uniqueSegments, rayIncrement, angle = 1.57){ #ie compiledList
  
  matrixIntersection <- list()
  
  #start set up unique segment 
  for (segment in 1:length(uniqueSegments)){
    matrixIntersection[[uniqueSegments[[segment]]]] <- matrix(NA, nrow = length(formatedData[[segment]]), ncol = 1)
  }
  #end set up
  angleRay = angle
  for (segment in 1:length(uniqueSegments)){ #and for each segment
    
    for (individualTrace in seq(length(dataOfEachCurveNNj[[segment]]))){ #for each trace in the segment (alphabetical??)
      if (length(formatedData[[segment]][[individualTrace]][1, all()])-1 > 1){
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
              
              if (is.na(myIntersection)){
                print("NA FOUND:", angleRay/rayIncrement)
              }else{
                # print(uniqueSegments[[segment]])
                # print(myIntersection)
              }
              matrixIntersection[[uniqueSegments[[segment]]]][[individualTrace, 1]] <- myIntersection
              break
            }
          }
          
        }
      }
      
      
    } #end individual trace for loop 
    
  } #end segment for loop
  # print("matrix intsection RIGHT BEFORE ")
  # print(matrixIntersection)
  return(matrixIntersection) # columns for rays
} 

plotStyleTraces <- function(rawTraces, matrixIntersection, polarTraces, dataOfEachCurveNNj, uniqueSegments, palette = c(),
                            rayIncrement, points.display = FALSE, mean.lines = TRUE, means.styles = c(),
                            bands.fill = TRUE, bands.lines = FALSE, legend.position = "topleft", 
                            standard.deviation.styles = "l", pdf.filename = c(), png.filename = c(), 
                            plot.ticks = FALSE, plot.labels = FALSE, legend.size = 3, transparency = 0.37,
                            bands.linewidth = 0.3, legend.linewidth = 5, means.linewidth = 3, tick.size = 2, 
                            maskCategories = c(), rays = list(), parallelRays =
                              FALSE,
                            quartile_points = FALSE, perpendicularRays = FALSE, h = 1, percentage = 0.5,
                            percentage_front = c(), percentage_back = c(), angle_neg = c(), angle_pos = c(), ray_color = "darkgrey",
                            elbow_color = "black"){
  
  plotbounds <- identifyPlotBounds(polarTraces)
  standardDeviation <- list()
  averagedRX <- list()
  averagedRY <- list()
  #assert each ray x_coor = 0, y_coor = 0, angle = 1.57, col_r = c()
  
  #h>=2
  #min
  num_rays = length(matrixIntersection[[1]][1, all()])
  ray_l_final = num_rays
  ray_m_final = 0
  for (segment in 1:length(uniqueSegments)){
    breakBool = TRUE
    inner = FALSE
    for (ray in 1:length(matrixIntersection[[segment]][1, all()])){
      if (breakBool && sum(!is.na(matrixIntersection[[segment]][, num_rays-ray])) >= h){ 
        breakBool = FALSE
        ray_l = (num_rays - ray)
      }
      if (sum(!is.na(matrixIntersection[[segment]][, ray])) >= h){ 
        if (!inner){
          ray_m = ray
        }
        inner = TRUE
        if (!breakBool){
          break
        }
      }
      
    }
    if (ray_l < ray_l_final){
      ray_l_final = ray_l
    }
    if (ray_m > ray_m_final){
      ray_m_final = ray_m
    }
  }
  
  
  #first segment, doesn't matter
  hx_coor_l = cos(rayIncrement*ray_l_final) * colMeans(matrixIntersection[[1]], na.rm = TRUE)[[ray_l_final]]
  hy_coor_l = sin(rayIncrement*ray_l_final) * colMeans(matrixIntersection[[1]], na.rm = TRUE)[[ray_l_final]]
  
  hx_coor_m = cos(rayIncrement*ray_m_final) * colMeans(matrixIntersection[[1]], na.rm = TRUE)[[ray_m_final]]
  hy_coor_m = sin(rayIncrement*ray_m_final) * colMeans(matrixIntersection[[1]], na.rm = TRUE)[[ray_m_final]]
  
  
  for (segment in 1:length(uniqueSegments)){
    averagedRX[[uniqueSegments[[segment]]]] <- list()
    averagedRY[[uniqueSegments[[segment]]]] <- list()
    
    standardDeviation[[uniqueSegments[[segment]]]] <- list()
    
    for (ray in 1:length(matrixIntersection[[segment]][1, all()])){
      if (!is.na(colMeans(matrixIntersection[[segment]], na.rm = TRUE)[[ray]])){ 
        #as long as there is a mean for each column, store the value
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
  
  #calculating slopes, making sure it matches up
  slopes_segments = list()
  for (segment in 1:length(uniqueSegments)){
    segment_slope <- find_curvature(averagedRX[[uniqueSegments[[segment]]]], averagedRY[[uniqueSegments[[segment]]]])
    slopes_segments[[uniqueSegments[[segment]]]] <- segment_slope
  }
  
  #rename to account for masking
  df <- do.call(rbind, slopes_segments)
  averaged_everything <- colMeans(df)
  # print(averaged_everything)
  
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
      
    }
  }
  
  xPlotAverage <- (plotbounds[[1]] + plotbounds[[2]])/2
  yPlotAverage <- (plotbounds[[3]] + plotbounds[[4]])/2
  
  x_ticks <- c(round(plotbounds[[1]],2), round(xPlotAverage,2), round(plotbounds[[2]],2))
  y_ticks <- c(round(plotbounds[[3]],2), round(yPlotAverage,2), round(plotbounds[[4]],2))
  
  if(plot.labels == TRUE){
    x_ticks_lables <- c(round(plotbounds[[1]],2)*100, round(xPlotAverage,2)*100, round(plotbounds[[2]],2)*100)
    y_ticks_lables <- c(round(plotbounds[[3]],2)*100, round(yPlotAverage,2)*100, round(plotbounds[[4]],2)*100)
  }else{
    x_ticks_lables <- c(NA, NA, NA)
    y_ticks_lables <- c(NA, NA, NA)
  }
  
  if (length(pdf.filename)!=0){
    cairo_pdf(filename = pdf.filename, family = "DejaVu Serif", width = 22 * (plotbounds[[2]] - plotbounds[[1]] - 0.05), height = 22 * (plotbounds[[4]] - plotbounds[[3]] -0.05), onefile = TRUE)
  }
  
  if (length(png.filename)!=0){
    png(filename = png.filename, width = 4000*(plotbounds[[2]] - plotbounds[[1]] - 0.05), height = 4000 * (plotbounds[[4]] - plotbounds[[3]] -0.05), units = "px")
  }
  
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mar = c(0, 4.4, 0, 1.25) + 2)
  
  
  
  plot(1, type = "n", xlab = "", ylab = "", ylim = c(plotbounds[[3]], plotbounds[[4]]), xlim = c(plotbounds[[1]], plotbounds[[2]]), xaxt = "n", yaxt = "n", asp = 1, family = "DejaVu Serif", cex.axis = 20)
  
  if(plot.ticks == TRUE){
    axis(1, at = x_ticks, labels = x_ticks_lables, cex.axis = tick.size)
    axis(2, at = y_ticks, labels = y_ticks_lables, cex.axis = tick.size)
  }else{
    axis(1, at = x_ticks, labels = x_ticks_lables, tck = 0, cex.axis = tick.size)
    axis(2, at = y_ticks, labels = y_ticks_lables, tck = 0, cex.axis = tick.size)
  }
  
  if (length(palette) == 0){
    numberOfColors <- length(uniqueSegments) + 2
    paletteColors <- (brewer.pal(numberOfColors, "Set1"))[2:numberOfColors] #PuRd nice
    
  }else{
    paletteColors <- palette
  }
  
  for (segment in 1:length(uniqueSegments)){
    #Shading
    x1 <- rev(unlist(lapply(xSDLow[[segment]], function(x) na.omit(x)), recursive = TRUE))
    y1 <- rev(unlist(lapply(ySDLow[[segment]], function(x) na.omit(x)), recursive = TRUE))
    
    
    x2 <- rev(unlist(lapply(xSDHigh[[segment]], function(x) na.omit(x)), recursive = TRUE))
    y2 <- rev(unlist(lapply(ySDHigh[[segment]], function(x) na.omit(x)), recursive = TRUE))
    
    #Plot the upper lower standard devation lines
    if (bands.lines == TRUE){
      lines(x1, y1, type = standard.deviation.styles, col = paletteColors[[segment]], lwd = bands.linewidth)#, ylim = c(ymin, ymax), xlim = c(xmin, xmax))
      lines(x2, y2, type = standard.deviation.styles, col = paletteColors[[segment]], lwd = bands.linewidth)
    }
    
    if (bands.fill == TRUE){
      # Create a polygon to shade the region between the arches
      polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = adjustcolor(paletteColors[[segment]], alpha.f = transparency), border = NA)
    }
    
    if (length(means.styles) < length(polarTraces)){
      for (i in 1:(length(polarTraces) - length(means.styles))){
        means.styles = append(means.styles, 1)
      }
    }
    
    if (mean.lines){
      if (!all(is.na(standardDeviation[[segment]]))){
        lines(averagedRX[[segment]], averagedRY[[segment]], type = "l", lty = means.styles[[segment]], col = paletteColors[[segment]] , lwd = means.linewidth) #you could also have black...
      }else{ #differentiating if there is a standard deviation band or not, currently no differation
        lines(averagedRX[[segment]], averagedRY[[segment]], type = "l", lty = means.styles[[segment]], col = paletteColors[[segment]] , lwd = means.linewidth)
      }
    }
    
    if (points.display){
      for (trace in 1:length(polarTraces[[segment]])){
        points(polarTraces[[segment]][[trace]][1, all()], polarTraces[[segment]][[trace]][2, all()], type = "l", col = paletteColors[[segment]], asp = 1)
      }
    }
    
    ltyNumerical = means.styles
    
    numberColumns = 1
    numberColumns = round(length(uniqueSegments)/5)
    if (numberColumns==0){
      numberColumns = 1
    }
    
    if (legend.position == "center"){
      # legend(xPlotAverage, yPlotAverage, legend = uniqueSegments,  col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = 5)
      legend("center", legend = if (length(maskCategories) == 0) uniqueSegments else maskCategories,  col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = legend.linewidth, ncol =  numberColumns)
    } else if (legend.position == "topleft"){
      # legend(plotbounds[[1]], plotbounds[[4]], legend = uniqueSegments, col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = 5)
      legend("topleft", legend = if (length(maskCategories) == 0) uniqueSegments else maskCategories, col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = legend.linewidth, ncol =  numberColumns)
    }else if (legend.position == "bottomright"){
      # legend((xPlotAverage + .5*plotbounds[[2]]), yPlotAverage,  legend = uniqueSegments, col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = 5, ncol = round(length(uniqueSegments)/5))
      legend("bottomright",  legend = if (length(maskCategories) == 0) uniqueSegments else maskCategories, col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = legend.linewidth, ncol =  numberColumns)
    } else if (legend.position == "bottomleft"){
      legend("bottomleft",  legend = if (length(maskCategories) == 0) uniqueSegments else maskCategories, col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = legend.linewidth, ncol =  numberColumns)
    } else if (legend.position == "topright"){
      legend("topright",  legend = if (length(maskCategories) == 0) uniqueSegments else maskCategories, col = paletteColors, cex = legend.size, bty = "n", lty=ltyNumerical, lwd = legend.linewidth, ncol =  numberColumns)
    }
  }
  
  perp_l = -1/averaged_everything[[1]]
  perp_m = -1/averaged_everything[[2]]
  
  
  if (length(rays) > 0) {
    # print("lLENGTH")
    # print(length(rays))
    for (ray in 1:length(rays)){
      col_user = "pink"
      if (rays[[ray]][[4]] != 0){
        col_user = rays[[ray]][[4]]
      }
      rays[[ray]][[4]] = 0
      if (rays[[ray]][[1]] != 0 || rays[[ray]][[2]]!= 0){
        points(rays[[ray]][[1]], rays[[ray]][[2]], col = col_user, pch = 19)
        segments(rays[[ray]][[1]], rays[[ray]][[2]], rays[[ray]][[1]] + 1*cos(rays[[ray]][[3]]), rays[[ray]][[2]] + 1*sin(rays[[ray]][[3]]), col = col_user, lwd = 2, lty = 2)
      }
    }
  }
  
  #not averaged everything, depending on h = 0 min, h = 1 at least 1 point, h = 2, standard deviation so 2nd min from each segment
  #change starting point
  #c(lm_front[2], lm_back[2], lm_front[1], lm_back[1], center_x_split, center_y_split, x_args[index_center/2], y_args[index_center/2], x_args[index_center + (length(x_args) - index_center) * split_point], y_args[index_center + (length(x_args) - index_center) * split_point], x_min, x_max)
  
  #centered intersection point
  x_int <- (averaged_everything[[4]] - averaged_everything[[3]]) / (averaged_everything[[1]] - averaged_everything[[2]])
  y_int <- averaged_everything[[1]]*x_int + averaged_everything[[3]]
  
  #farthest point
  if (h == 0){
    start_xl = min(df[, 11])
    y_xl = df[which.min(df[, 11]), 13]
    b2 <- y_xl - perp_l * start_xl
    
    x_adjusted_l <- (b2 - averaged_everything[[3]]) / (averaged_everything[[1]] - perp_l)
    y_adjusted_l <- averaged_everything[[1]] * x_adjusted_l +  averaged_everything[[3]]
    
    end_xm = max(df[, 12])
    y_xm = df[which.max(df[, 12]), 14]
    b2 <- y_xm - perp_m * end_xm
    x_adjusted_m <- (b2 - averaged_everything[[4]]) / (averaged_everything[[2]] - perp_m)
    y_adjusted_m <- averaged_everything[[2]] * x_adjusted_m +  averaged_everything[[4]]
    
  }else {
    start_xl = hx_coor_l
    y_xl = hy_coor_l #add in respective x coor
    b2 <- y_xl - perp_l * start_xl
    
    x_adjusted_l <- (b2 - averaged_everything[[3]]) / (averaged_everything[[1]] - perp_l)
    # print("adjustedl")
    # print(x_adjusted_l)
    y_adjusted_l <- averaged_everything[[1]] * x_adjusted_l +  averaged_everything[[3]]
    
    end_xm = hx_coor_m
    y_xm = hy_coor_m
    b2 <- y_xm - perp_m * end_xm
    x_adjusted_m <- (b2 - averaged_everything[[4]]) / (averaged_everything[[2]] - perp_m)
    y_adjusted_m <- averaged_everything[[2]] * x_adjusted_m +  averaged_everything[[4]]
  }
  
  if (parallelRays){
    segments(x_adjusted_l, y_adjusted_l, x_int, y_int, col = "black", lwd = 2, lty = 2)
    segments(x_int, y_int, x_adjusted_m, y_adjusted_m, col = "black", lwd = 2, lty = 2)
  }
  
  front_x_l = list()
  front_x_org = list()
  front_y_l = list()
  front_y_org = list()
  x_max = max(df[,12])
  
  neg_adjusted_angle = c()
  if (length(percentage_front)!=0){
    for (p in 1:length(percentage_front)){
      on_x_l = x_int + (x_adjusted_l - x_int)*percentage_front[[p]]
      on_y_l = y_int + (y_adjusted_l - y_int)*percentage_front[[p]]
      y_target_l = on_y_l + perp_l*(x_max - on_x_l)
      # print("angle perpedicular valuee")
      # print((atan(perp_l) + pi))
      up_l = ray_up(rawTraces, x_coor = x_max, y_coor = y_target_l, angle = (atan(perp_l) + pi))
      # print("maximum x is")
      # print(max(df[,12]))
      # print(y_target_l)
      x_1l = x_max + cos((atan(perp_l) + pi)) * up_l
      y_1l = y_target_l + sin((atan(perp_l) + pi)) * up_l
      front_x_l = append(front_x_l, x_1l)
      front_x_org = append(front_x_org, x_max)
      front_y_l = append(front_y_l, y_1l)
      front_y_org = append(front_y_org, y_target_l)
      points(x_1l,  y_1l, col = ray_color, pch = 19)
      
      adjusted_angle = 0
      if (length(angle_neg)!=0){
        adjusted_angle = (angle_neg[[p]])*pi/180
      }
      adjusted_angle = -adjusted_angle + atan(perp_l) + pi 
      neg_adjusted_angle = append(neg_adjusted_angle, adjusted_angle)
      # print("ADJUSTED ANGLEEE")
      # print(adjusted_angle)
      # print(tan(adjusted_angle))
      # print(y_1l)
      # print(y_1l + (tan(adjusted_angle))*(x_1l - averaged_everything[[11]]))
      end_coordinate = averaged_everything[[11]]
      sign_switch = FALSE
      if (adjusted_angle > pi){
        adjusted_angle = (adjusted_angle - pi)
        sign_switch = TRUE
      }
      if (tan(adjusted_angle) > 0){
        if (!sign_switch){
          end_coordinate = averaged_everything[[12]]
        }
      }
      segments(x_1l,  y_1l, end_coordinate,  y_1l - ((tan(adjusted_angle))*(x_1l - end_coordinate)), col = ray_color, lwd = 2, lty = 2)
      # print(pairwise_comparison(rawTraces, x_coor = x_1l, y_coor = y_1l, angle = atan(perp_l + pi), mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
    }
  }
  
  back_x_m = list()
  back_x_org = list()
  back_y_m = list()
  back_y_org = list()
  x_min = min(df[,11])
  
  pos_adjusted_angle = c()
  if (length(percentage_back)!=0){
    for (p in 1:length(percentage_back)){
      on_x_m = x_int + (x_adjusted_m - x_int)*percentage_back[[p]]
      on_y_m = y_int + (y_adjusted_m - y_int)*percentage_back[[p]]
      y_target_m = on_y_m + perp_m*(x_min - on_x_m)
      up_m = ray_up(rawTraces, x_coor = x_min, y_coor = y_target_m, angle = atan(perp_m))
      x_1m = x_min + cos(atan(perp_m)) * up_m
      y_1m = y_target_m + sin(atan(perp_m)) * up_m
      back_x_m = append(back_x_m, x_1m)
      back_x_org = append(back_x_org, x_min)
      back_y_m = append(back_y_m, y_1m)
      back_y_org = append(back_y_org, y_target_m)
      points(x_1m, y_1m, col = ray_color, pch = 19)
      
      adjusted_angle = 0
      if (length(angle_pos)!=0){
        adjusted_angle = (angle_pos[[p]])*pi/180
      }
      adjusted_angle = -adjusted_angle + atan(perp_m) 
      pos_adjusted_angle = append(pos_adjusted_angle, adjusted_angle)
      # print("POS")
      # print(adjusted_angle)
      # print(tan(adjusted_angle))
      end_coordinate = averaged_everything[[12]]
      sign_switch = FALSE
      if (adjusted_angle < 0){
        adjusted_angle = (adjusted_angle + pi)
        sign_switch = TRUE
      }
      if (tan(adjusted_angle) < 0){
        if (!sign_switch){
          end_coordinate = averaged_everything[[11]]
        }
      }
      
      segments(x_1m, y_1m, end_coordinate, y_1m + (tan(adjusted_angle))*(end_coordinate - x_1m), col = ray_color, lwd = 2, lty = 2)
      # print(pairwise_comparison(rawTraces, x_coor = x_1m, y_coor = y_1m, angle = atan(perp_m), mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
      
    }
  }
  
  # points(on_x_l, on_y_l, col = "red", pch = 19)
  # points(on_x_m, on_y_m, col = "red", pch = 19)
  #points(x_int, y_target_l, col = "red", pch = 19)
  #points(x_int, y_target_m, col = "red", pch = 19)
  
  #   #add in parameter
  #   # points(averaged_everything[[7]], averaged_everything[[8]], col = "purple", pch = 19, cex = 1.5)
  #   # points(averaged_everything[[9]], averaged_everything[[10]], col = "purple", pch = 19, cex = 1.5)
  
  if (quartile_points){
    for (segment in 1:length(uniqueSegments)){
      # points(slopes_segments[[uniqueSegments[[segment]]]][[7]], slopes_segments[[segment]][[8]], col = "red", pch = 19)
      # points(slopes_segments[[uniqueSegments[[segment]]]][[9]], slopes_segments[[segment]][[10]], col = "blue", pch = 19)
      points(slopes_segments[[uniqueSegments[[segment]]]][[5]], slopes_segments[[segment]][[6]], col = elbow_color, pch = 19, cex = .8)
    }
  }
  
  # print(pairwise_comparison(rawTraces, x_coor = max_xl, y_coor = min_yl, angle = (atan(perp_l) + pi), mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
  # 
  # print(pairwise_comparison(rawTraces, x_coor = min_xm, y_coor = min_ym, angle = atan(perp_m), mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
  #printing info to console after dev.off()
  
  if (length(rays) > 0){
    for (user_ray in 1:length(rays)){
      print("user specified")
      print(user_ray)
      print(pairwise_comparison(rawTraces, x_coor = rays[[user_ray]][[1]], y_coor = rays[[user_ray]][[2]], angle = rays[[user_ray]][[3]], mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
    }
  }
  
  if (length(percentage_front)!=0){
    for (p in 1:length(percentage_front)){
      print("PAIRWISE COMPARISON NEGATIVE ANGLE")
      print(neg_adjusted_angle[[p]])
      #points(front_x_l[[p]], front_y_l[[p]], col = "red")
      # print("X Y COORDINATE")
      # print(front_x_l[[p]])
      # print(front_y_l[[p]])
      print("percentage along")
      print(percentage_front[[p]])
      # print(pairwise_comparison(rawTraces, x_coor = front_x_l[[p]], y_coor = front_y_l[[p]], angle = neg_adjusted_angle[[p]], mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
      print(pairwise_comparison(rawTraces, x_coor = front_x_org[[p]], y_coor = front_y_org[[p]], angle = neg_adjusted_angle[[p]], mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
    }
  }
  
  if (length(percentage_back)!=0){
    for (p in 1:length(percentage_back)){
      print("PAIRWISE COMPARISON POSITIVE ANGLE")
      print(pos_adjusted_angle[[p]])
      print(percentage_back[[p]])
      print(pairwise_comparison(rawTraces, x_coor = back_x_org[[p]], y_coor = back_y_org[[p]], angle = pos_adjusted_angle[[p]], mask = maskCategories, paletteC = paletteColors, pdf_filename = pdf.filename))
    }
  }
  
  
  if(length(pdf.filename)!=0 || length(png.filename)!=0 ){
    dev.off()
  }
  
  return(slopes_segments)
}

makeTracesPolar <- function(rawTraces, origin.algorithm = "BottomMiddle", origin.x = NA, scaling.factor = 800/600, x_coor = 0, y_coor = 0){
  uniqueSegments <- get_unique_segments(rawTraces)
  dataOfEachCurveNNj <- read_in_data(rawTraces)
  xaverage <- identifyXAverage(rawTraces)
  
  
  if (origin.algorithm == "BottomMiddle"){
    if (is.na(origin.x)){
      origin.x = .5
    }
  }else if (origin.algorithm == "BottomMean"){
    origin.x = xaverage
  }
  
  polarTraces <- formating_data(dataOfEachCurveNNj, uniqueSegments, origin.x = origin.x, scaling.factor = scaling.factor, x_coor = x_coor, y_coor = y_coor)
  return(polarTraces)
}

filteringRawTraces <- function(rawTraces, tiernameAll = c(NA), categoriesAll = list(c(NA)), layersAll = c(NA), mergeCategories = c(FALSE)){
  
  expandedTraces <- rawTraces %>% unnest(c(segment, tiers_list))
  
  if (inherits(categoriesAll, "character") || inherits(categoriesAll, "NULL")){
    categoriesAll <- list(categoriesAll)
  }
  #padding 
  for (i in 1:length(categoriesAll)){
    if (length(tiernameAll) < length(categoriesAll)){
      tiernameAll <- append(tiernameAll, tiernameAll[[1]])
    }
    if (length(layersAll) < length(categoriesAll)){
      layersAll <- append(layersAll, layersAll[[1]])
    }
    if (length(mergeCategories) < length(categoriesAll)){
      mergeCategories <- append(mergeCategories, mergeCategories[[1]])
    }
  }
  loopLength <- max(length(tiernameAll), length(categoriesAll))
  
  filteredTraces <- data.frame()
  #filtering
  for (item in 1:loopLength){
    temporaryTraces <- expandedTraces
    
    if (!is.na(layersAll[[item]])[[1]]){
      temporaryTraces <- temporaryTraces[sapply(expandedTraces$layer, function(x) any(layersAll[[item]] %in% x)),]
      if (nrow(temporaryTraces) == 0){
        message(paste("layer " , layersAll[[item]], " does not exist in dataset"))
        next
      }
    }
    if (!is.na(tiernameAll[[item]])[[1]]){
      temporaryTraces <- temporaryTraces[sapply(temporaryTraces$tiers_list, function(x) any(tiernameAll[[item]] %in% x)),]
      if (nrow(temporaryTraces) == 0){
        message(paste("tiername " , tiernameAll[[item]], " does not exist within layer ", layersAll[[item]]))
        next
      }
    }
    if (!is.na(categoriesAll[[item]])[[1]]){
      temporaryTraces <- temporaryTraces[sapply(temporaryTraces$segment, function(x) any(categoriesAll[[item]] %in% x)),]
      if (nrow(temporaryTraces) == 0){
        message(paste("segment " , categoriesAll[[item]], " does not exist within toer ", tiernameAll[[item]]))
        next
      }
    }
    if (!( (is.na(layersAll[[item]])[[1]] && is.na(tiernameAll[[item]])[[1]]) && is.na(categoriesAll[[item]])[[1]]) ){
      if (mergeCategories[[item]] == TRUE){
        if (!is.na(categoriesAll[[item]])[[1]]){
          myVowelType <- paste(categoriesAll[[item]], collapse = "")
        }else{
          myVowelType <- layersAll[[item]] #or something else
        }
        temporaryTraces$segment <- myVowelType
        filteredTraces <- rbind(filteredTraces, temporaryTraces)
        
      }else{
        filteredTraces <- rbind(filteredTraces, temporaryTraces)
      }
    }
  }
  return(data.frame(filteredTraces))
}

ray_up <- function(filteredTraces, interval = 1, singleIncrements = TRUE,  origin.algorithm = "BottomMiddle", origin.x = NA,
                   scaling.factor = 800/600, x_coor, y_coor, angle, mask = c(), paletteC = c()){
  
  polarTraces <- makeTracesPolar(filteredTraces, origin.algorithm, origin.x, scaling.factor, x_coor, y_coor)
  
  rayIncrement = 3.14159/180 * interval 
  uniqueSegments <- get_unique_segments(filteredTraces)
  dataOfEachCurveNNj <- read_in_data(filteredTraces)
  
  matrixIntersection <- find_intersection_with_ray_difference_plot(polarTraces, dataOfEachCurveNNj, uniqueSegments, rayIncrement, angle)
  differences <- sapply(matrixIntersection, function(x) t(x))
  
  values <- c()
  categories <- c()
  
  remove_categories <- c()
  for (category in 1:(length(names(differences)))){
    if (length(differences[[category]]) == sum(is.na(differences[[category]]))){
      remove_categories <- append(remove_categories, category)
    }
  }
  
  differences2 <- data.frame()
  if (length(remove_categories) != 0){
    differences2 <- differences[-(remove_categories)]
    if (length(mask) != 0){
      mask <- mask[-(remove_categories)]
    }
  }else{
    differences2 <- differences
  }
  
  values <- as.vector(unlist(differences2))
  
  return(min(values, na.rm = TRUE) - 0.015) 
}


find_curvature <- function(xargs, yargs, split_point = 0.5){
  
  x_args <- rev(xargs[!is.na(xargs)])
  y_args <- rev(yargs[!is.na(yargs)])
  
  
  chordlength <- ceiling(length(x_args)/2)
  distances <- list()
  
  for (point in 1:(length(x_args) - chordlength)){
    x_1 = x_args[point]
    x_2 = x_args[point + chordlength]
    y_1 = y_args[point]
    y_2 = y_args[point + chordlength]
    x_middle = (x_1 + x_2)/2
    y_middle = (y_1 + y_2)/2
    
    curve_x = x_args[point + chordlength/2]
    curve_y = y_args[point + chordlength/2]
    
    distance <- ((curve_x - x_middle)^2 + (curve_y - y_middle)^2)^(1/2)
    distances <- append(distances, distance)
  }
  distances <- unlist(distances)
  
  split_index <- which(distances == max(distances))
  x_split_less = x_args[1:(split_index + chordlength/2)]
  x_split_more = x_args[(split_index + chordlength/2 + 1):length(x_args)]
  
  y_split_less = y_args[1:(split_index + chordlength/2)]
  y_split_more = y_args[(split_index + chordlength/2 + 1):length(y_args)]
  
  lm_front <- coef(lm(y_split_less ~ x_split_less))
  lm_back <- coef(lm(y_split_more ~ x_split_more))
  # print(x_args)
  center_x_split <- x_args[split_index + chordlength/2]
  center_y_split <- y_args[split_index + chordlength/2]
  index_center <- split_index + chordlength/2
  
  
  x_min <- x_args[1]
  y_min <- y_args[1]
  x_max <- x_args[length(x_args)]
  y_max <- y_args[length(y_args)]
  #double check x_indexes
  #can improve this to return "percentages" along the line
  return(c(lm_front[2], lm_back[2], lm_front[1], lm_back[1], center_x_split, center_y_split, x_args[index_center/2], y_args[index_center/2], x_args[index_center + (length(x_args) - index_center) * split_point], y_args[index_center + (length(x_args) - index_center) * split_point], x_min, x_max, y_min, y_max))
  
}

plotTraces <- function(rawTraces, polarTraces = "", tiernameAll = c(NA), categoriesAll = list(c(NA)), layersAll = c(NA), mergeCategories = c(FALSE), origin.algorithm = "BottomMiddle", origin.x = NA,
                       scaling.factor = 800/600, 
                       interval = 1, mean.lines = TRUE, points.display = FALSE,
                       palette = c(), bands.lines = FALSE, bands.fill = TRUE, legend.position = "topleft",
                       means.styles = c(), standard.deviation.styles = "l", plot.ticks = FALSE, plot.labels = FALSE,
                       legend.size = 3, transparency = 0.37, pdf.filename = c(), bands.linewidth = 0.3,
                       png.filename = c(), legend.linewidth = 5, means.linewidth = 3, tick.size = 2,
                       maskCategories = c(), rays = list(), parallelRays = FALSE,
                       quartile_points = FALSE, perpendicularRays = FALSE, h = 1, percentage = 0.5, percentage_front = c(),
                       percentage_back = c(), angle_neg = c(), angle_pos = c(), ray_color = "darkgrey", elbow_color = "black"){
  
  if (typeof(polarTraces) == "character"){
    rawTraces <- filteringRawTraces(rawTraces, tiernameAll, categoriesAll, layersAll, mergeCategories)
    rawTraces <- rawTraces %>% select(-tiers_list, -layer) 
    polarTraces <- makeTracesPolar(rawTraces, origin.algorithm, origin.x, scaling.factor)
  }
  
  rayIncrement = 3.14159/180 * interval
  
  uniqueSegments <- get_unique_segments(rawTraces)
  dataOfEachCurveNNj <- read_in_data(rawTraces)
  
  matrixIntersection <- find_intersection_with_ray(polarTraces, dataOfEachCurveNNj, uniqueSegments, rayIncrement)
  
  rx <- plotStyleTraces(rawTraces = rawTraces, matrixIntersection = matrixIntersection, polarTraces = polarTraces, 
                        dataOfEachCurveNNj = dataOfEachCurveNNj, uniqueSegments = uniqueSegments, 
                        rayIncrement = rayIncrement, mean.lines = mean.lines, points.display = points.display,
                        palette = palette, bands.lines = bands.lines, legend.position = legend.position, 
                        bands.fill = bands.fill, means.styles = means.styles, standard.deviation.styles = standard.deviation.styles,
                        plot.ticks = plot.ticks, legend.size = legend.size, transparency = transparency, pdf.filename = pdf.filename,
                        bands.linewidth = bands.linewidth, plot.labels = plot.labels, png.filename = png.filename, 
                        legend.linewidth = legend.linewidth, means.linewidth = means.linewidth, tick.size = tick.size,
                        maskCategories = maskCategories, rays = rays, parallelRays = parallelRays, quartile_points =
                          quartile_points, perpendicularRays = perpendicularRays, h = h, percentage = percentage, 
                        percentage_front = percentage_front, percentage_back = percentage_back, angle_neg = angle_neg, angle_pos = angle_pos,
                        ray_color = ray_color, elbow_color = elbow_color)
  #return(rx)
  return(rawTraces)
}

differencePlot <- function(filteredTraces, origin.algorithm = "BottomMiddle", origin.x = NA, scaling.factor = 800/600, x_coor = 0, y_coor = 0, interval = 1, singleIncrements = TRUE){
  
  polarTraces <- makeTracesPolar(filteredTraces, origin.algorithm, origin.x, scaling.factor, x_coor, y_coor)
  
  rayIncrement = 3.14159/180 * interval 
  uniqueSegments <- get_unique_segments(filteredTraces)
  dataOfEachCurveNNj <- read_in_data(filteredTraces)
  
  matrixIntersection <- find_intersection_with_ray(polarTraces, dataOfEachCurveNNj, uniqueSegments, rayIncrement)
  
  meanDifference <- list()
  standardDeviation <- list()
  redLine <- list()
  rayIncrement <- 1
  plotbounds <- identifyPlotBounds(polarTraces)
  
  for (ray in 1:length(matrixIntersection[[1]][1, all()])){
    #[[1]] and [[2]] for matrixIntersection ONLY
    if (!is.na(colMeans(matrixIntersection[[1]], na.rm = TRUE)[[ray]]) && !is.na(colMeans(matrixIntersection[[2]], na.rm = TRUE)[[ray]]) ){ 
      #both are not empty
      segment1 <- (matrixIntersection[[1]][all(), ray])[!is.na(matrixIntersection[[1]][all(), ray])]
      segment2 <- (matrixIntersection[[2]][all(), ray])[!is.na(matrixIntersection[[2]][all(), ray])]
      
      differences <- list()
      #4 by 4 x grid
      for (item1 in 1:length(segment1)){
        for (item2 in 1:length(segment2)){
          differences <- append(differences, segment1[item1] - segment2[item2])
        }
      }
      differences <- unlist(differences)
      meanDifference <- append(meanDifference, mean(differences))
      standardDeviation <- append(standardDeviation, sd(differences))
      redLine <- append(redLine, mean(differences)/(sd(differences)))
      
    }else{
      #for consistency when matching up with the angle loop
      meanDifference <- append(meanDifference, NA)
      standardDeviation <- append(standardDeviation, NA)
      redLine <- append(redLine, NA)
      
    }
    differences <- list() #reset/clear for next angle ray
  }
  meanX <- list()
  for (ray in 1:length(matrixIntersection[[1]][1, all()])){
    meanX <- append(meanX, cos(rayIncrement*ray * pi/180))
  }
  #plot label should be this cos converted to degrees.
  #so times 180/pi
  
  xSDHigh <- list()
  xSDLow <- list()
  ySDHigh <- list()
  ySDLow <- list()  
  
  for (ray in 1:length(matrixIntersection[[1]][1, all()])){
    
    xSDHigh<- append(xSDHigh, cos(rayIncrement*ray*pi/180)*(standardDeviation[[ray]]) + meanX[[ray]])
    xSDLow <-  append(xSDLow, cos(rayIncrement*ray*pi/180)*(-standardDeviation[[ray]]) + meanX[[ray]])
    
    ySDHigh <- append(ySDHigh, sin(rayIncrement*ray*pi/180)*(meanDifference[[ray]] + standardDeviation[[ray]]))
    ySDLow <-  append(ySDLow, sin(rayIncrement*ray*pi/180)*(meanDifference[[ray]] - standardDeviation[[ray]]))
    
  }
  
  x1 <- rev(unlist(lapply(xSDLow, function(x) na.omit(x)), recursive = TRUE))
  y1 <- rev(unlist(lapply(ySDLow, function(x) na.omit(x)), recursive = TRUE))
  
  
  x2 <- rev(unlist(lapply(xSDHigh, function(x) na.omit(x)), recursive = TRUE))
  y2 <- rev(unlist(lapply(ySDHigh, function(x) na.omit(x)), recursive = TRUE))
  
  ZscoreRange <- max(na.omit(unlist(redLine))) - min(na.omit(unlist(redLine)))
  #scales Z score line to fit the range of plot (mean difference line)
  scalingFactor <- ZscoreRange/(max(y2) - min(y1))
  #+ additional range (if there are more negative z scores)
  
  redLine2 <- lapply(unlist(redLine), function(x) x/scalingFactor)
  additionalRoom <- max(y2) - max(na.omit(unlist(redLine2)))
  if (additionalRoom < 0){
    additionalRoom <- 0
  }
  additionalRoom2 <- min(na.omit(unlist(redLine2))) - min(y1)
  if (additionalRoom2 < 0){
    additionalRoom2 <- 0
  }
  scalingFactor <- ZscoreRange/(max(y2) - min(y1) - additionalRoom - additionalRoom2 - 0.02)
  redLine <- lapply(unlist(redLine), function(x) x/scalingFactor)
  
  axisX <- lapply(lapply(meanX, function(x) x*180/pi), function(x) round(x))
  
  #nicely distributed and readable z scores, 5 labels
  ZScorePositioning <- list()
  ZScorePositioning <- append(ZScorePositioning, 0)
  ZScorePositioning <- append(ZScorePositioning, min(na.omit(unlist(redLine))))
  ZScorePositioning <- append(ZScorePositioning, min(na.omit(unlist(redLine)))/2)
  ZScorePositioning <- append(ZScorePositioning, max(na.omit(unlist(redLine))))
  ZScorePositioning <- append(ZScorePositioning, max(na.omit(unlist(redLine)))/2)
  
  ZScoreLabels <- list()
  ZScoreLabels <- lapply(ZScorePositioning, function(x) round(x*scalingFactor,1))
  
  #increments of one, just need to scale to red line
  additionalLightLines <- list()
  for (i in seq(round(min(unlist(ZScoreLabels))), round(max(unlist(ZScoreLabels))),1)){
    additionalLightLines <- append(additionalLightLines, i/scalingFactor)
  }
  
  #starting plotting
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #par(pty = "s")
  par(mar = c(4, 4.4, 1, 3) + 2)
  
  plot(meanX, meanDifference, type = "l", col = "blue", lwd = 2, ylim = c(min(y1), max(y2)), xlim = c(min(x1), max(x2)), xaxt = "n", xlab = "angle from origin", ylab = "mean difference")
  #labelling x axis with degree from origin
  axis(1, at = meanX, labels = axisX, tck = 0)
  axis(side=4, at = ZScorePositioning, labels=ZScoreLabels, col="red", col.axis="black")
  mtext("z score", side = 4, line = 3)
  
  lines(x1, y1, col = "blue", type = "l", lwd = 1)
  lines(x2, y2, col = "blue", type = "l", lwd = 1)
  polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = adjustcolor( "blue", alpha.f = 0.37), border = NA)
  
  lines(meanX, unlist(redLine), col = "red", lwd = 2)
  abline(h=0.0, lty = 2)
  abline(h= ZScorePositioning, col="red", lty="dotted")
  
  #increments of one for z score
  if (singleIncrements){
    for (line in 1:length(additionalLightLines)){
      abline(h = additionalLightLines[[line]], col = "pink", lty="dotted", lwd = 1)
    }
  }
  
  #vertical lines at min and max of z score
  indices <- which(unlist(redLine) %in% min(unlist(redLine), na.rm = TRUE))
  abline(v = unlist(meanX)[[indices]], col = "red")
  indices <- which(unlist(redLine) %in% max(unlist(redLine), na.rm = TRUE))
  abline(v = unlist(meanX)[[indices]], col = "red")
  
}

pairwise_comparison <- function(filteredTraces, interval = 1, singleIncrements = TRUE,  origin.algorithm = "BottomMiddle", origin.x = NA,
                                scaling.factor = 800/600, x_coor = 0, y_coor = 0, angle = 1, mask = c(), paletteC = c(), pdf_filename = c()){
  
  polarTraces <- makeTracesPolar(filteredTraces, origin.algorithm, origin.x, scaling.factor, x_coor, y_coor)
  
  rayIncrement = 3.14159/180 * interval 
  uniqueSegments <- get_unique_segments(filteredTraces)
  dataOfEachCurveNNj <- read_in_data(filteredTraces)
  
  matrixIntersection <- find_intersection_with_ray_difference_plot(polarTraces, dataOfEachCurveNNj, uniqueSegments, rayIncrement, angle)
  # print("matrix intsection AFTERWARDS pairwise")
  # print(matrixIntersection)
  differences <- sapply(matrixIntersection, function(x) t(x))
  
  values <- c()
  categories <- c()
  
  remove_categories <- c()
  for (category in 1:(length(names(differences)))){
    if (length(differences[[category]]) <= sum(is.na(differences[[category]])) + 1){
      remove_categories <- append(remove_categories, category)
    }
  }
  
  differences2 <- data.frame()
  if (length(remove_categories) != 0){
    differences2 <- differences[-(remove_categories)]
    if (length(mask) != 0){
      mask <- mask[-(remove_categories)]
      if (length(paletteC) > 0){
        paletteC <- paletteC[-(remove_categories)]
      }
    }
  }else{
    differences2 <- differences
  }
  
  values <- as.vector(unlist(differences2))
  values <- (values - min(values, na.rm = TRUE)) #/(max(values, na.rm = TRUE) - min(values, na.rm = TRUE)))
  number_col <- as.vector(sapply(differences2, function(x) length(x)))
  
  for (category in 1:(length(names(differences2)))){
    if (length(mask) != 0){
      categories <- append(categories, sapply(mask[[category]], function(x) rep(x, each = number_col[[category]])))
    }else{
      categories <- append(categories, sapply(names(differences2)[[category]], function(x) rep(x, each = number_col[[category]])))
    }
    
  }
  
  intersections_grouped <- data.frame(values, categories)
  intersections_grouped = na.omit(intersections_grouped)
  print(intersections_grouped)
  #print(intersections_grouped)
  pairwise_results <- pairwise.t.test(intersections_grouped$values, intersections_grouped$categories, paired = FALSE, pool.sd = FALSE, p.adjust.method = "bonferroni")
  
  
  #null hypothesis -- all group means are equal
  distances <- intersections_grouped$values
  segments <- intersections_grouped$categories
  #anova_results <- aov(distances ~ segments)
  #Turkey HSD differences in means should not be 0
  anova_fit <- lm(distances ~ segments)
  posthoc <- TukeyHSD(aov(anova_fit))
  plot(posthoc, cex.axis = .6)
  #summary(anova_fit) model to see which vowels significant in predicting category -- not rlly wanted? 
  #anova_model <- lm(values ~ categories, data = intersections_grouped)
  #turkey_results <- TurkeyHSD(anova_results)
  data2 <- data.frame(
    Groups= segments,
    Values= distances)
  
  #data2 <- data2[order(data2$Values), ]
  # data2$ValuesN <- (data2$Values - min(data2$Values))/(max(data2$Values) - min(data2$Values))
  # print("minimum")
  
  #standard ggplot not bvery good, significance levels overlap
  #gbox <- ggplot(data2, aes(x=Groups, y=Values)) + geom_boxplot()
  #  limits <- gbox + geom_signif(data=data2, comparisons = list(c("a","i"), c("i","u"), c("a", "u")), map_signif_level = TRUE)
  # print(limits)
  combos = t(combn(unique(segments),2))
  
  
  #print(combos)
  
  # for (combo in 1:nrow(combos)){
  #   combos_list <- append(combos_list, c(combos[combo, 1], combos[combo, 2]))
  # }
  
  combos_list <- matrix(t(combos), 2)
  paired_matrix <- matrix(combos_list, nrow = 2)
  combos_list <- as.list(as.data.frame(paired_matrix))
  #print(combos_list[1:5])
  
  # stat_compare_means(comparisons = combos_list, label = "p.signif", na.rm = TRUE, method = "t.test", p.adjust.method = "bonferroni",
  #                   var.equal = FALSE, ref.group = 0.5)
  # stat_compare_means(comparisons = combos_list, label = "p.signif", method = "t.test",
  #                      ref.group = "0.5")
  
  
  comparisons_plot = compare_means(Values ~ Groups, data = data2, 
                                   method = "t.test",
                                   paired = FALSE,
                                   var.equal = FALSE,
                                   pool.sd = FALSE, 
                                   p.adjust.method = "bonferroni")
  
  print(comparisons_plot)
  #comparisons_plot <- comparisons_plot %>% mutate(p.adj.signif = signif_stars(p.adj))
  #comparisons_plot <- comparisons_plot %>% add_xy_position(x = "Groups")
  comparisons_plot <- comparisons_plot %>% add_significance(p.col = "p.adj", output.col = "p.adj.signif")
  comparisons_plot <- comparisons_plot %>% add_y_position(data = data2, formula = Values ~ Groups)
  pAdj <- ggboxplot(data2, x = "Groups", y = "Values", color = "black") +
    stat_pvalue_manual(
      comparisons_plot,
      label = "p.adj.signif"
    )
  print(pAdj)
  
  
  # p <- ggboxplot(data2, x = "Groups", y = "Values", color = "black") +
  #   stat_compare_means(
  #     comparisons = combos_list,
  #     method = "t.test",
  #     paired = FALSE,
  #     var.equal = FALSE,
  #     aes(label=..p.adj..),
  #     pool.sd = FALSE,
  #     p.adjust.method = "bonferroni"
  #   )
  # 
  # print(p)
  #print(compare_means(Values ~ Groups,  data = data2, method = "t.test"))
  
  # fiddle <- ggviolin(data2, x = "Groups", y = "Values", fill = "Groups", color = "Groups", palette = paletteC, alpha = 0.37,
  #        add = "boxplot", add.params = list(fill = "white"))+
  #   guides(fill = guide_legend(override.aes = list(shape = 22, size = 6, color = NA)),
  #   color = "none"        # hide color legend (redundant)
  # )+
  # stat_compare_means(
  #     comparisons = combos_list,
  #     method = "t.test",
  #     paired = FALSE,
  #     var.equal = FALSE,
  #     aes(label=..p.adj..),
  #     pool.sd = FALSE, 
  #     p.adjust.method = "bonferroni"
  #   ) 
  fiddle <- ggviolin(data2, x = "Groups", y = "Values", fill = "Groups", color = "Groups", palette = paletteC, alpha = 0.37,
                     add = "boxplot", add.params = list(fill = "white"))+
    guides(fill = guide_legend(override.aes = list(shape = 22, size = 6, colour = paletteC )),
           color = "none"        # hide color legend (redundant)
    )+
    stat_pvalue_manual(
      comparisons_plot,
      label = "p.adj.signif"
    )
  
  print(fiddle)
  #list(c("a","i"), c("i","u"), c("y","ɯ"), c("ɯ","u"))
  
  #boxplot(distances ~ segments)
  #qqnorm(distances) #would be nice to group by segments
  #summary(anova_results) p-value for whether some of the groups have signifanct mean differences
  return(pairwise_results) #or anova(anova_fit)
}
