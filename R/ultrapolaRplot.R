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
            
            xyFileData <- (metaData$traces)$tongue$files[[plainTextname]][[frameNumber]]
            
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

formating_data <- function(dataOfEachCurveNNj, uniqueSegments, origin.x = .5, scaling.factor = 800/600){
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
    polarTraces[[uniqueSegments[[segment]]]] <- listofarrays
  }
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

plotStyleTraces <- function(matrixIntersection, polarTraces, dataOfEachCurveNNj, uniqueSegments, palette = c(),
                            rayIncrement, points.display = FALSE, mean.lines = TRUE, means.styles = c(),
                            bands.fill = TRUE, bands.lines = FALSE, legend.position = "topleft", 
                            standard.deviation.styles = "l", pdf.filename = c(), png.filename = c(), 
                            plot.ticks = FALSE, plot.labels = FALSE, legend.size = 3, transparency = 0.37,
                            bands.linewidth = 0.3, legend.linewidth = 5, means.linewidth = 3, tick.size = 2, 
                            maskCategories = c()){
  
  plotbounds <- identifyPlotBounds(polarTraces)
  
  standardDeviation <- list()
  averagedRX <- list()
  averagedRY <- list()
  
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
  #else {
  #still have "ticks" just way out of bounds so that they don't show up
  # x_ticks <- c(round(plotbounds[[1]],2) + 3, round(xPlotAverage,2) + 3, round(plotbounds[[2]],2) + 3)
  # y_ticks <- c(round(plotbounds[[3]],2) + 2, round(yPlotAverage,2) + 2, round(plotbounds[[4]],2) + 2)
  
  if(plot.labels == TRUE){
    x_ticks_lables <- c(round(plotbounds[[1]],2)*100, round(xPlotAverage,2)*100, round(plotbounds[[2]],2)*100)
    y_ticks_lables <- c(round(plotbounds[[3]],2)*100, round(yPlotAverage,2)*100, round(plotbounds[[4]],2)*100)
  }else{
    x_ticks_lables <- c(NA, NA, NA)
    y_ticks_lables <- c(NA, NA, NA)
  }
  
  if (length(pdf.filename)!=0){
    #pdf.options(encoding="MacRoman")
    #pdf.options(encoding = "utf-8")
    cairo_pdf(filename = pdf.filename, family = "DejaVu Serif", width = 22 * (plotbounds[[2]] - plotbounds[[1]] - 0.05), height = 22 * (plotbounds[[4]] - plotbounds[[3]] -0.05))
    #cairo_pdf(file = pdf.filename, family = "DejaVu Serif")
    # cairo_pdf(file = pdf.filename, family = "DejaVu Serif", width = 22, height = 22)
  }
  
  if (length(png.filename)!=0){
    png(filename = png.filename, width = 4000*(plotbounds[[2]] - plotbounds[[1]] - 0.05), height = 4000 * (plotbounds[[4]] - plotbounds[[3]] -0.05), units = "px")
  }
  
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #par(pty = "s")
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
    
    
    #print(xSDHigh[[segment]])
    
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
        points(polarTraces[[segment]][[trace]][1, all()], polarTraces[[segment]][[trace]][2, all()], type = "p", col = paletteColors[[segment]], asp = 1)
      }
    }
    
    # if (length(pdf.filename) == 0 && length(png.filename) == 0){
    #   legend.size = 0.6
    # }
    
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
    }
  }
  
  if(length(pdf.filename)!=0 || length(png.filename)!=0 ){
    dev.off()
  }
  
}

makeTracesPolar <- function(rawTraces, origin.algorithm = "BottomMiddle", origin.x = NA, scaling.factor = 800/600){
  
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
  
  polarTraces <- formating_data(dataOfEachCurveNNj, uniqueSegments, origin.x = origin.x, scaling.factor = scaling.factor)
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
    }
    if (!is.na(tiernameAll[[item]])[[1]]){
      temporaryTraces <- temporaryTraces[sapply(temporaryTraces$tiers_list, function(x) any(tiernameAll[[item]] %in% x)),]
    }
    if (!is.na(categoriesAll[[item]])[[1]]){
      temporaryTraces <- temporaryTraces[sapply( temporaryTraces$segment, function(x) any(categoriesAll[[item]] %in% x)),]
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

plotTraces <- function(rawTraces, polarTraces = "", tiernameAll = c(NA), categoriesAll = list(c(NA)), layersAll = c(NA), mergeCategories = c(FALSE), origin.algorithm = "BottomMiddle", origin.x = NA,
                       scaling.factor = 800/600, 
                       interval = 1, mean.lines = TRUE, points.display = FALSE,
                       palette = c(), bands.lines = FALSE, bands.fill = TRUE, legend.position = "topleft",
                       means.styles = c(), standard.deviation.styles = "l", plot.ticks = FALSE, plot.labels = FALSE,
                       legend.size = 3, transparency = 0.37, pdf.filename = c(), bands.linewidth = 0.3,
                       png.filename = c(), legend.linewidth = 5, means.linewidth = 3, tick.size = 2,
                       maskCategories = c()){
  
  if (typeof(polarTraces) == "character"){
    rawTraces <- filteringRawTraces(rawTraces, tiernameAll, categoriesAll, layersAll, mergeCategories)
    rawTraces <- rawTraces %>% select(-tiers_list, -layer) 
    polarTraces <- makeTracesPolar(rawTraces, origin.algorithm, origin.x, scaling.factor)
  }
  
  rayIncrement = 3.14159/180 * interval
  
  uniqueSegments <- get_unique_segments(rawTraces)
  dataOfEachCurveNNj <- read_in_data(rawTraces)
  
  matrixIntersection <- find_intersection_with_ray(polarTraces, dataOfEachCurveNNj, uniqueSegments, rayIncrement)
  
  plotStyleTraces(matrixIntersection = matrixIntersection, polarTraces = polarTraces, 
                  dataOfEachCurveNNj = dataOfEachCurveNNj, uniqueSegments = uniqueSegments, 
                  rayIncrement = rayIncrement, mean.lines = mean.lines, points.display = points.display,
                  palette = palette, bands.lines = bands.lines, legend.position = legend.position, 
                  bands.fill = bands.fill, means.styles = means.styles, standard.deviation.styles = standard.deviation.styles,
                  plot.ticks = plot.ticks, legend.size = legend.size, transparency = transparency, pdf.filename = pdf.filename,
                  bands.linewidth = bands.linewidth, plot.labels = plot.labels, png.filename = png.filename, 
                  legend.linewidth = legend.linewidth, means.linewidth = means.linewidth, tick.size = tick.size,
                  maskCategories = maskCategories)
}

differencePlot <- function(rawTraces, polarTraces, interval = 1){
  rayIncrement = 3.14159/180 * interval #30 degree for test
  uniqueSegments <- get_unique_segments(rawTraces)
  dataOfEachCurveNNj <- read_in_data(rawTraces)
  
  matrixIntersection <- find_intersection_with_ray(polarTraces, dataOfEachCurveNNj, uniqueSegments, rayIncrement)
  
  meanDifference <- list()
  standardDeviation <- list()
  redLine <- list()
  rayIncrement <- 1
  plotbounds <- identifyPlotBounds(polarTraces)
  
  #scalingFactor <- 100 #how to calculate
  
  for (ray in 1:length(matrixIntersection[[1]][1, all()])){
    #[[1]] and [[2]] for matrixIntersection ONLY
    if (!is.na(colMeans(matrixIntersection[[1]], na.rm = TRUE)[[ray]]) && !is.na(colMeans(matrixIntersection[[2]], na.rm = TRUE)[[ray]]) ){ 
      #both are not empty... because no point taking difference of like 16.59 - NA right?
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
      #what is proper scaling? 
      #redLine <- append(redLine, mean(differences)/(scalingFactor*sd(differences)))
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
  #print(scalingFactor)
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
  #print(scalingFactor)
  redLine <- lapply(unlist(redLine), function(x) x/scalingFactor)
  # labelsList <- c(round(min(x1)*180/pi), round((min(x1) + max(x2)/2)*180/pi), round(max(x2)*180/pi))
  # x_ticks <- c(round(plotbounds[[1]],2), round((plotbounds[[2]] + plotbounds[[1]])/2,2), round(plotbounds[[2]],2))
  #meanDifference
  
  axisX2 <- list()
  axisX2 <- lapply(meanX, function(x) round(x*180/pi))
  axisX2 <- seq(1,length(matrixIntersection[[1]][1,all()]), interval)
  #axisX2 <- lapply(axisX2, function(x) -(x-90))
  axisX <- lapply(lapply(meanX, function(x) x*180/pi), function(x) round(x))
  
  #total range for z score is 
  
  #print(ZscoreRange)
  
  #nicely distributed and readable z scores, 5 labels
  ZScorePositioning <- list()
  ZScorePositioning <- append(ZScorePositioning, 0)
  ZScorePositioning <- append(ZScorePositioning, min(na.omit(unlist(redLine))))
  ZScorePositioning <- append(ZScorePositioning, min(na.omit(unlist(redLine)))/2)
  ZScorePositioning <- append(ZScorePositioning, max(na.omit(unlist(redLine))))
  ZScorePositioning <- append(ZScorePositioning, max(na.omit(unlist(redLine)))/2)
  
  ZScoreLabels <- list()
  ZScoreLabels <- lapply(ZScorePositioning, function(x) round(x*scalingFactor,1))
  
  #code for increments of 1 
  # for (i in 1:round(max(na.omit(unlist(redLine)))*scalingFactor)){
  #   ZScoreLabels <- append(ZScoreLabels, round(max(na.omit(unlist(redLine)))*scalingFactor/round(max(na.omit(unlist(redLine)))*scalingFactor)*i,1))
  # }
  # 
  # for (i in round(min(na.omit(unlist(redLine)))*scalingFactor):-1){
  #   #ZScoreLabels <- append(ZScoreLabels, i)
  #   ZScoreLabels <- append(ZScoreLabels, round(max(na.omit(unlist(redLine)))*scalingFactor/round(max(na.omit(unlist(redLine)))*scalingFactor)*i,1))
  # }
  # ZScoreLabels <- sort(unlist(ZScoreLabels))
  # print(ZScoreLabels)
  
  #want it to clearly go through 0, however, 0 is very close but not quite due to rounding. 
  #replacing zero..
  # ZScorePositioning <- seq(min(na.omit(unlist(redLine))), max(na.omit(unlist(redLine))), length.out = length(ZScoreLabels))
  # indexZero <- which(ZScoreLabels == 0)
  # ZScorePositioning <- replace(ZScorePositioning, indexZero, 0)
  # print(ZScorePositioning)
  
  secondYAxis <- seq(min(y1), max(y2), (max(y2) - min(y1))/6)
  secondYAxisBetterPositioned <- lapply(secondYAxis, function(x) round(x,2))
  secondYAxisLabes <- lapply(secondYAxis, function(x) round(x*scalingFactor,2))
  
  #starting plotting
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #par(pty = "s")
  par(mar = c(4, 4.4, 1, 3) + 2)
  
  plot(meanX, meanDifference, type = "l", col = "blue", lwd = 2, ylim = c(min(y1), max(y2)), xlim = c(min(x1), max(x2)), xaxt = "n", xlab = "angle from origin", ylab = "mean difference")
  #labelling x axis with degree from origin
  axis(1, at = meanX, labels = axisX, tck = 0)
  
  #axis(side=4, at = secondYAxis, labels=secondYAxisLabes, col="red", col.axis="black")
  axis(side=4, at = ZScorePositioning, labels=ZScoreLabels, col="red", col.axis="black")
  mtext("z score", side = 4, line = 3)
  
  lines(x1, y1, col = "blue", type = "l", lwd = 1)#, ylim = c(ymin, ymax), xlim = c(xmin, xmax))
  lines(x2, y2, col = "blue", type = "l", lwd = 1)
  polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = adjustcolor( "blue", alpha.f = 0.37), border = NA)
  
  lines(meanX, unlist(redLine), col = "red", lwd = 2)
  abline(h=0.0, lty = 2)
  #abline(h=seq(min(y1), max(y2), (max(y2) - min(y1))/6), col="red", lty="dotted")
  abline(h= ZScorePositioning, col="red", lty="dotted")
  
  indices <- which(unlist(redLine) %in% min(unlist(redLine), na.rm = TRUE))
  #print(redLine[[indices]])
  abline(v = unlist(meanX)[[indices]], col = "red")
  
  indices <- which(unlist(redLine) %in% max(unlist(redLine), na.rm = TRUE))
  #print(redLine[[indices]])
  abline(v = unlist(meanX)[[indices]], col = "red")
  
}
