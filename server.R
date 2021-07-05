library(shiny)
library(shinythemes)
library(stringr)
library(dplyr)
library(xlsx)

myChisqFish <- function(targettotal, targetunpass, totaltotal, totalunpass){
  targetpass <- targettotal - targetunpass
  targetunpass <- targetunpass
  otherpass <- (totaltotal - targettotal) - (totalunpass - targetunpass)
  otherunpass <- totalunpass - targetunpass
  
  df <- data.frame(Pass = c(targetpass, otherpass), Unpass = c(targetunpass, otherunpass))
  colnames(df) <- c('\u5408\u683c\u6279\u6578','\u4e0d\u5408\u683c\u6279\u6578') # Target
  rownames(df) <- c('\u6a19\u7684','\u5176\u4ed6') # Other
  
  chiResult <- chisq.test(df)
  fishResult <- fisher.test(df)
  
  chiP <- chiResult$p.value
  fishP <- fishResult$p.value
  
  estimate <- paste0(
    ifelse(is(tryCatch(chisq.test(df), 
                       warning = function(w) w), 
              "warning")
           , "\u5361\u65b9\u8fd1\u4f3c\u503c\u53ef\u80fd\u4e0d\u6e96 \n" # Chi-squared approximation may be incorrect
           , ""
           ),
    ifelse(fishP < 10^(-10) & fishP > 0, 
           paste0("P < 1e-10 \n"), "")
    )
  
  return(list(df=df, chi = chiResult, fish = fishResult, chiP = chiP, fishP = fishP, estimate = estimate))
}





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### nav1
  
  
  # load input and do something 
  chidf <- reactive({
    validate(
      # check if input number integer or not 
      need(is.integer(input$TargetTotal) & is.integer(input$TotalTotal) & is.integer(input$TargetUnpass) & is.integer(input$TotalUnpass) &
             (input$TargetTotal >= 0) & (input$TotalTotal >= 0) & (input$TargetUnpass >= 0) & (input$TotalUnpass >= 0), 
           "!!!\t\u8acb\u8f38\u5165\u6b63\u6574\u6578"), 
      # check if input number can be applyed to the test
      need((input$TargetTotal >= input$TargetUnpass) & (input$TotalTotal >= input$TotalUnpass), 
           "\n!!!\t\u62bd\u9a57\u6279\u6578\u4e0d\u5f97\u5c0f\u65bc\u4e0d\u5408\u683c\u6279\u6578"),
      need(((input$TotalTotal - input$TotalUnpass) >= (input$TargetTotal - input$TargetUnpass)) & (input$TotalUnpass >= input$TargetUnpass), 
           "\n!!!\t\u7e3d\u8a08\u4e0d\u5f97\u5c0f\u65bc\u76ee\u6a19")
    )
    
    if (input$submit > 0) {
      myChisqFish(input$TargetTotal, input$TargetUnpass, input$TotalTotal, input$TotalUnpass)
    }
    
  })
  
  
  
  myrequire <- "
  req(is.integer(input$TargetTotal) & is.integer(input$TotalTotal) & is.integer(input$TargetUnpass) & is.integer(input$TotalUnpass) &
  (input$TargetTotal >= 0) & (input$TotalTotal >= 0) & (input$TargetUnpass >= 0) & (input$TotalUnpass >= 0) & 
  (input$TargetTotal >= input$TargetUnpass) & (input$TotalTotal >= input$TotalUnpass) & 
  ((input$TotalTotal - input$TotalUnpass) >= (input$TargetTotal - input$TargetUnpass)) & (input$TotalUnpass >= input$TargetUnpass))
  "
  
  
  
  # show data table
  output$inputData <- renderTable({

    
    if (!is.null(chidf())) {
      chidf()$df
    }
    
    }, 'include.rownames' = TRUE
    , 'include.colnames' = TRUE
    , 'sanitize.text.function' = function(x){x}
  )
  
  output$chiWarning <- renderText({
    
    eval(parse(text = myrequire))
    
    if ( any(chidf()$df < 5) ) {
      '\u8b66\u544a\uff1a\u4efb\u4e00\u7d30\u683c\u6578\u5c0f\u65bc5\uff0c\u5361\u65b9\u8fd1\u4f3c\u503c\u53ef\u80fd\u4e0d\u6b63\u78ba'
    }else{
      ''
    }
    
  })
  
  
  # show chi-square test result
  output$chiSummary <- renderPrint({
    
    eval(parse(text = myrequire))
    
    if (input$submit > 0) {
      chidf()$chi
    }
    
  })
  
  
  # show fisher test result
  output$fishSummary <- renderPrint({
    
    eval(parse(text = myrequire))
    
    if (input$submit > 0) {
      chidf()$fish
    }
    
  })
 
  
  
  
  
  
  
  
  
  ### nav2
  
  
  
  # check if the file is csv format
  uploadData <- reactive({
    file <- input$myfile
    ext <- tools::file_ext(file$datapath)
    
    validate(
      need(ext %in% c("xls", "xlsx"), "!!! \u8acb\u4e0a\u50b3 xls \u6216 xlsx \u683c\u5f0f\u4e4b\u6a94\u6848")
    )
    
    if( is.null(file) | !(ext %in% c("xls", "xlsx")) ){ return() }
    
    
    if (ext %in% c("xls", "xlsx")) {
      xlsx::read.xlsx(file = file$datapath, sheetIndex = 1, encoding="UTF-8")
    }
    # else if (ext == "csv") {
    #   read.csv(file = file$datapath)
    # }
    
  
  
  })
  
  
  
  # file name & result data
  testResult <- reactive({
    if (!is.null(uploadData())) {
      df <- uploadData()
      
      category <- colnames(df)[1]
      
      colnames(df)[1] <- 'Category'
      
      df <- dplyr::filter(df, !stringr::str_detect(Category, '\u8a08|total|Total'))
      
      
      colInsp <- which(colnames(df) == names(which.max(colSums(df[, 2:ncol(df)]))))
      colUnpass <- which(!1:ncol(df) %in% c(1, colInsp))
      
      colnames(df)[colInsp] <- 'TargetInsp' # paste0('Target', category, 'Insp')
      colnames(df)[colUnpass] <- 'TargetUnpass'
      
      df$TargetUnRate <- df[, colUnpass] / df[, colInsp]
      df$OtherInsp <- sum(df[, colInsp]) - df[, colUnpass] - (df[, colInsp] - df[, colUnpass])
      df$OtherUnpass <- sum(df[, colUnpass]) - df[, colUnpass]
      df$OtherUnRate <- df$OtherUnpass / df$OtherInsp
      
      df$ChiSquareP <- 0
      df$FisherP <- 0
      df$estimate <- 0
      for (i in 1:nrow(df)) {
        ans <- myChisqFish(df$TargetInsp[i], df$TargetUnpass[i], (df$OtherInsp[i]+df$TargetInsp[i]), (df$OtherUnpass[i]+df$TargetUnpass[i]))
        
        df$ChiSquareP[i] <- ans$chi$p.value
        df$FisherP[i] <- ans$fish$p.value
        df$estimate[i] <- ans$estimate
        
        rm(ans)
      }
      
      printdf <- df[, c('Category', 'TargetInsp', 'TargetUnpass', 'TargetUnRate', 
                        'OtherInsp', 'OtherUnpass', 'OtherUnRate', 
                        'ChiSquareP', 'FisherP', 'estimate')]
      colnames(printdf) <- c(category 
                             , paste0('\u8a72', category, '\u62bd\u9a57\u6279\u6578')             # TargetInsp 
                             , paste0('\u8a72', category, '\u4e0d\u5408\u683c\u6279\u6578')       # TargetUnpass
                             , paste0('\u8a72', category, '\u4e0d\u5408\u683c\u7387')             # TargetUnRate
                             , paste0('\u5176\u4ed6', category, '\u62bd\u9a57\u6279\u6578')       # OtherInsp
                             , paste0('\u5176\u4ed6', category, '\u4e0d\u5408\u683c\u6279\u6578') # OtherUnpass
                             , paste0('\u5176\u4ed6', category, '\u4e0d\u5408\u683c\u7387')       # TargetUnRate
                             , '\u5361\u65b9\u6aa2\u5b9aP\u503c'                                  # ChiSquareP
                             , '\u8cbb\u96ea\u6aa2\u5b9aP\u503c'                                  # FisherP
                             , '\u5099\u8a3b'                                                     # estimate
                             )
      
      # lapply(1:nrow(df), function(i) {
      #   ans <- myChisqFish(df$TargetInsp[i], df$TargetUnpass[i], df$OtherInsp[i], df$OtherUnpass[i])
      # 
      #   df$ChiSquareP[i] <- ans$chi$p.value
      #   df$FisherP[i] <- ans$fish$p.value
      # })
      
      return(list(category=category, printdf = printdf))
    }
    
  })
  
  
  # # show data table
  # output$inputData2 <- renderDataTable({
  # 
  #   testResult()$printdf
  # 
  # }
  # , options = list(scrollY = '700px'
  #                  , paging = FALSE
  #                  , searching = FALSE
  #                  , dom = 'tB' # Define the table control elements to appear on the page and in what order
  #                  )
  # , extensions = 'Buttons'
  # 
  # )
  
  
  
  
  
  
  output$inputData2  <- DT::renderDT({
    # Load data
    data <- testResult()$printdf
    # Show data
    DT::datatable(data
                  , extensions = 'Buttons'
                  , options = list(scrollY = '680px'
                                   , fixedColumns = TRUE
                                   , autoWidth = FALSE
                                   , paging = FALSE
                                   , searching = FALSE
                                   , dom = 'Bt' # Define the table control elements to appear on the page and in what order
                                   , buttons = c('copy', 'excel')
                                   )
                  ) %>%
      DT::formatPercentage(c(4, 7), 2) %>%
      DT::formatRound(c(8, 9), 10)
  })
  
  
  

  
  
  # # download data
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste0(category(), '\u5361\u65b9\u53ca\u8cbb\u96ea\u6aa2\u5b9a\u7d50\u679c', format(Sys.time(), '%Y%m%d%H%M'), '.xlsx')
  #   },
  #   content = function(con) {
  #     xlsx::write.xlsx()
  #   }
  # )

  
  
}
