# Define UI for application that draws a histogram
navbarPage(
  
  title = "Chi-Square Test",
  
  # first tab
  tabPanel(title = "簡易檢定", fluidPage(
    # css
    tags$head(
      tags$style(
        HTML(
          "body {
          font-family:Taipei Sans TC Beta; 
          font-size:12pt;
          font-weight:bold;
          }
          
          #chiSummary, #fishSummary {
          font-family:Taipei Sans TC Beta; 
          font-size:12pt;
          }
          "
          )
        )
    ),
    
    # page theme
    # theme = shinytheme("superhero"),
    
    # Application title
    titlePanel(""),
    
    # sidebar Layout 
    sidebarLayout(
      position = 'left',
      
      sidebarPanel( # panel
        column(6,
               numericInput("TargetTotal", "【目標】\t抽驗批數", value = NA, min = 0, step = 1),
               numericInput("TotalTotal", "【總計】\t抽驗批數", value = NA, min = 0, step = 1)
        ),
        column(6,
               numericInput("TargetUnpass", "【目標】\t不合格批數", value = NA, min = 0, step = 1),
               numericInput("TotalUnpass", "【總計】\t不合格批數", value = NA, min = 0, step = 1)
        ),
        actionButton("submit", "執行", style = "color:white; background-color:#6c8196;")

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        fluidRow(
          column(5,
                 tableOutput('inputData')
                 ),
          column(7,
                 span(textOutput('chiWarning'), style = "font-size:14pt; 
                                                          color:red; 
                                                          vertical-align:middle; 
                                                          font-weight:bold; 
                                                          text-decoration:underline")
                 ),
          ),
        
        verbatimTextOutput("chiSummary"),
        verbatimTextOutput("fishSummary")
        
      )
    )
  )),
  
  
  
  
  
  # second tab
  tabPanel(title = "讀檔檢定", fluidPage(
    # Application title
    titlePanel(""),
    
    # sidebar Layout 
    sidebarLayout(
      position = 'left',
      
      
      sidebarPanel( # panel
        width = 3,

        fileInput(inputId = 'myfile',
                  label = '請上傳檔案',
                  buttonLabel = '上傳...',
                  placeholder = 'xls, xlsx'),
        
        HTML(
          paste(span('輸入檔案僅保留3欄：'), '<br/>',
                span('1. 目標（國家、進口商等皆可）'), '<br/>',
                span('2. 抽驗批數'), '<br/>',
                span('3. 不合格批數')
          )
        )
        
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        width = 9,
        DT::dataTableOutput('inputData2')
      )
    )
  ))
)




