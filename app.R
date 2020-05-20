library(shiny)
library(nortest)
library(shinyhelper)
library(shinythemes)
library(magrittr)

source('prep.R')
source('histogram.R')
source('normal_qq.R')

ui <- fluidPage(
  theme = shinytheme('yeti'),
  
  tags$style(
    
    'body {
      margin: 30px;
    }
    
    .action-button {
      margin: 15px auto 5px;
      display: block;
      width: 80%;
      color: white;
      font-size: 1.1em;
      background-color: darkcyan;
      padding: 5px 10px;
      border-radius: 5px;
    }
    
    #main_div .fa {
      font-size: 1.7em;
    }'
    
  ),
  
  titlePanel('Test distributions for normality') %>%
    helper(content = 'main', fade = TRUE, id = 'main_div', colour = 'darkcyan'),
  fluidRow(br()),
  
  fluidRow(
    
    column(
      width = 4,
      
      wellPanel(
        
        h3('Load the data') %>%
          helper(content = 'load', fade = TRUE, colour = 'darkcyan'),
        selectInput(
          'builtin_dataset',
          'Choose a built-in dataset:',
          choices = set_choices,
          selected = 'mtcars'
        )
        
      )
      
    ),
    
    column(
      width = 4,
      
      wellPanel(
        
        h3('Choose a variable'),
        uiOutput('var_ui')
        
      )
      
    ),
    
    column(
      width = 4,
      
      wellPanel(
        
        h3('Execute'),
        actionLink('execute', 'Analyze variable', icon = icon('angle-right')),
        actionLink('clear', 'Clear all output', icon = icon('angle-right'))
        
      )
      
    )
    
  ),
  
  
  wellPanel(
    h3('Analyze') %>%
      helper(content = 'analyze', fade = TRUE, colour = 'darkcyan'),
    fluidRow(
      column(4, h4('Histogram'), fluidRow(br()), plotOutput('hist')),
      column(4, h4('Q-Q plot'), fluidRow(br()), plotOutput('qq')),
      column(
        4,
        h4('Goodness-of-fit tests'),
        fluidRow(br()),
        tableOutput('gof'),
        fluidRow(br()),
        h4('Sample size'),
        textOutput('n')
      )
    )
  )
  
)


server <- function(input, output) {
  
  observe_helpers()
  observeEvent(input$help_main, {
    showModal(modalDialog(help_main_text, title = 'Why test normality?'))
  })
  
  df <- reactive({ get(input$builtin_dataset) })
  
  output$var_ui <- renderUI({
    selectInput(
      'var',
      'Available (numeric) variables',
      choices = names(df()[sapply(df(), is.numeric)])
    )
  })
  
  df_var <- reactive({ na.omit(df()[, input$var]) })
  
  gof_p <- reactive({
    c(
      lillie.test(df_var())$p.value,
      shapiro.test(df_var())$p.value,
      ifelse(length(df_var()) <= 7, NA, ad.test(df_var())$p.value)
    )
  })
  
  
  r_hist <- eventReactive(input$execute, {
    histogram(df_var())
  })
  
  r_qq   <- eventReactive(input$execute, {
    normal_qq(df_var())
  })
  
  r_gof    <- eventReactive(input$execute, {
    data.frame(
      x = c('Kolmogorov-Smirnov', 'Shapiro-Wilk', 'Anderson-Darling'),
      y = format(gof_p(), digits = 3)
    )
  })
  
  r_n      <- eventReactive(input$execute, {
    length(df_var()) 
  })
  
  observeEvent(input$execute, {
    output$hist <- renderPlot({ r_hist() })
    output$qq   <- renderPlot({ r_qq()  })
    output$gof  <- renderTable({ r_gof() }, colnames = FALSE)
    output$n <- renderText({ r_n() })
  })
  
  observeEvent(input$clear, {
    output$hist <- NULL
    output$qq   <- NULL
    output$gof  <- NULL
    output$n <- NULL
  })
  
}


shinyApp(ui, server)