library(shiny)
library(nortest)

source('prep.R')
source('histogram.R')
source('normal_qq.R')

ui <- fluidPage(
  
  titlePanel('Test distributions for normality'),
  actionLink('main_help', 'What is this about?', icon('question-circle')),
  fluidRow(br()),
  
  fluidRow(
    
    column(
      width = 6,
      
      wellPanel(
        style = 'height:20vw;',
        
        h3('Step 1: load the data'),
        selectInput(
          'builtin_dataset',
          'Choose a built-in dataset:',
          choices = set_choices,
          selected = 'mtcars'
        )
        
      )
      
    ),
    
    column(
      6,
      
      wellPanel(
        style = 'height:20vw;',
        
        h3('Step 2: choose a variable'),
        fluidRow(
          column(5, textInput('var', 'Enter variable name:')),
          column(
            7,
            strong('Available (numeric) variables:'),
            em(textOutput('var_names'))
          )
        )
        
      )
      
    )
    
  ),
  
  
  wellPanel(
    h3('Tests'),
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
  
  df <- reactive({ get(input$builtin_dataset) })
  df_names <- reactive({ get_numeric_names(df()) })
  df_var <- reactive({ na.omit(df()[, input$var]) })
  gof_p <- reactive({
    c(
      lillie.test(df_var())$p.value,
      shapiro.test(df_var())$p.value,
      ad.test(df_var())$p.value
    )
  })
  
  
  output$var_names <- renderText( df_names() )
  
  output$hist <- renderPlot({
    if (input$var %in% names(df())) histogram(df_var())
  })
  
  output$qq <- renderPlot({
    if (input$var %in% names(df())) normal_qq(df_var())
  })
  
  output$gof <- renderTable({
    data.frame(
      x = c('Kolmogorov-Smirnov', 'Shapiro-Wilk', 'Anderson-Darling'),
      y = if (input$var %in% names(df())) format(gof_p(), digits = 3) else " "
    )
  },
  colnames = FALSE
  )
  
  output$n <- renderText( if (input$var %in% names(df())) length(df_var()) )
  
}


shinyApp(ui, server)