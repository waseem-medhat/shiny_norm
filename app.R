library(shiny)

source('prep.R')
source('histogram.R')

ui <- fluidPage(
  
  titlePanel('Test distributions for normality'),
  actionLink('main_help', 'What is this about?', icon('question-circle')),
  fluidRow(br()),
  
  h3('Step 1: load the data'),
  fluidRow(
    column(
      5,
      selectInput(
        'builtin_dataset',
        'Choose a built-in dataset:',
        choices = set_choices,
        selected = 'iris'
      )
    ),
    column(2, strong('or...')),
    column(5, fileInput('upload_dataset', 'Upload your own dataset:'))
  ),
  
  h3('Step 2: choose a variable'),
  fluidRow(
    column(5, textInput('var', 'Enter variable name:')),
    column(7, strong('Available variables:'), textOutput('var_names'))
  ),
  
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


server <- function(input, output) {
  
  df <- reactive({ get(input$builtin_dataset) })
  df_names <- reactive({ paste(names(df()), collapse = ' / ') })
  df_var <- reactive({ df()[, input$var] })
  
  
  output$var_names <- renderText( df_names() )
  output$hist <- renderPlot( histogram(df_var()) )
  output$qq <- renderPlot( normal_qq(df_var()) )
  output$gof <- renderTable({
    data.frame(
      x = c('Kolmogorov-Smirnov', 'Shapiro-Wilk', 'Anderson-Darling'),
      y = 1:3
    )
  },
  colnames = FALSE
  )
  
}


shinyApp(ui, server)