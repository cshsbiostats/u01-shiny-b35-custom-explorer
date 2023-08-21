library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)
library(shinyFeedback)

main <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(
    fileInput('uploaded_data', 'Upload Data'),
    uiOutput('dynamic_controls'),
    actionButton(
      'btn_ae_visualize',
      'Visualize',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    ),
    downloadButton(
      'report',
      'Download Report',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    )
  ),
  card(
    card_header('Sankey Diagram'),
    card_body(class = "p-0",
              plotOutput('sankey_plot')),
    full_screen = TRUE,
    fill = FALSE
  ),
  layout_column_wrap(
    card(card_header('Grade Duration'),
         card_body(class = "p-0",
                   plotOutput('grade_duration')), full_screen = TRUE),
    card(card_header('Toxicity Index'),
         card_body(class = "p-0",
                   plotOutput('ti_hist')), full_screen = TRUE),
    width = 1 / 2
  )
)

ui <- page_fillable(
  main
)

shinyApp(ui, function(input, output) {
  
  data <- reactive({
    file <- input$uploaded_data
    
    read_csv(file$datapath)
  })
  
  observeEvent(input$uploaded_data, {
    
    output$dynamic_controls <- renderUI({
      
      req(data())
      
      trt_options <- data() |> pull(trt) |> unique() |> sort()
      
      ae_options <- data() |>
        pull(ae) |>
        unique() |>
        sort()
      
      grade_options <- data() |>
        pull(ae_grade) |>
        unique() |>
        sort()
      
      tagList(
        selectInput(
          inputId = 'select_trt',
          label = '1. Select Treatment',
          choices = trt_options
        ),
        selectInput(
          inputId = 'select_ae',
          label = '2. Select AE',
          choices = ae_options
        )
      )
    })
  })
  
  results <- eventReactive(input$btn_ae_visualize, {
    
    out <- make_custom_sankey(
      data = data(),
      selected_treatment = input$select_trt,
      selected_ae = input$select_ae,
      selected_timeframe = input$select_timeframe,
      selected_grade = input$select_grade
    )
    
  })
  
  observeEvent(input$btn_ae_visualize, {
    
    req(results())
    
    
    
    output$sankey_plot <- renderPlot({
      
      results()$sankey
      
    })
    
    output$ti_hist <- renderPlot({
      
      results()$ti_hist
      
    })
    
    output$summary_descr <- renderPrint({
      
      results()$summary_description
    })
    
    output$grade_duration <- renderPlot({
      
      results()$grade_duration
    })
    
  })
  
  output$report <- downloadHandler(
    filename = \(x) {paste0('pro_ctcae_ae_sankey_', Sys.Date(), '.pdf')},
    content = function(file) {
      
      rmarkdown::render(
        'explore_qol_template.Rmd',
        output_file = file,
        params = list('results' = results()),
        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
})
