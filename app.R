library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)
library(shinyFeedback)

options(shiny.maxRequestSize = 100 * 1024^2)

main <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(
    fileInput('uploaded_data', 'Upload Data'),
    uiOutput('dynamic_controls'),
    uiOutput('timepoint_order_ui'),
    uiOutput('response_order_ui'),
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
    fill = TRUE
  )
)

ui <- page_fillable(
  main
)

shinyApp(ui, function(input, output) {
  
  data <- reactive({
    file <- input$uploaded_data
    
    file_type <- fs::path_ext(file$datapath)
    
    switch (file_type,
      'csv' = read_csv(file$datapath),
      'xlsx' = readxl::read_excel(file$datapath)
    )

  })
  
  observeEvent(input$uploaded_data, {
    
    output$dynamic_controls <- renderUI({
      
      req(data())
      
      col_names <- names(data())
      
      tagList(
        selectInput(
          inputId = 'select_patientid',
          label = '1. Select Patient ID Column',
          choices = col_names,
          selected = col_names[[1]]
        ),
        selectInput(
          inputId = 'select_timepoint',
          label = '2. Select Timepoint Column',
          choices = col_names,
          selected = col_names[[2]]
        ),
        selectInput(
          inputId = 'select_group',
          label = '3. Select AE / QOL Column',
          choices = col_names,
          selected = col_names[[3]]
        )
      )
    })
  })
  
  observeEvent(input$select_timepoint, {
    
    output$timepoint_order_ui <- renderUI({

      timepoint_choices <- data() |> pull(.data[[input$select_timepoint]])
      
      selectInput(
        inputId = 'timepoint_order',
        label = '4. Select Timepoint Ordering',
        choices = timepoint_choices,
        selectize = TRUE,
        multiple = TRUE
      )
    })
    
  })
  
  observeEvent(input$select_group, {

    output$response_order_ui <- renderUI({

      req(data())

      data <- data() |>
        pivot_longer(-one_of(input$select_patientid, input$select_timepoint))

      response_choices <- data |>
        filter(name == input$select_group) |>
        pull(value) |>
        unique()

      selectInput(
        inputId = 'response_order',
        label = '5. Select Response Ordering',
        choices = response_choices,
        selectize = TRUE,
        multiple = TRUE
      )
    })

  })

  results <- eventReactive(input$btn_ae_visualize, {
    
    req(data())
    
    out <- generate_custom_sankey(
      data = data(),
      timepoint_order = input$timepoint_order,
      response_order = input$response_order,
      selected_group = input$select_group,
      timepoint_col = input$select_timepoint,
      patientid_col = input$select_patientid
    )
    
    out
    
  })
  
  observeEvent(input$btn_ae_visualize, {
    
    req(results())
    
    
    output$sankey_plot <- renderPlot({
      
      results() 
      
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
