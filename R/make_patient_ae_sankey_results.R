
# data <- read_csv('data/u01_b35_data.csv')

calculate_ti <- function(x) {
  
  x <- sort(x, decreasing = T)
  ti <- NULL
  
  for (i in 1:length(x)) {
    ti[i] <- x[i] * prod((x[1:i-1] + 1)^-1)
  }
  
  return(sum(ti))
}

make_custom_sankey <- \(data, selected_treatment, selected_ae, selected_timeframe, selected_grade) {
  
  # selected_treatment <- 'Anastrozole'
  # selected_ae <- 'Arthralgia (joint pain)'
  
  data <- local({
    
    data <- data |> 
      filter(
        trt == selected_treatment & ae == selected_ae
      )
    
  })
  
  ti_hist <- local({
    plot_data <- data %>%
      group_by(patientid) %>%
      mutate(ae_grade = as.numeric(ae_grade))  %>%
      mutate(ae_grade = ifelse(is.na(ae_grade), 0, ae_grade)) |> 
      summarise(ti = calculate_ti(ae_grade)) |>
      ungroup()
    
    stats <- plot_data |> 
      skimr::skim(ti) |> 
      as_tibble() |> 
      mutate(across(numeric.mean:numeric.p100, \(x) scales::label_number(.01)(x)))

    plot <- ggplot(plot_data, aes(x = ti)) +
      geom_histogram() +
      theme_bw(base_size = 15) +
      scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, .5)) +
      labs(
        x = 'Toxicity Index (TI)',
        y = 'n',
        caption = glue::glue(
          'Mean (SD): {stats$numeric.mean} ({stats$numeric.sd})\nMedian [IQR]: {stats$numeric.p50} [{stats$numeric.p25}, {stats$numeric.p75}]'
        )
      )
    
    plot
    
  })

  grade_duration <- local({
    
    plot_data <- data %>%
      group_by(ae_grade, ncycle) %>%
      count() %>%
      mutate(
        ncycle = as_factor(ncycle),
        ae_grade = paste0('Grade ', ae_grade)
      ) %>%
      ungroup() |> 
      mutate(
        label = glue::glue('{ae_grade} - {ncycle} M')
      )
    
    ggplot(plot_data, aes(x = n, y = reorder(label, n))) + 
      geom_col() + 
      theme_bw(base_size = 15) + 
      labs(x = 'n', y = NULL)
  })
  
  
  ##### need error handling on whether there is any data
  
  total_patients <- data |>
    pull(patientid) |>
    unique() |>
    length()
  
  sankey <- local({
    
    plot_data <- data |>
      pivot_wider(names_from = ncycle, values_from = ae_grade)
    
    plot_data <- plot_data |>
      make_long(4:ncol(plot_data))
    
    grade_colors <- c(
      "0-1" = "#1F77B4FF",
      "2" = "#2CA02CFF",
      "3" = "#BCBD22FF",
      "4" = "#FF7F0EFF",
      "5" = "#D62728FF",
      "Off Treatment" = "#7F7F7FFF"
    )
    
    plot <- ggplot(
      plot_data,
      aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        fill = factor(node)
      )
    ) +
      geom_sankey(flow.alpha = .3) +
      theme_sankey(base_size = 20) +
      theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(face = 'bold'),
        axis.title = element_blank()
      ) +
      scale_color_manual(values = grade_colors) +
      scale_fill_manual(values = grade_colors) +
      labs(
        title = selected_ae,
        subtitle = glue::glue('{selected_treatment}, n = {total_patients}')
      )
    
    plot
    
  })
  
  # summary_description <- local({
  #   res <- data |>
  #     filter(ncycle == selected_timeframe[2]) |>
  #     count(ae_grade) |>
  #     mutate(ae_grade = factor(ae_grade, levels = response_order)) |>
  #     arrange(ae_grade) |>
  #     mutate(
  #       prop = n / sum(n),
  #       prop = scales::percent_format()(prop)
  #     )
  # 
  #   summary_res <- glue::glue_data(res, 'A total of {n} ({prop}) of the patients responded "{ae_grade}". ') |>
  #     glue::glue_collapse(sep = '<br>')
  # 
  # 
  #   summary_description <- glue::glue(
  #     'Among a total of {total_patients} patients who responded "{selected_grade}" for "{selected_ae}" at {selected_timeframe[1]} M.',
  #     'By the {selected_timeframe[2]} M timepoint',
  # 
  #     '{summary_res}',
  #     .sep = '<br><br>'
  #   )
  # })
  
  tibble::lst(
    sankey,
    # summary_description,
    ti_hist,
    grade_duration
  )
  
}

