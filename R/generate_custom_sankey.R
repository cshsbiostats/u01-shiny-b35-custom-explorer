generate_custom_sankey <- \(data, timepoint_order, response_order, selected_group, timepoint_col, patientid_col) {
  
  data <- data |> 
    rename(
      'timepoint' = !!timepoint_col,
      'patientid' = !!patientid_col
    )
  
  data <- data |>
    mutate(timepoint = factor(timepoint, levels = c(timepoint_order)))

  data <- data |>
    arrange(timepoint)
  
  data <- data |> 
    filter(!is.na(timepoint))

  data <- data |>
    pivot_longer(-c(timepoint, patientid))

  plot_data <- data |>
    filter(name == selected_group) |>
    mutate(value = as.character(value)) |> 
   pivot_wider(names_from = timepoint, values_from = value, values_fill = 'Missing')

  plot_data <- plot_data |> make_long(3:ncol(plot_data))

  plot_data <- plot_data |>
    mutate(node = factor(node, levels = c(response_order, 'Missing')))

  plot <- ggplot(plot_data,
                 aes(
                   x = x,
                   next_x = next_x,
                   node = node,
                   next_node = next_node,
                   fill = node
                 )) +
    geom_sankey(flow.alpha = .3) +
    theme_sankey(base_size = 20) +
    theme(
      legend.position = 'bottom',
      legend.title = element_blank(),
      axis.text.x = element_text(face = 'bold'),
      axis.title = element_blank()
    ) +
    scale_color_discrete(drop = FALSE)

  plot

}