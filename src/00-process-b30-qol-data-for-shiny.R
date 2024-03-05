library(tidyverse)
library(ggsankey)

qol <- readxl::read_excel('data/B30 data Main+AE+QOL 03-17-2021.xlsx', sheet = 5) |> 
  janitor::clean_names()

main <- readxl::read_excel('data/B30 data Main+AE+QOL 03-17-2021.xlsx', sheet = 2) |> 
  janitor::clean_names()

qol <- qol |> 
  select(patient_id, crs, hltht:difcon)

qol <- qol |> 
  complete(nesting(patient_id), crs)

data <- qol

data <- data |> 
  mutate(across(3:ncol(data), \(x) ifelse(is.na(x), 'Not mentioned', x)))

write_csv(data, 'data/processed_b30_qol_data.csv', na = '')


set.seed(1)
selected_cols <- names(data) |> 
  sample(10)

anon_data <- data |> 
  group_nest(patient_id) |> 
  mutate(patient_id = forcats::fct_anon(factor(patient_id))) |> 
  slice_sample(prop = .1) |> 
  unnest(data) |> 
  select(patient_id, crs, one_of(selected_cols))

write_csv(anon_data, 'data/anonymized_b30_data_template.csv')  

# -------------------------------------------------------------------------

ae <- readxl::read_excel('data/B30 data Main+AE+QOL 03-17-2021.xlsx', sheet = 3) |> 
  janitor::clean_names()

ae <- ae |> 
  select(patient_id, crs, ae, ae_grade) |> 
  filter(crs != 88) |> 
  pivot_wider(names_from = ae, values_from = ae_grade, values_fn = first, values_fill = 0) |> 
  select(-`NA`) |> 
  janitor::clean_names()


write_csv(ae, 'data/processed_b30_ae_data.csv', na = '')

# -------------------------------------------------------------------------



patientid_col <- 'patient_id'
timepoint_col <- 'crs'
group_col <- 'accpt'

data <- data |> 
  pivot_longer(-one_of(patientid_col, timepoint_col)) 

groups <- data |> pull(name) |> unique()

selected_group <- 'accpt'

response_order <- c("Not mentioned",
                    "Somewhat",
                    "A little bit",
                    "Very much",
                    "Quite a bit",
                    "Not at all")

timepoints <- data |> 
  pull(crs) |> 
  unique()

timepoint_order <- c(
  "Baseline",
  "Day 1 of Cycle 4",
  "6-month follow-up",
  "12-month follow-up",
  "18-month follow-up",
  "24-month follow-up"
)

generate_custom_sankey <- \(data, timepoint_order, response_order, selected_group, timepoint_col) {
  data <- data |>
    mutate(!!timepoint_col := factor(.data[[timepoint_col]], levels = timepoint_order, ordered = TRUE))
  
  data <- data |>
    arrange(!!sym(timepoint_col))
  
  plot_data <- data |>
    filter(name == selected_group) |>
    mutate(value = factor(value, levels = response_order)) |>
    pivot_wider(names_from = !!sym(timepoint_col), values_from = value)
  
  plot_data <- plot_data |> make_long(3:ncol(plot_data))
  
  plot_data <- plot_data |>
    mutate(node = factor(node, levels = response_order))
  
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

debugonce(generate_custom_sankey)

generate_custom_sankey(
  data = data,
  timepoint_order = timepoint_order,
  response_order = response_order,
  selected_group = selected_group,
  timepoint_col = timepoint_col
)
