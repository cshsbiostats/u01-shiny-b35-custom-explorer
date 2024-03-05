library(tidyverse)

ae <- readxl::read_excel('data/B30 data Main+AE+QOL 03-17-2021.xlsx', sheet = 3) |> 
  janitor::clean_names()

main <- readxl::read_excel('data/B30 data Main+AE+QOL 03-17-2021.xlsx', sheet = 2) |> 
  janitor::clean_names()


# -------------------------------------------------------------------------

trt <- main |> 
  select(patient_id, trt)
  
ae <- ae |> 
  select(patient_id, crs, ae, ae_grade) |> 
  left_join(trt, by = join_by(patient_id)) |> 
  relocate(trt, .after = patient_id)

data <- complete(ae, nesting(patient_id, trt, crs), ae, fill = list(ae_grade = 0))

data <- data |> 
  filter(crs != 88)

data <- data |> 
  filter(!is.na(ae))

data <- data |> 
  select(
    'patientid' = patient_id,
    trt,
    ae,
    'ncycle' = crs,
    ae_grade
  )

# data <- data |> 
#   group_nest(patientid) |> 
#   slice_sample(prop = .1) |> 
#   unnest(data)

write_csv(data, 'data/sankey_b30_data.csv')

# -------------------------------------------------------------------------

total_patients <- data |> 
  filter(ncycle == 1) |> 
  pull(patientid) |> 
  unique() |> 
  length()

data <- data |> 
  distinct(patientid, trt, ae, ncycle, .keep_all = TRUE)

timepoint <- data |> pull(ncycle) |> unique() |> sort()

data <- data |> 
  mutate(ncycle = factor(ncycle, levels = timepoint, ordered = TRUE)) |> 
  arrange(ncycle)

data <- data |> 
  pivot_wider(names_from = ncycle, values_from = ae_grade, values_fill = as.character(0))

temp <- data |>
  filter(trt == 'ATC' & ae == 'ARTHRL')

plot_data <- temp |> make_long(4:ncol(data))

plot <- ggplot(plot_data,
               aes(
                 x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 fill = factor(node, levels = 0:5)
               )) +
  geom_sankey(flow.alpha = .3) +
  theme_sankey(base_size = 20) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(face = 'bold'),
        axis.title = element_blank()) + 
  scale_color_discrete(drop = FALSE)
  # scale_color_manual(
  #   values = grade_colors
  # ) +
  # scale_fill_manual(
  # values = grade_colors
  # ) +
  # labs(title = ae, subtitle = glue::glue('{trt}, n = {total_patients}'))

plot
