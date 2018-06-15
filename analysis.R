library(tidyverse)
library(haven)
library(labelled)
library(here)
library(readxl)

here::here() %>%
  setwd() 

df <- read_dta('final_data_for_analysis.dta') %>%
  dplyr::mutate(
    network_factor = labelled::to_factor(network),
    time_slot_factor = labelled::to_factor(time_slot),
    day_of_week_factor = labelled::to_factor(day_of_week),
    predicted_issue_factor = labelled::to_factor(day_of_week),
    network_type = case_when(
      network <= 3 ~ 'Traditional',
      network > 3 ~ 'Political',
      TRUE ~ NA_character_
    )
  )

events <- read_excel('events_information_table.xlsx')

# *****************************************************************************
# Table 1
# *****************************************************************************
paste0(getwd(), '/Paper') %>%
  setwd()


# count the number of transcripts for each network
table_1 <- df %>%
  count(network_factor) %>%
  dplyr::mutate(
    network_factor = str_to_upper(network_factor),
    Percent = as.character((n / sum(n)) * 100) %>%
      str_extract('\\d{1,2}\\.\\d{2}') %>% paste0('%'),
    n = as.character(n)
  ) %>%
  dplyr::rename(Network = network_factor, Frequency = n)

last_row_in_table_1 <- tibble(
  Network = 'Total',
  Frequency = '1095505',
  Percent = '100.00%'
)

table_1 <- table_1 %>%
  bind_rows(last_row_in_table_1)

write_rds(table_1, 'Table_1_Data.rds')

# *****************************************************************************
# Table 2
# *****************************************************************************
table_2 <- tibble(
  Issue = c(colnames(dplyr::select(df, domestic_security:poverty)),
            'other'),
  `Owned By` = c(
    rep('Republican', 6),
    'Neutral',
    'Republican',
    rep('Neutral', 2),
    rep('Democrat', 7),
    '-'
  ),
  `Republican Advantage` = c(
    '14.50%', '13.90%', '8.50%', '8.50%', '6.60%', '6.00%', '4.80%', '3.90%',
    '2.40%', '0.80%', '-2.60%', '-10.40%', '-12.00%', '-12.40%', '-14.40%',
    '-17.80%', '-18.10%', '-'
  )
) %>%
  dplyr::mutate(
    Issue = str_replace(Issue, '_', ' ') %>% str_to_title()
  )

write_rds(table_2, 'Table_2_Data.rds')

# *****************************************************************************
# Figure 1 Data
# *****************************************************************************
library(estimatr)
library(broom)
library(scales)

# Create a function that will take a single issue (DV) and a single event (IV)
# and run regress issue on event and only save the row row of the resulting
# table that is relevant
run_model <- function(y, x) {
  y <- rlang::sym(y)
  x <- rlang::sym(x)
  
  model <- rlang::expr(
    lm_robust(
      formula = !!y ~ !!x + network_factor,
      data = df,
      se_type = 'stata',
      ci = T,
      return_vcov = F
    )
  )
  
  model <- rlang::eval_bare(model)
  model <- broom::tidy(model) %>%
    dplyr::slice(2)
}

# map the 'run_model()' function over each issue/event and bind the rows into
# a single dataframe we can use to make graphs
figure_1_data <- map2(
    events$issue, events$event_variable, run_model
  ) %>%
  bind_rows() %>%
  left_join(
    dplyr::select(
      events,
      event,
      owned_by,
      term = event_variable),
    by = 'term'
  ) %>%
  dplyr::select(
    event,
    estimate,
    se = std.error,
    p_value = p.value,
    ll = ci.lower,
    ul = ci.upper,
    dv = outcome,
    owned_by
  ) %>%
  as_tibble()

write_rds(figure_1_data, 'Figure_1_Data.rds')

figure_1_data %>%
  ggplot(aes(x = reorder(event, estimate), y = estimate, color = owned_by)) +
  geom_point() +
  geom_linerange(aes(ymin = ll, ymax = ul)) +
  ggtitle('Effect of World Events on Issue Coverage') +
  labs(x = "Event", y = 'Marginal Effect') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = 'bold',
                                  margin = margin(t = 10, r = 0, 
                                                  b = 30, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, 
                                                    b = 0, l = 10)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, 
                                                    b = 10, l = 0)),
        legend.position = c(0.5, 0.5),
        legend.title = element_blank()) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() + 
  expand_limits() +
  scale_color_manual(
    values = c(
      "Republican" = "Red",
      "Neutral" = "green", 
      "Democrat" = "blue"
    )
  ) 



# *****************************************************************************
# Table 3
# *****************************************************************************
format_percent <- function(x) {
  if (!is.numeric(x) & !is.character(x)) {
    return(x)
  }
  x <- as.character(x) %>%
    str_extract('^\\d{1,2}\\.\\d{1,2}') %>%
    map_chr(~ str_c(., '%', collapse = ''))
  
  return(x)
}

table_3 <- df %>%
  dplyr::select(network_factor, total_democrat, total_republican, total_neutral,
                other) %>%
  group_by(network_factor) %>%
  summarize_all(funs(sprintf("%.8f", mean(.) * 100))) %>%
  map_df(format_percent)
  

write_rds(table_3, 'Table_3_Data.rds')



# *****************************************************************************
# Table 4
# *****************************************************************************

table_4 <- df %>%
  dplyr::select(network_factor, republican_percentage,
                domestic_security:other) %>%
  group_by(network_factor) %>%
  summarize_all(funs(sprintf("%.8f", mean(.) * 100)))  %>%
  map_df(format_percent)

issue_names <- colnames(table_4)[2:length(colnames(table_4))]
network_names <- as.character(table_4$network_factor)

table_4 <- table_4 %>%
  dplyr::select(-network_factor) %>%
  t() %>% data.frame(stringsAsFactors = F) %>% as_tibble() %>%
  dplyr::mutate(Issue = str_replace(issue_names, '_', ' ') %>% 
                  str_to_title()) %>%
  dplyr::select(Issue, everything())
colnames(table_4) <- c('Issue', str_to_upper(network_names))

table_4$`Owned By` <- c(
  '-',
  rep('Republican', 6),
  'Neutral',
  'Republican',
  rep('Neutral', 2),
  rep('Democrat', 7),
  '-'
)

table_4$`Republican Advantage` <-  c(
  '-', '14.50%', '13.90%', '8.50%', '8.50%', '6.60%', '6.00%', '4.80%', '3.90%',
  '2.40%', '0.80%', '-2.60%', '-10.40%', '-12.00%', '-12.40%', '-14.40%',
  '-17.80%', '-18.10%', '-'
)

write_rds(table_4, 'Table_4_Data.rds')



# *****************************************************************************
# Table 5
# *****************************************************************************












