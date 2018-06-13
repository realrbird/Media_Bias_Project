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

# *****************************************************************************
# Table 1
# *****************************************************************************
paste0(getwd(), '/Table_and_Figure_Data') %>%
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


events <- read_excel('events_information_table.xlsx')

test <- lm_robust(
  formula = israel_palestine ~ foreign_affairs + network_factor,
  data = df,
  se_type = 'stata',
  ci = T,
  return_vcov = F
)

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


regressions <- events %>%
  map2(
    issue, event_variable, run_model
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

write_rds(regressions, 'Figure_1_Data.rds')

# regressions %>%
#   ggplot(aes(x = reorder(event, estimate), y = estimate, color = owned_by)) +
#   geom_point() + 
#   geom_linerange(aes(ymin = ll, ymax = ul)) +
#   geom_hline(aes(yintercept = 0), color = 'orange', linetype = 'dashed') +
#   coord_flip() + 
#   theme_classic()
#   



# *****************************************************************************
# Table 3
# *****************************************************************************

df %>%
  dplyr::select(network_factor, total_democrat, total_republican, total_neutral,
                other) %>%
  group_by(network_factor) %>%
  summarize_all(~ mean(.) * 100)
  







