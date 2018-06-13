library(tidyverse)
library(magrittr)
library(glmnet)
library(text2vec)
library(here)
library(readxl)

here::here() %>%
  setwd()

# Create a stopword vector to use for removing stopwords in the 
# TermDocumentMatrix
source('stopwords.R')
my_stopwords <- read_in_stopwords()
my_stopwords <- c(
  my_stopwords,
  tidytext::stop_words$word,
  tm::stopwords()
)
my_stopwords <- unique(my_stopwords)
my_stopwords <- my_stopwords[!is.na(my_stopwords)]


# *****************************************************************************
# Deal with the training data
# *****************************************************************************

# Read in training data
train <- read_excel('supervised_learning_sample.xlsx')
# Create a labelled and factor variable for the issues
train <- train %>%
  dplyr::mutate(
    category_factor = parse_factor(
      category, levels = c(
        "domestic security",
        "military",
        "immigration",
        "inflation",
        "crime",
        "foreign affairs",
        "trade",
        "taxes",
        "deficit",
        "economy",
        "energy",
        "education",
        "jobs",
        "healthcare",
        "social security",
        "environment",
        "poverty",
        "other"
      )
    )
  )

prep_fun <- tolower
tok_fun <- word_tokenizer

it_train = itoken(
  train$sample, 
  preprocessor = prep_fun, 
  tokenizer = tok_fun, 
  ids = train$id, 
  progressbar = F
)
vocab <- create_vocabulary(
  it_train,
  stopwords = my_stopwords
)
pruned_vocab <- prune_vocabulary(
  vocab,
  doc_proportion_max = 0.9,
  doc_proportion_min = 0.005
)
vectorizer <- vocab_vectorizer(pruned_vocab)
dtm_train <- create_dtm(it_train, vectorizer)


# train the model with glmnet
trained_model <- cv.glmnet(
  dtm_train,
  train$category_factor,
  family = "multinomial"
)

# Run predictions on the training set for accuracy statistics
train_class_pred <- predict(trained_model, newx = dtm_train,
                            s = "lambda.min", type = "class") %>%
data.frame() %>%
  as_tibble() %>%
  dplyr::rename(issue_predicted = X1)

train_pred <- predict(trained_model, newx = dtm_train,
                      s = "lambda.min", type = "response") %>%
  data.frame() %>%
  as_tibble()
column_names <- c(
  "domestic_security",
  "military",
  "immigration",
  "inflation",
  "crime",
  "foreign_affairs",
  "trade",
  "taxes",
  "deficit",
  "economy",
  "energy",
  "education",
  "jobs",
  "healthcare",
  "social_security",
  "environment",
  "poverty",
  "other"
)
colnames(train_pred) <- column_names
train <- train %>%
  bind_cols(train_pred) %>%
  bind_cols(train_class_pred)

# Write to disk
write_rds(train, 'training_data.rds')

# *****************************************************************************
# Deal with the testing data
# *****************************************************************************

# Read in testing data
df <- read_rds('complete_data_with_transcript.rds') 

# create the testing dtm
it_test = df$transcript_body %>% 
  prep_fun %>% tok_fun %>% 
  itoken(ids = df$unique_id)

dtm_test = create_dtm(it_test, vectorizer)

# Run predictions on the training set for accuracy statistics
test_class_pred <- predict(trained_model, newx = dtm_test,
                            s = "lambda.min", type = "class") %>%
  data.frame() %>%
  as_tibble() %>%
  dplyr::rename(issue_predicted = X1)

test_pred <- predict(trained_model, newx = dtm_test,
                      s = "lambda.min", type = "response") %>%
  data.frame() %>%
  as_tibble()
colnames(test_pred) <- column_names

predictions_df <- test_pred %>%
  dplyr::bind_cols(test_class_pred)



# *****************************************************************************
# parse the time and create the final dataset
# *****************************************************************************
library(lubridate)
library(haven)
library(labelled)
library(hms)
library(writexl)

df <- df %>%
  dplyr::select(-transcript_body)

# Save the list of unique shows to manually go through
# df %>%
#   dplyr::select(show) %$%
#   unique(show) %>%
#   tibble() %>%
#   write_xlsx('list_of_shows.xlsx')

# Create function to find the time from a single string
find_correct_time <- function(astring) {
  if (str_detect(
    astring,
    '\\d{2}:\\d{2}'
  )) {
    return(str_extract(astring, '\\d{2}:\\d{2}'))
  }
  if (str_detect(
    astring,
    '\\d{1}:\\d{2} \\b(am|pm)\\b'
  )) {
    if (str_detect(astring, '\\d{1}:\\d{2} \\bam\\b')) {
      var <- str_extract(astring, '\\d{1}:\\d{2} \\bam\\b') %>%
        str_extract('\\d{1}:\\d{2}') %>%
        str_split('') %>%
        unlist()
      return(paste0('0', var[1], var[2], var[3], var[4]))
    } else {
      var <- str_extract(astring, '\\d{1}:\\d{2} \\bpm\\b') %>%
        str_extract('\\d{1}:\\d{2}') %>%
        str_split('') %>%
        unlist()
      var[1] <- as.character(as.numeric(var[1]) + 12)
      return(paste0(var[1], var[2], var[3], var[4]))
    }
  }
  if (str_detect(
    astring,
    '\\d{4} \\b(am|pm|et|est|eastern)\\b'
  )) {
    var <- str_extract(astring, '\\d{4} \\b(am|pm|et|est|eastern)\\b') %>%
      str_extract('\\d{4}') %>%
      str_split('') %>%
      unlist()
    return(paste0(var[1], var[2], ':', var[3], var[4]))
  }
  if (str_detect(
    astring,
    '\\d{3} \\b(am|pm)\\b'
  )) {
    if (str_detect(astring, '\\d{3} \\bam\\b')) {
      var <- str_extract(astring, '\\d{3} \\bam\\b') %>%
        str_extract('\\d{3}') %>%
        str_split('') %>%
        unlist()
      return(paste0('0', var[1], ':', var[2], var[3]))
    } else {
      var <- str_extract(astring, '\\d{3} \\bpm\\b') %>%
        str_extract('\\d{3}') %>%
        str_split('') %>%
        unlist()
      var[1] <- as.character(as.numeric(var[1]) + 12)
      return(paste0(var[1], ':', var[2], var[3]))
    }
  }
  
  return(NA_character_)
}

# Create function to extract the time from a vector of strings of possible
# times
extract_correct_time <- function( item ) {
  
  if ( all(is.na(item)) | all(is.null(item)) | length(item) == 0 |
       all(!is.character(item))) {
    return(NA_character_)
  } else {
    for (i in seq_along(item)) {
      var <- find_correct_time(item[i])
      if(is.na(var) & i==length(item)) {
        return(NA_character_)
      } else {
        if (!is.na(var)) {
          return(var)
        } else {
          next
        }
      }
    } 
  }
  
  return(NA_character_)
}

# Run the 'extract_correct_time' over each element of the show variable
list_of_times <- str_extract_all(
  df$show,
  '\\d{1,2}:?\\d{2}[\\s)]\\s?(am|pm)?\\s?(et|est|eastern)?'
) %>% 
  map(~ str_trim(., 'both'))

time_variable <- map_chr(list_of_times, extract_correct_time) %>%
  paste0(., ':00')
# Midnight should be 00:00:00 but based on code could potentially be 24:00:00
# so need to fix this.
time_variable <- ifelse(time_variable == '24:00:00', '00:00:00', time_variable)
time_variable <- time_variable %>%
  as.hms(., tz = 'EST')
# make sure lengths are the same so we can column bind and everything is in
# the correct order
identical(nrow(df), length(time_variable))
# add the time column to the data
df$time <- time_variable

# Create the time_slot variable
df <- df %>%
  dplyr::mutate(
    time_slot = case_when(
      time >= as.hms('00:00:00') & time < as.hms('04:00:00') ~ 4,
      time >= as.hms('04:00:00') & time < as.hms('12:00:00') ~ 1,
      time >= as.hms('12:00:00') & time < as.hms('18:00:00') ~ 2,
      time >= as.hms('18:00:00') & time < as.hms('24:00:00') ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::mutate(
    time_slot = labelled(
      as.integer(time_slot),
      c(
        morning = 1,
        afternoon = 2,
        evening = 3,
        overnight = 4
      )
    )
  )

  
# Create the events data
add_event <- function(start_date, new_column_name) {
  
  new_df <- df %>%
    dplyr::select(date) %>%
    dplyr::filter(
      date >= as_date(start_date),
      date <= as_date(start_date) + days(61)
    ) %>%
    dplyr::distinct(date, .keep_all = T) %>%
    dplyr::arrange(date)
  
  event <- c(1)
  
  for (i in 2:nrow(new_df)) {
    
    if (new_df$date[i] == as_date(start_date) + days(61)) {
      event <- c(event, 0)
    } else {
      previous_date <- new_df$date[i - 1]
      current_date <- new_df$date[i]
      days_back <- as.numeric(current_date - previous_date)
      previous_value <- event[length(event)]
      if (days_back == 0) {
        event <- c(event, previous_value)
      } else if (days_back == 1) {
        event <- c(event, previous_value * .9485)
      } else {
        temp <- previous_value
        for (i in 1:days_back) {
          temp <- temp * .9485
        }
        event <- c(event, temp)
      }
    }
  }
  
  new_df$event <- event
  new_df <- new_df %>%
    dplyr::select(date, event)
  colnames(new_df) <- c('date', new_column_name)
  
  return(new_df)
}

# Map over each event and date
# Create vectors for the dates of each event and the neame of each event
events_dates <- c('2000-09-30', '2001-09-11', '2001-10-07', '2002-10-02', 
                  '2003-03-19', '2003-05-28', '2004-12-25', '2005-07-07',
                  '2006-02-02', '2006-08-25', '2007-06-07', '2007-07-24',
                  '2007-12-19', '2008-03-11', '2009-02-17', '2009-03-06',
                  '2009-11-05', '2010-04-20', '2010-03-23', '2011-05-02',
                  '2011-01-25', '2011-08-01', '2011-01-08', '2012-11-14',
                  '2012-10-29', '2012-09-10', '2012-12-14', '2012-07-20',
                  '2013-06-06', '2013-10-01', '2013-10-16', '2013-07-13',
                  '2013-09-16', '2013-04-15', '2014-05-23', '2014-12-20',
                  '2014-04-29', '2014-11-20', '2014-08-09', '2015-11-13',
                  '2015-06-06', '2015-06-17', '2015-12-02', '2016-06-12')

events_vector <- c('israel_palestine', 'attack_9_11', 'afghan_war',
                   'dc_sniper', 'iraq_war', 'bush_tax_cuts', 'asia_tsunami',
                   'london_terror_attack', 'social_security_reform',
                   'hurricane_katrina', 'stem_cell_veto', 'min_wage_increase',
                   'bush_energy_bill', 'bailout', 'stimulus_package',
                   'unemployment_increase', 'fort_hood', 'bp_spill',
                   'obamacare', 'bin_laden_killed', 'deficit_reduction',
                   'debt_ceiling_raised', 'giffords_shot',
                   'meningitis_outbreak', 'superstorm_sandy',
                   'chi_teach_strike', 'sandy_hook', 'movie_shooting',
                   'PRISM', 'govt_shutdown', 'end_govt_shutdown', 
                   'zimm_not_guilty', 'navy_yard_killing', 'boston_bombing',
                   'cali_shooting', 'police_shot', 'EPA_ruling', 'DACA',
                   'michael_brown', 'paris_attacks', 'prison_escape',
                   'charleston_shooting', 'san_bernadino', 'orlando_shooting')

events_list <- map2(events_dates, events_vector, add_event)

# Add the events list to the original dataframe
for (i in 1:length(events_list)) {
  var_name <- colnames(events_list[[i]])[2]
  df <- df %>%
    dplyr::left_join(events_list[[i]], by = 'date')
  df[[var_name]] <- ifelse(
    is.na(df[[var_name]]), 0, df[[var_name]] 
  )
}

# reorder variables
df <- df %>%
  dplyr::select(
    unique_id, date, network, show, everything()
  )


# Write the complete data without transcript to disc
write_rds(df, 'complete_data_minus_transcript.rds')



# *****************************************************************************
# label variables and values, export a cleaned dataset for analysis
# *****************************************************************************

# Create labels for the variables and values only for the data we need for this
# paper

df <- df %>%
  dplyr::select(
    unique_id,
    date,
    network,
    show,
    time_slot,
    domestic_security:orlando_shooting
  )

 # label the non-categorical variables
var_label(df$unique_id) <- 'Unique identifier for each transcript'
var_label(df$date) <- 'Date the transcript aired'
var_label(df$network) <- 'Network the transcript aired on'
var_label(df$show) <- 'Full show the transcript came from with extra text'
var_label(df$time_slot) <- 'Time slot in which the transcript aired'

issue_names_to_change <- colnames(df)[6:23]

for (i in seq_along(issue_names_to_change)) {
  x <- issue_names_to_change[i]
  new_x <- str_replace(x, '_', ' ') %>%
    str_to_title()
  the_label <- str_interp(
    'Probability of transcript belonging to the ${new_x} issue'
  )
  labelled::var_label(df[[x]]) <- the_label
}

# Label the categorical variables

# Some issues were never predicted because they occured very infrequently in
# the training set. They still got probablilities (the 'response' prediction) 
# but when glmnet only returned one of the issues (the 'class' prediction),
# some were never predicted. This resulted in the factor variable
# 'issue_predicted' not having all the levels. We need to fix this.
df <- df %>%
  dplyr::mutate(
    predicted_issue_factor = parse_factor(
      as.character(issue_predicted),
      levels = c(
        "domestic security",
        "military",
        "immigration",
        "inflation",
        "crime",
        "foreign affairs",
        "trade",
        "taxes",
        "deficit",
        "economy",
        "energy",
        "education",
        "jobs",
        "healthcare",
        "social security",
        "environment",
        "poverty",
        "other"
      )
    ),
    network_factor = labelled::to_factor(network),
    time_slot_factor = labelled::to_factor(time_slot)
  )

df <- df %>%
  dplyr::mutate(
    predicted_issue = labelled::labelled(
      as.integer(predicted_issue_factor),
      c(
        "domestic security" = 1,
        "military" = 2,
        "immigration" = 3,
        "inflation" = 4,
        "crime" = 5,
        "foreign affairs" = 6,
        "trade" = 7,
        "taxes" = 8,
        "deficit" = 9,
        "economy" = 10,
        "energy" = 11,
        "education" = 12,
        "jobs" = 13,
        "healthcare" = 14,
        "social security" = 15,
        "environment" = 16,
        "poverty" = 17,
        "other" = 18
      )
    )
  )
df <- df %>% dplyr::select(-issue_predicted)

var_label(df$network_factor) <- 
  'network variable as factor instead of labelled integer'
var_label(df$time_slot_factor) <- 
  'time_slot variable as factor instead of labelled integer'
var_label(df$predicted_issue_factor) <- 
  'predicted_issue variable as factor instead of labelled integer'

# make the unique show a categorical variable
shows_list <- read_excel('list_of_shows.xlsx',
                         col_types = "text")
df <- df %>%
  rename(original_text_show = show)

df <- df %>%
  dplyr::left_join(shows_list, by = 'original_text_show')

# add variables for total republican, total democrat, total nuetral
df$total_republican <- rowSums(
  dplyr::select(df, domestic_security:foreign_affairs, taxes)
)
var_label(df$total_republican) <- 
  'Probabilities of all republican issues summed up'
df$total_democrat <- rowSums(
  dplyr::select(df, energy:poverty)
)
var_label(df$total_democrat) <- 
  'Probabilities of all democratic issues summed up'
df$total_neutral <- rowSums(
  dplyr::select(df, trade, deficit, economy)
)
var_label(df$total_neutral) <- 
  'Probabilities of all neutral issues summed up'

# Add republican_percentage variable
df <- df %>%
  dplyr::mutate(
    republican_percentage = 
      total_republican / (total_republican + total_democrat)
  )
var_label(df$republican_percentage) <- 
  'percent republican of only republican and democratic issues'

# Add total_republican_percentage variable
df <- df %>%
  dplyr::mutate(
    total_republican_percentage = 
      total_republican / (total_republican + total_democrat + total_neutral)
  )
var_label(df$total_republican_percentage) <- 
  'percent republican of all consensus issues'

df <- df %>%
  dplyr::select(
    unique_id:network, show, time_slot,
    total_republican_percentage, republican_percentage,
    domestic_security:other,
    total_republican, total_democrat, total_neutral,
    predicted_issue,
    israel_palestine:orlando_shooting,
    network_factor, time_slot_factor, predicted_issue_factor
  )

# Save to disk
write_dta(df, 'final_data_for_analysis.dta')









