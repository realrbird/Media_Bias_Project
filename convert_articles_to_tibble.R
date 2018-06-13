# Load necessary libraries
library(magrittr)
library(tidyverse)
library(lubridate)
library(haven)
library(labelled)
library(here)
library(tm)

# set the working directory, this will need to be changed
here::here() %>%
  setwd()

# Create a function that loads in a single file and does some initial cleaning
load_in_data <- function(afile) {
  # Load in a single file of transcripts
  text <- readr::read_file(afile) %>%
    # Separate each transcript from each other and put in a list
    str_split('\\d{1,4} of \\d{1,4} DOCUMENTS') %>%
    # convert the list to a charactger vector
    unlist() %>%
    # Trim extra whitespace from all transcripts and remove any transcripts
    # that have less than 10 characters, basically empty items or anomolies.
    map_chr(~ str_trim(., 'both')) %>%
    Filter(function(x) str_length(x) > 10, .)
  
  return(text)
}


# Create a function that will take a single transcript and split it into meta
# data and the body and them remove spaces from each and trim whitespace on 
# each and return it as a length 2 character vector
separate_single_transcript <- function(item) {
  return_item <- item %>%
    str_split('(\r\n){3}[^\r\n]+', n = 2) %>%
    unlist() %>%
    map_chr(
      ~ str_replace_all(., '\\s+', ' ') %>% 
        str_trim('both')
    )
  length_is_2 <- length(return_item) == 2
  second_not_na <- !is.na(return_item[2])
  second_not_empty <- !return_item[2] == ''
  if (length_is_2 & second_not_na & second_not_empty) {
    return(return_item)
  } else {
    return_item <- c(item, item) %>%
      map_chr(
        ~ str_replace_all(., '\\s+', ' ') %>% 
          str_trim('both')
      )
    return(return_item)
  }
}

# Create a function that will pull out the necessary meta information, SHOW,
# DATE, BYLINE etc., and save it in a tibble along with the body of the 
# transcript
pull_out_meta_data <- function(avector) {
  
  # separate the meta data from the body of the item
  avector <- separate_single_transcript(avector)
  
  # store metadata from the vector and the transcript data
  meta_data <- avector[1]
  transcript_data <- avector[2]
  # store the meta data items to extract
  meta_data_items <- c(
    'SHOW', 'BYLINE', 'SECTION', 'LENGTH', 'HIGHLIGHT', 
    'GUESTS', 'ANCHORS'
  )
  # extract the metadata items into a character vector
  meta_data_info <- meta_data_items %>%
    map(~ str_extract(
      meta_data,
      regex(str_interp('${.}:.+?(?=([A-Z]{6,}:)|$)'), dotall = T)
    ) %>%
      str_trim('both') # trim whitespace
  ) %>%
    unlist() %>%
    # Remove 'SHOW:', 'BYLINE:' etc from the strings
    map2_chr(., meta_data_items, 
             function(x, y) str_replace(x, str_interp('${y}:'), '')) %>%
    str_trim('both')
  
  # store the network of the transcript
  network <- str_extract(meta_data, '^[^\\s]+') %>%
    str_trim('both')
  # store the date of the transcript as a string
  date <- str_extract(
    meta_data,
    '\\w{3,9} \\d{1,2}, \\d{4}'
  ) %>%
    str_trim('both')
  
  # Extract the meta data from within the transcript
  transcript_data_items <- c(
    'LOAD-DATE', 'LANGUAGE', 'TRANSCRIPT', 'DOCUMENT-TYPE',
    'PUBLICATION-TYPE'
  )
  transcript_data_info <- transcript_data_items %>%
    map(~ str_extract(
      transcript_data,
      regex(str_interp('${.}:.+?(?=([A-Z]{3,}:)|$)'), dotall = T)
    ) %>%
      str_trim('both') # trim whitespace
    ) %>%
    unlist() %>%
    # Remove 'SHOW:', 'BYLINE:' etc from the strings
    map2_chr(., transcript_data_items, 
             function(x, y) str_replace(x, str_interp('${y}:'), '')) %>%
    str_trim('both')
  
  # Create a list of all the character vector information including the meta
  # data and the body of the transcript
  list_to_convert <- c(
    network, date, meta_data_info, transcript_data_info, avector[2]
  ) %>% 
    as.list()
  # name the list so we can put it in a tibble
  names(list_to_convert) <- c(
    'network', 'date', str_to_lower(meta_data_items),
    str_to_lower(transcript_data_items), 'transcript_body'
  ) %>% 
    # we don't want hyphens in the variable names
    # of the tibble, so replace them with underscrores
    str_replace_all('-', '_')  %>%
    str_trim('both')
  
  
  return(
    list_to_convert %>%
      as_tibble() %>%
      map_df(~ as.character(.)) %>%
      dplyr::mutate(
        publication_type = str_replace(
          publication_type, regex(
            'Copyright.*', ignore_case = T, dotall = T), ' ')
    ) %>%
      map_df(~ str_trim(., 'both'))
  )
}

# Write a function that will create the tibble for a single transcript
create_transcript_tibble <- function(item) {
  item <- separate_single_transcript(item)
  item <- pull_out_meta_data(item)
  
  return(item)
}

# Write a function that will map over the 'create_transcript_tibble()' funciton
create_list_of_transcript_tibbles <- function(afile) {
  text <- load_in_data(afile)
  text <- text %>%
    map(safely(create_transcript_tibble)) %>%
    transpose()
  result <- text$result
  
  # Something might go wrong in the map function, record how many transcripts
  # had an error and returned NULL
  total_nulls <- result %>% map_lgl(~ is.null(.)) %>% sum()

  if (total_nulls > 0) cat(total_nulls, ' lost transcirpts\n',
                           as.character(unlist(text$error)))
  
  return(
    bind_rows(result)
  )
  
}

# Create a funciton that will take a file path, and will loop over each file
# in the path and run the create list of transcript tibbles function
create_network_tibble <- function(adirectory) {
  setwd(adirectory)
  # Grab all the files from the directory
  files <- list.files()
  # include only files that are text files
  files <- files[str_detect(files, '.TXT') | str_detect(files, '.txt')]
  # map the 'create_list_of_transcript_tibbles
  network_df <- files %>%
    map(create_list_of_transcript_tibbles) %>%
    bind_rows() %>%
    map_df(~ str_trim(., 'both'))
  # Change the date 
  network_df <- network_df %>%
    dplyr::mutate(
      date = as.Date(date, format = '%B %d, %Y')
    )
  
  #reset the working directory
  here::here() %>%
    setwd()
  
  # Set the network manually
  if (adirectory == abc_wd) {
    network_df$network <- 'ABC'
  }
  if (adirectory == cbs_wd) {
    network_df$network <- 'CBS'
  }
  if (adirectory == nbc_wd) {
    network_df$network <- 'NBC'
  }
  if (adirectory == cnn_wd) {
    network_df$network <- 'CNN'
  }
  if (adirectory == fox_wd) {
    network_df$network <- 'FOX'
  }
  if (adirectory == msnbc_wd) {
    network_df$network <- 'MSNBC'
  }
  
  return(network_df)
}

# create a functon that will run checks on the dataframe once it is completely
# processed
run_checks <- function(adf) {
  # Check that not an entire row is
  rows_all_missing <- c()
  total_network <- c()
  total_date <- c()
  total_transcript_body <- c()
  for (i in 1:nrow(adf)) {
    if (all(is.na(adf[i, ]))) {
      rows_all_missing <- c(rows_all_missing, i)
    }
    if (is.na(adf$network[i])) {
      total_network <- c(total_network, i)
    }
    if (is.na(adf$date[i])) {
      total_date <- c(total_date, i)
    }
    if (is.na(adf$transcript_body[i])) {
      total_transcript_body <- c(total_transcript_body, i)
    }
  }
  total_problems <- sum(
    rows_all_missing, total_network, total_date, total_transcript_body
  )
  if (total_problems < 1) {
    cat(
      str_interp('No Problems\n')
    )
    return(NULL)
  }
  cat(
    str_interp(
      'There are ${length(rows_all_missing)} rows that are all missing,
      they are ${rows_all_missing}.\n There are ${length(total_network)}
      networks that are all missing, they are ${total_network}.\n There are
      ${length(total_date)} dates that are all missing,
      they are ${total_date}.\n There are ${length(total_transcript_body)}
      transcripts that are all missing, they are ${total_transcript_body}.\n'
    )
  )
  return(NULL)
}

# store directory for each network
abc_wd <- getwd() %>%
  paste0(., '/Articles/abc_news')
cbs_wd <- getwd() %>%
  paste0(., '/Articles/cbs_news')
nbc_wd <- getwd() %>%
  paste0(., '/Articles/nbc_news')
cnn_wd <- getwd() %>%
  paste0(., '/Articles/cnn')
fox_wd <- getwd() %>%
  paste0(., '/Articles/fox_news')
msnbc_wd <- getwd() %>%
  paste0(., '/Articles/msnbc')

wd_networks <- c(abc_wd, cbs_wd, nbc_wd, cnn_wd, fox_wd, msnbc_wd)

# Create the complete network datasets with all transcripts for each network
final_data <- wd_networks %>%
  purrr::map(create_network_tibble) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    show = str_to_lower(show),
    network_id = case_when(
      network == 'ABC' ~ 1,
      network == 'CBS' ~ 2,
      network == 'NBC' ~ 3,
      network == 'CNN' ~ 4,
      network == 'FOX' ~ 5,
      network == 'MSNBC' ~ 6,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::arrange(network_id, date) %>%
  dplyr::mutate(unique_id = 1:n()) %T>%
  run_checks()

# Remove the copyright... from the meta data
copyright_data <- final_data %>%
  dplyr::select(
    load_date, language, transcript, document_type, publication_type
  ) %>%
  map_df(
    ~ str_replace(., regex(
      'Copyright.*', dotall = T, ignore_case = T
    ), '') %>%
      str_trim('both')
  )

# Bind the columns back in to the final_data
final_data <- final_data %>%
  dplyr::select(-c(
    load_date, language, transcript, document_type, publication_type
  )) %>%
  bind_cols(copyright_data)
rm(copyright_data)

# Recode categorical variables to be labelled instead of numeric/character
final_data <- final_data %>%
  dplyr::mutate(
    network = labelled(
      as.integer(network_id),
      c(abc = 1, cbs = 2, nbc = 3, cnn = 4, fox = 5, msnbc = 6)
    )
  )

here::here() %>%
  setwd()

# Write to disk with the full transcript and without the full transcript
final_data %>%
  dplyr::select(unique_id, network, date, show, highlight, 
                everything()) %>%
write_rds(., 'complete_data_with_transcript.rds')

# Write the data without the transcript
final_data <- final_data %>%
  dplyr::select(-transcript_body)
final_data %>%
  write_rds(., 'complete_data_without_transcript.rds')
