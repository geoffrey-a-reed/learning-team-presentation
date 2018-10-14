## @knitr load_analysis_packages
#
#
suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(tidytext)
  library(widyr)
})
# (End)


## @knitr load_stopwords
#
#
smart_stopwords <-
  get_stopwords(language = 'en', source = 'smart') %>%
  pull(word)
# (End)


## @knitr read_and_preprocess_text
#
#
grant_texts <-
  data_frame(
    text = read_file('texts/pg4367.txt'),
    text_num = 1
  ) %>%
  mutate(
    text =
      text %>%
      str_extract(
        regex(
          '(?:\\r\\n){5}CHAPTER.+herculean deeds of valor\\.',
          dotall = TRUE
        )
      )
  )

longstreet_texts <-
  data_frame(
    text = read_file('texts/pg38418.txt'),
    text_num = 2,
  ) %>%
  mutate(
    text =
      text %>%
      str_extract(
        regex(
          '(?:\\r\\n){5}CHAPTER.+Blessings on his brave heart!',
          dotall = TRUE)
      )
  )
# (End)


## @knitr extract_chapters
#
#
grant_chapters <-
  grant_texts %>%
  unnest_tokens(
    chapter,
    text,
    token = 'regex',
    pattern = '(?=CHAPTER)',
    to_lower = FALSE
  ) %>%
  mutate(chapter_num = 1:length(chapter))

longstreet_chapters <-
  longstreet_texts %>%
  unnest_tokens(
    chapter,
    text,
    token = 'regex',
    pattern = '(?=CHAPTER)',
    to_lower = FALSE
  ) %>%
  mutate(chapter_num = 1:length(chapter))
#



## @knitr exampleChunk
#
#
x <- c('testing', 'one', 'two', 'three')
x
# (End)
