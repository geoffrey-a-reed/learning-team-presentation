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
  mutate(chapter_num = c(0, 1:(length(chapter) - 1))) %>%
  filter(chapter_num > 0)

longstreet_chapters <-
  longstreet_texts %>%
  unnest_tokens(
    chapter,
    text,
    token = 'regex',
    pattern = '(?=CHAPTER)',
    to_lower = FALSE
  ) %>%
  mutate(chapter_num = c(0, 1:(length(chapter) - 1))) %>%
  filter(chapter_num > 0)
# (End)


## @knitr extract_paragraphs
#
#
grant_paragraphs <-
  grant_chapters %>%
  mutate(
    chapter = chapter %>% str_replace('^CHAPTER\\s+\\p{Lu}+\\.\\s+', '')
  ) %>%
  unnest_tokens(
    paragraph,
    chapter,
    token = 'regex',
    pattern = '(?:\\r\\n\\r\\n)',
    to_lower = FALSE
  ) %>%
  mutate(
    paragraph = paragraph %>% str_trim() %>% str_squish(),
    paragraph = paragraph %>% str_replace_all('\\*', ''),
    paragraph = paragraph %>% str_replace_all('\\p{Lu}+:--', '')
  ) %>%
  filter(
    paragraph %>% str_detect('^[\\p{Lu}\\p{Nd}-.,\'\\s]{3,}$') %>% not(),
    paragraph %>% str_detect('I am very respectfully') %>% not(),
    paragraph %>% str_detect('^To Brigadier-General') %>% not(),
    paragraph %>% str_detect('I am, sir, very respectfully') %>% not(),
    paragraph %>% str_detect('General S\\.B\\. Buckner, Confeder') %>% not(),
    paragraph %>% str_detect('^To Brig\\. Gen\'l') %>% not(),
    paragraph %>% str_detect('I am, sir, [Yy]our very ob') %>% not(),
    paragraph %>% str_detect('[Vv]ery respectfully, your ob') %>% not(),
    paragraph %>% str_detect('\\p{Lu}{3,}') %>% not(),
    paragraph %>% str_detect('[.]{4,}') %>% not(),
    paragraph %>% str_detect('^\\W*$') %>% not(),
    paragraph %>% str_detect('Commanding A\\. P\\.') %>% not(),
    paragraph %>% str_detect('^August 1, 1864') %>% not(),
    paragraph %>% str_detect('^Cypher\\. 6') %>% not(),
    paragraph %>% str_detect('^"?Ap(?:ril)? \\d{1,2}(?:th)?, \\d{4}') %>% not(),
    paragraph %>% str_detect('^"?October 11, 1864') %>% not()
  ) %>%
  mutate(paragraph_num = 1:length(paragraph))

longstreet_paragraphs <-
  longstreet_chapters %>%
  mutate(
    chapter = chapter %>% str_replace('^CHAPTER\\s+\\p{Lu}+\\.\\s+', '')
  ) %>%
  unnest_tokens(
    paragraph,
    chapter,
    token = 'regex',
    pattern = '(?:\\r\\n\\r\\n)',
    to_lower = FALSE
  ) %>%
  mutate(
    paragraph =
      paragraph %>%
      str_replace_all(regex('\\[.*\\]', dotall = TRUE), '') %>%
      str_replace_all('"\\p{Lu}+,--', '') %>%
      str_replace_all('_', '') %>%
      str_trim() %>%
      str_squish()
  ) %>%
  filter(
    paragraph %>% str_detect('\\p{Lu}{3,}') %>% not(),
    paragraph %>% str_detect('^\\W*$') %>% not(),
    paragraph %>% str_detect('[\\w ]+--[\\w ]+--[\\w ]+--[\\w ]+') %>% not(),
    paragraph %>% str_detect('\\d+$') %>% not(),
    paragraph %>% str_detect('-{4,}') %>% not(),
    paragraph %>% str_detect('^"?Sept(?:ember)? \\d{1,2}') %>% not(),
    paragraph %>% str_detect('I am, sir, respectfully') %>% not(),
    paragraph %>% str_detect('\\(Endorsement\\.\\)') %>% not(),
    paragraph %>% str_detect('I am, general, very respectfully') %>% not(),
    paragraph %>% str_detect('"?July \\d{1,2}') %>% not(),
    paragraph %>% str_detect('Commanding Confederate States Army') %>% not(),
    paragraph %>% str_detect('"?April \\d{1,2}') %>% not(),
    paragraph %>% str_detect('^"R\\.E\\. Lee,"') %>% not(),
    paragraph %>% str_detect('most obedient and humble servant') %>% not()
  ) %>%
  mutate(paragraph_num = 1:length(paragraph))
# (End)


## @knitr extract_sentences
#
#
grant_sentences <-
  grant_paragraphs %>%
  mutate(
    paragraph =
      paragraph %>%
      str_replace_all(
        c(
          'Mrs\\.' = 'Missus',
          'Jesse R\\. Grant' = 'Jesse Root Grant',
          'Mr\\.' = 'Mister',
          'John D\\. White' = 'John White',
          'Thomas L\\. Hamer' = 'Thomas Hamer',
          'General A\\. ?V\\. Kautz' = 'General Kautz',
          'C\\.F\\. Smith' = 'Charles Ferguson Smith',
          'F\\. ?T\\. Dent' = 'Mister Dent',
          'St\\.' = 'Saint',
          'U\\. ?S\\.' = 'US',
          'B\\.B\\. Howard' = 'Mister Howard',
          'E\\.B\\. Washburne' = 'Mister Washburne',
          'Hon\\.' = 'Honorable',
          'F\\.P\\. Blair' = 'Mister Blair',
          'S\\.A\\. Douglas' = 'Stephen Arnold Douglas',
          'John M\\. Palmer' = 'John Palmer',
          'C\\.B\\.' = '',
          'B\\.M\\.' = '',
          'Jefferson C\\. Davis' = 'Jefferson Columbus Davis',
          'W\\.H\\.L\\.' = '',
          'H\\.\\W.' = '',
          'S\\.B\\.' = '',
          'Lew\\.' = '',
          'J\\.D\\.' = '',
          'Col\\.' = 'Colonel',
          'Dr\\.' = 'Doctor',
          '1st\\.' = 'First:',
          '2d\\.' = 'Second:',
          '3d\\.' ='Third:',
          '\\b\\p{Lu}\\.{1,2}' = '',
          '\\(\\d+\\)' = '',
          '\\s+\\.\\s+' = ' '
        )
      )
  ) %>%
  unnest_tokens(sentence, paragraph, token = 'sentences', to_lower = FALSE) %>%
  mutate(
    sentence = sentence %>% str_trim() %>% str_squish(),
    sentence_num = 1:length(sentence)
  )

longstreet_sentences <-
  longstreet_paragraphs %>%
  mutate(
    paragraph =
      paragraph %>%
      str_replace_all(
        c(
          'Mr\\.' = 'Mister',
          'Dr\\.' = 'Doctor',
          'Mrs\\.' = 'Missus',
          '1\\.' = 'One: ',
          '2\\.' = 'Two: ',
          '3\\.' = 'Three: ',
          '4\\.' = 'Four: ',
          'First\\.' = 'First:',
          'Second\\.' = 'Second:',
          'Third\\.' = 'Third:;',
          'Fourth\\.' = 'Fourth:',
          'Fifth\\.' = 'Fifth:',
          'Sixth\\.' = 'Sixth:',
          'Inf\\.' = 'Infantry',
          'Col\\.' = 'Colonel',
          'Battn\\.' = 'Battalion',
          'Cav\\.' = 'Cavalry',
          'Co\\.' = 'Company',
          'Ind\\.' = '',
          'Capt\\.' = 'Captain',
          'Regt\\.' = 'Regiment',
          'Lieut\\.' = 'Lieutenant',
          '\\b\\p{Lu}\\.{1,2}' = ''
        )
      )
  ) %>%
  unnest_tokens(sentence, paragraph, token = 'sentences', to_lower = FALSE) %>%
  mutate(
    sentence = sentence %>% str_trim() %>% str_squish(),
    sentence_num = 1:length(sentence)
  ) %>% View()
# (End)





## @knitr exampleChunk
#
#
x <- c('testing', 'one', 'two', 'three')
x
# (End)
