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
  )
# (End)


## @knitr extract_tokens
#
#
grant_words <-
  grant_sentences %>%
  unnest_tokens(word, sentence, token = 'words') %>%
  filter(word %>% str_detect('\\d+') %>% not()) %>%
  mutate(word_num = 1:length(word))

grant_words_nostop <-
  grant_words %>%
  filter((word %in% smart_stopwords) %>% not())

grant_bigrams <-
  grant_sentences %>%
  unnest_tokens(bigram, sentence, token = 'ngrams', n = 2) %>%
  filter(
    bigram %>% is.na() %>% not(),
    bigram %>% str_detect('\\d+') %>% not()
  ) %>%
  mutate(bigram_num = 1:length(bigram))

grant_bigrams_nostop <-
  grant_bigrams %>%
  separate(bigram, c('word1', 'word2'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not()
  ) %>%
  unite(bigram, c('word1', 'word2'), sep = ' ')

grant_trigrams <-
  grant_sentences %>%
  unnest_tokens(trigram, sentence, token = 'ngrams', n = 3) %>%
  filter(
    trigram %>% is.na() %>% not(),
    trigram %>% str_detect('\\d+') %>% not()
  ) %>%
  mutate(trigram_num = 1:length(trigram))

grant_trigrams_nostop <-
  grant_trigrams %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not(),
    (word3 %in% smart_stopwords) %>% not()
  ) %>%
  unite(trigram, c('word1', 'word2', 'word3'), sep = ' ')

longstreet_words <-
  longstreet_sentences %>%
  unnest_tokens(word, sentence, token = 'words') %>%
  filter(word %>% str_detect('\\d+') %>% not()) %>%
  mutate(word_num = 1:length(word))

longstreet_words_nostop <-
  longstreet_words %>%
  filter((word %in% smart_stopwords) %>% not())

longstreet_bigrams <-
  longstreet_sentences %>%
  unnest_tokens(bigram, sentence, token = 'ngrams', n = 2) %>%
  filter(
    bigram %>% is.na() %>% not(),
    bigram %>% str_detect('\\d+') %>% not()
  ) %>%
  mutate(bigram_num = 1:length(bigram))

longstreet_bigrams_nostop <-
  longstreet_bigrams %>%
  separate(bigram, c('word1', 'word2'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not()
  ) %>%
  unite(bigram, c('word1', 'word2'), sep = ' ')

longstreet_trigrams <-
  longstreet_sentences %>%
  unnest_tokens(trigram, sentence, token = 'ngrams', n = 3) %>%
  filter(
    trigram %>% is.na() %>% not(),
    trigram %>% str_detect('\\d+') %>% not()
  ) %>%
  mutate(trigram_num = 1:length(trigram))

longstreet_trigrams_nostop <-
  longstreet_trigrams %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not(),
    (word3 %in% smart_stopwords) %>% not()
  ) %>%
  unite(trigram, c('word1', 'word2', 'word3'), sep = ' ')
# (End)


## @knitr token_counts
#
#
grant_word_counts <-
  grant_words %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count)

grant_word_chapter_counts <-
  grant_words %>%
  group_by(chapter_num, word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

grant_bigram_counts <-
  grant_bigrams %>%
  group_by(bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count)

grant_bigram_chapter_counts <-
  grant_bigrams %>%
  group_by(chapter_num, bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

grant_trigram_counts <-
  grant_trigrams %>%
  group_by(trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count)

grant_trigram_chapter_counts <-
  grant_trigrams %>%
  group_by(chapter_num, trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

longstreet_word_counts <-
  longstreet_words %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count)

longstreet_word_chapter_counts <-
  longstreet_words %>%
  group_by(chapter_num, word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

longstreet_bigram_counts <-
  longstreet_bigrams %>%
  group_by(bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count)

longstreet_bigram_chapter_counts <-
  longstreet_bigrams %>%
  group_by(chapter_num, bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

longstreet_trigram_counts <-
  longstreet_trigrams %>%
  group_by(trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count)

longstreet_trigram_chapter_counts <-
  longstreet_trigrams %>%
  group_by(chapter_num, trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)
# (End)


## @knitr extract_hapax
#

# (End)


## @knitr type_token_ratio
#
#

# (End)


## @knitr declare_tidykwic
#
#

# (End)


## @knitr kwic_hapax
#
#

# (End)


## @knitr tf_idf
#
#

# (End)


## @knitr token_frequencies
#
#
grant_word_freqs_nostop <-
  grant_words_nostop %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_word_chapter_freqs_nostop <-
  grant_words_nostop %>%
  group_by(chapter_num) %>%
  mutate(chapter_total = n()) %>%
  ungroup() %>%
  group_by(chapter_num, word) %>%
  summarize(
    chapter_total = first(chapter_total),
    count = n(),
    frequency = count / chapter_total
  ) %>%
  ungroup() %>%
  arrange(chapter_num, frequency %>% desc())

grant_bigram_freqs_nostop <-
  grant_bigrams_nostop %>%
  mutate(total = n()) %>%
  group_by(bigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_bigram_chapter_freqs_nostop <-
  grant_bigrams_nostop %>%
  group_by(chapter_num) %>%
  mutate(chapter_total = n()) %>%
  ungroup() %>%
  group_by(chapter_num, bigram) %>%
  summarize(
    chapter_total = first(chapter_total),
    count = n(),
    frequency = count / chapter_total
  ) %>%
  ungroup() %>%
  arrange(chapter_num, frequency %>% desc())

grant_trigram_freqs_nostop <-
  grant_trigrams_nostop %>%
  mutate(total = n()) %>%
  group_by(trigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_trigram_chapter_freqs_nostop <-
  grant_trigrams_nostop %>%
  group_by(chapter_num) %>%
  mutate(chapter_total = n()) %>%
  ungroup() %>%
  group_by(chapter_num, trigram) %>%
  summarize(
    chapter_total = first(chapter_total),
    count = n(),
    frequency = count / chapter_total
  ) %>%
  ungroup() %>%
  arrange(chapter_num, frequency %>% desc())

longstreet_word_freqs_nostop <-
  longstreet_words_nostop %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_word_chapter_freqs_nostop <-
  longstreet_words_nostop %>%
  group_by(chapter_num) %>%
  mutate(chapter_total = n()) %>%
  ungroup() %>%
  group_by(chapter_num, word) %>%
  summarize(
    chapter_total = first(chapter_total),
    count = n(),
    frequency = count / chapter_total
  ) %>%
  ungroup() %>%
  arrange(chapter_num, frequency %>% desc())

longstreet_bigram_freqs_nostop <-
  longstreet_bigrams_nostop %>%
  mutate(total = n()) %>%
  group_by(bigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_bigram_chapter_freqs_nostop <-
  longstreet_bigrams_nostop %>%
  group_by(chapter_num) %>%
  mutate(chapter_total = n()) %>%
  ungroup() %>%
  group_by(chapter_num, bigram) %>%
  summarize(
    chapter_total = first(chapter_total),
    count = n(),
    frequency = count / chapter_total
  ) %>%
  ungroup() %>%
  arrange(chapter_num, frequency %>% desc())

longstreet_trigram_freqs_nostop <-
  longstreet_trigrams_nostop %>%
  mutate(total = n()) %>%
  group_by(trigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_trigram_chapter_freqs_nostop <-
  longstreet_trigrams_nostop %>%
  group_by(chapter_num) %>%
  mutate(chapter_total = n()) %>%
  ungroup() %>%
  group_by(chapter_num, trigram) %>%
  summarize(
    chapter_total = first(chapter_total),
    count = n(),
    frequency = count / chapter_total
  ) %>%
  ungroup() %>%
  arrange(chapter_num, frequency %>% desc())
# (End)


## @knitr positional_token_frequencies
#
#
grant_word_starting_sentence_freqs_nostop <-
  grant_words %>%
  group_by(sentence_num) %>%
  summarize(word = first(word)) %>%
  ungroup() %>%
  filter((word %in% smart_stopwords) %>% not()) %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_bigram_starting_sentence_freqs_nostop <-
  grant_bigrams %>%
  group_by(sentence_num) %>%
  summarize(bigram = first(bigram)) %>%
  ungroup() %>%
  separate(bigram, c('word1', 'word2'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not()
  ) %>%
  unite(bigram, c('word1', 'word2'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(bigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_trigram_starting_sentence_freqs_nostop <-
  grant_trigrams %>%
  group_by(sentence_num) %>%
  summarize(trigram = first(trigram)) %>%
  ungroup() %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not(),
    (word3 %in% smart_stopwords) %>% not()
  ) %>%
  unite(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(trigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_word_ending_sentence_freqs_nostop <-
  grant_words %>%
  group_by(sentence_num) %>%
  summarize(word = last(word)) %>%
  ungroup() %>%
  filter((word %in% smart_stopwords) %>% not()) %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_bigram_ending_sentence_freqs_nostop <-
  grant_bigrams %>%
  group_by(sentence_num) %>%
  summarize(bigram = last(bigram)) %>%
  ungroup() %>%
  separate(bigram, c('word1', 'word2'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not()
  ) %>%
  unite(bigram, c('word1', 'word2'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(bigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

grant_trigram_ending_sentence_freqs_nostop <-
  grant_trigrams %>%
  group_by(sentence_num) %>%
  summarize(trigram = last(trigram)) %>%
  ungroup() %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not(),
    (word3 %in% smart_stopwords) %>% not()
  ) %>%
  unite(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(trigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_word_starting_sentence_freqs_nostop <-
  longstreet_words %>%
  group_by(sentence_num) %>%
  summarize(word = first(word)) %>%
  ungroup() %>%
  filter((word %in% smart_stopwords) %>% not()) %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_bigram_starting_sentence_freqs_nostop <-
  longstreet_bigrams %>%
  group_by(sentence_num) %>%
  summarize(bigram = first(bigram)) %>%
  ungroup() %>%
  separate(bigram, c('word1', 'word2'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not()
  ) %>%
  unite(bigram, c('word1', 'word2'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(bigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_trigram_starting_sentence_freqs_nostop <-
  longstreet_trigrams %>%
  group_by(sentence_num) %>%
  summarize(trigram = first(trigram)) %>%
  ungroup() %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not(),
    (word3 %in% smart_stopwords) %>% not()
  ) %>%
  unite(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(trigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_word_ending_sentence_freqs_nostop <-
  longstreet_words %>%
  group_by(sentence_num) %>%
  summarize(word = last(word)) %>%
  ungroup() %>%
  filter((word %in% smart_stopwords) %>% not()) %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_bigram_ending_sentence_freqs_nostop <-
  longstreet_bigrams %>%
  group_by(sentence_num) %>%
  summarize(bigram = last(bigram)) %>%
  ungroup() %>%
  separate(bigram, c('word1', 'word2'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not()
  ) %>%
  unite(bigram, c('word1', 'word2'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(bigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

longstreet_trigram_ending_sentence_freqs_nostop <-
  longstreet_trigrams %>%
  group_by(sentence_num) %>%
  summarize(trigram = last(trigram)) %>%
  ungroup() %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = '\\s+') %>%
  filter(
    (word1 %in% smart_stopwords) %>% not(),
    (word2 %in% smart_stopwords) %>% not(),
    (word3 %in% smart_stopwords) %>% not()
  ) %>%
  unite(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  mutate(total = n()) %>%
  group_by(trigram) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())
# (End)


## @knitr token_correlations
#
#
grant_word_sentence_corrs_nostop <-
  grant_words_nostop %>%
  group_by(word) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 5) %>%
  pairwise_cor(word, sentence_num, method = 'pearson') %>%
  filter(correlation < 0.99) %>%
  rename(word1 = item1, word2 = item2) %>%
  arrange(correlation %>% desc())

longstreet_word_sentence_corrs_nostop <-
  longstreet_words_nostop %>%
  group_by(word) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 5) %>%
  pairwise_cor(word, sentence_num, method = 'pearson') %>%
  filter(correlation < 0.99) %>%
  rename(word1 = item1, word2 = item2) %>%
  arrange(correlation %>% desc())
# (End)





## @knitr


## @knitr exampleChunk
#
#
x <- c('testing', 'one', 'two', 'three')
x
# (End)
