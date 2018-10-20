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
          '(?:\\r\\n){5}CHAPTER.+upon his own merit and without influence\\.',
          dotall = TRUE
        )
      ) %>%
      str_replace(
        regex('END OF VOLUME I.*?APPENDIX', dotall = TRUE),
        ''
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
          'Ind\\.' = 'Indiana',
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

grant_punctuation <-
  grant_sentences %>%
  mutate(sentence =
           sentence %>%
           str_replace_all('[^[:punct:]]', '') %>%
           str_split('')
  ) %>%
  unnest(sentence) %>%
  rename(punctuation = sentence)

longstreet_punctuation <-
  longstreet_sentences %>%
  mutate(sentence =
           sentence %>%
           str_replace_all('[^[:punct:]]', '') %>%
           str_split('')
  ) %>%
  unnest(sentence) %>%
  rename(punctuation = sentence)
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
grant_hapax_words <-
  grant_word_counts %>%
  filter(count == 1) %>%
  pull(word)

grant_dis_words <-
  grant_word_counts %>%
  filter(count == 2) %>%
  pull(word)

grant_tris_words <-
  grant_word_counts %>%
  filter(count == 3) %>%
  pull(word)

grant_tetrakis_words <-
  grant_word_counts %>%
  filter(count == 4) %>%
  pull(word)

grant_hapax_bigrams <-
  grant_bigram_counts %>%
  filter(count == 1) %>%
  pull(bigram)

grant_dis_bigrams <-
  grant_bigram_counts %>%
  filter(count == 2) %>%
  pull(bigram)

grant_tris_bigrams <-
  grant_bigram_counts %>%
  filter(count == 3) %>%
  pull(bigram)

grant_tetrakis_bigrams <-
  grant_bigram_counts %>%
  filter(count == 4) %>%
  pull(bigram)

grant_hapax_trigrams <-
  grant_trigram_counts %>%
  filter(count == 1) %>%
  pull(trigram)

grant_dis_trigrams <-
  grant_trigram_counts %>%
  filter(count == 2) %>%
  pull(trigram)

grant_tris_trigrams <-
  grant_trigram_counts %>%
  filter(count == 3) %>%
  pull(trigram)

grant_tetrakis_trigrams <-
  grant_trigram_counts %>%
  filter(count == 4) %>%
  pull(trigram)

longstreet_hapax_words <-
  longstreet_word_counts %>%
  filter(count == 1) %>%
  pull(word)

longstreet_dis_words <-
  longstreet_word_counts %>%
  filter(count == 2) %>%
  pull(word)

longstreet_tris_words <-
  longstreet_word_counts %>%
  filter(count == 3) %>%
  pull(word)

longstreet_tetrakis_words <-
  longstreet_word_counts %>%
  filter(count == 4) %>%
  pull(word)

longstreet_hapax_bigrams <-
  longstreet_bigram_counts %>%
  filter(count == 1) %>%
  pull(bigram)

longstreet_dis_bigrams <-
  longstreet_bigram_counts %>%
  filter(count == 2) %>%
  pull(bigram)

longstreet_tris_bigrams <-
  longstreet_bigram_counts %>%
  filter(count == 3) %>%
  pull(bigram)

longstreet_tetrakis_bigrams <-
  longstreet_bigram_counts %>%
  filter(count == 4) %>%
  pull(bigram)

longstreet_hapax_trigrams <-
  longstreet_trigram_counts %>%
  filter(count == 1) %>%
  pull(trigram)

longstreet_dis_trigrams <-
  longstreet_trigram_counts %>%
  filter(count == 2) %>%
  pull(trigram)

longstreet_tris_trigrams <-
  longstreet_trigram_counts %>%
  filter(count == 3) %>%
  pull(trigram)

longstreet_tetrakis_trigrams <-
  longstreet_trigram_counts %>%
  filter(count == 4) %>%
  pull(trigram)

shared_hapax_words <- intersect(
  grant_hapax_words,
  longstreet_hapax_words
)

shared_dis_words <- intersect(
  grant_dis_words,
  longstreet_dis_words
)

shared_tris_words <- intersect(
  grant_tris_words,
  longstreet_tris_words
)

shared_tetrakis_words <- intersect(
  grant_tetrakis_words,
  longstreet_tetrakis_words
)

shared_hapax_bigrams <- intersect(
  grant_hapax_bigrams,
  longstreet_hapax_bigrams
)

shared_dis_bigrams <- intersect(
  grant_dis_bigrams,
  longstreet_dis_bigrams
)

shared_tris_bigrams <- intersect(
  grant_tris_bigrams,
  longstreet_tris_bigrams
)

shared_tetrakis_bigrams <- intersect(
  grant_tetrakis_bigrams,
  longstreet_tetrakis_bigrams
)

shared_hapax_trigrams <- intersect(
  grant_hapax_trigrams,
  longstreet_hapax_trigrams
)

shared_dis_trigrams <- intersect(
  grant_dis_trigrams,
  longstreet_dis_trigrams
)

shared_tris_trigrams <- intersect(
  grant_dis_trigrams,
  longstreet_dis_trigrams
)

shared_tetrakis_trigrams <- intersect(
  grant_tetrakis_trigrams,
  longstreet_tetrakis_trigrams
)
# (End)


## @knitr type_token_ratio
#
#
grant_ttr_words_by_text <-
  grant_words %>%
  group_by(text_num) %>%
  summarize(
    word_count = n(),
    distinct_word_count = n_distinct(word),
    ttr_word = distinct_word_count / word_count
  ) %>%
  ungroup()

grant_ttr_words_by_chapter <-
  grant_words %>%
  group_by(chapter_num) %>%
  summarize(
    word_count = n(),
    distinct_word_count = n_distinct(word),
    ttr_word = distinct_word_count / word_count
  ) %>%
  ungroup()

grant_ttr_bigrams_by_text <-
  grant_bigrams %>%
  group_by(text_num) %>%
  summarize(
    bigram_count = n(),
    distinct_bigram_count = n_distinct(bigram),
    ttr_bigram = distinct_bigram_count / bigram_count
  ) %>%
  ungroup()

grant_ttr_bigrams_by_chapter <-
  grant_bigrams %>%
  group_by(chapter_num) %>%
  summarize(
    bigram_count = n(),
    distinct_bigram_count = n_distinct(bigram),
    ttr_bigram = distinct_bigram_count / bigram_count
  ) %>%
  ungroup()

grant_ttr_trigrams_by_text <-
  grant_trigrams %>%
  group_by(text_num) %>%
  summarize(
    trigram_count = n(),
    distinct_trigram_count = n_distinct(trigram),
    ttr_trigram = distinct_trigram_count / trigram_count
  ) %>%
  ungroup()

grant_ttr_trigrams_by_chapter <-
  grant_trigrams %>%
  group_by(chapter_num) %>%
  summarize(
    trigram_count = n(),
    distinct_trigram_count = n_distinct(trigram),
    ttr_trigram = distinct_trigram_count / trigram_count
  ) %>%
  ungroup()

grant_ttr_text <-
  grant_ttr_words_by_text %>%
  inner_join(grant_ttr_bigrams_by_text, by = 'text_num') %>%
  inner_join(grant_ttr_trigrams_by_text, by = 'text_num')

grant_ttr_chapter <-
  grant_ttr_words_by_chapter %>%
  inner_join(grant_ttr_bigrams_by_chapter, by = 'chapter_num') %>%
  inner_join(grant_ttr_trigrams_by_chapter, by = 'chapter_num')

longstreet_ttr_words_by_text <-
  longstreet_words %>%
  group_by(text_num) %>%
  summarize(
    word_count = n(),
    distinct_word_count = n_distinct(word),
    ttr_word = distinct_word_count / word_count
  ) %>%
  ungroup()

longstreet_ttr_words_by_chapter <-
  longstreet_words %>%
  group_by(chapter_num) %>%
  summarize(
    word_count = n(),
    distinct_word_count = n_distinct(word),
    ttr_word = distinct_word_count / word_count
  ) %>%
  ungroup()

longstreet_ttr_bigrams_by_text <-
  longstreet_bigrams %>%
  group_by(text_num) %>%
  summarize(
    bigram_count = n(),
    distinct_bigram_count = n_distinct(bigram),
    ttr_bigram = distinct_bigram_count / bigram_count
  ) %>%
  ungroup()

longstreet_ttr_bigrams_by_chapter <-
  longstreet_bigrams %>%
  group_by(chapter_num) %>%
  summarize(
    bigram_count = n(),
    distinct_bigram_count = n_distinct(bigram),
    ttr_bigram = distinct_bigram_count / bigram_count
  ) %>%
  ungroup()

longstreet_ttr_trigrams_by_text <-
  longstreet_trigrams %>%
  group_by(text_num) %>%
  summarize(
    trigram_count = n(),
    distinct_trigram_count = n_distinct(trigram),
    ttr_trigram = distinct_trigram_count / trigram_count
  ) %>%
  ungroup()

longstreet_ttr_trigrams_by_chapter <-
  longstreet_trigrams %>%
  group_by(chapter_num) %>%
  summarize(
    trigram_count = n(),
    distinct_trigram_count = n_distinct(trigram),
    ttr_trigram = distinct_trigram_count / trigram_count
  ) %>%
  ungroup()

longstreet_ttr_text <-
  longstreet_ttr_words_by_text %>%
  inner_join(longstreet_ttr_bigrams_by_text, by = 'text_num') %>%
  inner_join(longstreet_ttr_trigrams_by_text, by = 'text_num')

longstreet_ttr_chapter <-
  longstreet_ttr_words_by_chapter %>%
  inner_join(longstreet_ttr_bigrams_by_chapter, by = 'chapter_num') %>%
  inner_join(longstreet_ttr_trigrams_by_chapter, by = 'chapter_num')
# (End)


## @knitr tf_idf
#
#
word_text_tf_idf <- grant_word_counts %>%
  mutate(text_num = 1) %>%
  bind_rows(longstreet_word_counts %>% mutate(text_num = 2)) %>%
  bind_tf_idf(word, text_num, count)

bigram_text_tf_idf <- grant_bigram_counts %>%
  mutate(text_num = 1) %>%
  bind_rows(longstreet_bigram_counts %>% mutate(text_num = 2)) %>%
  bind_tf_idf(bigram, text_num, count)

trigram_text_tf_idf <- grant_trigram_counts %>%
  mutate(text_num = 1) %>%
  bind_rows(longstreet_trigram_counts %>% mutate(text_num = 2)) %>%
  bind_tf_idf(trigram, text_num, count)
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

grant_punct_freqs <-
  grant_punctuation %>%
  mutate(total = n()) %>%
  group_by(punctuation) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())

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

longstreet_punct_freqs <-
  longstreet_punctuation %>%
  mutate(total = n()) %>%
  group_by(punctuation) %>%
  summarize(
    total = first(total),
    count = n(),
    frequency = count / total
  ) %>%
  ungroup() %>%
  arrange(frequency %>% desc())
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


## @knitr summary_statistics
#
#

# (End)


## @knitr load_graphics_packages
#
#
suppressPackageStartupMessages({
  library(cowplot)
  library(extrafont)
  library(ggplotify)
  library(ggplot2)
  library(scales)
  library(wordcloud)
})
# (End)


## @knitr declare_fonts
#
#
docx_fonts <- c(
  headings = 'Franklin Gothic Demi Cond',
  body = 'Franklin Gothic Medium Cond'
)
# (End)


## @knitr load_and_check_system_fonts
#
#
if (docx_fonts %in% fonts() %>% all() %>% not()) {
  warning('Importing fonts... this will take some time!')
  font_import(prompt = FALSE)
} else if (docx_fonts %in% fonts() %>% all() %>% not()) {
  stop('Required fonts not found.')
}
# (End)


## @knitr load_and_check_windows_fonts
#
#
if (.Platform$OS.type != 'windows') {
  stop('Unsupported operating system.')
} else if (docx_fonts %in% windowsFonts() %>% all() %>% not()) {
  loadfonts(device = 'win')
}
# (End)


## @knitr declare_colors
#
#
docx_colors <- c(
  text_background_dark1 = rgb(0, 0, 0, maxColorValue = 255),
  text_background_light1 = rgb(255, 255, 255, maxColorValue = 255),
  text_background_dark2 = rgb(69, 95, 81, maxColorValue = 255),
  text_background_light2 = rgb(227, 222, 209, maxColorValue = 255),
  accent1 = rgb(84, 158, 57, maxColorValue = 255),
  accent2 = rgb(138, 184, 51, maxColorValue = 255),
  accent3 = rgb(192, 207, 58, maxColorValue = 255),
  accent4 = rgb(2, 150, 118, maxColorValue = 255),
  accent5 = rgb(74, 181, 196, maxColorValue = 255),
  accent6 = rgb(9, 137, 177, maxColorValue = 255),
  hyperlink = rgb(107, 159, 37, maxColorValue = 255),
  followed_hyperlink = rgb(186, 105, 6, maxColorValue = 255)
)
# (End)


## @knitr declare_desaturation_function
#
#
desaturate = function(colors, ds=0.4, dv=0.7) {
  colors = rgb2hsv(col2rgb(colors))
  colors["v", ] = colors["v", ] + dv * (1 - colors["v", ])
  colors["s", ] = ds * colors["s", ]
  apply(colors, 2, function(color) hsv(color[1], color[2], color[3]))
}
# (End)


## @knitr create_stats_plots
#
#
grant_sentence_lengths <-
  grant_words %>%
  group_by(chapter_num, sentence_num) %>%
  mutate(sentence_length = n()) %>%
  ungroup() %>%
  group_by(chapter_num) %>%
  summarize(
    sentence_length = mean(sentence_length),
  ) %>%
  ggplot(aes(x = chapter_num, y = sentence_length)) +
  geom_hline(aes(yintercept = mean(sentence_length)), size = 1) +
  geom_segment(
    aes(xend = chapter_num, yend = mean(sentence_length)),
    size = 1
  ) +
  geom_point(size = 2) +
  ggtitle(label = 'Grant') +
  scale_x_continuous(name = 'Chapter') +
  scale_y_continuous(name = 'Mean Sentence Length') +
  theme_cowplot() +
  theme(
    text = element_text(family = docx_fonts['body']),
    title = element_text(family = docx_fonts['headings']),
    axis.line = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

longstreet_sentence_lengths <-
  longstreet_words %>%
  group_by(chapter_num, sentence_num) %>%
  mutate(sentence_length = n()) %>%
  ungroup() %>%
  group_by(chapter_num) %>%
  summarize(
    sentence_length = mean(sentence_length),
  ) %>%
  ggplot(aes(x = chapter_num, y = sentence_length)) +
  geom_hline(aes(yintercept = mean(sentence_length)), size = 1) +
  geom_segment(
    aes(xend = chapter_num, yend = mean(sentence_length)),
    size = 1
  ) +
  geom_point(size = 2) +
  ggtitle(label = 'Longstreet') +
  scale_x_continuous(name = 'Chapter') +
  scale_y_continuous(name = 'Mean Sentence Length') +
  theme_cowplot() +
  theme(
    text = element_text(family = docx_fonts['body']),
    title = element_text(family = docx_fonts['headings']),
    axis.line = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

sentence_lengths <-
  plot_grid(grant_sentence_lengths, longstreet_sentence_lengths, ncol = 1)
# (End)


## @knitr create_freq_plots
#
#
grant_word_freq_plot <-
  grant_word_freqs_nostop %>%
  mutate(word = reorder(word, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_bigram_freq_plot <-
  grant_bigram_freqs_nostop %>%
  mutate(bigram = reorder(bigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = bigram)) +
  geom_segment(aes(xend = bigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_trigram_freq_plot <-
  grant_trigram_freqs_nostop %>%
  mutate(trigram = reorder(trigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = trigram)) +
  geom_segment(aes(xend = trigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_punct_freq_plot <-
  grant_punct_freqs %>%
  mutate(punctuation = reorder(punctuation, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = punctuation)) +
  geom_segment(aes(xend = punctuation, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_word_freq_plot <-
  longstreet_word_freqs_nostop %>%
  mutate(word = reorder(word, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_bigram_freq_plot <-
  longstreet_bigram_freqs_nostop %>%
  mutate(bigram = reorder(bigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = bigram)) +
  geom_segment(aes(xend = bigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_trigram_freq_plot <-
  longstreet_trigram_freqs_nostop %>%
  mutate(trigram = reorder(trigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = trigram)) +
  geom_segment(aes(xend = trigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_punct_freq_plot <-
  longstreet_punct_freqs %>%
  mutate(punctuation = reorder(punctuation, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = punctuation)) +
  geom_segment(aes(xend = punctuation, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_word_starting_sentence_freqs_plot <-
  grant_word_starting_sentence_freqs_nostop %>%
  mutate(word = reorder(word, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_word_ending_sentence_freqs_plot <-
  grant_word_ending_sentence_freqs_nostop %>%
  mutate(word = reorder(word, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_bigram_starting_sentence_freqs_plot <-
  grant_bigram_starting_sentence_freqs_nostop %>%
  mutate(bigram = reorder(bigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = bigram)) +
  geom_segment(aes(xend = bigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_bigram_ending_sentence_freqs_plot <-
  grant_bigram_ending_sentence_freqs_nostop %>%
  mutate(bigram = reorder(bigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = bigram)) +
  geom_segment(aes(xend = bigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_trigram_starting_sentence_freqs_plot <-
  grant_trigram_starting_sentence_freqs_nostop %>%
  mutate(trigram = reorder(trigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = trigram)) +
  geom_segment(aes(xend = trigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

grant_trigram_ending_sentence_freqs_plot <-
  grant_trigram_ending_sentence_freqs_nostop %>%
  mutate(trigram = reorder(trigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = trigram)) +
  geom_segment(aes(xend = trigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_word_starting_sentence_freqs_plot <-
  longstreet_word_starting_sentence_freqs_nostop %>%
  mutate(word = reorder(word, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_word_ending_sentence_freqs_plot <-
  longstreet_word_ending_sentence_freqs_nostop %>%
  mutate(word = reorder(word, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_bigram_starting_sentence_freqs_plot <-
  longstreet_bigram_starting_sentence_freqs_nostop %>%
  mutate(bigram = reorder(bigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = bigram)) +
  geom_segment(aes(xend = bigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_bigram_ending_sentence_freqs_plot <-
  longstreet_bigram_ending_sentence_freqs_nostop %>%
  mutate(bigram = reorder(bigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = bigram)) +
  geom_segment(aes(xend = bigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_trigram_starting_sentence_freqs_plot <-
  longstreet_trigram_starting_sentence_freqs_nostop %>%
  mutate(trigram = reorder(trigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = trigram)) +
  geom_segment(aes(xend = trigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_trigram_ending_sentence_freqs_plot <-
  longstreet_trigram_ending_sentence_freqs_nostop %>%
  mutate(trigram = reorder(trigram, frequency)) %>%
  slice(1:5) %>%
  ggplot(aes(x = trigram)) +
  geom_segment(aes(xend = trigram, y = 0, yend = frequency), size = 1) +
  geom_point(aes(y = frequency), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

word_freq_plots <-
  plot_grid(
    grant_word_freq_plot,
    longstreet_word_freq_plot,
    nrow = 1
  )


bigram_freq_plots <-
  plot_grid(
    grant_bigram_freq_plot,
    longstreet_bigram_freq_plot,
    nrow = 1
  )

trigram_freq_plots <-
  plot_grid(
    grant_trigram_freq_plot,
    longstreet_trigram_freq_plot,
    nrow = 1
  )

punct_freq_plots <-
  plot_grid(
    grant_punct_freq_plot,
    longstreet_punct_freq_plot,
    nrow = 1
  )

trigram_freq_plots <-
  plot_grid(
    grant_trigram_freq_plot,
    longstreet_trigram_freq_plot,
    nrow = 1
  )

word_starting_sentence_freqs_plots <-
  plot_grid(
    grant_word_starting_sentence_freqs_plot,
    longstreet_word_starting_sentence_freqs_plot,
    nrow = 1
  )

bigram_starting_sentence_freqs_plots <-
  plot_grid(
    grant_bigram_starting_sentence_freqs_plot,
    longstreet_bigram_starting_sentence_freqs_plot,
    nrow = 1
  )

trigram_starting_sentence_freqs_plots <-
  plot_grid(
    grant_trigram_starting_sentence_freqs_plot,
    longstreet_trigram_starting_sentence_freqs_plot,
    nrow = 1
  )

word_ending_sentence_freqs_plots <-
  plot_grid(
    grant_word_ending_sentence_freqs_plot,
    longstreet_word_ending_sentence_freqs_plot,
    nrow = 1
  )

bigram_ending_sentence_freqs_plots <-
  plot_grid(
    grant_bigram_ending_sentence_freqs_plot,
    longstreet_bigram_ending_sentence_freqs_plot,
    nrow = 1
  )

trigram_ending_sentence_freqs_plots <-
  plot_grid(
    grant_trigram_ending_sentence_freqs_plot,
    longstreet_trigram_ending_sentence_freqs_plot,
    nrow = 1
  )
# (End)


## @knitr create_zipf_plots
#
#
grant_zipf_plot <-
  grant_words %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(freq = count / total) %>%
  ungroup() %>%
  arrange(freq %>% desc()) %>%
  mutate(step_rank = freq %>% dense_rank()) %>%
  ungroup() %>%
  mutate(
    rank = row_number(),
    color_rank =
      case_when(
        step_rank == min(step_rank) ~ 'Hapax Legomenon',
        step_rank == min(step_rank) + 1 ~ 'Dis Legomenon',
        step_rank == min(step_rank) + 2 ~ 'Tris Legomenon',
        step_rank == min(step_rank) + 3 ~ 'Tetrakis Legomenon',
        TRUE ~ '(more frequent)'
      ) %>% factor(levels = c(
        '(more frequent)',
        'Tetrakis Legomenon',
        'Tris Legomenon',
        'Dis Legomenon',
        'Hapax Legomenon'
      )),
    label_rank = if_else(step_rank > max(step_rank) - 2, word, NA_character_)
  ) %>%
  ggplot(aes(x = rank, y = freq, color = color_rank)) +
  geom_point(size = 2) +
  geom_text(
    aes(label = label_rank),
    vjust = -0.3,
    hjust = -1,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_x_log10(name = 'Rank') +
  scale_y_log10(name = 'Frequency',) +
  scale_color_manual(
    name = NULL,
    values = docx_colors[c(1, 7:10)] %>% unname()
  ) +
  ggtitle(label = 'Grant') +
  theme_cowplot() +
  theme(
    text = element_text(family = docx_fonts['body']),
    title = element_text(family = docx_fonts['headings']),
    legend.position = c(0.05, 0.2)
  )

longstreet_zipf_plot <-
  longstreet_words %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = n()) %>%
  group_by(word) %>%
  summarize(freq = count / total) %>%
  ungroup() %>%
  arrange(freq %>% desc()) %>%
  mutate(step_rank = freq %>% dense_rank()) %>%
  ungroup() %>%
  mutate(
    rank = row_number(),
    color_rank =
      case_when(
        step_rank == min(step_rank) ~ 'Hapax Legomenon',
        step_rank == min(step_rank) + 1 ~ 'Dis Legomenon',
        step_rank == min(step_rank) + 2 ~ 'Tris Legomenon',
        step_rank == min(step_rank) + 3 ~ 'Tetrakis Legomenon',
        TRUE ~ '(more frequent)'
      ) %>% factor(levels = c(
        '(more frequent)',
        'Tetrakis Legomenon',
        'Tris Legomenon',
        'Dis Legomenon',
        'Hapax Legomenon'
      )),
    label_rank = if_else(step_rank > max(step_rank) - 2, word, NA_character_)
  ) %>%
  ggplot(aes(x = rank, y = freq, color = color_rank)) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_text(
    aes(label = label_rank),
    vjust = -0.3,
    hjust = -1,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_x_log10(name = 'Rank') +
  scale_y_log10(name = 'Frequency',) +
  scale_color_manual(
    name = NULL,
    values = docx_colors[c(1, 7:10)] %>% unname()
  ) +
  ggtitle(label = 'Longstreet') +
  theme_cowplot() +
  theme(
    text = element_text(family = docx_fonts['body']),
    title = element_text(family = docx_fonts['headings']),
  )

zipf_plots <-
  plot_grid(grant_zipf_plot, longstreet_zipf_plot, nrow = 1, align = 'h')
# (End)


## @knitr create_ttr_plots
#
#
grant_ttr_plot <-
  grant_ttr_words_by_chapter %>%
  ggplot(aes(x = chapter_num, y = ttr_word)) +
  geom_hline(aes(yintercept = mean(ttr_word)), size = 1) +
  geom_segment(
    aes(xend = chapter_num, yend = mean(ttr_word)),
    size = 1
  ) +
  geom_point(size = 2) +
  ggtitle(label = 'Grant') +
  scale_x_continuous(name = 'Chapter') +
  scale_y_continuous(name = 'TTR') +
  theme_cowplot() +
  theme(
    text = element_text(family = docx_fonts['body']),
    title = element_text(family = docx_fonts['headings']),
    axis.line = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

longstreet_ttr_plot <-
  longstreet_ttr_words_by_chapter %>%
    ggplot(aes(x = chapter_num, y = ttr_word)) +
    geom_hline(aes(yintercept = mean(ttr_word)), size = 1) +
    geom_segment(
      aes(xend = chapter_num, yend = mean(ttr_word)),
      size = 1
    ) +
    geom_point(size = 2) +
    ggtitle(label = 'Longstreet') +
    scale_x_continuous(name = 'Chapter') +
    scale_y_continuous(name = 'TTR') +
    theme_cowplot() +
    theme(
      text = element_text(family = docx_fonts['body']),
      title = element_text(family = docx_fonts['headings']),
      axis.line = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank()
    )

ttr_plot <- plot_grid(grant_ttr_plot, longstreet_ttr_plot, ncol = 1)
# (End)


## @knitr create_sentiment_plot
#
#
grant_sentiment_plot <-
  grant_words %>%
  inner_join(get_sentiments(lexicon = 'afinn'), by = 'word') %>%
  group_by(chapter_num) %>%
  summarize(score = mean(score)) %>%
  ggplot(aes(x = chapter_num, y = score)) +
  geom_hline(aes(yintercept = mean(score)), size = 1) +
  geom_segment(
    aes(xend = chapter_num, yend = mean(score)),
    size = 1
  ) +
  geom_point(size = 2) +
  ggtitle(label = 'Grant') +
  scale_x_continuous(name = 'Chapter') +
  scale_y_continuous(name = 'Mean AFINN Sentiment') +
  theme_cowplot() +
  theme(
    text = element_text(family = docx_fonts['body']),
    title = element_text(family = docx_fonts['headings']),
    axis.line = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

longstreet_sentiment_plot <-
  longstreet_words %>%
  inner_join(get_sentiments(lexicon = 'afinn'), by = 'word') %>%
  group_by(chapter_num) %>%
  summarize(score = mean(score)) %>%
  ggplot(aes(x = chapter_num, y = score)) +
  geom_hline(aes(yintercept = mean(score)), size = 1) +
  geom_segment(
    aes(xend = chapter_num, yend = mean(score)),
    size = 1
  ) +
  geom_point(size = 2) +
  ggtitle(label = 'Longstreet') +
  scale_x_continuous(name = 'Chapter') +
  scale_y_continuous(name = 'Mean AFINN Sentiment') +
  theme_cowplot() +
  theme(
    text = element_text(family = docx_fonts['body']),
    title = element_text(family = docx_fonts['headings']),
    axis.line = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

sentiment_plot <- plot_grid(
  grant_sentiment_plot,
  longstreet_sentiment_plot,
  ncol = 1
)
# (End)


## @knitr create_correlation_plot
#
#
grant_corrs <- grant_word_sentence_corrs_nostop %>%
  filter(word1 == 'negro') %>%
  arrange(correlation) %>%
  mutate(word = reorder(word2, correlation)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = correlation), size = 1) +
  geom_point(aes(y = correlation), size = 2) +
  coord_flip() +
  ggtitle('Grant') +
  scale_x_discrete(position = 'top') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

longstreet_corrs <- longstreet_word_sentence_corrs_nostop %>%
  filter(word1 == 'negro') %>%
  arrange(correlation) %>%
  mutate(word = reorder(word2, correlation)) %>%
  slice(1:5) %>%
  ggplot(aes(x = word)) +
  geom_segment(aes(xend = word, y = 0, yend = correlation), size = 1) +
  geom_point(aes(y = correlation), size = 2) +
  coord_flip() +
  ggtitle('Longstreet') +
  scale_x_discrete(position = 'top') +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_cowplot() +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(family = docx_fonts['body']),
        title = element_text(family = docx_fonts['header']))

correlation_plot <-
  plot_grid(grant_corrs, longstreet_corrs, nrow = 1)
# (End)
