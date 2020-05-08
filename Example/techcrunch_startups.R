library(text2vec)
library(stopwords)
library(stringr)
library(magrittr)

# a snapshot of TechCrunch's Startups category
# ranging from 2020-01-21 to 2020-03-02 period
tc_startups <- readr::read_csv("Example/techcrunch_startups.csv")

# convert to lowercase and remove numbers (123, 1,500, 0.5, 0.777)
prep_func <- function(v) {
    str_replace_all(tolower(v), "\\d[[:punct:]\\d+]*", "")
}

# make iterators so that text2vec can process data in memory-friendly chunks
# id is good to have but not absolute must
iters  <- itoken(tokens, ids = tc_startups$id, tokenizer = word_tokenizer, preprocessor = prep_func)

# make vocabulary (essentially a data.table dataframe)
vocabs <- create_vocabulary(iters, stopwords = stopwords("en"))

# fine tune vocabulary on case-by-case basis
vocabs <- prune_vocabulary(vocabs, term_count_min = 30, doc_proportion_max = 0.7)

# create document-term matrix
dtm    <- create_dtm(iters, vocab_vectorizer(vocabs), type = "dgTMatrix")

# export as rds (filename extension is abritrary)
saveRDS(dtm, file = "techcrunch_startups.dtm")


