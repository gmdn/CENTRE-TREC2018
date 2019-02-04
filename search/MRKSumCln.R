library(solrium)
library(dplyr)

source("load_queries.R")
#source("load_UMLS_expansions.R")

# connect to Solr client
solr_con <- SolrClient$new(host = "localhost", path = "/solr/ct_stoplist/query")

# set number of documents to retrieve 
num_docs <- 1000

# set number of documents for Pseudo Relevance Feedback
k <- 30

# set run name
run_name <- "MRKSumCln"

# prepare dataframe
run <- tibble(topic = character(),
              Q0 = character(),
              document = character(),
              ranking = integer(),
              score = numeric(),
              run_name = character()
              )

# set topic id
topic_idx <- 1

# get query
for (topic in topics_df$summary) { # topic <- topics_df$summary[1]
#for (topic in topics_df$note) { # topic <- topics_df$note[1]
  
  print(paste("ranking topic", topic_idx))
  
  topic_terms <- tolower(topic)
  topic_terms <- gsub("[[:punct:]]+", " ", topic_terms)
  topic_terms <- trimws(gsub("[ ]+", " ", topic_terms))
  topic_terms <- unique(strsplit(topic_terms, split = " ")[[1]])
  topic_terms <- paste0(topic_terms, sep = "^1.0", collapse = " ")
  
  # PRF round
  prf_ranking <- solr_con$search(params = list(q = topic_terms, 
                                           rows = k, 
                                           fl = "pmc, title"))
  
  # get title terms
  prf_terms <- paste(prf_ranking$title, collapse = " ")
  prf_terms <- tolower(prf_terms)
  prf_terms <- gsub("[[:punct:]]+", " ", prf_terms)
  prf_terms <- trimws(gsub("[ ]+", " ", prf_terms))
  prf_terms <- unique(strsplit(prf_terms, split = " ")[[1]])
  prf_terms <- paste0(prf_terms, sep = "^0.1", collapse = " ")
  
  
  # rank (first 1000) documents
  ranking <- solr_con$search(params = list(q = paste(topic_terms, prf_terms), 
                                           rows = num_docs, 
                                           fl = "pmc, score"))
  
  
  # create TREC style format
  ranking <- ranking %>% 
    mutate(topic = topic_idx, Q0 = "Q0", ranking = 1:num_docs, run_name) %>%
    select(topic, Q0, pmc, ranking, score, run_name)
  
  run <- rbind(run, ranking)
  
  topic_idx <- topic_idx + 1
  
}

write.table(run, paste0("./run/", run_name, ".txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
