library(solrium)
library(dplyr)

source("load_queries.R")

# connect to Solr client
solr_con <- SolrClient$new(host = "localhost", path = "/solr/ct_stemming/query")

# set number of documents to retrieve 
num_docs <- 1000

# set run name
run_name <- "baseline_BM25"

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
for (topic in topics_df$summary) { # query <- topics_df$summary[1]
#for (topic in topics_df$note) { # query <- topics_df$summary[1]
  
  print(paste("ranking topic", topic_idx))
  
  # rank (first 1000) documents
  ranking <- solr_con$search(params = list(q = topic, rows = num_docs, fl = "pmc, score"))
  
  # create TREC style format
  ranking <- ranking %>% 
    mutate(topic = topic_idx, Q0 = "Q0", ranking = 1:num_docs, run_name) %>%
    select(topic, Q0, pmc, ranking, score, run_name)
  
  run <- rbind(run, ranking)
  
  topic_idx <- topic_idx + 1
  
}

write.table(run, paste0("./run/", run_name, ".txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
