# read table of UMLS query expansion
terms_query_expansion <- read.table("./data/expanded_query_terms.txt", 
                             sep = "\t", stringsAsFactors = FALSE)
names(terms_query_expansion) <- c("topic", "query", "sem_typ", "code", "preferred", "terms")
#str(terms_query_expansion)

#additional_terms <- terms_query_expansion[terms_query_expansion$topic == 1, ]
