path_to_trec_eval <- "/Users/gmdn/Dropbox/Research/Paper/2018/TREC/trec_eval.9.0/trec_eval"
#path_to_trec_eval <- "/Users/giorgiomariadinunzio/Dropbox/Research/Paper/2018/TREC/trec_eval.9.0/trec_eval"

path_to_sample_eval <- "/Users/gmdn/Dropbox/Research/Paper/2018/TREC/script_inferred_measures/sample_eval.pl"
#path_to_sample_eval <- "/Users/giorgiomariadinunzio/Dropbox/Research/Paper/2018/TREC/script_inferred_measures/sample_eval.pl"

path_to_qrels_sample <- "../../../data/qrels/qrels-sampleval-2016.txt"
path_to_qrels_trec_eval <- "../../../data/qrels/qrels-treceval-2016.txt"

runs <- c("MRKPrfNote", "MRKSumCln", "MRKUmlsSolr")

#path_to_runs <- paste0("run/Lucene_stoplist/ims_unipd-", runs, ".txt")
#path_to_runs <- paste0("run/Smart_stoplist/ims_unipd-", runs, ".txt")
path_to_runs <- paste0("run/Extended_stoplist/ims_unipd-", runs, ".txt")

# index of run
i <- 1

results_ims <- data.frame(measure = character(),
                      topic = integer(),
                      value = numeric(),
                      run = character(),
                      stringsAsFactors = FALSE)

for (path_to_run in path_to_runs) { #path_to_run <- path_to_runs[1]
  
  # run command line and save string
  out_save <- system(command = paste(path_to_trec_eval, 
                                     "-q",
                                     path_to_qrels_trec_eval, 
                                     path_to_run), intern = TRUE)
  
  # get output length
  length_data <- length(out_save)
  
  # split output 
  out_list <- sapply(out_save, strsplit, split = "\t")
  
  # extract elements (and trim white spaces)
  measures <- sapply(out_list, function(x) trimws(x[1]))
  topics <- sapply(out_list, function(x) trimws(x[2]))
  values <- sapply(out_list, function(x) trimws(x[3]))
  
  # rename 
  names(measures) <- NULL
  names(topics) <- NULL
  names(values) <- NULL
  
  # cast values
  values <- as.numeric(values)
  
  # check index of NA value (the line with the name of the run)
  all_idx <- which(is.na(values))
  
  # subset records (only topics, exclude "all")
  run_results <- data.frame(measure = measures[1:(all_idx - 1)],
                            topic = as.integer(topics[1:(all_idx - 1)]),
                            value = as.numeric(values[1:(all_idx - 1)]),
                            run = runs[i],
                            stringsAsFactors = FALSE)
  
  results_ims <- rbind(results_ims, run_results[order(run_results$topic), ])
  
  i <- i + 1
  
}


### INFERRED MEASURES

# index of run
i <- 1

for (path_to_run in path_to_runs) { #path_to_run <- path_to_runs[1]
  
  out_save <- system(command = paste(path_to_sample_eval, 
                                     "-q",
                                     path_to_qrels_sample, 
                                     path_to_run), intern = TRUE)
  
  out_save <- gsub(" ", "", out_save, fixed = TRUE)
  out_save <- gsub("\t", " ", out_save, fixed = TRUE)
  out_save <- gsub("  ", " ", out_save, fixed = TRUE)
  
  length_data <- length(out_save)
  
  out_list <- sapply(out_save, strsplit, split = " ")
  
  # extract elements
  measures <- sapply(out_list, function(x) x[1])
  topics <- sapply(out_list, function(x) x[2])
  values <- sapply(out_list, function(x) x[3])
  
  names(measures) <- NULL
  names(topics) <- NULL
  names(values) <- NULL
  
  # cast values
  topics <- as.integer(topics)
  
  # check index of NA value (the name of the run)
  all_idx <- which(is.na(topics))[1]
  
  
  run_results_inf <- data.frame(measure = measures[1:(all_idx - 1)],
                                topic = as.integer(topics[1:(all_idx - 1)]),
                                value = as.numeric(values[1:(all_idx - 1)]),
                                run = runs[i],
                                stringsAsFactors = FALSE)
  
  #run_results_inf <- run_results_inf[order(run_results_inf$topic), ]
  
  results_ims <- rbind(results_ims, run_results_inf[order(run_results_inf$topic), ])
  
  i <- i + 1
  
}

dim(results_ims)

tail(results_ims)

results_ims %>%
   filter(measure == "infNDCG") %>%
   group_by(run) %>%
   summarise(average = mean(value)) 

results_ims %>%
  filter(measure == "infAP") %>%
  group_by(run) %>%
  summarise(average = mean(value)) 

results_ims %>%
  filter(measure == "Rprec") %>%
  group_by(run) %>%
  summarise(average = mean(value)) 


path_to_runs <- paste0("../../../data/runs/task1/", runs)

# index of run
i <- 1

results_paper <- data.frame(measure = character(),
                            topic = integer(),
                            value = numeric(),
                            run = character(),
                            stringsAsFactors = FALSE)

for (path_to_run in path_to_runs) { #path_to_run <- path_to_runs[1]

  # run command line and save string
  out_save <- system(command = paste(path_to_trec_eval,
                                     "-q",
                                     path_to_qrels_trec_eval,
                                     path_to_run), intern = TRUE)

  # get output length
  length_data <- length(out_save)

  # split output
  out_list <- sapply(out_save, strsplit, split = "\t")

  # extract elements (and trim white spaces)
  measures <- sapply(out_list, function(x) trimws(x[1]))
  topics <- sapply(out_list, function(x) trimws(x[2]))
  values <- sapply(out_list, function(x) trimws(x[3]))

  # rename
  names(measures) <- NULL
  names(topics) <- NULL
  names(values) <- NULL

  # cast values
  values <- as.numeric(values)

  # check index of NA value (the line with the name of the run)
  all_idx <- which(is.na(values))

  # subset records (only topics, exclude "all")
  run_results <- data.frame(measure = measures[1:(all_idx - 1)],
                            topic = as.integer(topics[1:(all_idx - 1)]),
                            value = as.numeric(values[1:(all_idx - 1)]),
                            run = runs[i],
                            stringsAsFactors = FALSE)

  results_paper <- rbind(results_paper, run_results[order(run_results$topic), ])

  i <- i + 1

}


# index of run
i <- 1

for (path_to_run in path_to_runs) { #path_to_run <- path_to_runs[1]

  out_save <- system(command = paste(path_to_sample_eval,
                                     "-q",
                                     path_to_qrels_sample,
                                     path_to_run), intern = TRUE)

  out_save <- gsub(" ", "", out_save, fixed = TRUE)
  out_save <- gsub("\t", " ", out_save, fixed = TRUE)
  out_save <- gsub("  ", " ", out_save, fixed = TRUE)

  length_data <- length(out_save)

  out_list <- sapply(out_save, strsplit, split = " ")

  # extract elements
  measures <- sapply(out_list, function(x) x[1])
  topics <- sapply(out_list, function(x) x[2])
  values <- sapply(out_list, function(x) x[3])

  names(measures) <- NULL
  names(topics) <- NULL
  names(values) <- NULL

  # cast values
  topics <- as.integer(topics)

  # check index of NA value (the name of the run)
  all_idx <- which(is.na(topics))[1]


  run_results_inf <- data.frame(measure = measures[1:(all_idx - 1)],
                                topic = as.integer(topics[1:(all_idx - 1)]),
                                value = as.numeric(values[1:(all_idx - 1)]),
                                run = runs[i],
                                stringsAsFactors = FALSE)

  #run_results_inf <- run_results_inf[order(run_results_inf$topic), ]

  results_paper <- rbind(results_paper, run_results_inf[order(run_results_inf$topic), ])

  i <- i + 1

}

dim(results_paper)

head(results_paper)

#library(xtable)
library(dplyr)
library(ggplot2)

res_ims <- results_ims %>%
  filter(measure == "infNDCG" & run == "MRKUmlsSolr") %>%
  select(topic, value) %>%
  rename(ims = value) 

res_ori <- results_paper %>%
  filter(measure == "infNDCG" & run == "MRKUmlsSolr") %>%
  select(topic, value) %>%
  rename(paper = value) 

ggp <- res_ims %>%
  inner_join(res_ori) %>%
  mutate(difference = ims - paper) %>%
  ggplot(aes(x = topic, y = difference, fill = as.factor(sign(difference))))

ggp + geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_continuous(breaks = 1:30, minor_breaks = NULL) +
  ylab("difference infNDCG") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = alpha(c("red", "blue", "green"), .5))

t.test(paired = TRUE, x = res_ims$ims, y = res_ori$paper)


res_ims <- results_ims %>%
  filter(measure == "infAP" & run == "MRKUmlsSolr") %>%
  select(topic, value) %>%
  rename(ims = value) 

res_ori <- results_paper %>%
  filter(measure == "infAP" & run == "MRKUmlsSolr") %>%
  select(topic, value) %>%
  rename(paper = value) 


ggp <- res_ims %>%
  inner_join(res_ori) %>%
  mutate(difference = ims - paper) %>%
  ggplot(aes(x = topic, y = difference, fill = as.factor(sign(difference))))


ggp + geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_continuous(breaks = 1:30, minor_breaks = NULL) +
  ylab("difference infAP") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = alpha(c("red", "blue", "green"), .5))

t.test(paired = TRUE, x = res_ims$ims, y = res_ori$paper)


res_ims <- results_ims %>%
  filter(measure == "Rprec" & run == "MRKUmlsSolr") %>%
  select(topic, value) %>%
  rename(ims = value) 

res_ori <- results_paper %>%
  filter(measure == "Rprec" & run == "MRKUmlsSolr") %>%
  select(topic, value) %>%
  rename(paper = value) 

ggp <- res_ims %>%
  inner_join(res_ori) %>%
  mutate(difference = ims - paper) %>%
  ggplot(aes(x = topic, y = difference, fill = as.factor(sign(difference))))


ggp + geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_continuous(breaks = 1:30, minor_breaks = NULL) +
  ylab("difference RPrec") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = alpha(c("red", "green", "blue"), .5))

t.test(paired = TRUE, x = res_ims$ims, y = res_ori$paper)
