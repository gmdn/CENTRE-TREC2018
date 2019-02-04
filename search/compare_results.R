library(dplyr)
library(tidyr)
library(ggplot2)


runs <- c("MRKPrfNote", "MRKSumCln", "MRKUmlsSolr")

for (run in runs) { #run = runs[1]
  
  infNDCG_ims <- results_ims %>% filter(measure == "infNDCG") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  infNDCG_paper <- results_paper %>% filter(measure == "infNDCG") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  ggp <- infNDCG_paper %>% 
    select(one_of(run)) %>%
    rename(paper = one_of(run)) %>%
    bind_cols(infNDCG_ims) %>%
    select(paper, one_of(run)) %>%
    rename(ims = one_of(run)) %>%
    mutate(difference = paper - ims, topic = 1:30) %>%
    ggplot(aes(x = topic, y = difference))
  
  ggp <- ggp + 
    ggtitle(run) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = 1:30, minor_breaks = NULL) +
    ylab("difference infNDCG") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  print(ggp)
  
}


for (run in runs) { #run = runs[1]
  
  infNDCG_ims <- results_ims %>% filter(measure == "infAP") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  infNDCG_paper <- results_paper %>% filter(measure == "infAP") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  ggp <- infNDCG_paper %>% 
    select(one_of(run)) %>%
    rename(paper = one_of(run)) %>%
    bind_cols(infNDCG_ims) %>%
    select(paper, one_of(run)) %>%
    rename(ims = one_of(run)) %>%
    mutate(difference = paper - ims, topic = 1:30) %>%
    ggplot(aes(x = topic, y = difference))
  
  ggp <- ggp + 
    ggtitle(run) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = 1:30, minor_breaks = NULL) +
    ylab("difference infAP") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  print(ggp)
  
}


for (run in runs) { #run = runs[1]
  
  infNDCG_ims <- results_ims %>% filter(measure == "map") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  infNDCG_paper <- results_paper %>% filter(measure == "map") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  ggp <- infNDCG_paper %>% 
    select(one_of(run)) %>%
    rename(paper = one_of(run)) %>%
    bind_cols(infNDCG_ims) %>%
    select(paper, one_of(run)) %>%
    rename(ims = one_of(run)) %>%
    mutate(difference = paper - ims, topic = 1:30) %>%
    ggplot(aes(x = topic, y = difference))
  
  ggp <- ggp + 
    ggtitle(run) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = 1:30, minor_breaks = NULL) +
    ylab("difference map") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  print(ggp)
  
}

for (run in runs) { #run = runs[1]
  
  infNDCG_ims <- results_ims %>% filter(measure == "P_20") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  infNDCG_paper <- results_paper %>% filter(measure == "P_20") %>%
    select(topic, value, run) %>%
    spread(key = run, value = value) %>%
    #select(MRKPrfNote, MRKSumCln, MRKUmlsSolr)
    select(one_of(run))
  
  ggp <- infNDCG_paper %>% 
    select(one_of(run)) %>%
    rename(paper = one_of(run)) %>%
    bind_cols(infNDCG_ims) %>%
    select(paper, one_of(run)) %>%
    rename(ims = one_of(run)) %>%
    mutate(difference = paper - ims, topic = 1:30) %>%
    ggplot(aes(x = topic, y = difference))
  
  ggp <- ggp + 
    ggtitle(run) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = 1:30, minor_breaks = NULL) +
    ylab("difference P@20") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  print(ggp)
  
}
