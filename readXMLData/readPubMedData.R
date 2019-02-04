library(XML)
library(rvest)

# Path to folder with PubMed data
path_to_dir <- "~/Downloads/pmc-00"
#path_to_dir <- "~/Downloads/pmc-01"
#path_to_dir <- "~/Downloads/pmc-02"
#path_to_dir <- "~/Downloads/pmc-03"

list_dir <- list.files(path = path_to_dir, full.names = T)

for (dir in list_dir) { #dir <- list_dir[1]
  
  print(dir)
  
  add_node <- xmlTree(tag = "add")
  
  files_nxml <- list.files(dir)
  
  for (file_nxml in files_nxml) { #file_nxml <- files_nxml[1]
    
    # open xml tag
    add_node$addNode("doc", close = FALSE)
    
    # read document
    doc <- xmlParse(file = paste0(dir, "/", file_nxml))  
    
    # get pmc id
    pmc <- unlist(xpathApply(doc, 
                             "/article/front/article-meta/article-id[@pub-id-type=\"pmc\"]",
                             xmlValue))
    
    # add pmc node
    add_node$addNode(name = "field", attrs = c(name = "pmc"), pmc)
    
    # get title
    title <- unlist(xpathApply(doc, 
                               "/article/front/article-meta/title-group/article-title",
                               xmlValue))
    
    if (is.null(title)) {
      # add title node
      add_node$addNode(name = "field", 
                       attrs = c(name = "title"))
    } else {
      # add title node
      add_node$addNode(name = "field", 
                       attrs = c(name = "title"), title)
    }
    
    # get abstract
    abstract <- xpathApply(doc, 
                           "/article/front/article-meta/abstract")
    
    if (length(abstract) == 0) {
      # add abstract node
      add_node$addNode(name = "field", 
                       attrs = c(name = "abstract"))
    } else {
      
      abstract <- paste(unlist(xmlToList(abstract[[1]], simplify = T)), collapse = " ")
      
      # add abstract node
      add_node$addNode(name = "field", 
                       attrs = c(name = "abstract"), abstract)
    }
    
    # get body
    body <- unlist(xpathApply(doc, 
                              "/article/body"))
    
    if (length(body) == 0) {
      # add body node
      add_node$addNode(name = "field", 
                       attrs = c(name = "body"))
    } else {
      body <- paste(unlist(xmlToList(body[[1]], simplify = T)), collapse = " ")
      # add body node
      add_node$addNode(name = "field", 
                       attrs = c(name = "body"), body)
    }
    
    # get journal title
    journal_title <- unlist(xpathApply(doc, 
                                       "/article/front/journal-meta/journal-title", 
                                       xmlValue))
    
    if (length(journal_title) == 0) {
      # add journal title node
      add_node$addNode(name = "field", 
                       attrs = c(name = "journal-title"))
    } else {
      # add journal title node
      add_node$addNode(name = "field", 
                       attrs = c(name = "journal-title"), journal_title)
    }
    
    # close doc tag
    add_node$closeTag()
    
  }
  
  saveXML(doc = add_node, 
          #file = "/Volumes/INTENSO/pmc-00-solr/temp_100.xml", encoding = "UTF-8")
          file = paste0(path_to_dir, "-solr/", 
                        substr(dir, start = nchar(dir) - 1, stop = nchar(dir)), 
                        ".xml"), 
          encoding = "UTF-8")
  
  #break
  
}
