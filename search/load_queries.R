library(XML)

# read query from XML
topics_XML <- xmlParse("../../../data/topics/topics2016.xml")

# convert to dataframe
topics_df <- xmlToDataFrame(topics_XML, stringsAsFactors = FALSE)
