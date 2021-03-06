The IMS UNIPD Research Group at CENTRE @ TREC2018
-------------------------------------------------

This GitHub repository collects the source code of the paper *The
University of Padua IMS Research Group at CENTRE @ TREC 2018* presented
at the Text REtrieval Conference 2018 (a link will be added as soon as
the proceedings will be online).

### readXMLData

In this subfolder, there is the source code (one single file) used to
parse the PubMed collection in order to extract the fields *pmc*,
*title*, *abstract*, *body*, *journal title*, and *journal type* and
generate an XML file that can be indexed by the Solr search engine.

### search

In the **search** subfolder, you can find the source code used to:

-   query the Solr engine and reproduce the three runs of the original
    paper: *MRKPrfNote.R*, *MRKSumCln.R*, *MRKUmlsSolr.R*. We also added
    a baseline run named *baselineBM25.R*.
-   compute results of the reproduced runs (*compute\_results.R*) and
    compare results with the original runs (*compare\_results.R*).

The reproduced runs are in the folder *./search/run*.
