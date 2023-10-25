
# COIPoND Data Collection -------------------------------------------------

# This file provides the key assets for PubMed search queries and article metadata collection. 

# Pre-Flight --------------------------------------------------------------

#load libraries 
library(tidyverse)
library(rentrez)
library(stringr)
library(XML)

# Set Entrez API key (https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/)
key = ""
set_entrez_key(key)

#load target queries
queries <- ###



# Get Target PMIDs --------------------------------------------------------

# Define PubMed Search Function 
pmsearch <- function(x){
  res <- entrez_search(db="pubmed", term = queries$query[x], retmax=200)
  cbind.data.frame(id = queries$id[x], search=queries$query[x],trans=res$QueryTranslation,res=res$count,pmids=res$ids) %>% 
    mutate(comp = search==trans)
}         


# Get Article Meta Data ---------------------------------------------------

# Define Extract Text Function 
extract_text <- function(x){
  file <- entrez_fetch("pubmed", id = x,  rettype="xml",parsed = FALSE)
  xml_1 <- xmlParse(file)
  title <- paste(xpathSApply(xml_1, '//ArticleTitle', xmlValue, encoding = "UTF-8"), collapse = "|")
  abstract <- paste(xpathSApply(xml_1, '//AbstractText', xmlValue, encoding = "UTF-8"), collapse = "|")
  PubDate <- paste(xpathSApply(xml_1, '//PubDate', xmlValue, encoding = "UTF-8"), collapse = "|")
  journal <- paste(xpathSApply(xml_1, '//MedlineTA', xmlValue, encoding = "UTF-8"), collapse = "|")
  PubType <- paste(xpathSApply(xml_1, '//PublicationType', xmlValue, encoding = "UTF-8"), collapse = "|")
  lang <- paste(xpathSApply(xml_1, '//Language', xmlValue, encoding = "UTF-8"), collapse = "|")
  doi <- paste(xpathSApply(xml_1, '//ELocationID[@EIdType="doi"]', xmlValue, encoding = "UTF-8"), collapse = "|")
  pmcid <- paste(xpathSApply(xml_1, '//ArticleId [@IdType="pmc"]', xmlValue, encoding = "UTF-8"), collapse = "|")
  coi <- paste(xpathSApply(xml_1, '//CoiStatement', xmlValue, encoding = "UTF-8"), collapse = "|")
  pmid <- paste(xpathSApply(xml_1, '//PMID', xmlValue, encoding = "UTF-8"), collapse = "|")
  pmid.val <- x
  Sys.sleep(.101)
  cbind.data.frame(pmid,pmid.val,pmcid,PubDate,title,journal,PubType,abstract,lang,doi,coi)
}




