
# PMC Full Text Download and Disclosure Extraction ------------------------
# This script downloads target full text articles indexed on PubMed Central and extracts disclosure staements 



# Pre-Flight --------------------------------------------------------------

#Load libraries
library(plyr)
library(tidyverse)
library(XML)
library(xml2)
library(purrr)
library(rentrez)
library(assertthat)

# Define relevant patterns for search
relevant_patterns <- c("\'conflict\'", "\'financial-disclosure\'", "\'financial\'", "\'COI\'","\'Interest\'","\'competing\'")

# Define an xpath for looking for patterns described above. 
path <- paste(paste0("(//*[@*[contains(.,",regex(relevant_patterns, ignore_case = TRUE),")]])"),collapse = "|")


# Define get_coi function -------------------------------------------------

##################################################################################################################
## get_coi: Returns a dataframe with a pmcid, conflict of interest statement, and corresponding xml attribute value
## pmicd: The unique pmc id number of the article
## path: xpath of the element to search for in the full text xml
##################################################################################################################
get_coi <- function(pmcid, path) {
  # Error handling in case no relevant patterns were identified
  retVal<- tryCatch ({
    #construct the search query, fetch results using entrez_fetch API, and parse the retrieved xml 
    r_search <-entrez_search(db="pmc", term=paste0(pmcid,"[All Fields]"))
    r <- entrez_fetch(db="pmc", id=r_search$ids, rettype='xml', parsed=FALSE)
    r_parsed <- read_xml(r)
    
    # Create an empty dataframe with only the pmcid
    coi_df <- bind_cols("pmcid" = pmcid, "coi" = "NA", "coi_attr" = "NA", "warning"= "NA", "error" = "NA")
    
    # Read warning from the parsed xml; it is present in the xml element "//article/comment()" 
    # In case no warning is retrieved, use "NA"
    coi_df$warning <- r_parsed %>% 
      xml_find_all(., "//article/comment()" , ns = xml_ns(.)) %>% 
      xml_text(.) %>% 
      `if`(identical(., character(0)), "NA", .) 
    
    # Retrieve all nodes that contain the relevant xml pattern identified earlier
    coi_nodes <- xml_find_all(r_parsed, path , ns = xml_ns(r_parsed))
    # Remove titles/prefixes from COI statements
    xml_remove(xml_find_all(coi_nodes, ".//title" )) 
    
    #Make sure coi_nodes is not empty before operating on it
    if(not_empty(coi_nodes))
    {
      # extract COI statement
      coi <-  coi_nodes %>%
        as_list() %>% 
        as_tibble("") %>% 
        unlist() %>% 
        str_extract(.,"[^:]*$") %>% 
        str_trim()
      
      # Extract XML attributes that contained the COI statement extracted in the previous step
      coi_attr <- coi_nodes %>% 
        xml_attrs(.) %>% 
        as_tibble("") %>% 
        unlist() %>%       
        str_subset(regex("conflict|disclosure|financial|COI|Interest|competing", ignore_case = TRUE)) %>%
        first()
      
      # Combine coi and coi attr into the dataframe created earlier
      coi_df <-  tibble(coi = coi, coi_attr = as.character(coi_attr), pmcid = pmcid) %>% 
        as.data.frame() %>% 
        mutate(coi_attr = ifelse(is.null(coi_attr),NA,coi_attr)) %>%
        left_join(coi_df, by = c("pmcid", "coi", "coi_attr")) %>%
        filter(coi!="") %>% 
        unique()
    }
    coi_df<-coi_df %>% 
      mutate(coi_attr = ifelse(!is.na(coi) & is.na(coi_attr), "PMC", coi_attr)) %>% 
      filter(coi != str_match_all(coi, "(?i)Conflict of interest|Funding.|disclosure|author disclosure statement|Declaration of interest|Not applicable"))
  },
  # In case of error or warning, record the corresponding message into the dataframe and fail gracefully.
  error = function (e) {
    bind_cols("pmcid" = pmcid, "coi" = "NA", "coi_attr" = "NA", "warning"= "NA", "error" = e$message)
  },
  warning = function (w) {
    bind_cols("pmcid" = pmcid, "coi" = "NA", "coi_attr" = "NA", "warning"= w$message, "error" = "")
  }
  )
}

