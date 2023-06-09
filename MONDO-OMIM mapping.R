# based on https://stackoverflow.com/questions/52254298/parsing-hpo-obo-file-to-extract-xrefs
#

library(tidyverse)
library(ontologyIndex)
library(here)

#works with HPO
hpo <- get_ontology(here("hp.obo.txt"),
                    extract_tags = "everything") #Read HPO file 
hpo_table <- simplify2array(hpo) %>% #Convert to array
  as_tibble() %>% #Convert to tibble
  select(id,xref) %>% #select HPO ID and xref
  unnest(c(id,xref)) %>% #unnest list columns
  separate(xref, into = c("Ontology","Term"), sep = ":") %>% #separate ontology from code
  pivot_wider(id_cols = id, names_from = "Ontology",
              values_from = Term,
              values_fn = \(x)paste(x,collapse = ";")) #pivot wider and combine terms with paste

#works with MONDO
#download.file("http://purl.obolibrary.org/obo/mondo.obo",here("mondo.obo"))

mondo<-get_ontology(here("mondo.obo"), extract_tags="everything") #Read MONDO file
mondo_table <- simplify2array(mondo) %>% #Convert to array
  as_tibble() %>% #Convert to tibble
  select(id, name, xref) %>% #select MONDO ID and xref
  unnest(c(id, name, xref)) %>% #unnest list columns
  separate(xref, into = c("Ontology","Term"), sep = ":") %>% #separate ontology from code
  pivot_wider(id_cols = id, names_from = "Ontology",
              values_from = c(name,Term),
              values_fn = \(x)paste(x,collapse = ";")) #pivot wider and combine terms with paste


#only keep MONDO and OMIM and remove rows without OMIM
mondo_omim_table <- simplify2array(mondo) %>% #Convert to array
  as_tibble() %>% #Convert to tibble
  select(id,name,xref) %>% #select MONDO ID, name and xref
  unnest(c(id,name,xref)) %>% #unnest list columns
  separate(xref, into = c("Ontology","Term"), sep = ":") %>% #separate ontology from code
# filter(Ontology %in% c('OMIM','OMIMPS')) #if we want to include the OMIMPS (OMIM Phenotypic Series)
  filter(Ontology == 'OMIM')

#select all disorders under 'skeletal dysplasia' in MONDO in the MONDO-OMIM table 
mondo_skeldys_IDs<-get_descendants(mondo, roots = "MONDO:0018230") #skeletal dysplasia
mondo_skeldys_table<-mondo_table %>%
  filter(id %in% mondo_skeldys_IDs)
mondo_OMIM_skeldys_table<-mondo_omim_table %>%
  filter(id %in% mondo_skeldys_IDs)

