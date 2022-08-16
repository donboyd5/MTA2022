
# This program does the following:
#   - reads the bib file that I saved from Zotero
#   - extract the citation keys
#   - prepend the @ sign
#   - sort
#   - save as plain text file that I will insert in the bibliography section of the report

# library(RefManageR)

# also see https://www.anthonyschmidt.co/post/2021-10-25-a-zotero-workflow-for-r/

library(bibtex)
library(bib2df)
library(btools)
library(tidyverse)

# bibfile <- here::here("report", "references.bib")  # make sure to save this from Zotero
bibfile <- here::here("sites", "baseline", "_baseline.bib")  

# biblist <- read.bib(file=bibfile) # look to see whether anything failed to import

df1 <- bib2df(bibfile, separate_names = TRUE)

df2 <- df1 |>
  lcnames() |> 
  select(bibtexkey, author, year) |> 
  unnest(cols = c(author)) |> 
  mutate(bibtexkey=str_trim(bibtexkey),
         bibtexkey=paste0("@", bibtexkey),
         lastchar=str_sub(bibtexkey, -1),
         sortyear=ifelse(str_detect(lastchar, "[0-9]"),
                          str_sub(bibtexkey, -4, -1),
                          str_sub(bibtexkey, -5, -1))) |> 
  select(bibtexkey, full_name, sortyear) # |> arrange(full_name, sortyear)
df2

# 
# df <- tibble(token=tokens, author=authors, year=years) %>%
#   arrange(author, year, token)
# df

df2 |> 
  pull(bibtexkey) |> 
  writeLines(con=here::here("sites", "baseline", "all_references.txt"), sep="\n")


