library(eurlex)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidytext)
library(stringr)
library(textstem)
library(wordcloud2)
library(lubridate)

#Step 2: get the data - directives - 374 internal market directives since 1992

raw.dataDIR <- eurlex::elx_make_query(
  resource_type = "directive", sector = "3", #3-legal acts
  include_lbs = TRUE, 
  include_force = TRUE, 
  include_date = TRUE,
  include_corrigenda = FALSE,
)  |> eurlex::elx_run_query()

library(stringr)

internalmarketDIR1 <- filter(raw.dataDIR, lbcelex == "11992E100A") #Maastricht
internalmarketDIR2 <- filter(raw.dataDIR, lbcelex == "11997E095") #Amsterdam
internalmarketDIR3 <- filter(raw.dataDIR, lbcelex == "12002E095") #Nice
internalmarketDIR4 <- filter(raw.dataDIR, lbcelex == "12006E095") #consolidated
internalmarketDIR5 <- filter(raw.dataDIR, lbcelex == "12010E114") #Lisbon
internalmarketDIR6 <- filter(raw.dataDIR, lbcelex == "12012E114")
internalmarketDIR7 <- filter(raw.dataDIR, lbcelex == "12016E114")

#Step 2: get the data - 251 regulations - internal market regulations since 1992

raw.dataREG <- eurlex::elx_make_query(
  resource_type = "manual", manual_type = "REG", sector = "3", #3-legal acts
  include_lbs = TRUE, 
  include_force = TRUE, 
  include_date = TRUE,
  include_corrigenda = FALSE,
)  |> eurlex::elx_run_query()

library(stringr)

internalmarketREG1 <- filter(raw.dataREG, lbcelex == "11992E100A") #Maastricht
internalmarketREG2 <- filter(raw.dataREG, lbcelex == "11997E095") #Amsterdam
internalmarketREG3 <- filter(raw.dataREG, lbcelex == "12002E095") #Nice
internalmarketREG4 <- filter(raw.dataREG, lbcelex == "12006E095") #consolidated
internalmarketREG5 <- filter(raw.dataREG, lbcelex == "12010E114") #Lisbon
internalmarketREG6 <- filter(raw.dataREG, lbcelex == "12012E114")
internalmarketREG7 <- filter(raw.dataREG, lbcelex == "12016E114")

total_dirs <- rbind(internalmarketDIR1, internalmarketDIR2, internalmarketDIR3,
                    internalmarketDIR4, internalmarketDIR5, internalmarketDIR6,
                    internalmarketDIR7)
total_regs <- rbind(internalmarketREG1, internalmarketREG2, internalmarketREG3,
                    internalmarketREG4, internalmarketREG5, internalmarketREG6,
                    internalmarketREG7)
total_data <- rbind(total_dirs, total_regs)
