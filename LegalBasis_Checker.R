#################################

library(dplyr)
library(eurlex)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)

setwd("C:\\Users\\84025jkr\\OneDrive - Erasmus University Rotterdam\\Phd\\Courses\\SICCS Summer School\\Internal_market_project")


##########################################################
#### DIRECTIVES ######


dirs <- elx_make_query(resource_type = "directive", include_date = TRUE, include_force = TRUE) %>% 
  elx_run_query() %>% 
  rename(date = `callret-3`)

df <- dirs

df <- na.omit(df)
df2 <- data.frame(CELEX=df$celex,
                  date= df$date,
                  force=df$force,
                  text=unlist(lapply(df$work, elx_fetch_data, 
                                     language_1="en",
                                     type="text")),
                  title = unlist(lapply(df$work, elx_fetch_data, 
                                        language_1="en",
                                        type="title")))


#to find cited Article of legal basis I use regular expressions
#the legal basis is included in every text, introduced by 'in particular Art. X thereof' 
df2$lb_Article <- sapply(str_match_all(df2$text,
                                       "in particular.*?thereof"), "[", 1)

#to find the law that has been cited of the Article
#the law of legal basis is included in every text, introduced by 'Having regard to LAW in particular'
df2$lb_law <- sapply(str_match_all(df2$text,
                                   "Having regard to.*?in particular"), "[", 1)



#removing unnecessary strings that the RE also detected
df2$lb_Article <- gsub('in particular', '', df2$lb_Article)
df2$lb_Article <- gsub('thereof', '', df2$lb_Article)
df2$lb_law <- gsub('Having regard to', '', df2$lb_law)
df2$lb_law <- gsub(', and in particular', '', df2$lb_law)

df2$text <- NULL #deleting the text column to make our dataframe smaller and easier to handle

#saving the df
save(df2, file= "directives.RData")


#######################
###Plotting our data###

###PLOT 1: legal basis of Art 114 over time ###

#First step: extract only Directives with Art 114 as legal basis
Art_114 <- df2[grepl("114", df2[["lb_Article"]]), ]
Art_114$date <- as.Date(Art_114$date, format= "%Y-%m-%d")


#create categorical variable if there is a joint legal basis (indicated by "and")

Art_114$type <- as.factor(ifelse(grepl("and", Art_114[["lb_Article"]]), 1, 0))

#plot our data
Art_114 %>% ggplot(aes(date,fill=type)) +
  geom_histogram() +
  ylab("n") + xlab("Entry into force") +
  ggtitle("Number of Directives with Art. 114 TFEU as a legal basis") + 
  scale_fill_discrete(name= "", 
                     labels= c("Sole basis", "Joint basis")) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(color = 'black', size = 0.2, linetype = 1),
        panel.grid.major = element_line(color = 'lightgrey'),
        panel.grid.minor = element_line(color = 'lightgrey', size = 0.1)) +
  theme(legend.position = "bottom")

###Plot 2: legal basis of Art 114 and its predecessor Art 95 EC over time

#extract both Art 114 TFEU and Art 95 EC
Art_114_95 <- df2[grepl("114", df2[["lb_Article"]]) | grepl("95", df2[["lb_Article"]]) & grepl("Treaty establishing the European Community", df2[["lb_law"]]), ]
Art_114_95$date <- as.Date(Art_114_95$date, format= "%Y-%m-%d")

#create categorical variable for either 95 or 114
Art_114_95$type <- as.factor(ifelse(grepl("95", Art_114_95[["lb_Article"]]), 1, 0))

#plot the data
Art_114_95 %>% ggplot(aes(date, fill=type)) +
  geom_histogram() +
  ylab("n") + xlab("Entry into force") +
  ggtitle("Number of Directives with Art. 114 TFEU / Art. 95 EC Treaty as a legal basis") + 
  scale_fill_discrete(name= "", breaks=c("1", "0"),
                      labels= c("Art. 95 EC Treaty", "Art. 114 TFEU") ) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(color = 'black', size = 0.2, linetype = 1),
        panel.grid.major = element_line(color = 'lightgrey'),
        panel.grid.minor = element_line(color = 'lightgrey', size = 0.1)) +
  theme(legend.position = "bottom")


########Adding 100a

Art_114_95_100a <- df2[grepl("114", df2[["lb_Article"]]) | grepl("95", df2[["lb_Article"]]) & grepl("Treaty establishing the European Community", df2[["lb_law"]]) | grepl("100a", df2[["lb_Article"]]) & grepl("Treaty establishing the European Community", df2[["lb_law"]]), ]
Art_114_95_100a$date <- as.Date(Art_114_95_100a$date, format= "%Y-%m-%d")

#create categorical variable for either 95 or 114
Art_114_95_100a$type <- as.factor(ifelse(grepl("95", Art_114_95_100a[["lb_Article"]]), 1, 
                                         ifelse(grepl("100a", Art_114_95_100a[["lb_Article"]]), 2, 0)))

#plot the data
Art_114_95_100a %>% ggplot(aes(date, fill=type)) +
  geom_histogram() +
  ylab("n") + xlab("Entry into force") +
  ggtitle("Number of Directives with Art. 114 TFEU / Art. 95 EC Treaty as a legal basis") + 
  scale_fill_discrete(name= "", breaks=c("2","1", "0"),
                      labels= c( "Art. 100a EC Treaty", "Art. 95 EC Treaty", "Art. 114 TFEU") ) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(color = 'black', size = 0.2, linetype = 1),
        panel.grid.major = element_line(color = 'lightgrey'),
        panel.grid.minor = element_line(color = 'lightgrey', size = 0.1)) +
  theme(legend.position = "bottom")


#####################################################################
####REGULATION


regs <- elx_make_query(resource_type = "regulation", include_date = TRUE, include_force = TRUE, include_lbs = TRUE) %>% 
  elx_run_query() %>% 
  rename(date = `callret-3`)


regs$date <- as.Date(regs$date, format= "%Y-%m-%d")

df <- subset(regs, regs$date >= "1992-01-01")



df2 <- data.frame(CELEX=df$celex,
                  date= df$date,
                  force=df$force,
                  text=unlist(lapply(df$work, elx_fetch_data, 
                                     language_1="en",
                                     type="text")),
                  title = unlist(lapply(df$work, elx_fetch_data, 
                                        language_1="en",
                                        type="title")))

#finds cited Article
df2$lb_Article <- sapply(str_match_all(df2$text,
                                       "in particular.*?thereof"), "[", 1)

#finds law
df2$lb_law <- sapply(str_match_all(df2$text,
                                   "Having regard to.*?in particular"), "[", 1)

#removing unnecessary strings
df2$lb_Article <- gsub('in particular', '', df2$lb_Article)
df2$lb_Article <- gsub('thereof', '', df2$lb_Article)
df2$lb_law <- gsub('Having regard to', '', df2$lb_law)
df2$lb_law <- gsub(',and in particular', '', df2$lb_law)

df2$text <- NULL



save(df3, file= "directives_text.RData")

