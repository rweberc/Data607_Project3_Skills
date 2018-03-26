# Author: Ryan Weber
# Adapted from example highlighted in class: https://github.com/plb2018/DATA607/blob/master/Project%203/indeed_scraper.rmd

rm(list=ls())

library(rvest)
library(RCurl)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Note: just one location used currently
location <- c("united+states")

target.job <- "data+scientist"  

base.url <- "https://www.ziprecruiter.com/"

links <- NULL

numPages <- 20

# Function to clean the raw html - removing commas, tabs, line changers, etc  
clean.text <- function(text) {str_replace_all(text, regex('\r\n|\n|\t|\r|,|/|<|>|\\.|[:space:]'), ' ')} # Note: some are terms are likely redundant

# First get list of links off of pages of interest
# Only one location used currently

for (loc in location){
  
  print(paste0("Getting page links for: ", loc))
  
  for (start in 1:numPages){
    
    print(paste0("Page: ", start))
    
    # Load page
    url <- paste(base.url,"candidate/search?search=",target.job,"&location=",loc,"&page=", start ,sep="")
    page <- read_html(url)
    Sys.sleep(1)
    
    # Get the job links on this site
    links <- c(links,page %>%
                 html_nodes("div") %>%
                 html_nodes(xpath = '//*[@class="job_link t_job_link"]') %>%
                 html_attr("href"))
  }
}

# Clean the raw html - removing commas, tabs, line changers, etc  
clean.text <- function(text)
{
  str_replace_all(text, regex('\r\n|\n|\t|\r|,|/|<|>|\\.|[:space:]'), ' ')
}

#create a structure to hold our summaries
summary.full <- rep(NA, length(links))

for ( n in 1:length(links) ){
  
  print(paste0("Getting job summary for link: ", n))
  
  #build the link
  link <- links[n]
  
  
  tryCatch({

    #pull the link
    page <- read_html(link)
    
    #get the full summary
    s.full <- page %>%
      html_nodes("div")  %>%
      html_nodes(xpath = '//*[@class="jobDescriptionSection"]') %>%
      html_text() 
    
    s.full <- clean.text(s.full)
    
    #check to make sure we got some data and if so, append it.
    if (length(s.full) > 0 ){
      summary.full[n] = s.full  
    }
  }, error=function(e){print(paste0("Failure to load the following: "))})
  
}


jobs.data <- data.frame(links,summary.full)

jobs.data$summary_lower <- str_to_lower(summary.full)

# Skills
jobs.data$Hadoop <- as.numeric(str_detect(jobs.data$summary_lower, 'hadoop'))
jobs.data$Python <- as.numeric(str_detect(jobs.data$summary_lower, 'python'))
jobs.data$SQL <- as.numeric(str_detect(jobs.data$summary_lower, '\\bsql'))
jobs.data$NoSQL <- as.numeric(str_detect(jobs.data$summary_lower, 'nosql'))
jobs.data$R <- as.numeric(str_detect(jobs.data$summary_lower, '\\br\\b'))
jobs.data$Spark <- as.numeric(str_detect(jobs.data$summary_lower, 'spark'))
jobs.data$SAS <- as.numeric(str_detect(jobs.data$summary_lower, '\\bsas\\b'))
jobs.data$Excel <- as.numeric(str_detect(jobs.data$summary_lower, 'excel\\b'))
jobs.data$Hive <- as.numeric(str_detect(jobs.data$summary_lower, 'hive'))
jobs.data$C <- as.numeric(str_detect(jobs.data$summary_lower, '\\bc\\b'))
jobs.data$Java <- as.numeric(str_detect(jobs.data$summary_lower, 'java'))
jobs.data$Tableau <- as.numeric(str_detect(jobs.data$summary_lower, 'tableau'))

# Education
jobs.data$BA <- as.numeric(str_detect(jobs.data$summary_lower, '(\\bb[\\.| ]?a\\.?\\b)|(\\bb[\\.| ]?s\\.?\\b)|\1.?\2|\2.?\1|bachelor'))
jobs.data$MA <- as.numeric(str_detect(jobs.data$summary_lower, '(\\bm[\\.| ]?a\\.?\\b)|(\\bm[\\.| ]?s\\.?\\b)|\1.?\2|\2.?\1|master'))
jobs.data$PHD <- as.numeric(str_detect(jobs.data$summary_lower, 'ph[\\.| ]?d|doctorate'))

counts <- jobs.data %>%
  summarize_at(4:length(jobs.data), sum, na.rm = TRUE) %>%
  gather("Skill", "Count", 1:length(.)) %>%
  mutate(Percent = round(Count/length(jobs.data$summary_lower[!is.na(jobs.data$summary_lower)]), 2))

# Visualization
ggplot(counts, aes(reorder(Skill,-Percent), Percent)) + geom_bar(stat="identity") +
  labs(x = 'Language', y = 'Frequency (%)', title = paste0('Language (%) for Data Scientist in the United States')) %>%
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))

write.csv(jobs.data, "/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/ZipRecruiter_FullDf.csv")
write.csv(counts, "/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/ZipRecruiter_Counts.csv")
write.table(paste(jobs.data$summary_lower, collapse=" "), file = "/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/ZipRecruiter_Corpus.txt", 
            append = FALSE, quote = FALSE, sep = " ",
            eol = " ", na = " ", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")

zipFullDf <- jobs.data
zipCounts <- counts
zipCorpus <- paste(jobs.data$summary_lower, collapse=" ")

save(zipFullDf, zipCounts, zipCorpus, file="/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/Project 3/ZipData.RData")


