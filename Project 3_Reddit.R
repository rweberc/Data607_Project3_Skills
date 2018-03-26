# Author: Ryan Weber
# Adapted from example highlighted in class: https://github.com/plb2018/DATA607/blob/master/Project%203/indeed_scraper.rmd

rm(list=ls())

library(rvest)
library(RCurl)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

current.link <-"https://www.reddit.com/r/datascience/search?q=data+science+skills&restrict_sr=on&sort=relevance&t=all"

link.list <- c(current.link)

numPages <- 5

# Clean the raw html - removing commas, tabs, line changers, etc  
clean.text <- function(text) {str_replace_all(text, regex('\r\n|\n|\t|\r|,|/|<|>|\\.|[:space:]'), ' ')} # Note: some are terms are likely redundant

finalDf <- data.frame(post.title = NA, post.score = NA, post.link = NA, responses = NA, stringsAsFactors = FALSE)

for ( n in 1:numPages){
  
  print(paste0("Reading page: ", n))
  
  # Read in page
  page <- read_html(current.link)
  Sys.sleep(1)
  
  # Get all links on the page... 
  #get the job title
  post.title <- page %>% 
    html_nodes("div") %>%
    html_nodes(xpath = '//*[@class="search-result-header"]') %>%
    html_text()
  
  # Get post score
  post.score <- page %>% 
    html_nodes("div")  %>% 
    html_nodes(xpath = '//*[@class="search-score"]')  %>% 
    html_text() 
  
  post.link <- page %>%
    html_nodes("div") %>%
    html_nodes(xpath = '//*[@class="search-result-header"]') %>% # Someone here can maybe use x-path + html_attr("href") to get the link... I think there a span in the way
    html_nodes(xpath = 'a') %>%
    html_attr("href")
  
  # Bind the vectors into data frames from each page
  # Note... if note all same length, might get dirty data
  if (length(post.title) == length(post.score) & length(post.title) == length(post.link))
  {
    pageDf <- data.frame(post.title, post.score, post.link, responses = NA, stringsAsFactors = FALSE)
    
    # with the links, which could then be called in the next section
    for (i in 1:nrow(pageDf))
    {
      print(paste0("Reading post: ", i))
      
      post.page <- read_html(pageDf$post.link[i])
      
      responses <- post.page %>%
        html_nodes("div") %>%
        html_nodes(xpath = '//*[@class="usertext-body may-blank-within md-container "]') %>%
        html_nodes(xpath = '//*[@class="md"]') %>%
        html_text()
  
      responses <- clean.text(responses) 
      
      # scores <- post.page %>%
      #   html_nodes("div") %>%
      #   html_nodes(xpath = '//*[@class="dislikes]') %>%
      #   html_text()
      
      # Could get individual post scores here, but for convenience, just making these one block
      # remove first because it is the original post with other text
      pageDf$responses[i] <- paste(responses[2:length(responses)], collapse=" ")
      
    }
  }
  
  finalDf <- bind_rows(finalDf, pageDf)
  
  # Get link to next page (notice reddit can't just use page or item count due to how url is updated between pages)
  links <- page %>% 
    html_nodes("div") %>%
    html_nodes(xpath = '//a[@rel = "nofollow next"]') %>%
    #html_nodes(xpath = '//*[@class="nextprev"]') %>%
    html_attr("href")
  
  # Get link for next page
  current.link <- links[1]
  
}

# Here, just getting the corpus
finalDf$responses <- str_to_lower(finalDf$responses)

# Skills
finalDf$Hadoop <- as.numeric(str_detect(finalDf$responses, 'hadoop'))
finalDf$Python <- as.numeric(str_detect(finalDf$responses, 'python'))
finalDf$SQL <- as.numeric(str_detect(finalDf$responses, '\\bsql'))
finalDf$NoSQL <- as.numeric(str_detect(finalDf$responses, 'nosql'))
finalDf$R <- as.numeric(str_detect(finalDf$responses, '\\br\\b'))
finalDf$Spark <- as.numeric(str_detect(finalDf$responses, 'spark'))
finalDf$SAS <- as.numeric(str_detect(finalDf$responses, '\\bsas\\b'))
finalDf$Excel <- as.numeric(str_detect(finalDf$responses, 'excel\\b'))
finalDf$Hive <- as.numeric(str_detect(finalDf$responses, 'hive'))
finalDf$C <- as.numeric(str_detect(finalDf$responses, '\\bc\\b'))
finalDf$Java <- as.numeric(str_detect(finalDf$responses, 'java'))
finalDf$Tableau <- as.numeric(str_detect(finalDf$responses, 'tableau'))

# Education
finalDf$BA <- as.numeric(str_detect(finalDf$responses, '(\\bb[\\.| ]?a\\.?\\b)|(\\bb[\\.| ]?s\\.?\\b)|\1.?\2|\2.?\1|bachelor'))
finalDf$MA <- as.numeric(str_detect(finalDf$responses, '(\\bm[\\.| ]?a\\.?\\b)|(\\bm[\\.| ]?s\\.?\\b)|\1.?\2|\2.?\1|master'))
finalDf$PHD <- as.numeric(str_detect(finalDf$responses, 'ph[\\.| ]?d|doctorate'))

counts <- finalDf %>%
  summarize_at(5:length(finalDf), sum, na.rm = TRUE) %>%
  gather("Skill", "Count", 1:length(.)) %>%
  mutate(Percent = round(Count/length(finalDf$responses[!is.na(finalDf$responses)]), 2))

# Visualization
ggplot(counts, aes(reorder(Skill,-Percent), Percent)) + geom_bar(stat="identity") +
  labs(x = 'Language', y = 'Frequency (%)', title = paste0('Language (%) for Data Scientist in the United States')) %>%
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))

write.csv(finalDf, "/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/Reddit_FullDf.csv")
write.csv(counts, "/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/Reddit_Counts.csv")
write.table(paste(finalDf$responses, collapse=" "), file = "/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/Reddit_Corpus.txt", 
            append = FALSE, quote = FALSE, sep = " ",
            eol = " ", na = " ", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"),
            fileEncoding = "")

redditFullDf <- finalDf
redditCounts <- counts
redditCorpus <- paste(finalDf$responses, collapse=" ")

save(redditFullDf, redditCounts, redditCorpus, file="/Users/ryanweber/Desktop/CUNY/Data 607 Db/Projects/Project 3/RedditData.RData")
