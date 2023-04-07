#HIS393 Digital History Project
#Text Analysis of Canada's Constitutions 
setwd("/Users/bradleywood-maclean/Desktop/HIS393/HIS393_Creative_Project")

#Load Tools
install.packages('pdftools')
require(pdftools)
install.packages('tidytext')
require(tidytext)
require(dplyr)
require(ggplot2)
install.packages('wordcloud2')
require(wordcloud2)  

#British North America Act, 1867
#Create variable of a pdf file name
BNA <- list.files(pattern = "BNA1867.pdf$")

#load all pdf file
BNA <- lapply(BNA, pdf_text)
typeof(BNA)

#BNA will load as a list
#The variable needs to be converted to a vector
#To do this we unlist the variable
BNA <- unlist(BNA)
typeof(BNA)

#We need to convert the vector to a dataframe
#In this example "line" represents each page of the pdf
#There are 34 pages in BNA
text_df2 <- data_frame(line = 1:34, text = BNA)

#convert pdf text into "tokens"
#A token is a word in this example
tidy_pdf <- text_df2 %>%
  unnest_tokens(word, text)

#Remove stop words
tidy_pdf_results <- tidy_pdf %>%
  anti_join(stop_words)

#find the most common words
tidy_pdf_results %>%
  count(word, sort = TRUE)

#Create a histogram of it
tidy_pdf_results %>%
  count(word, sort = TRUE) %>%
  filter (n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue", color = "black") +
  ylab('Word Frequencies') +
  xlab('Word') +
  labs(title = "Fig. 5. British North America Act Word Frequencies") +
  coord_flip()

#Constitution Act, 1982
#Create variable of a pdf file name
CA <- list.files(pattern = "CA1982.pdf$")

#load all pdf file
CA <- lapply(CA, pdf_text)
typeof(CA)

#BNA will load as a list
#The variable needs to be converted to a vector
#To do this we unlist the variable
CA <- unlist(CA)
typeof(CA)

#We need to convert the vector to a dataframe
#In this example "line" represents each page of the pdf
#There are 16 pages in CA
text_df3 <- data_frame(line = 1:16, text = CA)

#convert pdf text into "tokens"
#A token is a word in this example
tidy_pdf2 <- text_df3 %>%
  unnest_tokens(word, text)

#Remove stop words
tidy_pdf_results2 <- tidy_pdf2 %>%
  anti_join(stop_words)

#find the most common words
tidy_pdf_results2 %>%
  count(word, sort = TRUE)

#Create a histogram of it
tidy_pdf_results2 %>%
  count(word, sort = TRUE) %>%
  filter (n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "pink", color = "black") +
  xlab("Word") +
  ylab("Word Frequencies")+
  labs(title = "Fig. 6. Constitution Act Word Frequencies") +
  coord_flip() 

#Make a WordCloud of CA
df <- tidy_pdf_results2 %>%
  count(word, sort = TRUE)
wordcloud2(data=df, size=1.6, color='random-dark')

#Make a WordCloud of BNA
df2 <- tidy_pdf_results %>%
  count(word, sort = TRUE)
wordcloud2(data=df2, size=1.6, color='random-dark')
