#load in library
install.packages("tidytext")
library(tidytext)
library(tidyverse)
library(ggplot2)

#load in textfiles
hp1 = read.delim("hp1.txt", header = TRUE, sep = "\t")
hp2 = read.delim("hp2.txt", header = TRUE, sep = "\t")
hp3 = read.delim("hp3.txt", header = TRUE, sep = "\t")
hp4 = read.delim("hp4.txt", header = TRUE, sep = "\t")
hp5 = read.delim("hp5.txt", header = TRUE, sep = "\t")
hp6 = read.delim("hp6.txt", header = TRUE, sep = "\t")
hp7 = read.delim("hp7.txt", header = TRUE, sep = "\t")

#harry potter 1
hp1 = hp1 %>% mutate(line=row_number(), text=Harry.Potter.and.the.Sorcerer.s.Stone) %>% select(-1)
hp1 = hp1 %>% unnest_tokens(word, text) %>% group_by(word) %>% summarise(count = n())
hp1_tot = sum(hp1$count)
hp1$total = replicate(nrow(hp1), hp1_tot)
hp1$book = replicate(nrow(hp1), "Sorcerer's Stone")

#harry potter 2
hp2 = hp2 %>% mutate(line=row_number(), text=X1) %>% select(-1)
hp2 = hp2 %>% unnest_tokens(word, text) %>% group_by(word) %>% summarise(count = n())
hp2_tot = sum(hp2$count)
hp2$total = replicate(nrow(hp2), hp2_tot)
hp2$book = replicate(nrow(hp2), "Chamber of Secrets")

#harry potter 3
hp3 = hp3 %>% mutate(line=row_number(), text=CHAPTER.ONE.OWL.POST) %>% select(-1)
hp3 = hp3 %>% unnest_tokens(word, text) %>% group_by(word) %>% summarise(count = n())
hp3_tot = sum(hp3$count)
hp3$total = replicate(nrow(hp3), hp3_tot)
hp3$book = replicate(nrow(hp3), "Prisoner of Azkaban")

#harry potter 4
hp4 = hp4 %>% mutate(line=row_number(), text=Harry.Potter.and.the.Goblet.of.Fire.by.J.K..Rowling) %>% select(-1)
hp4 = hp4 %>% unnest_tokens(word, text) %>% group_by(word) %>% summarise(count = n())
hp4_tot = sum(hp4$count)
hp4$total = replicate(nrow(hp4), hp4_tot)
hp4$book = replicate(nrow(hp4), "Goblet of Fire")

#harry potter 5
hp5 = hp5 %>% mutate(line=row_number(), text=Harry.Potter.and.the.Order.of.the.Phoenix) %>% select(-1)
hp5 = hp5 %>% unnest_tokens(word, text) %>% group_by(word) %>% summarise(count = n())
hp5_tot = sum(hp5$count)
hp5$total = replicate(nrow(hp5), hp5_tot)
hp5$book = replicate(nrow(hp5), "Order of the Pheonix")

#harry potter 6
hp6 = hp6 %>% mutate(line=row_number(), text=Table.of.Contents.) %>% select(-1)
hp6 = hp6 %>% unnest_tokens(word, text) %>% group_by(word) %>% summarise(count = n())
hp6_tot = sum(hp6$count)
hp6$total = replicate(nrow(hp6), hp6_tot)
hp6$book = replicate(nrow(hp6), "Half-Blood Prince")

#harry potter 7
hp7 = hp7 %>% mutate(line=row_number(), text=Harry.Potter.and.the.Deathly.Hallows.By.J..K..Rowling) %>% select(-1)
hp7 = hp7 %>% unnest_tokens(word, text) %>% group_by(word) %>% summarise(count = n())
hp7_tot = sum(hp7$count)
hp7$total = replicate(nrow(hp7), hp7_tot)
hp7$book = replicate(nrow(hp7), "Deathly Hallows")

#combine into one big dataframe for tf and tf-idf analysis
all_books = rbind(hp1, hp2, hp3, hp4, hp5, hp6, hp7)

#the general frequency of words
ggplot(all_books, aes(count/total, fill = book)) +
  geom_histogram(show.legend=FALSE) + facet_wrap(~book)

#tf, idf, tf_idf
all_books = all_books %>% bind_tf_idf(word, book, count) %>% arrange(desc(tf_idf))

#plotting the words with the highest tf_idf
all_books %>% group_by(book) %>% top_n(10) %>% ungroup() %>% mutate(word = as.factor(word)) %>%
  ggplot(aes(x=word, y=tf_idf, fill=book)) + geom_col(show.legend=FALSE) + 
  facet_wrap(~book, scales="free") +
  coord_flip()
