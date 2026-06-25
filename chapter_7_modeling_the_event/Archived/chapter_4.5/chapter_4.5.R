

In the previous chapters, we saw how to think about the 1832 reform act in terms of prereform and postreform speakers and their most distinctive words.  

But there are multiple ways of getting to 

Same thing with an alternative measure -- log likelihood. 



Now, looking for distinctiveness with log likelihood. The *dhmeasures* package contains the *log_likelihood()* function, which acts much like *bind_tf_idf()*, accepting a data frame with columns *word*, *group*, and *n*, and producing a score of distinctiveness ("ll") for each word-group pair. 


```{r}
# Applying Log Likelihood: 
## Which words are most distinctive of the period before and after 1832?

# take the scores of distinctiveness of each word per period 
all_words_ll <- all_words_counted %>%
  log_likelihood(word = "word", 
                 group = "period", 
                 n = "wordsperperiod",
                 group_list = unique(all_words_counted$period),
                 word_list = unique(all_words_counted$word))  # create tf-idf scores per each word-period pair, using the counts of words per period 

# create a dataset of the most distinctive words 
top_words_ll <- all_words_ll %>%
  arrange(desc(ll)) %>% # arrange the data in each period by descending tf-idf score
  slice_head(n =10) %>% # retain only the first n lines of each grouping -- that is, the top tf-idf scores
  mutate(word = reorder_within(word, wordsperperiod, period))  # rearrange the data so that for each period we have the top words by raw count

# tell the computer to put the periods in the correct order
top_words_ll$period <- factor(top_words_ll$period, levels = c("prereform", "postreform"))

# graph the results
ggplot(top_words_ll, aes( # create a plot 
  x = word,  # word goes on the x axis 
  y= wordsperperiod,  # the count of words per period goes on the y axis 
  fill = factor(ll))) + # each bar will be colored with a shade indicating how distinctive the word is of the period
  geom_col() + # create a bar chart 
  facet_wrap(~period, # subdivide the visualization into mini-charts for each period
             scales = "free") + # let each sub-chart have its own x and y data on the axes
  coord_flip() + # a shortcut for formatting, which switches the x and y axes
  scale_fill_grey() + # uses grayscale rather than color for the result 
  scale_x_reordered() + # use the rearranged data, so that each subplot will be arranged in order of descending words per period 
  guides(fill = FALSE) + # don't show a legend for the fill colors 
  labs(title = "Which Words Are Most Distinctive",  # add a title
       subtitle = "of the period before and after the Reform Act of 1832?",
       caption = "With Log Likelihood Measure", # add a subtitle
       y = "word count") # relabel the y axis in words 
```







```{r}
# Applying Tf-idf with a Different Data Architecture Part 1a:
# Measuring the most distinctive words of prereform only vs continuous speakers for the same prereform years

# create a dataset of prereform speech 
prereform_words_cleaned <- prereform_words %>%
  filter(!str_detect(word,"^\\s*[0-9]*\\s*$")) %>% # remove all numbers 
  anti_join(stop_words, by = "word") %>% # remove stopwords 
  #  anti_join(custom_stop_words, by = "word")  %>% # remove any words in the custom_stop_words list
  mutate(word = lemmatize_words(word))  # lemmatize the word, reducing each word to its word stem for counting 

# add speaker classifications so that our dataset of words is annotated by which were spoken by prereform only speakers and which were spoken by speakers who stayed in parliament after 1832
prereform_words_w_speaker_classifications <- prereform_words_cleaned %>%
  left_join(all_classes)

# count the words for each group -- prereform only speakers and continuous speakers 
prereform_words_w_speaker_classifications_counted <- prereform_words_w_speaker_classifications %>%
  group_by(class, word) %>%
  summarize(wordsperperiod = n())

# create an index of how distinctive the words are of each category
prereform_words_w_speaker_classifications_tfidf <- prereform_words_w_speaker_classifications_counted %>%
  bind_tf_idf(word, class, wordsperperiod) %>%
  select(word, class, wordsperperiod, tf_idf)

# find only the most distinctive words for each period
prereform_top_words_w_speaker_classifications_tfidf <- prereform_words_w_speaker_classifications_tfidf %>%
  group_by(class) %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n =15) %>%
  mutate(word = reorder_within(word, wordsperperiod, class)) 

# tell the computer what order to put the periods in
prereform_top_words_w_speaker_classifications_tfidf$class <- factor(prereform_top_words_w_speaker_classifications_tfidf$class, levels = c("prereform only", "postreform only", "both periods"))

# graph the results
ggplot(prereform_top_words_w_speaker_classifications_tfidf, aes(
  x = word, 
  y= wordsperperiod, 
  fill = factor(tf_idf))) +
  geom_col() +
  facet_wrap(~class, scales = "free") +
  coord_flip() +
  scale_fill_grey() +
  scale_x_reordered() +
  guides(fill = FALSE) +
  labs(title = "Which Words Are Most Distinctive", 
       subtitle = "of the prereform only speakers vs those who also spoke after the Reform Act of 1832?",
       y = "word count")
```




```{r}
# Applying Tf-idf with a Different Data Architecture Part 1b:
# measure the most distinctive words of postreform only vs continuous speakers for the same prereform years


postreform_words_cleaned <- postreform_words %>%
  filter(!str_detect(word,"^\\s*[0-9]*\\s*$")) %>% # remove all numbers 
  anti_join(stop_words, by = "word") %>% # remove stopwords 
  #  anti_join(custom_stop_words, by = "word")  %>% # remove any words in the custom_stop_words list
  mutate(word = lemmatize_words(word))  # lemmatize the word, reducing each word to its word stem for counting 

postreform_words_w_speaker_classifications <- postreform_words_cleaned %>%
  left_join(all_classes)

postreform_words_w_speaker_classifications_counted <- postreform_words_w_speaker_classifications %>%
  group_by(class, word) %>%
  summarize(wordsperperiod = n())

postreform_words_w_speaker_classifications_tfidf <- postreform_words_w_speaker_classifications_counted %>%
  bind_tf_idf(word, class, wordsperperiod) %>%
  select(word, class, wordsperperiod, tf_idf)

postreform_top_words_w_speaker_classifications_tfidf <- postreform_words_w_speaker_classifications_tfidf %>%
  group_by(class) %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n =15) %>%
  mutate(word = reorder_within(word, wordsperperiod, class)) 

# tell the computer what order to put the periods in
postreform_top_words_w_speaker_classifications_tfidf$class <- factor(postreform_top_words_w_speaker_classifications_tfidf$class, levels = c("prereform only", "postreform only", "both periods"))

ggplot(postreform_top_words_w_speaker_classifications_tfidf, aes(x = word, y= wordsperperiod, fill = factor(tf_idf))) +
  geom_col() +
  facet_wrap(~class, scales = "free") +
  coord_flip() +
  scale_fill_grey() +
  scale_x_reordered() +
  guides(fill = FALSE) +
  labs(title = "Which Words Are Most Distinctive", 
       subtitle = "of the postreform only speakers vs those who also spoke before the Reform Act of 1832?",
       y = "word count")
```




```{r}
# Applying Tf-idf with a Different Data Architecture Part 2:
# Measure the most distinctive words of prereform only vs postreform only speakers

words_w_speaker_classifications <- all_words_cleaned %>%
  left_join(all_classes)

words_w_speaker_classifications_counted <- words_w_speaker_classifications %>%
  group_by(class, word) %>%
  summarize(wordsperperiod = n())

words_w_speaker_classifications_tfidf <- words_w_speaker_classifications_counted %>%
  bind_tf_idf(word, class, wordsperperiod) %>%
  select(word, class, wordsperperiod, tf_idf)

top_words_w_speaker_classifications_tfidf <- words_w_speaker_classifications_tfidf %>%
  filter(!class == "both periods") %>%
  group_by(class) %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n =15) %>%
  mutate(word = reorder_within(word, wordsperperiod, class)) 

# tell the computer what order to put the periods in
top_words_w_speaker_classifications_tfidf$class <- factor(top_words_w_speaker_classifications_tfidf$class, levels = c("prereform only", "postreform only", "both periods"))


ggplot(top_words_w_speaker_classifications_tfidf, aes(x = word, y= wordsperperiod, fill = factor(tf_idf))) +
  geom_col() +
  facet_wrap(~class, scales = "free") +
  coord_flip() +
  scale_fill_grey() +
  scale_x_reordered() +
  guides(fill = FALSE) +
  labs(title = "Which Words Are Most Distinctive", 
       subtitle = "of the speakers who only spoke before or after the Reform Act of 1832?",
       y = "word count")
```



There were lots of backbenchers. Does it matter if we only look at the most prominent speakers?
  
  
  ```{r}
# Use Basic Counting and a Histogram to Investigate How Many Speakers Spoke at the Maximum and Minimum Amounts
# Part 1: Prereform

prereform_only_speakers_wordcount <- prereform_speakers_wordcount %>% 
  inner_join(prereform_only_speakers)

hist(prereform_only_speakers_wordcount$wordcount)

ggplot(prereform_only_speakers_wordcount, aes(x = wordcount)) +
  #geom_dotplot(binwidth=1000)# +
  geom_histogram(binwidth = 1000) +
  labs(title = "How many prereform speakers spoke how many words?", x = "words spoken", y= "number of speakers")
```
Lots of speakers -- nearly 2000 in all -- only spoke less than 7000 words, total, in the prereform period.  Let's look at just those who spoke 7000 words or more. 

```{r }
# Use Basic Counting and a Histogram to Investigate How Many Speakers Spoke at the Maximum and Minimum Amounts
# Part 2: Postreform

postreform_only_speakers_wordcount <- postreform_speakers_wordcount %>% 
  inner_join(postreform_only_speakers)

hist(postreform_only_speakers_wordcount$wordcount)

ggplot(postreform_only_speakers_wordcount, aes(x = wordcount)) +
   #geom_dotplot(binwidth=1000)# +
   geom_histogram(binwidth = 1000) +
   labs(title = "How many postreform speakers spoke how many words?", x = "words spoken", y= "number of speakers")

```
Immediately after the reform of 1832, the shape of the histogram changes. The shape of the histogram is thicker in the middle. There are more speakers overall, of course.  

Visualizations of this kind are useful for investigating the data -- that is, getting answers to basic questions about how many people spoke and when. We can use them to inform further questions. 

For example, we might be interested in whether the "frontbenchers" -- the party leaders who spoke all the time -- were any different prereform and postreform, or whether they look just like the aggregate story of all speakers we told below.  

The histogram helps us to make informed decisions about where to put the cutoff for a "major speaker>' If the vast majority both before and after 1832 spoke less than 5000 words in the entire period (i.e., less than a ten-page paper), we might elect to look just at those few speakers who spoke over 10,000 words. 

The code will also allow us to vary the cutoff number to see if doing so changes the outcome.  

```{r}
# Applying Tf-idf with a Different Data Architecture Part 3:
# For comparing postreform and prereform speech, does it matter whether we only look at the speakers who spoke a lot? 

library(textstem)

major_prereform_speakers <- prereform_only_speakers_wordcount %>%
  filter(wordcount >= 10000) # <--- here is the cutoff. Change it and see if the results differ!

major_postreform_speakers <- postreform_only_speakers_wordcount %>%
  filter(wordcount >= 10000)

major_speakers <- rbind(major_prereform_speakers, major_postreform_speakers)

top_words_w_speaker_classifications_tfidf <- words_w_speaker_classifications %>%
  filter(suggested_speaker %in% major_speakers$suggested_speaker) %>%
  group_by(class, word) %>%
  summarize(wordsperperiod = n()) %>%
  bind_tf_idf(word, class, wordsperperiod) %>%
  select(word, class, wordsperperiod, tf_idf) %>%
  group_by(class) %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n = 15) %>%
  mutate(word = reorder_within(word, wordsperperiod, class))

# tell the computer what order to put the periods in
top_words_w_speaker_classifications_tfidf$class <- factor(top_words_w_speaker_classifications_tfidf$class, levels = c("prereform only", "postreform only", "both periods"))


ggplot(top_words_w_speaker_classifications_tfidf, aes(x = word, y= wordsperperiod, fill = factor(tf_idf))) +
  geom_col() +
  facet_wrap(~class, scales = "free") +
  coord_flip() +
  scale_fill_grey() +
  scale_x_reordered() +
  guides(fill = FALSE) +
  labs(title = "Which Words Are Most Distinctive", 
       subtitle = "of the speakers who only spoke before or after the Reform Act of 1832?",
       y = "word count")
```

COMMENT HERE


Verona, Chateaubriand  to Australia, Persia, Stockdale


```{r}
# Applying Tf-idf with a Different Data Architecture Part 4:
# Who were the most distinctive prereform/postreform speakers and their words?

words_w_speaker_classifications_tfidf <- words_w_speaker_classifications %>%
  filter(suggested_speaker %in% major_speakers$suggested_speaker) %>%
  group_by(class, suggested_speaker, word) %>%
  summarize(wordsperspeaker = n()) %>%
  bind_tf_idf(word, suggested_speaker, wordsperspeaker) %>%
  ungroup() %>%
  select(word, suggested_speaker, class, wordsperspeaker, tf_idf)

most_distinctive_speakers <- words_w_speaker_classifications_tfidf %>%
  filter(!str_detect(suggested_speaker, "said")) %>%
  filter(!str_detect(suggested_speaker, "rose")) %>%
  group_by(class, suggested_speaker) %>%
  summarize(speaker_distinctiveness = sum(tf_idf), total_words = sum(wordsperspeaker)) %>%
  ungroup() %>%
  group_by(class) %>%
  arrange(desc(speaker_distinctiveness)) %>% 
  slice_head(n = 5) 
# note that some of the total_words counted may be below 7000 as we have eliminated stopwords etc.

most_distinctive_speakers <- words_w_speaker_classifications_tfidf %>%
  filter(suggested_speaker %in% most_distinctive_speakers$suggested_speaker) %>%
  left_join(words_w_speaker_classifications_tfidf) %>% 
  left_join(speaker_key) %>%
  ungroup() %>%
  select(-suggested_speaker) 

most_distinctive_speakers_top_words <- most_distinctive_speakers %>%
  group_by(class, speaker) %>%
  arrange(desc(tf_idf)) %>%
  mutate(index_number = row_number()) %>%
  slice_head(n=5)

# tell the computer what order to put the periods in\
custom_class_order <- c("prereform only", "postreform only", "both periods")

most_distinctive_speakers_top_words <- most_distinctive_speakers_top_words %>%
  mutate(class = factor(class,levels = custom_class_order)) %>%
  arrange(class)

# 
# fig1 <- ggplot(most_distinctive_speakers_top_words, aes(x = word, y= wordsperspeaker, fill = factor(ll))) +
#   geom_col() +
#   facet_grid(rows = vars(suggested_speaker), cols = vars(class), scales = "free") +
#   coord_flip() +
#   scale_fill_grey() +
#   scale_x_reordered() +
#   guides(fill = FALSE) +
#   labs(title = "Which Words Are Most Distinctive", 
#        subtitle = "of the speakers who only spoke before or after the Reform Act of 1832?",
#        y = "word count")

#install.packages("gt")
library(tidyr)

# Pivot the data to wider format
most_distinctive_speakers_top_words_wide <- most_distinctive_speakers_top_words %>%
  select(-wordsperspeaker, -tf_idf) %>%
  pivot_wider(
    names_from = index_number,
    values_from = word
  )

# Create gt table
most_distinctive_speakers_top_words_wide %>%
  gt(rowname_col = "suggested_speaker", groupname_col = "class") %>%
  tab_header(
    title = md("Most Distinctive Words by Speaker and Period"),
    subtitle = md("These are the speakers with more than 7000 words who had the most distinctive lexicons.")
  )%>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_row_groups()
  )

```

The Marquis of Blandford  was amongst the ultra-Tories who advocated the Reform Act as a way of containing Catholic power. The fact that he appears marked out for distinctiveness in the prereform era for his specific discussions of the intricacies of the vote ("burgess," "borough") as well as his engagement with radical advocates of reform ("tooke," an allusion to John Horne Tooke, the radical clergyman tried for treason in 1794) stands out.

We also see evidence of the passing of certain styles of engagement. After the reform act, the clergy who discussed church business ("diocess," "clergy," "diocesses," "communicant," "clerical") or divorce law ("divorce," "adultery," "marriage") like the Anglican Bishop of Limerick and Dr. Phillimore were heard from no more. Likewise, speakers oriented to the diplomacy on the continent of Europe ("a'court," "chateaubriand," "spain," "verona") like Mr. Macdonald disappeared, as did the voice of slaveholders like Mr. Marryat, a sloveholder who had opposed abolitionism.

The most distinctive stewards of the reformed parliament included representatives of the new industrial towns ("cloth," "mill," "stockport," "corn," "wool," "export") like Mr. Ferrand and Mr. Cayley. Similar were Mr. Buckingham, who criticized the East India Company, advocated for the freedom of the press and the abolition of slavery, and urged the end of flogging for sailors; we learn from the measure of distinctiveness that the cause of "impressment" and issues in "india" distinguished him from his peers more than his advocacy of the press or abolition.  We also see a portrait of the new face of conservatism post-1832, embodied by Mr. James Emerson Tennent, who attempted to defend the textile industry in Ireland through the advocacy of copyright laws for designs for printed fabric, and Sir George Sinclair -- who was ahead of his time in using the language of "conservative" rather than "Tory."

The portrait of change we have here is exceptional rather than the rule. We're looking at the voices who stood out from the pack before and after reform -- at the boundaries for discourse made available by a new form of democracy -- rather than the causes endorsed by the party leaders. This is a different picture of reform than that available to us in traditional histories like that of Dame Antonia Frasier, who reads the diaries of court commentators and society hostesses for commentary on a string of prime ministers. To draw a contemporary metaphor, our method allows us to read for the exceptional -- for the Jeremy Corbyn and Nigel Farage or Ted Cruz and Alexandra Ocasio-Cortez as indexes of the political variations of the nineteenth century, rather than for history as told through the memoirs of the Boris Johnsons, Theresa Mays, Donald Trumps and Joe Bidens. It is not necessarily a portrait of power -- of what was executed. But as a portrait of belief, ideas, and enthusiasms, it is necessarily more complete.