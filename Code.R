pacman::p_load(tidyverse,tm,tidytext,ggplot2,quanteda,stm)


stop <- tibble(text = stopwords("SMART"))
nrc <- sentiments %>% filter(lexicon == "nrc") %>% select(-score, -lexicon)
text_cleanish <- read_csv("GitHub/Inside-Out/text_cleanish.csv") 

count_syllable <- function(ortho) {
	
	# Can add words to these lists of 2 syllable and 3 syllable 'exceptions'
	# Note that final -e is stripped before checking these lists!
	Specials.2 <- c('every', 'different', 'family', 'girl', 'girls', 'world', 'worlds', 'bein', 'being', 'something', 'mkay', 'mayb')
	Specials.3 <- c('anyon', 'everyon') # final -e is dropped	
	
	# Regular Expression exceptions
	# SubSyl - remove a syllable from the count for each sub-string match
	SubSyl <- c('cial',
		  	 	'tia',
		       	'cius',
		 	  	'cious',
			  	'giu',              # belgium!
			  	'ion',
			  	'iou',
			  	'^every',           # every, but also everything, everybody
			  	'sia$',
			  	'.ely$',            # absolutely! (but not ely!)
			  	'[^szaeiou]es$',    # fates, but not sasses
			  	'[^tdaeiou]ed$',    # trapped, but not fated
			  	'^ninet',           # nineteen, ninety
			  	'^awe'				# awesome
		  	   )

	# AddSyl - add a syllable to the count for each sub-string match
	AddSyl <- c('ia',
		  	 	'rie[rt]',
		 	 	'dien',
			 	'ieth',
			 	'iu',
			 	'io',
			 	'ii',
			 	'ienc',	      # ambience, science, ...
			 	'les?$',
			 	'[aeiouym][bp]l$',  # -Vble, plus -mble and -Vple
			 	'[aeiou]{3}',       # agreeable
			 	'ndl(ed)?$',        # handle, handled
			 	'mpl(ed)?$',	    # trample, trampled
				'^mc',				# McEnery
			 	'ism$',             # -isms
			 	'([^aeiouy])\\1l(ed)?$',  # middle twiddle battle bottle, etc.
			 	'[^l]lien',         # alien, salient [1]
			 	'^coa[dglx].',      # [2]
			 	'[^gq]ua[^aeiou]',  # i think this fixes more than it breaks
			 	'[sd]nt$',          # couldn't, didn't, hasn't, wasn't,...
			 	'\\wshes$',          # add one back for esh (since it's -'d)
			 	'\\wches$',          #  and for affricate (witches)
			 	'\\wges$',           #  and voiced (ages)
			 	'\\wces$',	      #  and sibilant 'c's (places)
			 	'\\w[aeiouy]ing[s]?$'   # vowels before -ing = hiatus
		  	   )
		  
	tot_syls <- 0
	ortho.l <- tolower(ortho)
	stripchars <- "[:'\\[\\]]"
	ortho.cl <- gsub(stripchars, "", ortho.l, perl=T)
	spacechars <- "[\\W_]" # replace other non-word chars with space
	ortho.cl <- gsub(spacechars, " ", ortho.cl, perl=T)
	ortho.vec <- unlist(strsplit(ortho.cl, " ", perl=T))
	ortho.vec <- ortho.vec[ortho.vec!=""]
	for (w in ortho.vec) {
		w <- gsub("e$", "", w, perl=T) # strip final -e
		syl <- 0
		# is word in the 2 syllable exception list?
		if (w %in% Specials.2) {
			syl <- 2
		
		# is word in the 3 syllable exception list?
		} else if (w %in% Specials.3) {
			syl <- 3
			
		# if not, than check the different parts...
		} else {
			for (pat in SubSyl) {
				if (length(grep(pat, w, perl=T))>=1) 
					syl <- syl - 1
			}
			for (pat in AddSyl) {
				if (length(grep(pat, w, perl=T))>=1) 
					syl <- syl + 1
			}
			if (nchar(w)==1) {
				syl <- 1
			} else {
				chnk <- unlist(strsplit(w, "[^aeiouy:]+"))
				chnk <- chnk[chnk!=""]
				syl <- syl + length(chnk)
				if (syl==0) syl <- 1
			}
		}
		tot_syls <- tot_syls + syl
	}
	tot_syls
}
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

##cleaning

text.dat <- text_cleanish %>% 
  as.tibble() %>%
  janitor::clean_names() %>%
  mutate(text = removePunctuation(as.character(text))) %>% 
  unnest_tokens(text, text) %>%
  anti_join(stop)

text.dat %>%
  count(text, sort = TRUE)
speaker_count <- text.dat %>%
  count(speaker, sort = TRUE)

text.dat.1 <- text.dat %>%
  filter(speaker == list("Joy","Sadness","Bing Bong","Fear","Anger",
                         "Disgust","Mom","Riley","Dad"))

##Structural stuff
speaker_tf_idf <- text.dat.1 %>%
  count(speaker, text, sort = TRUE) %>%
  bind_tf_idf(text, speaker, n) %>%
  arrange(-tf_idf) %>%
  group_by(speaker) %>%
  top_n(10) %>%
  ungroup() 

speaker_tf_idf %>%
    mutate(word = reorder_within(text, tf_idf, speaker)) %>%
    ggplot(aes(word, tf_idf, fill = speaker)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ speaker, scales = "free", ncol = 3) +
    scale_x_reordered() +
    coord_flip() +
    theme(strip.text=element_text(size=11)) +
    labs(x = NULL, y = "tf-idf",
         title = "Highest tf-idf words",
         subtitle = "Individual stories focus on different narrative elements")

inside_sparse <- text.dat %>%
  count(speaker,text, sort = TRUE) %>%
  cast_sparse (speaker,text,n)

topic_model <- stm(inside_sparse, K = 6, verbose = FALSE, init.type = "Spectral")
summary(topic_model)
td_inside <- tidy(topic_model)

td_inside %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")

##sentiment analysis first
sent.join <- text.dat %>%
  inner_join(nrc, by = c("text" = "word")) 

sent.speaker <- text.dat %>%
  inner_join(nrc, by = c("text" = "word")) %>%
  count(speaker, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive-negative, 
         total = anger + anticipation + disgust + fear + joy + 
           sadness + surprise + trust + negative + positive,
         ratio_positive = (anticipation + joy + surprise + trust + positive)/total) %>%
  filter(ratio_positive != 1 & ratio_positive != 0) %>%
  arrange(desc(ratio_positive))

sent.join %>%
  count(text, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(text, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

##gobbledygook
text_cleanish %>%
  summary(n_text = n_distinct(speaker))

count_syllable("I want to work on my project")
nsyllable("I want to work on my project")

gobble_text <- text_cleanish %>%
  unnest_tokens(text,text, drop = FALSE) %>%
  rowwise() %>%
  mutate(n_syllables = count_syllable(text)) %>%
  ungroup()

results <- left_join(gobble_text %>%
                       group_by(speaker) %>%
                       summarise(n_text = n_distinct(text)),
                     gobble_text %>%
                       group_by(speaker) %>%
                       filter(n_syllables >= 3) %>%
                       summarise(n_polysyllables = n())) %>%
    mutate(SMOG = 1.0430 * sqrt(30 * n_polysyllables/n_text) + 3.1291) %>%
  filter(is.na(SMOG) == FALSE & n_text > 5) %>%
  arrange(desc(SMOG))

sentence_smog <- text_cleanish %>%
  rowwise() %>%
  mutate(n_syllables = count_syllable(text)) %>%
  ungroup() %>%
  arrange(desc(n_syllables))

sentence_results <- left_join(sentence_smog %>%
                                group_by(text) %>%
                                summarise(n_text = n_distinct(text)),
                              sentence_smog %>%
                                group_by(text) %>%
                                filter(n_syllables >= 3) %>% 
                                summarise(n_polysyllables = n())) %>%
  mutate(SMOG = 1.0430 * sqrt(30 * n_polysyllables/n_text) + 3.1291) %>%
  arrange(desc(SMOG))

sentence_results <- sentence_smog %>%
  mutate(complexity = textstat_readability(text,"SMOG")) %>%
  arrange(desc(complexity))

### tab stuff

tab.sent.speaker <- sent.speaker %>%
  mutate(total = joy + fear + sadness + anger + disgust) %>%
  filter(total > 50)  %>% 
  select(speaker,joy,fear,sadness,anger,disgust) %>%
  mutate(total_emotions = joy + fear + sadness + anger + disgust,
         joy_percent = joy/total_emotions,
         fear_percent = fear/total_emotions,
         sadness_percent = sadness/total_emotions,
         anger_percent = anger/total_emotions,
         disgust_percent = disgust/total_emotions) %>%
  select(speaker,joy,joy_percent,fear,fear_percent,sadness,sadness_percent,
         anger,anger_percent,disgust,disgust_percent)
  
  