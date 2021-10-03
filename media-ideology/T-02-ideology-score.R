########################
#### IDEOLOGY SCORE ####
########################

# Load libraries
library(tidyverse)
library(plotly)

# Load data
T1 <- readRDS("../data-twitter/timelineFT.rds")

# Set order of parties
T1$parti = factor(T1$parti, levels = c("A", "B", "C", "D", "F", "I", "O",
                                       "V", "Ø", "Andet"))

# We're going to count up all of the news media domains shared by MPs
media_domains <- c("berlingske.dk|b.dk", "jyllands-posten.dk|jp.dk", "borsen.dk",
                   "ekstrabladet.dk", "bt.dk",
                   "altinget.dk", "politiken.dk", "information.dk", 
                   "kristeligt-dagblad.dk", "arbejderen.dk",
                   "dr.dk", "tv2.dk",
                   "weekendavisen.dk", "lokalavisen.dk",
                   "finans.dk", "ing.dk",
                   "denkorteavis.dk", "zetland.dk",
                   "raeson.dk", "journalisten.dk",
                   "ritzau.dk", 
                   "piopio.dk", "avisen.dk", "jv.dk", "fyens.dk",
                   "ditoverblik.dk")

# We need to create the regular expression search queries for these news domains
# The "(\\.|//)" bit will make sure that there is either a period of a //
# prior to each news domain, and a // after it.
media_domains_search <- str_c("(\\.|//)", media_domains, "/")
media_domains_search_all <- str_c(media_domains_search, collapse = "|")

# Create a data with only observations that include a URL we care about
M <- subset(T1, str_detect(T1$urls_expanded_url, regex(media_domains_search_all, ignore_case = TRUE)))

# Let's look at the resulting data...
head(M$urls_expanded_url)


# Here, I'm going to create a matrix of the counts of URL shares from each
# Member of parliament
get_counts <- function(M) {
  
  out <- sapply(media_domains_search, function(query) sum(str_count(M$urls_expanded_url, regex(query, ignore_case = TRUE))))
  out <- data.frame(t(out))
  names(out) <- gsub("X.......|.$", "", names(out))
  return(out)
  
}

O <- M %>%
  group_by(screen_name, parti) %>%
  group_modify(~ get_counts(.x)) %>%
  arrange(desc(parti))

###############################
#### MEDIA IDEOLOGY SCORES ####

# Make affiliation 'red' and 'blue' for the two wings in danish politics
O <- O %>%
  mutate(affiliation = recode(parti,
                              "A" = "red",
                              "AA" = "red",
                              "O" = "blue",
                              "OE" = "red",
                              "C" = "blue",
                              "I" = "blue",
                              "D" = "blue",
                              "B" = "red",
                              "F" = "red",
                              "V" = "blue"))

# To get the loop below to work
media_domains_loop <- c("berlingske.dk.b.dk", "jyllands.posten.dk.jp.dk", "borsen.dk",
                        "ekstrabladet.dk", "bt.dk",
                        "altinget.dk", "politiken.dk", "information.dk", 
                        "kristeligt.dagblad.dk", "arbejderen.dk",
                        "dr.dk", "tv2.dk",
                        "weekendavisen.dk", "lokalavisen.dk",
                        "finans.dk", "ing.dk",
                        "denkorteavis.dk", "zetland.dk",
                        "raeson.dk", "journalisten.dk",
                        "ritzau.dk", 
                        "piopio.dk", "avisen.dk", "jv.dk", "fyens.dk",
                        "ditoverblik.dk")

# Calculate ideology in a loop:
ideology_loop <- NA
for(i in 1:length(media_domains)) {
  ideology_loop[i] <- sum(O[O$affiliation == "blue", media_domains_loop[i]]) / sum(O[, media_domains_loop[i]])
  
}

# Count how many times each domain has been tweeted
media_count <- colSums(O[,3:28])

# Merge this to a data.frame so we can plot it
I_DF <- data.frame(ideology_loop, media_domains_loop, media_count)

# change names of columns 
I_DF <- I_DF %>%
  rename(
    ideologi_score = ideology_loop,
    medie = media_domains_loop
  )

# round to only two decimals
I_DF$ideologi_score <- round(I_DF$ideologi_score, digits = 2)

# Make DF so we can highlight specific mediaoutlets
Media_highlight <- I_DF %>%
  filter(medie == "zetland.dk")

# Plot
Media_ideology <- I_DF %>%
  filter(!medie == "journalisten.dk") %>%
  ggplot(aes(x = ideologi_score, y = reorder(medie, ideologi_score),
             text = paste(
               "Medie: ", medie, "\n",
               "Ideologi-score: ", ideologi_score, "\n",
               "Antal delinger: ", media_count
             ))) +
  geom_point() +
  geom_point(data = Media_highlight, 
             aes(x=ideologi_score,y=medie), 
             color='red',
             size=3) +
  geom_vline(xintercept = mean(I_DF$ideologi_score), linetype = "dotted") +
  labs(title = "Danske mediers 'ideologi' estimeret ud fra \nfolketingspolitikeres delinger på Twitter", x = "", y = "", color = "") +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = str_wrap(c("0 - Kun delinger fra røde partier", "0.25","0.5", "0.75", "1 - Kun delinger fra blå partier"), width = 1)) +
  theme_minimal() +
  theme(legend.position = "top")

ggplotly(Media_ideology, tooltip = "text")

#pdf("../plots/T-02-media-ideology.pdf", height = 6, width = 9)
#Media_ideology
#dev.off()