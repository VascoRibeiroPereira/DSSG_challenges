# Libs
library(dplyr)
library(sf)
library(ggplot2)
library(rtweet)
library(readr)
library(tm)
library(RWeka)
library(wordcloud2) 
library(webshot)
library(htmlwidgets)
library(grid)

# Twitter

## Not Run
# appname <- "xxx"
# key = "xxx"
# secret = "xxx"
# access_token = "xxx-xxx"
# access_secret = "xxx"
# 
# twitter_token <- create_token(
#   app = appname,
#   consumer_key = key,
#   consumer_secret = secret,
#   access_token = access_token,
#   access_secret = access_secret)
# 
# freedom_tweets <- search_tweets(q = "#25deAbril",
#                                n = 2500)
# 
# freedom_tweets = freedom_tweets %>% filter(grepl("Portugal", location))
# 
# write_csv(freedom_tweets, "freedom/files/tweets.csv")

freedom_tweets = read_csv("freedom/files/tweets.csv")

corpus <- Corpus(VectorSource(freedom_tweets$text))

corpus <- freedom_tweets$text %>% 
  removePunctuation() %>% 
  removeWords(stopwords(kind = "pt")) %>% 
  removeWords(stopwords(kind = "es")) %>%
  removeWords(stopwords(kind = "en")) %>%
  stripWhitespace() %>% 
  VectorSource() %>% 
  Corpus()

tdm <- TermDocumentMatrix(corpus)

m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d = d[-grep("https", d$word),] ## remove links as terms
d = d %>% filter(!(word %in% c("leões", "fcporto", "dia"))) ## remove footbal terms
#head(d, 10)

# DSSGPT
# freedom_link = "https://github.com/dssgPT/Plotting-Good-DSSG/blob/main/desafios/003_Liberdade_OSM/ruas_abril.geojson?raw=true"
# download.file(freedom_link, "freedom/files/freedom_data.geojson")
# freedom_data <- geojson_read("freedom/files/freedom_data.geojson",  what = "sp") # missing some data

# All PT road names
#download.file(pt_url,"freedom/files/pt_raw") ## do not run more than once
#unzip("freedom/files/pt_raw.zip") ## do not run more than once
# pt_roads_raw <- st_read("freedom/files/pt_raw/gis_osm_roads_free_1.shp")
# pt_roads <- pt_roads_raw$name
# pt_roads <- pt_roads[!is.na(pt_roads)] %>% tolower() # remove NA and remove uppers as a variable

# write(pt_roads, "freedom/files/pt_roads.csv")

pt_roads = read_csv("freedom/files/pt_roads.csv", col_names = FALSE)

pt_roads = pt_roads$X1

sorted_roads = table(pt_roads) %>% as.data.frame() %>% arrange(desc(Freq)) %>% mutate(ppm = Freq/sum(Freq)*1000000) # sort from more frequent to least frequent with ‰

sorted_roads %>% head()

# PT expressions associated with 25 April

freedom_expr = c("25 de abril", 
                 "liberdade", 
                 "salgueiro maia", 
                 "capitães de abril", 
                 "movimento das forças armadas", 
                 "mfa", 
                 "spínola",
                 "spinola",
                 "cravos",
                 "revolução")


freedom_roads = tibble()

for (i in freedom_expr) {
  
  tmp = grep(i, sorted_roads$pt_roads)
  
  freedom_roads = bind_rows(sorted_roads[tmp,], freedom_roads)
}

## arrange data and remove duplicates
freedom_roads = freedom_roads %>% arrange(desc(Freq))
duplicate_roads = freedom_roads %>% duplicated()
freedom_roads = freedom_roads[!duplicate_roads,]

write_csv(freedom_tweets, "freedom/files/freedom_roads.csv")


# Printing Images

## Wordcloud for Tweets

set.seed(1234) # for reproducibility 
tweet_cloud = wordcloud2(data=d, size=3, color='random-light', backgroundColor="black")
saveWidget(tweet_cloud,"freedom/files/tweets.html",selfcontained = F)
webshot::webshot("freedom/files/tweets.html","freedom/files/tweets.png",vwidth = 680, vheight = 680, delay =10)


## How important is freedom in Portugal?

### Background Image

# img.file <- system.file(file.path("freedom/files/", "lib.png"),
#                         package = "ggpubr")
img <- png::readPNG("freedom/files/lib.png")

### Highlights
highlight_df = inner_join(freedom_roads,sorted_roads[1:20,])

p = sorted_roads[1:20,] %>% 
  ggplot(aes(reorder(pt_roads, ppm), ppm)) + 
  geom_col(aes(fill = ppm)) + 
  scale_fill_gradient2(low = "darkblue", 
                       high = "darkgreen", 
                       midpoint = median(sorted_roads[1:20,]$ppm)) +
  geom_col(data=highlight_df, aes(pt_roads,ppm),color="red",fill=NA) +
  theme(axis.text.x = element_text(angle = 0, size = 8, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        title = element_text(face = "bold", family = "Arial"),
        plot.subtitle = element_text(face = "italic", family = "Arial"),
        plot.caption = element_text(face = "italic", family = "Arial", colour = "grey"),
        plot.background = element_rect(fill=NA),
        legend.background = element_rect(fill=NA)) +
  labs(x = "Rua") +
  coord_flip() + 
  labs(title = "Nomes de Ruas mais comuns em Portugal",
       subtitle = "Assinalam-se os nomes de ruas referentes a liberdade e ao 25 de Abril",
       caption = "Fonte: Open Street Map (dados tratados por Vasco Ribeiro Pereira)")

grid.draw(gList(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), 
                ggplotGrob(p)))

png(filename = "freedom/files/freedom_roads.png", width = 1080, height = 680, res = 150)
grid.draw(gList(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), 
                ggplotGrob(p)))
dev.off()
