rm( list = ls() ) 

library(rvest)
library(xml2)
library(data.table)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(ggthemes)
library("aws.comprehend")
library("aws.translate")
library("aws.s3")


keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
} 

keyTable <- read.csv(keyfile, header = T) # *accessKeys.csv == the CSV downloaded 
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 


# Sligthly skewed

# Get article texts

politicourl <- "https://www.politico.com/news/2021/11/19/rittenhouse-not-guilty-in-kenosha-murder-case-523049"
ijrrurl <- "https://ijr.com/rittenhouse-acquitted-on-all-charges/"

politicoweb <- read_html(politicourl)
ijrweb <- read_html(ijrrurl)

politicotext <- politicoweb %>% html_nodes(".story-text__paragraph") %>% html_text()
ijrcaptions <- ijrweb %>% html_nodes(".caption")  %>% html_text()
ijrtext <- ijrweb %>% html_nodes("p")  %>% html_text()

# Only relevant parts

politicotextrelevant <- paste( unlist( politicotext ) , collapse = ' ' )
ijrtextone <- paste( unlist( ijrtext[ c(1:2 , 5:7 , 11:13) ] ) ,  collapse = ' ' ) 
ijrtextrelevant <- paste( unlist( ijrcaptions ) , ijrtextone , collapse = ' ' ) 

# Function to split text into 4000 digit chunks

text_splitter <- function( text ) {
  segments <- NULL
  if (nchar(text) > 4000) {
    for (i in 1:ceiling(nchar( text)/4000)) {
      segments[i] <- substr( text , i*4000 - 3999 , i*4000 )
    } 
    return(segments)
  } else {
    return(text)
  } 
}

# Turning ~5000 character chunk into two segments

politico_segments <- text_splitter( politicotextrelevant)

politicotextrelevantfirst <- politico_segments[1]
politicotextrelevantsecond <- politico_segments[2]


# Hyper skewed

msnbcurl <- "https://www.msnbc.com/opinion/kyle-rittenhouse-s-not-guilty-verdict-symptom-bigger-sickness-n1284130"
nreviewurl <- "https://www.nationalreview.com/2021/11/rittenhouse-jury-gets-it-right/"

msnbcweb <- read_html(msnbcurl)
nreviewweb <- read_html(nreviewurl)

msnbctext <- msnbcweb %>% html_nodes("p") %>% html_text()
nreviewtext <- nreviewweb %>% html_nodes("p")  %>% html_text()

msnbctextrelevant <- paste( unlist( msnbctext[ c(8:13 , 15:27 , 29:31 , 33:37) ] ) 
                            , collapse = ' ' )
nreviewtextrelevant <- paste( unlist( nreviewtext[ 8:15 ] ) , collapse = ' ' )

msnbc_segments <- text_splitter( msnbctextrelevant)

msnbctextrelevantfirst <- msnbc_segments[1]
msnbctextrelevantsecond <- msnbc_segments[2]
msnbctextrelevantthird <- msnbc_segments[3]

# Most Extreme

wonketteurl <- "https://www.wonkette.com/this-is-legal-now"
amgreaturl <- "https://amgreatness.com/2021/11/20/the-right-outcome-for-rittenhouse-but/"

wonketteweb <- read_html(wonketteurl)
amgreatweb <- read_html(amgreaturl)

wonkettetext <- wonketteweb %>% html_nodes("p") %>% html_text()
wonketteblock <- wonketteweb %>% html_nodes("blockquote") %>% html_text()
amgreattext <- amgreatweb %>% html_nodes("p")  %>% html_text()

wonkettetext1 <- paste( unlist( wonkettetext[ 1:9 ] ) , collapse = ' ' )
wonkettetext3 <- paste( unlist( wonkettetext[ 10:13 ] ) , collapse = ' ' )

wonkettetextrelevant <- paste( wonkettetext1 , wonketteblock , wonkettetext3
                            , collapse = ' ' )
amgreattextrelevant <- paste( unlist( amgreattext[ 1:30 ] ) , collapse = ' ' )

amgreat_segments <- text_splitter( amgreattextrelevant)

amgreattextrelevantfirst <- amgreat_segments[1]
amgreattextrelevantsecond <- amgreat_segments[2]
amgreattextrelevantthird <- amgreat_segments[3]

#Hungarian websites

pestiurl <- "https://pestisracok.hu/felmentettek-rittehouse-t-aki-lelott-ket-embert-egy-tavalyi-blm-tuntetesen-haborog-az-amerikai-baloldal-joe-bidennel-az-elen/"
negyurl <- "https://444.hu/2021/11/19/minden-vadpont-alol-felmentettek-kyle-rittenhouse-t"

pestiweb <- read_html(pestiurl)
negyweb <- read_html(negyurl)

pestitext <- pestiweb %>% html_nodes("p , strong") %>% html_text()
negytext <- negyweb %>% html_nodes("p") %>% html_text()

negytextrelevant <- paste( unlist( negytext[ 1:7 ] ) , collapse = ' ' )
pestitextrelevant <- paste( unlist( pestitext[ c( 4:5 , 7:9 , 12:14 ) ] ) , collapse = ' ' )

negytexteng <- translate(negytextrelevant , from = "hu" , to = "en" )
negytexteng <- negytexteng[[1]]

pesttexteng <- translate(pestitextrelevant , from = "hu" , to = "en" )
pesttexteng <- pesttexteng[[1]]


# Putting mp3 in bin

# bucketlist()
# bucket_name <- "ceu-class-abognar-1bg"
# put_object(file="Ben_Shapiro.mp3", bucket = bucket_name)
# put_object(file="Tyt.mp3", bucket = bucket_name)

# download JSON

shapiroJSON <- fromJSON("shapiro.json")
shapirotext <- trimws(shapiroJSON$results$transcripts)

tytJSON <- fromJSON("tyt.json")
tyttext <- trimws(tytJSON$results$transcripts)

shapiro_segments <- text_splitter( shapirotext)
tyt_segments <- text_splitter( tyttext)

shapirotextfirst <- shapiro_segments[1]
shapirotextsecond <- shapiro_segments[2]
shapirotextthird <- shapiro_segments[3]
shapirotextfourth <- shapiro_segments[4]
shapirotextfifth <- shapiro_segments[5]

tyttextfirst <- tyt_segments[1]
tyttextsecond <- tyt_segments[2]
tyttextthird <- tyt_segments[3]
tyttextfourth <- tyt_segments[4]
tyttextfifth <- tyt_segments[5]

# 1st table

# Function to sentiment analysis a segment

sentiment_creator <- function(text) {
  sentiment <- detect_sentiment(text)
  return(sentiment)
}

# Function to calculate length of a segment

nchar_creator <- function(text) {
  nchar <- nchar(text)
  return(nchar)
}

# List of text segments

list_of_ustexts <- list( ijrtextrelevant , amgreattextrelevantfirst ,
                         amgreattextrelevantsecond , amgreattextrelevantthird ,
                         nreviewtextrelevant , politicotextrelevantfirst ,
                         politicotextrelevantsecond , msnbctextrelevantfirst ,
                         msnbctextrelevantsecond , msnbctextrelevantthird ,
                         wonkettetextrelevant)

# Creating the table with the functions

us_dt_list <- lapply(list_of_ustexts, sentiment_creator)
us_dt_list <- rbindlist( us_dt_list )

us_char_vec <- sapply(list_of_ustexts, nchar_creator)
us_dt_list$nchar <- us_char_vec
us_dt_list$affiliation <- c( replicate( 5 , "right" ) , replicate( 6 , "left" ) )
us_dt_list <- us_dt_list[, lapply(mget(c("Mixed","Negative","Neutral","Positive"))
                                     , weighted.mean, w = nchar ), 
          by = list(affiliation)]

# 2nd table

top_phrases <- function(text) {
  detect_list <- detect_phrases(text)
  dt_phrases <- data.table( table(detect_list$Text) )
  setnames(dt_phrases , "N" , 'number')
  setnames(dt_phrases , "V1" , 'Phrase')
  return(dt_phrases)
}

list_of_tyt <- list( tyttextfirst , tyttextsecond , tyttextthird , tyttextfourth , 
                     tyttextfifth )
list_of_shapiro <- list( shapirotextfirst , shapirotextsecond , shapirotextthird,
                         shapirotextfourth , shapirotextfifth)

dt_of_phrases_tyt <- lapply(list_of_tyt, top_phrases)
dt_Tyt_phrase <- rbindlist(dt_of_phrases_tyt )

dt_of_phrases_shap <- lapply(list_of_shapiro, top_phrases)
dt_Shapiro_phrase <- rbindlist(dt_of_phrases_shap )

dt_Tyt_phrase <- 
  dt_Tyt_phrase[,.( 'sum_number' = sum(number) ),by = Phrase][order(-sum_number)][1:10]

dt_Shapiro_phrase <- 
  dt_Shapiro_phrase[,.( 'sum_number' = sum(number) ),by = Phrase][order(-sum_number)][1:10]


# 3 table
list_of_huntexts <- list( negytexteng , pesttexteng )

hun_dt_list <- lapply(list_of_huntexts, sentiment_creator)
hun_dt_list <- rbindlist( hun_dt_list )
hun_dt_list$affiliation <- c( "opposition" , "government" )
hun_dt_list$nchar <- c( nchar(negytexteng)  , nchar(pesttexteng) )

# 4 table

dt_of_phrases_444 <- top_phrases(negytexteng)
dt_of_phrases_PS <- top_phrases(pesttexteng)

dt_of_phrases_444 <- dt_of_phrases_444[order(-number)][1:10]
dt_of_phrases_PS <- dt_of_phrases_PS[order(-number)][1:10]

# GGPLOTS
# 1 - sentiment US

us_dt_list_long <- melt( us_dt_list , id.vars = c("affiliation") )
us_dt_list_long <- data.table(us_dt_list_long)
us_dt_list_long <- transform(us_dt_list_long, 
                             variable = reorder(variable, -value))

ggplot( us_dt_list_long, aes( affiliation , value  , fill =  variable )) +
  geom_bar(position="dodge",stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  ylab('Percentage') +
  xlab('Political Affiliation')  + 
  scale_fill_manual("Sentiment", values = c("Neutral" = "#33638DFF", 
                                         "Negative" = "#481567FF" , 
                                         "Positive" = "#20A387FF" ,
                                         "Mixed" = "#95D840FF"))

# 2 - Video key words

dt_Shapiro_phrase <- transform(dt_Shapiro_phrase, 
                             variable = reorder(Phrase, sum_number))

g1 <- ggplot( dt_Shapiro_phrase, aes( sum_number , variable)) +
        geom_bar(position="dodge",stat="identity" , fill = "brown1") +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) + 
        ylab('Key phrases') +
        xlab('Number of occurrences')  +
        theme_economist_white() + theme(axis.text=element_text(size=10),
                                        axis.title=element_text(size=12),
                                        axis.title.y = element_text(vjust=4))


dt_Tyt_phrase <- transform(dt_Tyt_phrase, 
                              variable = reorder(Phrase, sum_number))

g2 <- ggplot( dt_Tyt_phrase, aes( sum_number , variable)) +
  geom_bar(position="dodge",stat="identity" , fill = "blue2") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  ylab('Key phrases') +
  xlab('Number of occurrences')  +
  theme_economist_white() + theme(axis.text=element_text(size=10),
                                  axis.title=element_text(size=12),
                                  axis.title.y = element_text(vjust=4))

ggarrange(labels = c("Shapiro", "TYT"),
          g1, g2,
          hjust = -0.6,
          ncol = 2, nrow = 1)


# 3 - HUN sentiment

hun_dt_list[ ,`:=`(Index =  NULL, nchar = NULL, Sentiment = NULL)]
setcolorder(hun_dt_list, c("affiliation", "Mixed", "Negative" , "Neutral", "Positive"))
hun_dt_list_long <- melt( hun_dt_list , id.vars = c("affiliation") )
hun_dt_list_long <- data.table(hun_dt_list_long)
hun_dt_list_long$value <- format(hun_dt_list_long$value, scientific = FALSE) 
hun_dt_list_long$value <- round(as.numeric(hun_dt_list_long$value, scientific = FALSE ),6)
hun_dt_list_long <- transform(hun_dt_list_long, 
                             variable = reorder(variable, -value))

ggplot( hun_dt_list_long, aes( affiliation , value  , fill =  variable )) +
  geom_bar(position="dodge",stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  ylab('Percentage') +
  xlab('Political Affiliation')  + 
  scale_fill_manual("Sentiment", values = c("Neutral" = "#33638DFF", 
                                         "Negative" = "#481567FF" , 
                                         "Positive" = "#20A387FF" ,
                                         "Mixed" = "#95D840FF"))

# 4 - HUN Phrase

dt_of_phrases_PS <- transform(dt_of_phrases_PS, 
                              variable = reorder(Phrase, number))

g3 <- ggplot( dt_of_phrases_PS, aes( number , variable)) +
  geom_bar(position="dodge",stat="identity" , fill = "darkorange2") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  ylab('Key phrases') +
  xlab('Number of occurrences')  +
  theme_economist_white() + theme(axis.text=element_text(size=10),
                                  axis.title=element_text(size=12),
                                  axis.title.y = element_text(vjust=4))


dt_of_phrases_444 <- transform(dt_of_phrases_444, 
                              variable = reorder(Phrase, number))

g4 <- ggplot( dt_of_phrases_444, aes( number , variable)) +
  geom_bar(position="dodge",stat="identity" , fill = "yellow3") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  ylab('Key phrases') +
  xlab('Number of occurrences')  +
  theme_economist_white() + theme(axis.text=element_text(size=10),
                                  axis.title=element_text(size=12),
                                  axis.title.y = element_text(vjust=4))

ggarrange(labels = c("PS", "444"),
          g3, g4,
          hjust = -0.6,
          ncol = 2, nrow = 1)
