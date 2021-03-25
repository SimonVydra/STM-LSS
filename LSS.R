#THIS SCRIPT GENERATES LSS MODELS TO BE USED IN THE MAIN 'STM' SCRIPT

#IMPORTS
library(quanteda)
library(LSX)
library(dplyr)
library(datetime)
library(feather)
library(stringr)

#IMPORTING AND MANIPULATING DATA
#Read the dataframe
df <- read_feather('C:/Paper4/dataframe.feather')
df[c("Timestamp")] <- lapply(df[c("Timestamp")], function(x) as.datetime(x,format='%a %b %d %X %z %Y'))
df[c("Crisis")] <- lapply(df[c("Crisis")], function(x) as.numeric(x))
#Filter out everything without keyword
df <- filter(df, Keywords != "")
#Pull texts and metadata
metadata <- df[, c("Crisis", "Followers", "Frequency")]
texts <- pull(df, var='Text')
#Remove @ tags
texts <- str_remove_all(texts, "@\\s\\S+\\s")
#Reshape to sentence level
sentences <- corpus(texts)%>%
  corpus_reshape("sentences")
#Tokenize  
toks <- tokens(sentences, remove_punct = TRUE, what=c("fastestword"))%>%
  tokens_remove(stopwords("dutch"))
#Make corpus removing   
dfm_sentences <- dfm_trim(dfm(toks), min_termfreq = 200)


#POSSIBLE DIMENSIONS - SELECT ONE TO GENERATE 'DIMENSION' OBJECT
#A sentiment dimension
dimension <- vector(mode="list", length=2)
names(dimension) <- c("positive", "negative")
dimension[[1]] <- c("positief","goed",  "blij", "gelukkig")
dimension[[2]] <- c("negatief","slecht", "zielig", "boos")
dimension <- dictionary(dimension)
check <- c("tevreden", "lief","prachtig","mooi","kwaad","idioot","belachelijk","dom", "uitkering")
#A success vs. failure dimension
dimension <- vector(mode="list", length=2)
names(dimension) <- c("success", "fail")
dimension[[1]] <- c("succesvol","handig","lukken","behalen")
dimension[[2]] <- c("onnodig","mislukken","falen","verliezen")
dimension <- dictionary(dimension)
check <- c("bereiken","afmaken","realiseren","nutteloos","tekort", "kinderbijslag", "kinderopvang")

#OPTIONAL - including the keyness-based restriction
#specific <- char_keyness(toks, c("bijstand"), p = 0.05)
  
#TRAINING LSS
lss <- textmodel_lss(dfm_sentences, as.seedwords(dimension), k = 250) #optionally include 'terms=specific'

#PLOT RESULTING MODEL
#This generates figure 5 & 6 in the associated paper
textplot_terms(lss, highlighted=c(dimension[[1]],dimension[[2]],check))

#MANUAL INSPECTION OF MOST POLARIZED WORDS
print("Associated with +1 polarity")
head(coef(lss), 20)
print("Associated with -1 polarity")
tail(coef(lss), 20)
