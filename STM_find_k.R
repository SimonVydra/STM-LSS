#IDENTIFYING APPROPRIATE TOPIC NUMBER FOR CORPUS SUB-SECTIONS

#IMPORTS
library(feather)
library(stm)
library(dplyr)
library(datetime)
library(tidyverse)
library(quanteda)

#Reading and manipulating the dataframe
df <- read_feather('C:/Paper4/dataframe.feather')
df[c("Timestamp")] <- lapply(df[c("Timestamp")], function(x) as.datetime(x,format='%a %b %d %X %z %Y'))
df[c("Crisis")] <- lapply(df[c("Crisis")], function(x) as.numeric(x))
names(df)[9]<-"keyword_group"

#WHICH DATA TO SELECT - SELECT ONE OPTION TO GENERATE DF1
#Filter out everything without keyword
df1 <- filter(df, Keywords != "")
#Keep only ECEC
df1 <- filter(df, grepl('ecec', keyword_group, fixed=TRUE))
#Keep only LM
df1 <- filter(df, grepl('lm_employment', keyword_group, fixed=TRUE)|
                grepl('lm_phrases', keyword_group, fixed=TRUE)|
                grepl("participatiewet|participatie wet|gesubsidieerde arbeid|Werkbedrijf|werk.nl|werkplein
                      |werkpleinen|arbeidsadviseur|uwv|arbeidsbemiddelaar|arbeidsbemiddeling|loopbaan coach|
                      werk coach|ww-uitkering|uitkering|bijstand|bijstandsuitkering"
                      , Keywords))

#CREATE CORPUS AND RUN MODELS
#Tokenize and make corpus
texts <- pull(df1, var='Text')
toks <- tokens(texts, what=c("fastestword"), remove_punct = TRUE)
toks <- tokens_remove(toks, stopwords("dutch"))
corpus <- dfm(toks, stem=FALSE)
out <- convert(corpus, to=c('stm'), docvars=df1[, c("Crisis", "Followers", "Frequency")])
#Make 10% heldout
set.seed(808) #replication seed
heldout <- make.heldout(out$documents, out$vocab, proportion = 0.4)

#Run models & metrics
#Given that this is computationally expensive the following can be run model-by-model
for (K in c(20)){ #Include topic numbers to run here
  model <-stm(heldout$documents, heldout$vocab, data=out$meta, K=K, prevalence =~ Followers+Frequency+Crisis,
                verbose=TRUE, emtol=.001, init.type = "Spectral") #emtol=.00005
  print("Model done")
  semcoh <- list(semanticCoherence(model, heldout$documents))
  print("coherence done")
  exclus <- list(exclusivity(model, M = 20, frexw = 0.7))
  print("exclusivity done")
  residual <- list(checkResiduals(model, heldout$documents, tol = 0.01))
  print("residuals done")
  held <- list(eval.heldout(model, heldout$missing))
  print("heldout likelihood done")
  addition <- tibble_row(K=K, model=model, semcoh=semcoh, exclus=exclus, held=held) #residual="NA",
  if (exists("results")){
    results <- full_join(results, addition)
  } else {
    results <- addition
  }
  print("Saved")
}

#SAVING RESULTS
saveRDS(results, file = "results2_lm.rds")
saveRDS(results, file = "results_ecec.rds")

#INDIVIDUAL LINE PLOTS OF THE FOUR METRICS
#This generates figures 2 & 3 in the associated paper
#Loading and aggregating results
results <- readRDS(file = "results_ecec.rds")
results_plot <- results %>%
  select(K, semcoh, exclus, residual, held) %>%
  transmute(
    K=K,
    coherence = unlist(map(semcoh, mean)),
    exclusivity = unlist(map(exclus, mean)),
    residuals = unlist(map(residual, "dispersion")),
    heldout = unlist(map(held, "expected.heldout")))

#Ploting semantic coherence
ggplot(results_plot, aes(x=K, y=coherence)) + 
  geom_line(color="blue") + 
  geom_point() +
  labs(y="Semantic Coherence", x="Topic Number")
#Ploting Exclusivity
ggplot(results_plot, aes(x=K, y=exclusivity)) + 
  geom_line(color="blue") + 
  geom_point() +
  labs(y="Exclusivity", x="Topic Number")
#Ploting dispersion of residuals
ggplot(results_plot[1:13,], aes(x=K, y=residuals)) + 
  geom_line(color="blue") + 
  geom_point() +
  labs(y="Dispersion of residuals", x="Topic Number")
#Ploting heldout likelihood
ggplot(results_plot, aes(x=K, y=heldout)) + 
  geom_line(color="blue") + 
  geom_point() +
  labs(y="Heldout likelihood", x="Topic Number")

#SCATTER PLOT OF SEMANTIC COHERENCE AND EXCLUSIVITY FOR INDIVIDUAL TOPICS
#This generates plot 4 in the associated paper
#Loading and manipulating results
results <- readRDS(file = "results2_lm.rds")
results_dots <- results %>%
  select(K, semcoh, exclus) %>%
  filter(K %in% c(25, 30,35,40)) %>%
  unnest() %>%
  mutate(K = as.factor(K))

#Ploting the results  
ggplot(results_dots, aes(x=semcoh, y=exclus, color = K)) +
  geom_point(size = 3, alpha=0.5) +
  labs(x = "Semantic coherence", y = "Exclusivity")