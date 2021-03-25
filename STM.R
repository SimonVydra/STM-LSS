#MAIN SCRIPT COMBINING STM AND LSS MODELING

#IMPORTS
library(quanteda)
library(feather)
library(stm)
library(dplyr)
library(LDAvis)
library(datetime)
library(ggplot2)

#IMPORT DATA
df <- read_feather('C:/Paper4/dataframe.feather')
df[c("Timestamp")] <- lapply(df[c("Timestamp")], function(x) as.datetime(x,format='%a %b %d %X %z %Y'))
df[c("Crisis")] <- lapply(df[c("Crisis")], function(x) as.numeric(x))
names(df)[9]<-"keyword_group"

#LSS ADDITON
#requires an 'lss' object which is a trained lss model 
tok <- tokens(as.character(pull(df, var='Text')), what=c("fastestword"), remove_punct = TRUE)
test <- data.frame(predict(lss, se.fit = TRUE, rescaling = FALSE, newdata = dfm(tok, stem=FALSE))) %>%
  mutate(fit = ifelse(abs(fit)>1.96*(se.fit), fit, NA))
df$test <- test$fit

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

#Tokenize and make corpus
metadata <- df1[, c("Crisis", "Followers", "Frequency", "Identifier")]
texts <- pull(df1, var='Text')
toks <- tokens(texts, what=c("fastestword"), remove_punct = TRUE)
toks <- tokens_remove(toks, stopwords("dutch"))
dfm_corpus <- dfm(toks, stem=FALSE)
out <- convert(dfm_corpus, to=c('stm'), docvars=metadata)

#TRAIN STM MODEL
number <- 20 #insert topic number here
model <-stm(out$documents, out$vocab, data=out$meta,K=number, 
            prevalence =~ Followers+Frequency+Crisis, verbose=TRUE, init.type = "Spectral")
saveRDS(model, file = paste("ID_", number, ".rds", sep = "", collapse = NULL)) #Change name for saved models

#PLOT MODEL OUTPUT
#Generic model output, used to generate appendix summaries in the associated paper
plot(model, type = "summary", n=10, xlim = c(0, 0.3))

#Including English translation:
labels1 <- c('Topic 1: daycare, childminder, year, extracurricular, child, 4, 2, after-school, 3, day',
             'Topic 2: childcare, make, big, concern, problem, help, distant, look, town, possible',
             'Topic 3: babysitter, |, the, babysitter, i, a, ????, nanny, amp',
             'Topic 4: question, 1, allowed, write, childcare, website, answer, online, view, ????',
             'Topic 5: child benefit, child, pay, money, benefit, 2, surcharge, budget, country, social',
             'Topic 6: childcare, education, free, via, own, contribution, new, good, company, Volkskrant',
             'Topic 7: fast, childcare allowance, teacher, national, known, 2020, error, state secretary, primary education',
             'Topic 8: | childcare allowance, parent, gene, leave, file, early, destroy, letter, tax authorities',
             'Topic 9: get, year, parent, month, entitlement, when, received, let, each, child, stand',
             'Topic 10: childcare allowance, tax authorities, ????, parent, allowance, investigation, case, via, stop',
             'Topic 11: nursery, today, baby, toddler, day, beautiful, preschool, playgroup, see, book',
             'Topic 12: childcare, leader, media, joof, childminder, new, ????, thank, babysitter, childminder',
             'Topic 13: employee, pedagogical, out-of-school care, day care center, via, fun, childcare, vacancy, work, organization',
             'Topic 14: vacancy, new, you, hour, we, per, search, job childcare, lookup, you',
             'Topic 15: babysitter, bye, briefly, again, ????, playgroup, nice, old, holiday, house',
             'Topic 16: school, weather, childcare, elementary school, week, reading, open, important, learning, from',
             'Topic 17: work, man, different, say, woman, need, questions, never, often, time',
             'Topic 18: child, nursery, may, parent, childcare, young, bring, let, tested, rivm',
             'Topic 19: childcare, 1, p, kdv, the, groningen, bso, latest, fire alarm, to',
             'Topic 20: go, must, we, well, well, will, want, come, find, see')
labels2 <- c('Topic 1: vacancy, work, manager, rotterdam, search, assistant, see, amersfoort, breda, tilburg',
             'Topic 2: freelance, ict, read, assignment, the, freelance, to, log, it, latest',
             'Topic 3: |, advisor, link, via, energy, direct, legal, sustainable, municipality, below',
             'Topic 4: vacancy, region, follow, online, asksteveforjob, account, eindhoven, province, randstad, zoetermeer',
             'Topic 5: vacancy, employee, technical, commercial, m, administrative, financial, logistics, apeldoorn',
             'Topic 6: vacancy, self-employed, info, freelance, interim, assignment, new, den_haag, driver, project',
             'Topic 7: benefit, social assistance, person, get, work, must, money, pay, live, come',
             'Topic 8: vacancy, education, 1, teacher, fte, June, 2020, september, 2019, training',
             'Topic 9: vacancy, amsterdam, maastricht, 5, indeed, warehouse worker, fun, operator, detail',
             'Topic 10: freelance, planetinterim, freelance, checking, interim, ???, assignment, new, month, interim vacancy',
             'Topic 11: vacancy, view, via, job, communication, senior, marketing, hr, search, developer',
             'Topic 12: permanent, contract, temporary, you, unemployed, unemployment, pay, yobs, salary, job',
             'Topic 13: new, vacancy, job, beautiful, location, utrecht, apply, search, look up, arnhem',
             'Topic 14: will, say, just, want, own, just, you, never, unemployed',
             'Topic 15: child, pedagogical, person, employee, diverse, a.o., flexible, childcare, aow, fun, vacancy childcare',
             'Topic 16: uwv, via, get, problem, municipality, tax authorities, personnel, request, receive, go',
             'Topic 17: vacancy, environment, apply, direct, engineer, take, contact, battle, via, challenging',
             'Topic 18: unemployment, year, large, high, less, always, low, number, young, corona',
             'Topic 19: looking, standing, website, open, future, vacancy, information, applying, possibility, site',
             'Topic 20: make, other, people, where, needed, competence, questions, must, correctly, often',
             'Topic 21: amp, i, a, o, e, d, recruiter, safety, innovation, r',
             'Topic 22: search, vacancy, colleague, respond, we, gene, interest, lookup, we, complete',
             'Topic 23: good, work, well, go, again, very, really, think, know, we',
             'Topic 24: care, vacancy, welfare, nurse, counselor, care, caregiver, level, nurse, care',
             'Topic 25: you, we, you, want, look up, work, we, team, like, come',
             'Topic 26: hour, per, week, 24, 32, function, p, 40, 36, 16',
             'Topic 27: vacancy, real estate, technology, mechanic, broker, construction, carpenter, hospitality industry',
             'Topic 28: self-employed, self-employed, oblige, entrepreneur, salaried employment, self-employed, must, pension',
             'Topic 29: vacancy, zwolle, experienced, work planner, project manager, groningen, sales, junior',
             'Topic 30: see, 2, year, netherlands, 3, two, 4, three, full-time, student')
plot(model, type = "summary", topic.names="", 
     custom.labels=labels2, xlim = c(0, 0.4)) #labels1 & labels2 used in associated paper

#Generating LDAvis visualisation
visual <- toLDAvis(model, out$documents, reorder.topics=FALSE) #out.dir="name" to save the visualisation

#INSPECTING INDIVIDUAL TOPICS
top_num <- 5 #Insert topic number to inspect here
temp <- data.frame(id=pull(df1, var='Identifier'), text=pull(df1, var='Text'),
                   topic=model$theta[,top_num])
temp<- temp[order(temp$topic, decreasing=TRUE),]
print(temp$text[1:5])
temp <- filter(temp, topic<=0.85)
print(temp$text[1:5])
temp <- filter(temp, topic<=0.8)
print(temp$text[1:5])
temp <- filter(temp, topic<=0.75)
print(temp$text[1:5])
temp <- filter(temp, topic<=0.7)
print(temp$text[1:5])
temp <- filter(temp, topic<=0.65)
print(temp$text[1:5])

#INSPECTING TOPICS + LSS
#dim <- "test"
top_num <- 5 #Insert topic number to inspect here

temp <- data.frame(id=pull(df1, var='Identifier'), text=pull(df1, var='Text'),
                   topic=model$theta[,top_num], crisis=pull(df1, var='Crisis'), 
                   dim=pull(df1, var=test))
temp <- filter(temp, topic>=0.5)
tok <- tokens(as.character(pull(temp, var='text')), what=c("fastestword"), remove_punct = TRUE)
tok <- tokens_remove(tok, stopwords("dutch"))
#Most positively polarized words
temp<- temp[order(temp$dim, decreasing=TRUE),]
temp$text[1:20]
#Most negatively polarized words 
temp<- temp[order(temp$dim, decreasing=FALSE),]
temp$text[1:20]
#STATISTICAL TESTS OF DIFFERENCE
print("T-test for difference from 0")
qqnorm(temp$dim)
qqline(temp$dim)
t.test(temp$dim)
print("Period of normalcy tweets:")
length(filter(temp, crisis == 0)$dim)
length(na.omit(filter(temp, crisis == 0)$dim))
print("Period of crisis tweets:")
length(filter(temp, crisis == 1)$dim)
length(na.omit(filter(temp, crisis == 1)$dim))
print("T-test for difference between crisis and normalcy")
t.test(filter(temp, crisis == 0)$dim, filter(temp, crisis == 1)$dim)
print("Mann-Whitney U test")
wilcox.test(filter(temp, crisis == 0)$dim, filter(temp, crisis == 1)$dim)


#Showing topics correlations
topic_cors <- topicCorr(model50, method=c('simple'), verbose=TRUE, cutoff=0.05)
plot(topic_cors, vlabels=as.character(1:50))


#PLOTTING PREVALENCE DIFFERENCE BETWEEN NORMALCY AND CRISIS 
#This is used to generate figures 7 & 8 in the associated paper

#Estimate effect
prep <- estimateEffect(formula=1:20 ~ Followers+Frequency+Crisis, stmobj=model, #here take into account topic number
                       metadata=out$meta, uncertainty="Global") 

#Select relevant topics here
tops <- c(1, 2, 3) #Place selected topic numbers here
top_labels <- c('Topic 1', 'Topic 2', 'Topic 3') #Labels for those topics (ordered)
counter <- 1
rm(topic_model_time)

#Generate plot for selected topics
for (top in tops) {
  plot1 <- plot.estimateEffect(prep, "Crisis", method = "pointestimate", topics = top, printlegend = TRUE, 
                               ci.level = .95, nsims=100, labeltype = "custom", custom.labels = c("No", "Yes"))
  means1 <- data.frame(plot1$means) 
  names(means1)[1] <- "mean"
  cis1 <- data.frame(t(data.frame(plot1$cis)))
  names(cis1)[1] <- "lower"
  names(cis1)[2] <- "upper"
  labels <- data.frame(plot1$labels) %>% rename(Crisis=plot1.labels)
  topic <- data.frame(means1, cis1, labels) %>% mutate(Topic=top_labels[counter])
  counter <- counter + 1
  if (exists("topic_model_time")){
    topic_model_time <- rbind(topic_model_time, topic)
  } else {
    topic_model_time <- topic
  }
}

topics_across_crisis <- ggplot(topic_model_time, aes(Crisis, mean)) +
  geom_errorbar( aes(ymin = lower, ymax = upper), 
                 data = topic_model_time, width = 0.2, size=1) +
  facet_wrap(~Topic, ncol = 100) + #change number of columns if necessary
  ylab("Proportion") +
  xlab("Crisis") +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size=20)) +
  scale_y_continuous(limits = c(0.02, 0.1), breaks = seq(0, 0.25, by = 0.02))
topics_across_crisis


