library(dplyr)
library(rvest)
#set working directory
setwd("./cleaning")

### get movie data ###
######################

#### 1. imdb titles ####

#you can download this from imdb.com/interfaces
imdb = read.delim("title.basics.tsv", stringsAsFactors = F, na.strings = c("\\N",""))
imdb$runtimeMinutes = as.numeric(imdb$runtimeMinutes)
#some TV episodes aren't structured as numbers, but we'll filter those out anyway
#filter: movies with runtime at least 35 minutes (or unknown), startYear at least 1925 -> eligible for oscars, with bit of slack
imdb = imdb %>% filter(titleType %in% c("movie", "tvMovie", "short"), runtimeMinutes >= 35 | is.na(runtimeMinutes), startYear >= 1927)
save.image("tvtropes_all.RData")
#679403 rows

#### 2. oscar winners and nominees from pre-saved awards database ####

#from http://awardsdatabase.oscars.org/
tmp = read_html("source_awardsdatabase_winners_bestpicture") %>%
  html_nodes(".awards-result-chron")

#number of films per year
len = tmp %>% html_nodes(".awards-result-subgroup-items") %>%
  sapply(., function(elem) elem %>% html_nodes(".awards-result-film-title") %>% length)
#year of the nomination/win. use the higher of the two years for the first 6 ceremonies (up to 1932/33)
year = tmp %>% html_nodes(".result-group-header") %>% html_text() %>%
  regmatches(gregexpr("\\b\\d{4}\\b", .)) %>% as.numeric %>% rep(., len) %>% 
  ifelse(. <= 1932, . + 1, .)
#movie names
name = tmp %>% html_nodes(".awards-result-film-title") %>% html_text() %>% trimws()
#did they win? 0=no, 1=yes
winner = tmp %>% html_nodes(".awards-result-actingorsimilar") %>%
  sapply(., function(elem) elem %>% html_nodes(".glyphicon-star") %>% length)
#combine into data frame
oscars = data.frame(year, name, winner, stringsAsFactors = F); rm(year, name, winner, tmp, len)

#546 nominees and winners, of which :
table(oscars$winner) #456 nominees, 90 winners (was scraped before 91st nominees announcement)

#prepare for matching: lowercase, paste year to end, remove "the" and "a" at beginning as well as punctuation and whitespace 
oscars$matchname = oscars$name %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., oscars$year)


### 3. list of eligible releases ###

# source: https://www.atogt.com/askoscar/display-reminder-list-text.php 
movies = data.frame(eligibleTitle = character(), oscarsYear = character(), stringsAsFactors = F)
#loop through each year's page
for(i in 1:91){
  cat(i,"\n")
  tmp = read_html(paste0("https://www.atogt.com/askoscar/display-reminder-list-text.php?yr=",i))
  tmp = data.frame(
    eligibleTitle = c(tmp %>% html_node("div.column1") %>% html_nodes("div") %>% html_node("span") %>% html_text(),
              tmp %>% html_node("div.column2") %>% html_nodes("div") %>% html_node("span") %>% html_text()),
    oscarsYear = i + 1927
  )
  movies = rbind(movies, tmp)
};rm(tmp, i)
movies$oscarsYear = as.numeric(movies$oscarsYear)
#some movies were nominated twice, so take duplicates out
movies = movies[!duplicated(movies),] #28219 rows

#prepare for matching: lowercase, paste year to end, remove "the" and "a" at beginning as well as punctuation and whitespace 
movies$matchname = movies$eligibleTitle %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., movies$oscarsYear)

save.image("tvtropes_all.RData")
#double-check with official lists from the past years #https://www.oscars.org/sites/oscars/files/90th_reminder_list.pdf

### match the 3 movie data sources #####
########################################
# goal: one list with winners, nominees and eligible movies with respective background info ###

#### compare oscars with movies from eligible list ####

#is everything there?
missing = oscars[!( oscars$matchname %in% movies$matchname),1:3]
write.csv(missing, "oscars_eligible_missing_names.csv", row.names = F)
#go through list. if necessary, change titles in "movies" database and check again
missing = read.csv("oscars_eligible_missing_names_corrected.csv", sep=";", stringsAsFactors = F, na.strings = "")
#correct titles
movies$eligibleTitle[match(missing$listed.name %>% na.omit, movies$eligibleTitle)] = missing$name[missing$missing == 0]

#truly missing: "wings" and "7th Heaven" from 1928. add those to "movies" list
movies = movies %>% bind_rows(data.frame(eligibleTitle = missing$name[missing$missing == 1], oscarsYear = 1928, stringsAsFactors = F))
#28221 rows
rm(missing)

#update matchname
movies$matchname = movies$eligibleTitle %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., movies$oscarsYear)
#bind oscar winner/nominee status to movies database
oscars$winner = oscars$winner + 1
#1: nominee, 2:winner
movies = left_join(movies, oscars[3:4], by = "matchname")
#0: eligible, but not nominated
movies$winner[is.na(movies$winner)] = 0


### match "movies" dataset with IMDB titles ###
###############################################

#to get background info 
#create 3 match attempts for IMDB: oscars year, year - 1, year - 2

#match 1: oscars year = start year
imdb$matchname = imdb$primaryTitle %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., imdb$startYear)
movies = left_join(movies, imdb, by="matchname") %>% ungroup #28697 rows

#match 2: for the previous 1 year
tmp = movies %>% filter(is.na(tconst)) %>% mutate(oscarsYear = oscarsYear - 1) %>% select(1:5) %>% 
  mutate(matchname = eligibleTitle %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., oscarsYear)) %>% 
  left_join(imdb, by="matchname") %>% 
  #reverse changes
  mutate(oscarsYear = oscarsYear + 1, matchname = eligibleTitle %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., oscarsYear))
movies = bind_rows(movies[!is.na(movies$tconst),], tmp) %>% arrange(oscarsYear, eligibleTitle) #28775

#match 3: for the previous 2 years
tmp = movies %>% filter(is.na(tconst)) %>% mutate(oscarsYear = oscarsYear - 2) %>% select(1:5) %>% 
  mutate(matchname = eligibleTitle %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., oscarsYear)) %>% 
  left_join(imdb, by="matchname") %>% 
  #reverse changes
  mutate(oscarsYear = oscarsYear + 2, matchname = eligibleTitle %>% tolower %>% gsub("^(the\\b|a\\b)|\\W","",.) %>% paste0(., oscarsYear))
movies = bind_rows(movies[!is.na(movies$tconst),], tmp) %>% arrange(oscarsYear, eligibleTitle) #28797
movies = arrange(movies, oscarsYear, eligibleTitle) #28797

movies$tconst %>% is.na %>% table


#### search TVtropes for movies and extract tropes ####
#######################################################

### get links for movies #####
##############################
head(q)
#search queries
q = paste0("https://www.startpage.com/do/asearch?query=",
           sapply(paste0('"',movies$eligibleTitle,'"'),URLencode,reserved=T),"+",
           ifelse(is.na(movies$startYear), paste0("(",movies$oscarsYear, "%20OR%20", gsub("[0-9]$","0", movies$oscarsYear), ")"), movies$startYear), "+film+host%3Atvtropes.org")

#for every query, get first search result that matches one of the appropriate categories
ctg = paste(paste0("tvtropes.org/pmwiki/pmwiki.php/",c("Animation","Anime","Bollywood","Disney","Film","Literature","Theatre","WesternAnimation"),"/"),collapse = "|")
#movies$link = ""
#movies$searchYear = ""

#optional after first round: test NAs again, might have been IP blocking
j = which(movies$match == 0)
j = movies$link %>% is.na %>% which
movies$tst = is.na(movies$link)

i=1
for(i in i:length(q)){
  if(!is.na(movies$link[i]) ) next
  
  repeat{
    if(i %% 10 == 0){ cat(i, "von ",length(q),"\t",round((i/length(q))*100,1),"%\n") } 
    #get search result links, make attempts until connection is established
    soup = try(read_html(q[i]))
    if(is.list(soup)){ break }
  }
  
  tmp = soup %>% html_nodes("h3.search-item__title>a") %>% html_attr("href")
  #check which links are of the appropriate categories
  ix = grep(ctg,tmp,ignore.case = T)
  #if there is one, take the first as result, if not, set to NA
  if(length(ix) > 0) {
        movies$link[i] = tmp[min(ix)]
        #look for year number in the search result text snippet
        tmp = soup %>% html_nodes("p.search-item__body") %>% `[`(min(ix)) %>%
              html_text() %>% regmatches(gregexpr("\\b\\d{4}\\b", .)) %>% "[["(1)
        movies$searchYear[i] = ifelse(length(tmp) > 0, tmp[1], NA)
  } else {movies$link[i] = NA}
};rm(i,tmp,ctg,ix, soup)
movies$searchYear = movies$searchYear %>% as.numeric()

#how many NAs are there?
(table(is.na(movies$link))/nrow(movies)*100) %>% round #21% weren't found, 22661 found links left
#check if there are streches of NA values. there might have been an IP blocking problem with those
hist(which(is.na(movies$link)), breaks = seq(1,30000,1000))
#find groups of NA rows
s <- split(which(is.na(movies$link)), cumsum(c(TRUE, diff(which(is.na(movies$link))) != 1)))
#check rows with 5 or more consecutive NAs again
j = s[lengths(s) >= 5] %>% unlist

rm(q,s,j)
save.image("tvtropes_all.RData")
load("tvtropes_all.RData")

### filter wrong links with OpenRefine ###
##########################################

#compare link with name of movie
movies$linkname = gsub(".*/(.+)$","\\1",movies$link) %>% gsub("\\d{4}$","",.) %>% tolower 
movies$compareTitle = gsub(" ","",movies$eligibleTitle) %>% tolower %>% gsub("[\\.,\\/#!$%\\^&\\*;:{}=\\—\\+_`~()'’]","",.)
#write for openrefine
write.csv(movies, file = "oscars_tropelinks_raw.csv", row.names = F, na="")

#create column "match": 1 for correct, 0 for wrong, NA for not tested
#set all without link to 0 x
# same title, searchYear same as startYear -> 1 x
# same title, searchYear 0 to 2 years around oscarsYear -> 1 x
# same title, searchYear more than 20 years apart from oscarsYear-> 0 x
# same title, searchYear more than 10 years after oscarsYear -> 0 x
# same title, searchYear more than 10 years before oscarsYear -> check for book adaptions, plays or similar x
# same title, searchYear 3 to 10 years apart from oscarsYear
      # theatre -> 1 x
      # searchYear before oscarsYear
            #searchYear 3 or 5 years before -> 1 x
            #4 to 9 before -> check
      # searchYear after oscarsYear -> 0 x
# same title, no searchYear
      # type = Disney, Theatre, Literature, Anime, Western Animation -> check x
      # type = Film
            # linkname length > 20 characters -> 1
            # year >= 1980 -> 1 (newer movies are in there more often) x
            # year < 1980 x
                  # one-word titles (no space) -> 0
                  # more than three words -> 1
                  # 2 or 3 words -> check
# different title
      # comparetitle and linktitle match without "the" at the start -> check x
      # number in compareTitle -> check x
      # linkname occurs more than 10 times -> 0 x
      # ":" in eligibleTitle x
            # searchYear 0 to 2 years around oscarsYear -> check 
            # otherwise -> 0 
      #title contains punctuation [\.,\/#!$%\^&\*;:{}=\-—\+_`~()'’] x
            # searchYear 0 to 2 years around oscarsYear -> 1 x
            # otherwise -> 0 x
      #type != "Film" -> check x
      # searchYear equals startYear, eligibleTitle has 5 words or more -> check x
      # linkname occurs more than 5 times -> 0 x
      # searchYear equals startYear -> check
      # searchYear 0 to 4 years around oscarsYear -> check
      # otherwise -> 0
tmp = movies          
movies = read.csv(file = "oscars_tropelinks_processed.csv", stringsAsFactors = F, na.strings = "")
#28784 movies
rm(tmp)
save.image("tvtropes_all.RData")

#statistics
table(movies$match)/nrow(movies) #7251 matched movies (25,2%)
table(movies$match, movies$winner)
#      0     1     2
#0 21508    25     0
#1  6716   442    93
#got all winners and all but 25 nominees
table(is.na(movies$link), movies$winner)
#          0     1     2
#FALSE 22107   461    93
#TRUE   6117     6     0
hist(movies$oscarsYear[movies$match == 1])
#more movies in recent years


#### filter movies to only keep  matches ####
#############################################
movies_all = movies
movies = movies %>% filter(match == 1) %>% select(-1) #7251

#sort out duplicate movies via open refine
movies$tconst %>% is.na %>% table #fill in imdb IDs for remaining 75 matched movies via open refine
write.csv(movies, file = "oscars_matched_raw.csv", row.names = F, na="")
tmp = movies          
tmp = read.csv(file = "oscars_matched_processed.csv", stringsAsFactors = F, na.strings = "")
#7001 movies remaining with duplicates removed
movies = tmp
rm(tmp)

#make movie IDs
movies$id = paste0("m",1:nrow(movies) %>% stringr::str_pad(4, "left","0"))


####################
### get tropes #####
####################

edits = data.frame(id = movies$id, edits = "", nrusers = "", stringsAsFactors = F)
tropes = vector("list", nrow(movies)); i=1
for(i in i:nrow(movies)){
      
      if(i %% 10 == 0){ cat(i, "von ",nrow(movies),"\t",round((i/nrow(movies))*100,1),"%\n") } 
      #cat(i, "von ",nrow(movies),"\t",round((i/nrow(movies))*100,1),"%\n")
      
      soup = read_html(movies$link[i])
      #get number of edits and user edits
      editlink = soup %>% html_node("li.link-history>a") %>% html_attr("href") %>% paste0("https://tvtropes.org/",.,"&more=t") %>% 
            read_html()
      edits$edits[i] = editlink %>% html_node("div#main-article>p.text-center") %>% html_text %>% regmatches(gregexpr("\\b\\d+\\b", .)) %>% "[["(1) %>% "["(1)
      edits$nrusers[i] = editlink %>% html_nodes("div.item-history>div.panel-heading>div.pull-left") %>% html_text() %>% unique %>% length
      
      #get every link in the main article
      tropes[[i]] = soup %>% html_nodes("div#main-article") %>% html_nodes("a.twikilink") %>% html_attr("href") %>%
            #filter for tropes (have "/Main/" in the URL) and remove duplicates
            `[`(grepl("pmwiki.php/Main/",.)) %>% unique
};rm(i,soup,editlink)
names(tropes) = movies$id


edits = edits %>% mutate(edits = as.numeric(edits), nrusers = as.numeric(nrusers))
table(edits$nrusers > 1) #364 films have been edited by less than 2 people
movies$winner[edits$nrusers <= 1] %>% table #12 nominees among those
movies[edits$nrusers <= 1,] %>% View
#remove movies with less than 2 contributing users
movies = movies[edits$nrusers > 1,]
tropes = tropes[edits$nrusers > 1]

save.image("tvtropes_all.RData")

#make list of unique encountered tropes, give them name and ID
tr = table(unlist(tropes)) %>% as.data.frame %>% rename(tropelink = Var1, tropefreq = Freq) %>%
      mutate(tropelink = gsub(".*/(.+)$","\\1", tropelink)) %>%
      group_by(tropelink) %>% summarize(tropefreq = sum(tropefreq), tropename = "")

#some tropes have different URLs that lead to the same page. to get accurate titles, scrape them
j = which(tr$tropename == "")
for(i in j[j >= i]){
      if(tr$tropename[i] != "") next
      if(which(j == i) %% 10 == 0){ cat(which(j == i), "von",length(j),"\t(",i,")\t",round((which(j == i)/length(j))*100,1),"%\n") }
      #if(i %% 10 == 0){ cat(i, "von",nrow(tr),"\t",round((i/nrow(tr))*100,1),"%\n") }
      #get title
      trname = paste0("https://tvtropes.org/pmwiki/pmwiki.php/Main/",tr$tropelink[i]) %>% read_html %>%
            html_nodes("h1.entry-title") %>% html_text
      if(length(trname) > 0){ tr$tropename[i] = trname } else { next }
};rm(i, trname)

#check for empty links
tr$tropelink[tr$tropename == ""]
#remove from tropes
tmp = tr$tropelink[tr$tropename == ""]
tropes = sapply(tropes, function(l){ l[!grepl(tmp,l)] })
rm(tmp)
#remove from tr
tr = tr[!tr$tropename == "",]

#clean trope names
tr$tropename = tr$tropename %>% gsub("( / \nFilm)?\naka: .+$","",.) %>% stringr::str_trim()
#make trope ids from trope names (not links!): convert to factor, then to numeric, then pad
tr$tropeid = tr$tropename %>% as.factor %>% as.numeric %>% stringr::str_pad(5, "left","0") %>% paste0("tr",.)

#replace URLs in trope list with trope names
tropes = sapply(tropes, function(movie){
  movie = gsub(".*/(.+)$","\\1", movie)
  tr$tropename[match(movie,tr$tropelink)] %>% sort
})

#remove duplicates from tropes
tropes = sapply(tropes, unique)

#remove duplicates from tr
#tropelink: take the one that's equal to name with space and puncuation removed. if none, take the one that occurs most often
tr = tr %>% mutate(eq = tropelink == gsub("[\\.,\\/#!$%\\^&\\*;:{}=\\—\\+_`~()'’ ]","",tropename)) %>% 
      group_by(tropeid, tropename) %>%
      summarize(tropefreq = sum(tropefreq),
                tropelink = ifelse(any(eq), tropelink[which(eq)], tropelink[which.max(tropefreq)]))

save.image("tvtropes_all.RData")
tropelist = tr;rm(tr)
save(movies, tropelist, tropes,file = "../tvtropes_analysis_oscars.RData")
save(movies, tropelist, tropes,file = "../tvtropes_analysis_stereotypes.RData")
