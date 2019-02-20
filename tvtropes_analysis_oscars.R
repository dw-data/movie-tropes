library(tidyverse)
setwd("")
load("tvtropes_analysis_oscars.RData")
#load DW colors
dw_grey = c("grey14" = "#323c45", "grey13" = "#3b444d", "grey12" = "#4b545c", "grey11" = "#5c666e", "grey10" = "#6d7780", "grey9" = "#7f8891", "grey8" = "#9099a3", "grey7" = "#a1abb4", "grey6" = "#b2bcc5", "grey5" = "#bfc7ce", "grey4" = "#cbd2d8", "grey3" = "#d8dde2", "grey2" = "#e4e8eb", "grey1" = "#f1f3f5") %>% rev()
dw_info = c("hellblau" = "#00a5ff", "dunkelblau" = "#002d5a", "orangerot" = "#d44820", "grün" = "#96be00", "rot" = "#be232d", "gelb" = "#f0c80f")
dw_gradient = c("blau6" = "#002d5a", "blau5" = "#004887", "blau4" = "#0064b4", "blau3" = "#007acd", "blau2" = "#008fe6", "blau1" = "#00a5ff", "gelb" = "#f0c80f", "orangegelb" = "#f0aa00", "Hellorange" = "#ee8c0a", "Orange" = "#eb6e14", "Orangerot" = "#d44820", "rot" = "#be232d")



#### most common tropes in 2019 nominees ####
#############################################

l = tropes[movies$id[movies$winner == 1 & movies$oscarsYear == 2018]]
library(rvest)
l[[8]] = read_html("https://tvtropes.org/pmwiki/pmwiki.php/Film/Roma") %>%
    html_nodes("div#main-article") %>% html_nodes("a.twikilink") %>% html_attr("href") %>%
    `[`(grepl("pmwiki.php/Main/",.)) %>% unique %>% gsub("/pmwiki/pmwiki.php/Main/","",.) %>% tolower %>% as.data.frame(stringsAsFactors=F) %>%
    left_join(mutate(tropelist, t = tolower(tropelink)), by = c("." = "t")) %>% `[[`(3)
names(l) = c(movies$eligibleTitle[movies$winner == 1 & movies$oscarsYear == 2018],"Roma")
x = data.frame(movie = rep(names(l), lengths(l)), trope = unlist(l))
x = x %>% group_by(trope) %>% summarise(n = n()) %>% arrange(desc(n))
head(x)
#half of all nominee movies had alcoholic characters
rm(l,x)

### tropes and movies per year #####
####################################

#movies per year by winner status: 0=eligible, 1=nominated
mpyw = data.frame(oscarsYear = rep(movies$oscarsYear,lengths(tropes)), winner = rep(movies$winner,lengths(tropes)), trope = unlist(tropes)) %>%
    mutate(winner = ifelse(winner == 0, 0, 1)) %>%
    group_by(year = oscarsYear, winner) %>% summarise(ntropes = n())
#movies and tropes per year
mpy = data.frame(oscarsYear = rep(movies$oscarsYear,lengths(tropes)), mid = rep(movies$id,lengths(tropes)), trope = unlist(tropes)) %>%
    group_by(oscarsYear) %>% summarise(ntropes = n(), nmovies = length(unique(mid)), tpm = ntropes/nmovies)
ggplot(mpy, aes(oscarsYear, tpm)) + geom_line() # more tropes per movie over time
ggplot(mpy, aes(oscarsYear, nmovies)) + geom_line() # more movies over time
# --> use nr of tropes as normalization when analyzing frequencies over time
rm(mpyw, mpy)


### which genres appear most often? ####
########################################

#among eligible, nominees and winners
movies$genres %>% is.na %>% table #only 70 movies (around 1%) don't have genres listed
genres = movies[c(4, 14:15)] %>% separate_rows(genres, sep = ",") %>% filter(!is.na(genres)) %>% 
  group_by(oscarsYear, genres, winner) %>% summarise(n = n()) %>% ungroup


### plot grenres ####

#get 5 most common genres among nominees
x = genres %>% filter(winner > 0) %>% group_by(genres) %>% summarise(n = sum(n)) %>% arrange(-n) %>% top_n(5,n)
#get genre share by winner status
tmp = genres %>% mutate(genres = ifelse(genres %in% x$genres, genres, "Others")) %>% group_by(winner, genres) %>% summarise(n = sum(n)) %>%
  mutate(share = n/sum(n), genres = factor(genres, levels = c("Others",rev(x$genres))))
tmp$winner = factor(tmp$winner) %>% plyr::revalue(c("0" = "eligible", "1" = "nominees", "2" = "winners"))
ggplot(tmp, aes(winner, share, fill = genres)) + geom_col() + theme_minimal() + theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c(dw_grey[3], dw_info[c(6, 4, 3, 1, 2)]) %>% unname) +
  ggtitle("Dramas are considerably overrepresented at the Oscars", sub = "Share of 5 most popular genres by winner status") +
  scale_y_continuous(labels = scales::percent)
ggsave("plots/original/oscars_genres.png", device = "png", scale = 2, width= 70, height= 80, units="mm")
ggsave("plots/original/oscars_genres.svg", device = "svg", scale = 2, width= 70, height= 80, units="mm")
rm(x, tmp)
#--> dramas are way overrepresented


#### Which film clichés will earn you an Oscars nomination? ####
################################################################

# cutoff: year 2000, since tastes have changed
# which tropes have a statistically significant impact (alpha = 0.05) on chances of getting a nomination, be it positive or negative?

find_impact_tropes = function(tr2000, w, m, yearstart, yearend){
    
      #group by trope
      #some movies might be excluded because their tropes are too uncommon
      trc = tr2000 %>% group_by(trope) %>%
            #calculate percentage of movies who won a nomination with and without each trope
            summarise(m.wt = n(), nr.win = sum(winner), m.wot = m - m.wt, nr.win.wot = w - nr.win,
                      p.wt = nr.win/m.wt, p.wot = nr.win.wot / m.wot) %>% 
            #filter out tropes that occur less than 50 times
            filter(m.wt >= 50)
      rm(m,w)
      #for small sample sizes, fisher's exact test is better-suited than chi-squared-test (see bortz 2010 p. 141, groß 2010 p. 168 f.)
      #h0: p.wt = p.wot, h1: p.wt != p.wot. save p-values
      #odds radio > 1 means: win is more likely with win than without
      trc = trc %>% group_by(trope) %>% 
            mutate(odds.ratio = p.wt/p.wot,
                   p.val = fisher.test(matrix(c(nr.win, nr.win.wot, m.wt-nr.win, m.wot-nr.win.wot), nrow = 2))$p.value) %>% arrange(desc(odds.ratio))
      
      #choose bonferroni-adjusted significance level of 0.05
      trc$accept = trc$p.val <= (0.05 / nrow(trc))
      trc$accept %>% table
      
      trope_impact = trc %>% filter(accept) %>% select(-accept) %>% ungroup %>% mutate(trope = as.character(trope))
      rm(trc)
      trope_impact
}

#### tropes that help you get from eligible to nominee ####

# make dataframe: trope, percentage of winner > 0 (at least nomination) for movies with trope, same for movies without trope
tr2000 = data.frame(winner = rep(movies$winner,lengths(tropes[names(tropes) %in% movies$id])),
                    year = rep(movies$oscarsYear,lengths(tropes[names(tropes) %in% movies$id])),
                    trope = unlist(tropes[names(tropes) %in% movies$id]), mid = rep(names(tropes[names(tropes) %in% movies$id]), lengths(tropes[names(tropes) %in% movies$id]))) %>%
      mutate(winner = ifelse(winner == 0, 0, 1)) %>% filter(year >= 2000 & year < 2019) %>% select(-year)
#get total number of successes and total number of movies
w = sum(movies$winner[movies$oscarsYear >= 2000 & movies$oscarsYear < 2019] > 0); m = tr2000$mid %>% unique %>% length
trope_impact = find_impact_tropes(tr2000, w, m, 2000, 2019)
#32 tropes are accepted as having an impact


#### tropes that help you get from nominee to winner ####
tr2000 = data.frame(winner = rep(movies$winner,lengths(tropes[names(tropes) %in% movies$id])),
                    year = rep(movies$oscarsYear,lengths(tropes[names(tropes) %in% movies$id])),
                    trope = unlist(tropes[names(tropes) %in% movies$id]), mid = rep(names(tropes[names(tropes) %in% movies$id]), lengths(tropes[names(tropes) %in% movies$id]))) %>% 
      filter(winner > 0) %>% mutate(winner = ifelse(winner == 1, 0, 1)) %>% filter(year >= 2000 & year < 2019) %>% select(-year)
w = sum(movies$winner[movies$oscarsYear >= 2000 & movies$oscarsYear < 2019] > 1); m = tr2000$mid %>% unique %>% length
trope_impact_12 = find_impact_tropes(tr2000, w, m, 2000, 2019)
#no tropes have an impact on getting from nominee to winner

#### tropes that help you get from eligible to winner ####
tr2000 = data.frame(winner = rep(movies$winner,lengths(tropes[names(tropes) %in% movies$id])),
                    year = rep(movies$oscarsYear,lengths(tropes[names(tropes) %in% movies$id])),
                    trope = unlist(tropes[names(tropes) %in% movies$id]), mid = rep(names(tropes[names(tropes) %in% movies$id]), lengths(tropes[names(tropes) %in% movies$id]))) %>% 
      mutate(winner = ifelse(winner == 1, 0, ifelse(winner == 2,1,0))) %>% filter(year >= 2000 & year < 2019) %>% select(-year)
w = sum(movies$winner[movies$oscarsYear >= 2000 & movies$oscarsYear < 2019] > 1); m = tr2000$mid %>% unique %>% length
trope_impact_02 = find_impact_tropes(tr2000, w, m, 2000, 2019)
#only 2 tropes have a significant impact on getting from eligible to winner
rm(m,w,tr2000,trope_impact_02, trope_impact_12)

#write to csv file
trope_impact %>% select(-p.val) %>% left_join(tropelist[-3], by = c("trope" = "tropename")) %>% 
      mutate(tropelink = paste0("https://tvtropes.org/pmwiki/pmwiki.php/Main/", tropelink)) %>% 
      write.csv(., file = "oscars_tropes_impact.csv", row.names = F, na = "")

save.image("tvtropes_analysis_oscars.RData")


### plot the impact of 5 selected tropes ####
tmp = trope_impact %>% filter(trope %in% c("Rule of Drama", "Based on a True Story", "Honor Before Reason", "The Alcoholic", "Bittersweet Ending", "Scenery Porn")) %>% 
    arrange(desc(p.wt)) %>% 
    mutate(trope = factor(trope, levels = rev(trope)))

ggplot(tmp, aes(y = trope)) +
    geom_tile(aes(x=1, fill = p.wot)) + geom_tile(aes(x=2, fill = p.wt)) +
    scale_fill_gradientn(breaks = seq(0,0.25, length.out = 6), colors = dw_gradient[6:1]) +
    geom_text(aes(x=1, label = paste0(round(p.wot*1000)/10,"%"))) +
    geom_text(aes(x=2, label = paste0(round(p.wt*1000)/10,"%"))) + 
    geom_text(aes(x=2.6, label = round(odds.ratio*10)/10), hjust = 0) + guides(fill=F) +
    theme_minimal()
ggsave("plots/original/oscars_tropes_impact.png", device = "png", scale = 2, width= 70, height= 70, units="mm")
ggsave("plots/original/oscars_tropes_impact.svg", device = "svg", scale = 2, width= 70, height= 70, units="mm")  
rm(tmp)


#### Frequency in all vs. frequency in nominees ####
####################################################
#for each trope, see how often it appears in nominees vs. all movies, compare in scatterplot, highlight impactful tropes

#make data frame with winner status, trope
tmp = data.frame(winner = rep(movies$winner,lengths(tropes[names(tropes) %in% movies$id])),
                    trope = unlist(tropes)) %>%
    mutate(winner = ifelse(winner == 0, 0, 1)) %>% 
    #count how often each trope occurs by winner status
    group_by(trope, winner) %>% summarise(n = n()) %>% mutate(sum = sum(n)) %>% 
    #replace count of winner=0 with grand total of trope so we count all movies, not just non-nominees
    mutate(n = ifelse(winner == 0, sum, n)) %>% group_by(winner) %>% mutate(freq = n/sum(n)) %>%
    #filter tropes that occur less than 50 times total, make one column for each winner status
    filter(sum > 1) %>% select(-n, -sum) %>% spread(winner, freq, fill=0) %>% 
    #mark for highlighting: impactful tropes, more popular with academy, more popular in all movies
    mutate(highlight = ifelse(`1` > `0`, "academy", "all") %>% ifelse(trope %in% trope_impact$trope, "impact", .))
#for freq < 0.05%: choose 1000 random tropes instead of displaying all, make sure impact tropes are in there
set.seed(42)
tmp = tmp[c(which(!(tmp$`0`< 0.00008 & tmp$`1`< 0.00008) | tmp$highlight == "impact"),
            which(  tmp$`0`< 0.00008 & tmp$`1`< 0.00008) %>% sample(., 500)),]

ggplot(tmp, aes(x = `0`, y = `1`, color=highlight)) +
    geom_point(size = 3) + geom_abline(intercept = 0, slope = 1, color = dw_grey[12]) +
    theme_minimal() + scale_color_manual(values = unname(c(dw_info[1],dw_grey[5],dw_info[2]))) +
    xlab("Frequency among all movies") + ylab("Frequency among nominees") +
    scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent)
ggsave("plots/original/oscars_tropes_frequency.png", device = "png", scale = 2, width = 80, height = 70, units="mm")
ggsave("plots/original/oscars_tropes_frequency.svg", device = "svg", scale = 2, width = 80, height = 70, units="mm")
rm(tmp)

save.image("tvtropes_analysis_oscars.RData")
