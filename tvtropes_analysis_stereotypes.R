library(tidyverse)
#set working directory
setwd("")
load("tvtropes_analysis_stereotypes.RData")


#function for tracking tropes over time, one value for each decade
tropesearch = function(x){
    #movies per year by decade
    mpyw = data.frame(year = rep(movies$oscarsYear,lengths(tropes)), winner = rep(movies$winner,lengths(tropes)), trope = unlist(tropes)) %>%
        mutate(era = floor(year/10)*10) %>%
        group_by(era) %>% summarise(ntropes = n())
    data.frame(year = rep(movies$oscarsYear,lengths(tropes)), trope = unlist(tropes)) %>% filter(trope %in% x) %>%
        #fill up values with n=0
        mutate(era = floor(year/10)*10) %>%
        group_by(era, trope) %>% summarise(n = n()) %>% ungroup %>% 
        complete(era, nesting(trope), fill=list(n=0)) %>% 
        full_join(mpyw, by = c("era")) %>%
        mutate(n = ifelse(is.na(n),0,n), perc = n/ntropes)
}

#plot frequency over time as line
freqplot = function(x, limits = NULL, file_end){
    tmp = tropesearch(x) %>% mutate(trope = trope %>% as.character %>% factor(., levels = x)) %>% filter(era > 1920)
    ggplot(tmp, aes(x = era, y = perc)) +
        facet_wrap(~trope, ncol=3) + geom_line(color="#00a5ff") +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent, limits = limits)
    ggsave(paste0("plots/original/movies_line_",file_end,".png"), device = "png", scale = 2, width = 80, height = 70, units="mm")
    ggsave(paste0("plots/original/movies_line_",file_end,".svg"), device = "svg", scale = 2, width = 80, height = 70, units="mm")
}

#find movies that contain a trope within a timeframe
moviesearch = function(x, startyear = 1925, endyear = 2020){
    data.frame(year = rep(movies$oscarsYear,lengths(tropes)), winner = rep(movies$winner,lengths(tropes)), 
               movie = rep(movies$eligibleTitle,lengths(tropes)), trope = unlist(tropes)) %>% 
        filter(trope %in% x, year >= startyear, year <= endyear)
}

##Look up tropes over time

## Asians/Asian-Americans
#make list
x = c("Yellow Peril", "Yellowface", "Asian Speekee Engrish", "Interchangeable Asian Cultures", "Mighty Whitey and Mellow Yellow", "Asian and Nerdy", "All Asians Know Martial Arts", "Asian Store-Owner", "Identical-Looking Asians")
#plot
freqplot(x, file_end = "asia")
#find movies
moviesearch(x) %>% View

#Black people
x = c("Scary Black Man", "Black Dude Dies First", "Black Best Friend", "Sassy Black Woman", "Blackface", "But Not Too Black")
freqplot(x, file_end = "black")
moviesearch(x) %>% View

rm(x)
save("tvtropes_analysis_stereotypes.RData")