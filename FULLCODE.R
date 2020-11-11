allstars = read.csv("allstarlist.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors = T, encoding = "UTF-8")
allstars$Player = as.character(allstars$Player)
library(nbastatR)
library(dplyr)
draftlist = drafts(draft_years = 1989:2019)
draftlist$allstarflag <- ifelse(draftlist$namePlayer %in% allstars$Player, 1, 0)
players = nbastatR::bref_players_stats(seasons = 1989:2020, tables = "Totals", include_all_nba = T)
allnba = subset(players, isAllNBA == 1)
allnba = distinct(allnba, namePlayer, .keep_all = T)
draftlist$allnbaflag <- ifelse(draftlist$namePlayer %in% allnba$namePlayer, 1, 0)
draftlist = subset(draftlist, yearDraft != 2019)

a = c()
b = c()

draftlist %>%
  filter(numberPickOverall == 12 & allstarflag == 1) %>%
  count(namePlayer)

draftlist %>%
  filter(numberRound == 1 & numberPickOverall > 14 & allstarflag == 1) %>%
  count()


for (i in 1:60) {
  a[i] = as.numeric(draftlist %>% 
                      filter(numberPickOverall == i) %>%
                      count())
  b[i] = as.numeric(draftlist %>%
                 filter(numberPickOverall == i & allstarflag == 1) %>%
                 count())
}

c = as.data.frame(cbind(b,a))
DraftPickNumber = c(1:60)
allstarprob = as.data.frame(cbind(DraftPickNumber,c))
allstarprob$ProbabilityofBeingAllstar = allstarprob$b / allstarprob$a
colnames(allstarprob) = c("DraftPickNumber", "TotalAllStarCount", 
                         "TotalNumberofPicksfromthisPick", "ProbabilityofBeingAllstar")

theme_set = theme_bw()

library(ggplot2)

ggplot(subset(allstarprob), aes(x=DraftPickNumber, y=ProbabilityofBeingAllstar)) + 
  geom_point() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se = F) +
  labs(title = "How Likely For Each Draft Pick to Become an All-Star?", 
       subtitle = "Years 1989-2019",
       caption = 'graph: @burakcankoc \nSource: basketball-reference.com') +
  scale_x_continuous(name = 'Draft Pick Number',
                     breaks = seq(0, 60, 5),
                     limits=c(0, 60)) +
  scale_y_continuous(name = 'Probability of Being an All-Star',
                     breaks = seq(0, 0.75, 0.25),
                     limits=c(0, 0.75))
  ##annotate("label", x=20, y=0.6, label = "Years 1989-2019")

library(xlsx)
write.xlsx(allstarprob,
           "allstarsprobs.xlsx",
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE,
           showNA = TRUE,
           password = NULL
)


###############
draftlist %>% 
  filter( numberPickOverall > 14) %>%
  count(numberPickOverall, namePlayer, sort = T) %>% 
  print(n = 30)

res <- numeric(length = length(draftlist))
str(draftlist)


as.numeric(draftlist %>% 
  filter(numberRound == 1) %>% 
  count())



c = as.numeric(draftlist %>% 
                     filter(numberPickOverall == 1) %>%
                     count())
  
draftlist %>%
    a = count(filter(numberPickOverall == j & allstarflag == 1, sort = T))
}

a

draftlist %>% 
  filter(numberPickOverall == 2) %>% 
  count(slugTeam, sort = T)

aa = nbastatR::player_profiles(player_ids = players$idPlayer)

aa = nbastatR::player_profiles(players = draft19$namePlayer)

player_profiles()

library(dplyr)
left_join(draft19, allstars)

draft19 %>% 
  mutate()

asd <- merge(draft19, allstars, by.y="Player", by.x = "namePlayer")
left_join(allstars, draft19, )
aaaaaa = players_tables(players = draft19$namePlayer)
denemes = players_tables(player_ids = 2544)

asd = nbastatR::bref_players_stats(seasons = 1951:2020, include_all_nba = T, tables = "totals")

nbastatR::
anti_join
