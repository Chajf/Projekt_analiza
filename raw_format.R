library(tidyverse)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(lattice)
dane <- read.csv("C:\\Users\\Patryk\\Downloads\\DS3_weapon.csv")

ggplot(dane, aes(x=Weight))+
  geom_histogram()
shapiro.test(dane$Weight)

dane2 <- filter(dane,Category %in% c("Katanas","Greatswords", "Straight Swords","Ultra Greatswords"))
dane2 %>% 
  group_by(Category) %>% 
  shapiro_test(Physical_dmg)

unique(dane$Category)

no_weapon <- dane %>% 
  group_by(Category) %>% 
  summarise(n = n())

no_weapon %>%
  mutate(Category = fct_reorder(Category, n)) %>%
  ggplot(aes(y=Category,x=n))+
    geom_bar(stat = "identity")

dane %>% 
  group_by(Reinforcement) %>% 
  shapiro_test(Strength)

dane %>% 
  group_by(Reinforcement) %>% 
  summarise(n = n())

dane %>% 
  ggboxplot(x= "Reinforcement",
            y="Strength",
            combine=1,
            color="Reinforcement",
            add = "jitter")

dane %>% 
  select(Strength,Physical_dmg) %>% 
  mshapiro_test(.)

dane %>% 
  cor_test(Physical_dmg,Strength)

box_m(dane[,c("Strength","Physical_dmg")], dane[,"Reinforcement"])

mod1 <- lm(Physical_dmg~Strength, dane)
summary(mod1)

dane3$Sell.Price <-  as.numeric(dane3$Sell.Price)

stats <- NULL
stats <- as.data.frame(stats)
for(i in 1:nrow(dane)){
  x <- str_extract_all(dane$Stat.Requirements[i],"[:digit:]+")
  x <- as.numeric(unlist(x))
  stats <- rbind(stats,x) 
}
colnames(stats) <- c("Str","Dex","Int","Fth")
dane <- subset(dane, select=-Stat.Requirements)
dane3 <- cbind(dane,stats)

damage <- NULL
damage <- as.data.frame(damage)
for(i in 1:nrow(dane)){
  x <- str_extract_all(dane$Damage[i],"[:digit:]+")
  x <- as.numeric(unlist(x))
  damage <- rbind(damage,x) 
}
colnames(damage) <- c("Physical_dmg","Magic","Fire","Lightning","Dark")
damage <- damage %>% 
  mutate(Sum_dmg = rowSums(across(where(is.numeric))))
dane3 <- subset(dane3, select=-Damage)
dane3 <- cbind(dane3,damage[,c("Physical_dmg","Sum_dmg")])

bonuses <- NULL
bonuses <- as.data.frame(bonuses)
for(i in 1:nrow(dane)){
  x <- str_extract_all(dane$Stat.Bonuses[i],"[^/]+")
  x <- as.character(unlist(x))
  bonuses <- rbind(bonuses,x) 
}
bonuses <- bonuses[,1:2]
colnames(bonuses) <- c("Str_bonus","Dex_bonus")
bonuses$Str_bonus <- as.factor(bonuses$Str_bonus)
bonuses$Dex_bonus <- as.factor(bonuses$Dex_bonus)
dane3 <- subset(dane3, select=-Stat.Bonuses)
dane3 <- cbind(dane3,bonuses)

dane3 %>% 
  group_by(Reinforcement) %>% 
  summarise_at(vars(Str,Dex,Int,Fth),list(~mean(.),~sd(.)))

dane3 %>% 
  filter(., !(Reinforcement %in% c("Spell Tool","Bow"))) %>% 
  group_by(Reinforcement) %>% 
  shapiro_test(Physical_dmg)

dane3 %>% 
  filter(.,Category %in% c("Katanas","Greatswords", "Straight Swords","Ultra Greatswords")) %>% 
  group_by(Category) %>% 
  shapiro_test(Sum_dmg)

dane4 <- filter(dane3,!(Category %in% c("Small Shields" ,"Greatshields", "Bows","Staves","Chimes",
                                        "Crossbows","Pyromancy Flames","Normal Shields","Torches",
                                        "Greatbows","Talismans")))

unique(dane4$Category)

filter(dane4,Category %in% c("Great Hammers", "Hammers"))

mod2 <- lm(Sum_dmg ~ Int+Fth+Str+Dex, dane4)
summary(mod2)

ggboxplot(dane4,x="Reinforcement",y="Physical_dmg",fill = "Reinforcement")

xyplot(Physical_dmg ~ Weight | Str_bonus, data=dane4, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, col = "red")
       })

mod3 <- lm(Weight~Str+Dex+Int+Fth,dane4)
summary(mod3)

dane4 %>% 
  cor_test(Physical_dmg,Str)

dane4 %>% 
  group_by(Reinforcement) %>% 
  identify_outliers(Sum_dmg) %>% 
  select(is.extreme,everything())

mod <- lm(Sum_dmg ~ Reinforcement, dane4)
ggqqplot(residuals(mod))
shapiro_test(residuals(mod))

#Test Kruskala na różnice w wadze pomiędzy grupami
res.kruskal <- dane4 %>% 
  kruskal_test(Weight ~ Reinforcement)
res.kruskal

dane4 %>% 
  kruskal_effsize(Weight ~ Reinforcement)

pwc <- dane4 %>% 
  dunn_test(Weight ~ Reinforcement, p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "Reinforcement")
ggboxplot(dane4, x = "Reinforcement", y = "Weight") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#Test Kruskala na różnice w obrażeniach pomiędzy grupami
res.kruskal <- dane4 %>% 
  kruskal_test(Sum_dmg ~ Reinforcement)
res.kruskal

dane4 %>% 
  kruskal_effsize(Sum_dmg ~ Reinforcement)

pwc <- dane4 %>% 
  dunn_test(Sum_dmg ~ Reinforcement, p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "Reinforcement")
ggboxplot(dane4, x = "Reinforcement", y = "Sum_dmg") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#Test Kruskala na różnice w wymaganej sile pomiędzy grupami
res.kruskal <- dane4 %>% 
  kruskal_test(Str ~ Reinforcement)
res.kruskal

dane4 %>% 
  kruskal_effsize(Str ~ Reinforcement)

pwc <- dane4 %>% 
  dunn_test(Str ~ Reinforcement, p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "Reinforcement")
ggboxplot(dane4, x = "Reinforcement", y = "Str") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#Test Kruskala na różnice w wymaganej zrecznosci pomiędzy grupami
res.kruskal <- dane4 %>% 
  kruskal_test(Dex ~ Reinforcement)
res.kruskal

dane4 %>% 
  kruskal_effsize(Dex ~ Reinforcement)

pwc <- dane4 %>% 
  dunn_test(Dex ~ Reinforcement, p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "Reinforcement")
ggboxplot(dane4, x = "Reinforcement", y = "Dex") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#Test Kruskala na różnice w wytrzymalosci broni pomiędzy grupami
res.kruskal <- dane4 %>% 
  kruskal_test(Durability ~ Reinforcement)
res.kruskal

dane4 %>% 
  kruskal_effsize(Durability ~ Reinforcement)

pwc <- dane4 %>% 
  dunn_test(Durability ~ Reinforcement, p.adjust.method = "bonferroni") 
pwc

pwc <- pwc %>% add_xy_position(x = "Reinforcement")
ggboxplot(dane4, x = "Reinforcement", y = "Durability") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

dane4 %>% 
  select(Reinforcement,Str,Dex) %>% 
  pivot_longer(cols = -Reinforcement) %>% 
  ggboxplot(x = "Reinforcement",
            y = "value",
            color = "name",
            add = "jitter")

dane4 %>%  
  select(Reinforcement,Str,Dex) %>% 
  group_split(Reinforcement) %>%  
  map(~mshapiro_test(.x[,2:3]))

