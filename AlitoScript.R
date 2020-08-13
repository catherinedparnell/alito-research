## Alito Research
## Jordan Sanz and Catherine Parnell
##
## 8/6/2020
## Cleaning, Present data load-in, T-testing

library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(extrafont)
library(RColorBrewer)

#setwd("/Users/catherineparnell/alito-research")

df <- read.csv("AlitoDataset.csv")

justices <- list('Alito','Roberts','Stevens','Scalia','Kennedy','Souter','Thomas','Ginsburg','Breyer','Sotomayor','Kagan','Gorsuch')

# makes justice booleans, 0 being dissent, 1 being majority, 2 being other
df <- df %>% 
  mutate(Roberts = if_else(str_detect(Majority, "Roberts"), 1, 
                           if_else(str_detect(Dissent, "Roberts"), 0, 2))) %>%
  mutate(Stevens = if_else(str_detect(Majority, "Stevens"), 1, 
                           if_else(str_detect(Dissent, "Stevens"), 0, 2))) %>%
  mutate(Scalia = if_else(str_detect(Majority, "Scalia"), 1, 
                          if_else(str_detect(Dissent, "Scalia"), 0, 2))) %>%
  mutate(Kennedy = if_else(str_detect(Majority, "Kennedy"), 1, 
                           if_else(str_detect(Dissent, "Kennedy"), 0, 2))) %>%
  mutate(Souter = if_else(str_detect(Majority, "Souter"), 1, 
                          if_else(str_detect(Dissent, "Souter"), 0, 2))) %>%
  mutate(Thomas = if_else(str_detect(Majority, "Thomas"), 1, 
                          if_else(str_detect(Dissent, "Thomas"), 0, 2))) %>%
  mutate(Ginsburg = if_else(str_detect(Majority, "Ginsburg"), 1, 
                            if_else(str_detect(Dissent, "Ginsburg"), 0, 2))) %>%
  mutate(Breyer = if_else(str_detect(Majority, "Breyer"), 1, 
                          if_else(str_detect(Dissent, "Breyer"), 0, 2))) %>%
  mutate(Sotomayor = if_else(str_detect(Majority, "Sotomayor"), 1, 
                             if_else(str_detect(Dissent, "Sotomayor"), 0, 2))) %>%
  mutate(Kagan = if_else(str_detect(Majority, "Kagan"), 1, 
                         if_else(str_detect(Dissent, "Kagan"), 0, 2))) %>%
  mutate(Gorsuch = if_else(str_detect(Majority, "Gorsuch"), 1, 
                           if_else(str_detect(Dissent, "Gorsuch"), 0, 2)))
df$Characterized.Answer

# creates answer flag and splits answers when 2
df <- df %>%
  separate(Characterized.Answer, c("Answer1", "Answer2"), sep=",", fill="right") %>%
  mutate(AnswerFlag1 = case_when(
    str_detect(Answer1, "Yes")~1,
    str_detect(Answer1, "No")~0,
    TRUE~2
  )) %>%
  mutate(AnswerFlag2 = case_when(
    str_detect(Answer2, "Yes")~1,
    str_detect(Answer2, "No")~0,
    TRUE~2
  ))

# puts color total for each case in according column

df <- df %>%
  mutate(Pink = I.Count.Pink + MA1.Count.Pink + R.Count.Pink + C.Count.Pink) %>%
  mutate(Green = I.Count.Green + MA1.Count.Green + R.Count.Green + C.Count.Green) %>%
  mutate(Blue = I.Count.Blue + MA1.Count.Blue + R.Count.Blue + C.Count.Blue) %>%
  mutate(Orange = I.Count.Orange + MA1.Count.Orange + R.Count.Orange + C.Count.Orange) %>%
  mutate(Purple = I.Count.Purple + MA1.Count.Purple + R.Count.Purple + C.Count.Purple) %>%
  mutate(Yellow = I.Count.Yellow + MA1.Count.Yellow + R.Count.Yellow + C.Count.Yellow)
df

colors_totals <- df %>%
  summarize(Pinktot = sum(Pink, na.rm=TRUE), Greentot = sum(Green, na.rm=TRUE), Bluetot = sum(Blue, na.rm = TRUE), Orangetot = sum(Orange, na.rm = TRUE), Purpletot = sum(Purple, na.rm=TRUE), Yellowtot = sum(Yellow, na.rm=TRUE)) %>%
  gather(key="Color", value="Count") %>%
  mutate(Color = fct_relevel(Color, c("Greentot", "Pinktot", "Yellowtot", "Orangetot", "Purpletot", "Bluetot"))) 
colors_totals

ggplot(colors_totals, aes(x = Color, y=Count, fill = Color)) + geom_bar(stat="identity") +
  labs(title="Sum of colors used overall",
       x="Color",
       y="Count") + theme_classic() + scale_fill_manual(values = c("#23A635", "#DC31D7", "#DCD431", "#DC8731", "#8E2BC5", "#2B5EC5")) + 
  theme(
    text = element_text(family="Times New Roman")
  )


# plot for year present data


getPalette = colorRampPalette(brewer.pal(9, "Blues"))

ggplot(df, aes(x = as.factor(Year), fill=as.factor(Year))) + geom_bar() + labs(title="Present Years Data", x="Year", y="Count") + theme_classic() + 
  theme(
  legend.position= "none",
  text = element_text(family = "Times New Roman")
) + scale_fill_manual(values=c("#57B0D0", "#52A2C7", "#4C94BE", "#4786B5", "#4278AC", "#3C6AA3", "#375B9B", "#314D92", "#2C3F89", "#273180", "#212377", "#1C156E"))

# makes labels for vote splits
df <- df %>%
  mutate(Vote.Split = case_when(
    Vote.Split == 0 ~ "Unanimous",
    Vote.Split == 1 ~ "Contentious",
    Vote.Split == 2 ~ "Ambiguous"
  ))


## plots vote split frequency
ggplot(df, aes(x=as.factor(Vote.Split), fill=as.factor(Vote.Split))) + geom_bar() + labs(
  title="Count of Vote Splits in Data",
  x = "Vote Split Type",
  y = "Count",
  fill = "Vote Type"
) + theme_classic() + scale_fill_manual(values=c("#585FBA", "#DF5F36", "#60CD75")) + theme(
  text = element_text(family = "Times New Roman")
)


## finds how often each person is in the majority/dissent
people <- df %>%
  select(c(Roberts, Stevens, Scalia, Kennedy, Souter, Thomas, Ginsburg, Breyer, Sotomayor, Kagan, Gorsuch)) %>%
  gather(key="People", value="InMajority") %>%
  mutate(InMajority=case_when(
    InMajority==0 ~ "Dissent",
    InMajority==1 ~ "Majority",
    TRUE ~ "Not Involved"
  )) %>%
  filter(InMajority != "Not Involved") %>%
  mutate(People = fct_relevel(People, "Kennedy", "Roberts", "Thomas","Scalia", "Breyer", "Ginsburg", "Kagan", "Sotomayor", "Stevens", "Souter", "Gorsuch"))
people

# plots majority barplot
ggplot(people, aes(x=People, fill=InMajority)) + geom_bar(position="dodge") + theme_classic() +
  labs(
    x="Justice",
    y="Number of Cases",
    title="Justices in Majority with Alito",
    fill="Majority?"
  ) + scale_fill_manual(values=c("#4A76D6", "#D6644A")) + theme(
    text = element_text(family = "Times New Roman")
  )

pink_mean = mean(df$Pink, na.rm = TRUE)
green_mean = mean(df$Green, na.rm = TRUE)
blue_mean = mean(df$Blue, na.rm = TRUE)
orange_mean = mean(df$Orange, na.rm = TRUE)
purple_mean = mean(df$Purple, na.rm = TRUE)
yellow_mean = mean(df$Yellow, na.rm = TRUE)

# jojo can u plot the means :)

df <- df %>%
  mutate(PinkNorm = (I.Count.Pink + MA1.Count.Pink + R.Count.Pink + C.Count.Pink) / Opinion.Length) %>%
  mutate(GreenNorm = (I.Count.Green + MA1.Count.Green + R.Count.Green + C.Count.Green) / Opinion.Length) %>%
  mutate(BlueNorm = (I.Count.Blue + MA1.Count.Blue + R.Count.Blue + C.Count.Blue) / Opinion.Length) %>%
  mutate(OrangeNorm = (I.Count.Orange + MA1.Count.Orange + R.Count.Orange + C.Count.Orange) / Opinion.Length) %>%
  mutate(PurpleNorm = (I.Count.Purple + MA1.Count.Purple + R.Count.Purple + C.Count.Purple) / Opinion.Length) %>%
  mutate(YellowNorm = (I.Count.Yellow + MA1.Count.Yellow + R.Count.Yellow + C.Count.Yellow) / Opinion.Length)
df

pink_norm_mean = mean(df$PinkNorm, na.rm = TRUE)
green_norm_mean = mean(df$GreenNorm, na.rm = TRUE)
blue_norm_mean = mean(df$BlueNorm, na.rm = TRUE)
orange_norm_mean = mean(df$OrangeNorm, na.rm = TRUE)
purple_norm_mean = mean(df$PurpleNorm, na.rm = TRUE)
yellow_norm_mean = mean(df$YellowNorm, na.rm = TRUE)

# can u plot this too babe <3



# comparing two highest categories: pragmatism and doctrinalism - not as statistically significant...
pinkVgreen = t.test(df$Pink, df$Green)
pinkNormVgreenNorm = t.test(df$PinkNorm, df$GreenNorm)

# comparing pragmatism and originalism: common conceptions between conservative/liberal judges - extremely statistically significant !!
blueVgreen = t.test(df$Blue, df$Green)
blueNormVgreenNorm = t.test(df$BlueNorm, df$GreenNorm)

# we can ask what others she is specifically curious about



# comparing two highest categories: pragmatism and doctrinalism where yes v. no - even less statistically significant...
pinkVgreenYes = t.test(df$Pink[df$Answer1 == 'Yes'], df$Green[df$Answer1 == 'Yes'])
pinkNormVgreenNormYes = t.test(df$PinkNorm[df$Answer1 == 'Yes'], df$GreenNorm[df$Answer1 == 'Yes'])

pinkVgreenNo = t.test(df$Pink[df$Answer1 == 'No'], df$Green[df$Answer1 == 'No'])
pinkNormVgreenNormNo = t.test(df$PinkNorm[df$Answer1 == 'No'], df$GreenNorm[df$Answer1 == 'No'])

# comparing pragmatism and originalism: where yes v. no - also extremely statistically significant !!
blueVgreenYes = t.test(df$Blue[df$Answer1 == 'Yes'], df$Green[df$Answer1 == 'Yes'])
blueNormVgreenNormYes = t.test(df$BlueNorm[df$Answer1 == 'Yes'], df$GreenNorm[df$Answer1 == 'Yes'])

blueVgreenNo = t.test(df$Blue[df$Answer1 == 'No'], df$Green[df$Answer1 == 'No'])
blueNormVgreenNormNo = t.test(df$BlueNorm[df$Answer1 == 'No'], df$GreenNorm[df$Answer1 == 'No'])



# comparing two highest categories: pragmatism and doctrinalism based on vote split
pinkVgreenSplit = t.test(df$Pink[df$Vote.Split == 1], df$Green[df$Vote.Split == 1])
# this p value is significantly less *in comparison* so likely more difference
pinkNormVgreenNormSplit = t.test(df$PinkNorm[df$Vote.Split == 1], df$GreenNorm[df$Vote.Split == 1])

# p value for these surprisingly high, more likely to be more similar, insinuates more doctrinalism when speaking for majority
pinkVgreenMaj = t.test(df$Pink[df$Vote.Split == 0], df$Green[df$Vote.Split == 0])
pinkNormVgreenNormMaj= t.test(df$PinkNorm[df$Vote.Split == 0], df$GreenNorm[df$Vote.Split == 0])

# comparing pragmatism and originalism: based on vote split not much change at all, still v significant
blueVgreenSplit = t.test(df$Blue[df$Vote.Split == 1], df$Green[df$Vote.Split == 1])
blueNormVgreenNormSplit = t.test(df$BlueNorm[df$Vote.Split == 1], df$GreenNorm[df$Vote.Split == 1])

blueVgreenMaj = t.test(df$Blue[df$Vote.Split == 0], df$Green[df$Vote.Split == 0])
blueNormVgreenNormMaj = t.test(df$BlueNorm[df$Vote.Split == 0], df$GreenNorm[df$Vote.Split == 0])


# comparing pragmatism across yes/no and vote split variables
greenVgreenYes =  t.test(df$Green, df$Green[df$Answer1 == 'Yes']) # not significant
greenVgreenYesNorm =  t.test(df$GreenNorm, df$GreenNorm[df$Answer1 == 'Yes']) # not significant

greenVgreenNo = t.test(df$Green, df$Green[df$Answer1 == 'No']) # even less significant
greenVgreenNoNorm = t.test(df$GreenNorm, df$GreenNorm[df$Answer1 == 'No']) # even even less significant - basically the same

greenVgreenSplit = t.test(df$Green, df$Green[df$Vote.Split == 1]) # significant: green < green split
greenVgreenSplitNorm = t.test(df$GreenNorm, df$GreenNorm[df$Vote.Split == 1]) # not significant

greenVgreenMaj = t.test(df$Green, df$Green[df$Vote.Split == 0]) # statistically significant: green > green maj
greenVgreenMajNorm = t.test(df$GreenNorm, df$GreenNorm[df$Vote.Split == 0]) # not significant

# comparing originalism across yes/no and vote split variables
blueVblueYes =  t.test(df$Blue, df$Blue[df$Answer1 == 'Yes']) # not significant
blueVblueYesNorm =  t.test(df$BlueNorm, df$BlueNorm[df$Answer1 == 'Yes']) # even less so

blueVblueNo = t.test(df$Blue, df$Blue[df$Answer1 == 'No']) # not significant
blueVblueNoNorm = t.test(df$BlueNorm, df$BlueNorm[df$Answer1 == 'No']) # even less so

blueVblueSplit = t.test(df$Blue, df$Blue[df$Vote.Split == 1]) # not significant
blueVblueSplitNorm = t.test(df$BlueNorm, df$BlueNorm[df$Vote.Split == 1]) # not significant

blueVblueMaj = t.test(df$Blue, df$Blue[df$Vote.Split == 0]) # a little more so, but not significant
blueVblueMajNorm = t.test(df$BlueNorm, df$BlueNorm[df$Vote.Split == 0]) # very much less significant





