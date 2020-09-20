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

green <- "#189D1D"
pink <- "#D824C1"
yellow <-"#D4D824"
orange <- "#D89F24"
purple <-"#A824D8"
blue <- "#244CD8"

greenR <- "#156e18"
pinkR <- "#ab1b99"
yellowR <- "#afb31e"
orangeR <- "#b0821e"
purpleR <- "#8219a8"
blueR <- "#1c3799"

rOutline <- "#000000"
maOutline <- "#dbdbdb"
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

## Create flag for type of case
df <- df %>%
  mutate(StatutoryFlag = case_when(
    str_detect(Case.Type, "Statutory") ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(ConstitutionalFlag = case_when(
    str_detect(Case.Type, "Constitutional") ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(CommonLawFlag = case_when(
    str_detect(Case.Type, "Common Law") ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(CivProFlag = case_when(
    str_detect(Case.Type, "Civ Pro") ~ 1,
    TRUE ~ 0,
  ))
df

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
  ) + scale_y_continuous(expand = expansion(mult = c(0, .1)))


## plot for color sums over time

df_grouped <- df %>%
  group_by(Year)%>%
  summarize(year_sum_green = sum(Green, na.rm=TRUE), year_sum_purple = sum(Purple, na.rm = TRUE), year_sum_pink = sum(Pink, na.rm = TRUE), year_sum_yellow = sum(Yellow, na.rm = TRUE), year_sum_orange = sum(Orange, na.rm = TRUE), year_sum_blue = sum(Blue, na.rm = TRUE)) %>%
  gather(-Year, key="Color", value="Sum")

colors_over_time <- ggplot(df_grouped, aes(x = as.factor(Year), y = Sum, color = Color, group=Color)) + geom_line(size = 1) + theme_classic() + 
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "None"
  ) +
  scale_color_manual(values = c(blue, green, orange, pink, purple, yellow)) +
  labs(
    x = "Year",
    y = "Yearly Sum (Not Cumulative)",
    title = "Sum of Colors Per Year"
  )

colors_over_time

faceted_colors_over_time <- colors_over_time + facet_wrap(~Color)

faceted_colors_over_time


# plot for year present data

getPalette = colorRampPalette(brewer.pal(9, "Blues"))

ggplot(df, aes(x = as.factor(Year), fill=as.factor(Year))) + scale_y_continuous(expand = expansion(mult = c(0, .1))) + geom_bar() + labs(title="Present Years Data", x="Year", y="Count") + theme_classic() + 
  theme(
  legend.position= "none",
  text = element_text(family = "Times New Roman")
) + scale_fill_manual(values=c("#57B0D0", "#52A2C7", "#4C94BE", "#4786B5", "#4278AC", "#3C6AA3", "#375B9B", "#314D92", "#2C3F89", "#273180", "#212377", "#1C156E"))


# makes labels for vote splits
df <- df %>%
  mutate(Vote.SplitLabel = case_when(
    Vote.Split == 0 ~ "Unanimous",
    Vote.Split == 1 ~ "Contentious",
    Vote.Split == 2 ~ "Ambiguous"
  ))


## plots vote split frequency
ggplot(df, aes(x=as.factor(Vote.SplitLabel), fill=as.factor(Vote.SplitLabel))) + geom_bar() + scale_y_continuous(expand = expansion(mult = c(0, .1))) + labs(
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
ggplot(people, aes(x=People, fill=InMajority)) + geom_bar(position="dodge") + scale_y_continuous(expand = expansion(mult = c(0, .1))) + theme_classic() +
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


mean_dataframe <- data.frame(pink_mean, green_mean, blue_mean, orange_mean, purple_mean, yellow_mean)
mean_dataframe <- mean_dataframe %>%
  gather(key="Color", value="Mean") %>%
  mutate(Color = case_when(
    Color == "green_mean" ~ "Green",
    Color == "pink_mean" ~ "Pink",
    Color == "yellow_mean" ~ "Yellow",
    Color == "orange_mean" ~ "Orange",
    Color == "purple_mean" ~ "Purple",
    Color == "blue_mean" ~ "Blue",
  )) %>%
  mutate(Color = fct_relevel(Color, "Green", "Pink", "Yellow", "Orange", "Purple", "Blue"))
mean_dataframe

ggplot(mean_dataframe, aes(x=Color, y=Mean, fill=Color)) + geom_bar(stat="identity") + scale_y_continuous(expand = expansion(mult = c(0, .1))) + theme_classic() +
  scale_fill_manual(values = c("#189D1D", "#D824C1", "#D4D824", "#D89F24", "#A824D8", "#244CD8")) + 
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none"
  ) +
  labs(
    title = "Mean of Colors Overall",
    x = "Color",
    y = "Mean"
  )



df <- df %>%
  
  # total normalization
  mutate(PinkNorm = (I.Count.Pink + MA1.Count.Pink + R.Count.Pink + C.Count.Pink) / Opinion.Length) %>%
  mutate(GreenNorm = (I.Count.Green + MA1.Count.Green + R.Count.Green + C.Count.Green) / Opinion.Length) %>%
  mutate(BlueNorm = (I.Count.Blue + MA1.Count.Blue + R.Count.Blue + C.Count.Blue) / Opinion.Length) %>%
  mutate(OrangeNorm = (I.Count.Orange + MA1.Count.Orange + R.Count.Orange + C.Count.Orange) / Opinion.Length) %>%
  mutate(PurpleNorm = (I.Count.Purple + MA1.Count.Purple + R.Count.Purple + C.Count.Purple) / Opinion.Length) %>%
  mutate(YellowNorm = (I.Count.Yellow + MA1.Count.Yellow + R.Count.Yellow + C.Count.Yellow) / Opinion.Length) %>%
  
  # Main argument normalization
  mutate(PinkMA1Norm = MA1.Count.Pink / MA1.Length) %>%
  mutate(GreenMA1Norm = MA1.Count.Green / MA1.Length) %>%
  mutate(BlueMA1Norm = MA1.Count.Blue / MA1.Length) %>%
  mutate(OrangeMA1Norm = MA1.Count.Orange / MA1.Length) %>%
  mutate(PurpleMA1Norm = MA1.Count.Purple / MA1.Length) %>%
  mutate(YellowMA1Norm = MA1.Count.Yellow / MA1.Length) %>%
  
  # Rebuttal normalization
  mutate(PinkRebuttalNorm = R.Count.Pink / R.Length) %>%
  mutate(GreenRebuttalNorm = R.Count.Green / R.Length) %>%
  mutate(BlueRebuttalNorm = R.Count.Blue / R.Length) %>%
  mutate(OrangeRebuttalNorm = R.Count.Orange / R.Length) %>%
  mutate(PurpleRebuttalNorm = R.Count.Purple / R.Length) %>%
  mutate(YellowRebuttalNorm = R.Count.Yellow / R.Length)
  

# Main argument vs rebuttal data frame creation
mainvsreb <- df %>%
  filter(R.Length > 0) %>%
  select(PinkMA1Norm, PinkRebuttalNorm, GreenMA1Norm, GreenRebuttalNorm, BlueMA1Norm, BlueRebuttalNorm,
         OrangeMA1Norm, OrangeRebuttalNorm, PurpleMA1Norm, PurpleRebuttalNorm, YellowMA1Norm, YellowRebuttalNorm) %>%
  summarize(PinkMA = sum(PinkMA1Norm), PinkRebuttal = sum(PinkRebuttalNorm), GreenMA = sum(GreenMA1Norm), GreenRebuttal = sum(GreenRebuttalNorm), 
            BlueMA = sum(BlueMA1Norm), BlueRebuttal = sum(BlueRebuttalNorm), OrangeMA = sum(OrangeMA1Norm), OrangeRebuttal = sum(OrangeRebuttalNorm), 
            PurpleMA = sum(PurpleMA1Norm), PurpleRebuttal = sum(PurpleRebuttalNorm), YellowMA = sum(YellowMA1Norm), YellowRebuttal = sum(YellowRebuttalNorm)) %>%
  gather(key="Type", val="Value") %>%
  mutate(Color = case_when(
    str_detect(Type, "Pink") ~ "Pink",
    str_detect(Type, "Green") ~ "Green",
    str_detect(Type, "Blue") ~ "Blue",
    str_detect(Type, "Orange") ~ "Orange",
    str_detect(Type, "Purple") ~ "Purple",
    str_detect(Type, "Yellow") ~ "Yellow"
  )) %>%
  mutate(Argument = case_when(
    str_detect(Type, "MA") ~ "Main Argument",
    str_detect(Type, "Rebuttal") ~ "Rebuttal",
  ))
mainvsreb

## Main argument vs rebuttal color changes plot
ggplot(mainvsreb, aes(x = Argument, y=Value, fill=Color)) + geom_bar(stat="identity", position="dodge")+ scale_y_continuous(expand = expansion(mult = c(0, .1))) + theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
  ) +
  scale_fill_manual(values = c(blue, green, orange, pink, purple, yellow
                              )) +
  scale_color_manual(values = c(maOutline, rOutline)) + 
  labs(
    title = "Main Argument vs Rebuttal Color Changes, Normalized",
    x = "Color",
    y = "Normalized Line Count"
  )


pink_norm_mean = mean(df$PinkNorm, na.rm = TRUE)
green_norm_mean = mean(df$GreenNorm, na.rm = TRUE)
blue_norm_mean = mean(df$BlueNorm, na.rm = TRUE)
orange_norm_mean = mean(df$OrangeNorm, na.rm = TRUE)
purple_norm_mean = mean(df$PurpleNorm, na.rm = TRUE)
yellow_norm_mean = mean(df$YellowNorm, na.rm = TRUE)

norm_mean_dataframe <- data.frame(pink_norm_mean, green_norm_mean, blue_norm_mean, orange_norm_mean, purple_norm_mean, yellow_norm_mean)
norm_mean_dataframe <- norm_mean_dataframe %>%
  gather(key="Color", value="Mean") %>%
  mutate(Color = case_when(
    Color == "green_norm_mean" ~ "Green",
    Color == "pink_norm_mean" ~ "Pink",
    Color == "yellow_norm_mean" ~ "Yellow",
    Color == "orange_norm_mean" ~ "Orange",
    Color == "purple_norm_mean" ~ "Purple",
    Color == "blue_norm_mean" ~ "Blue",
  )) %>%
  mutate(Color = fct_relevel(Color, "Green", "Pink", "Yellow", "Orange", "Purple", "Blue"))
norm_mean_dataframe

ggplot(norm_mean_dataframe, aes(x=Color, y=Mean, fill=Color)) + geom_bar(stat="identity") + scale_y_continuous(expand = expansion(mult = c(0, .1))) + theme_classic() +
  scale_fill_manual(values = c("#189D1D", "#D824C1", "#D4D824", "#D89F24", "#A824D8", "#244CD8")) + 
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none"
  ) +
  labs(
    title = "Normalized Mean of Colors Overall",
    x = "Color",
    y = "Mean"
  )

## Case type color totals
df <- df %>%
  mutate(PinkStat = if_else(StatutoryFlag == 1, PinkNorm, 0),
         PinkConst = if_else(ConstitutionalFlag == 1, PinkNorm, 0),
         PinkCivPro = if_else(CivProFlag == 1, PinkNorm, 0),
         PinkCommon = if_else(CommonLawFlag == 1, PinkNorm, 0),
         
         GreenStat = if_else(StatutoryFlag == 1, GreenNorm, 0),
         GreenConst = if_else(ConstitutionalFlag == 1, GreenNorm, 0),
         GreenCivPro = if_else(CivProFlag == 1, GreenNorm, 0),
         GreenCommon = if_else(CommonLawFlag == 1, GreenNorm, 0),
         
         BlueStat = if_else(StatutoryFlag == 1, BlueNorm, 0),
         BlueConst = if_else(ConstitutionalFlag == 1, BlueNorm, 0),
         BlueCivPro = if_else(CivProFlag == 1, BlueNorm, 0),
         BlueCommon = if_else(CommonLawFlag == 1, BlueNorm, 0),
         
         OrangeStat = if_else(StatutoryFlag == 1, OrangeNorm, 0),
         OrangeConst = if_else(ConstitutionalFlag == 1, OrangeNorm, 0),
         OrangeCivPro = if_else(CivProFlag == 1, OrangeNorm, 0),
         OrangeCommon = if_else(CommonLawFlag == 1, OrangeNorm, 0),
         
         PurpleStat = if_else(StatutoryFlag == 1, PurpleNorm, 0),
         PurpleConst = if_else(ConstitutionalFlag == 1, PurpleNorm, 0),
         PurpleCivPro = if_else(CivProFlag == 1, PurpleNorm, 0),
         PurpleCommon = if_else(CommonLawFlag == 1, PurpleNorm, 0),
         
         YellowStat = if_else(StatutoryFlag == 1, YellowNorm, 0),
         YellowConst = if_else(ConstitutionalFlag == 1, YellowNorm, 0),
         YellowCivPro = if_else(CivProFlag == 1, YellowNorm, 0),
         YellowCommon = if_else(CommonLawFlag == 1, YellowNorm, 0),
  )

CaseTypeDataframe <- df %>%
  summarize(PinkStat = sum(PinkStat, na.rm=TRUE), PinkCommon = sum(PinkCommon, na.rm=TRUE), 
            PinkConst = sum(PinkConst, na.rm=TRUE), PinkCivPro = sum(PinkCivPro, na.rm=TRUE),
            
            GreenStat = sum(GreenStat, na.rm=TRUE), GreenCommon = sum(GreenCommon, na.rm=TRUE), 
            GreenConst = sum(GreenConst, na.rm=TRUE), GreenCivPro = sum(GreenCivPro, na.rm=TRUE),
            
            BlueStat = sum(BlueStat, na.rm=TRUE), BlueCommon = sum(BlueCommon, na.rm=TRUE), 
            BlueConst = sum(BlueConst, na.rm=TRUE), BlueCivPro = sum(BlueCivPro, na.rm=TRUE),
            
            OrangeStat = sum(OrangeStat, na.rm=TRUE), OrangeCommon = sum(OrangeCommon, na.rm=TRUE), 
            OrangeConst = sum(OrangeConst, na.rm=TRUE), OrangeCivPro = sum(OrangeCivPro, na.rm=TRUE),
            
            PurpleStat = sum(PurpleStat, na.rm=TRUE), PurpleCommon = sum(PurpleCommon, na.rm=TRUE), 
            PurpleConst = sum(PurpleConst, na.rm=TRUE), PurpleCivPro = sum(PurpleCivPro, na.rm=TRUE),
            
            YellowStat = sum(YellowStat, na.rm=TRUE), YellowCommon = sum(YellowCommon, na.rm=TRUE), 
            YellowConst = sum(YellowConst, na.rm=TRUE), YellowCivPro = sum(YellowCivPro, na.rm=TRUE),
  ) %>%
  gather(key="Descriptor", value = "Value") %>%
  mutate(Color = case_when(
    str_detect(Descriptor, "Pink") ~ "Pink",
    str_detect(Descriptor, "Green") ~ "Green",
    str_detect(Descriptor, "Blue") ~ "Blue",
    str_detect(Descriptor, "Orange") ~ "Orange",
    str_detect(Descriptor, "Purple") ~ "Purple",
    str_detect(Descriptor, "Yellow") ~ "Yellow"
  )) %>%
  mutate(CaseType = case_when(
    str_detect(Descriptor, "Stat") ~ "Statutory",
    str_detect(Descriptor, "Const") ~ "Constitutional",
    str_detect(Descriptor, "Common") ~  "Common Law",
    str_detect(Descriptor, "CivPro") ~ "Civ Pro"
  ))

CaseTypeDataframe

ggplot(CaseTypeDataframe, aes(x = CaseType, y=Value, fill = Color)) + geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme_classic() + theme(
    text = element_text(family = "Times New Roman"),
  ) + scale_fill_manual(values = c(blue, green, orange, pink, purple, yellow)) +
  labs(
    title = "Case Type Color Prevalence, Normalized",
    x = "Case Type",
    y = "Normalized Line Count",
    legend = "Color"
  )



# comparing two highest categories: pragmatism and doctrinalism - not as statistically significant...
pinkVgreen = t.test(df$Pink, df$Green)
pinkNormVgreenNorm = t.test(df$PinkNorm, df$GreenNorm)

# comparing pragmatism and originalism: common conceptions between conservative/liberal judges - extremely statistically significant !!
blueVgreen = t.test(df$Blue, df$Green)
blueNormVgreenNorm = t.test(df$BlueNorm, df$GreenNorm)

pinkNormVyellowNorm = t.test(df$PinkNorm, df$YellowNorm) # doctrinalism v textualism: no question that he uses case law far more than textualism: super small p value
greenNormVyellowNorm = t.test(df$GreenNorm, df$YellowNorm) # also extremely statistically significant
blueNormVyellowNorm = t.test(df$BlueNorm, df$YellowNorm) # also extremely statistically significant
purpleNormVyellowNorm = t.test(df$PurpleNorm, df$YellowNorm) # also statistically significant

# we can ask what others she is specifically curious about

# main argument vs rebuttal: overall do any of the color change proportions shift
# categories for types of cases - are they really that much outliers? anything overall different about how color graphs look for those particular cases

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



# comparing two highest categories: pragmatism and doctrinalism based on vote split (contentious)
pinkVgreenSplit = t.test(df$Pink[df$Vote.Split == 1], df$Green[df$Vote.Split == 1])
# this p value is significantly less *in comparison* so likely more difference
pinkNormVgreenNormSplit = t.test(df$PinkNorm[df$Vote.Split == 1], df$GreenNorm[df$Vote.Split == 1])

# p value for these surprisingly high, more likely to be more similar, insinuates more doctrinalism when speaking for unanimous majority
pinkVgreenMaj = t.test(df$Pink[df$Vote.Split == 0], df$Green[df$Vote.Split == 0])
pinkNormVgreenNormMaj= t.test(df$PinkNorm[df$Vote.Split == 0], df$GreenNorm[df$Vote.Split == 0])

# comparing pragmatism and originalism: based on vote split not much change at all, still not significant
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





