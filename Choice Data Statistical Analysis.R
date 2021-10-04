#MBL Summer 2021 Choice Experiment
choice_data <- read.table("2021_09_09 Choice Experiment R Studio Analysis.csv", sep = ",", header = TRUE)
library(dplyr)
library(ggplot2)
summary(choice_data)
#decision in morning are characters and need to be numeric
#trying to make it numeric
#the following below didn't work

#CAN IGNORE THIS
#choice_data$Decision.In.Morning <- as.numeric(gsub("\\.", "", choice_data$Decision.In.Morning))
#choice_data$Length..cm. <- as.numeric(gsub("\\.", "", choice_data$Length..cm.))

#second attempt
#char <- choice_data$Decision.In.Morning
#char
#typeof(char)

#num <- as.numeric(char)
#num
#typeof(num)

#couldn't make den choice numerical so we made den choice as a binary dummy variable where 0= control and 1=magnet

#Linear model testing if den choice was dependent/influenced by size
#NEED TO UPDATE LABELS FOR DUMMY VARIABLES IN THE CSV FILE
choice_data_lmsize <- lm(X~Length..cm., data = choice_data)
summary(choice_data_lmsize)
anova(choice_data_lmsize)

#linear model testing if den choice was dependent/influenced by moon coverage
choice_data_lmmoon <- lm(X~X..Moon.Coverage, data = choice_data)
summary(choice_data_lmmoon)
anova(choice_data_lmmoon)
#significant p value (wtf????) p-value= 0.007444 (can i get a yee haw)

#linear model testing if den choice was dependent/influenced by the interaction of moon coverage and size
choice_data_lmsize_moon <- lm(X~X..Moon.Coverage*Length..cm., data = choice_data)
summary(choice_data_lmsize_moon)
anova(choice_data_lmsize_moon)
#only moon coverage had significant effect

#linear model testing if den choice was dependent/influenced by whether den was east or west (cardinal direction)
#am expecting a difference because they preferred the east den over the west
choice_data_lmcarddir <- lm(X~X.1, data = choice_data)
summary(choice_data_lmcarddir)
anova(choice_data_lmcarddir)
#i don't think this is a good test, better to do chi square analysis to compare counts of east versus west and expected number

#chisquare test on east versus west
EvsW <-matrix(c(27,13, 20,20), nrow =2)
EvsW
chisq.test(EvsW)
#X-squared = 1.8569, df = 1, p-value = 0.173

#####PLOTTING####
ggplot(choice_data, aes(x=Moon.Phase.Dummy.Variable, y=X..Choosing.Control)) + geom_line (color= "grey") + geom_point(color="black") + theme_classic() + labs(y= "Percentage of Fish Choosing Control Den (%)", x="Moon Phase") + scale_y_continuous(breaks = round(seq(min(0), max(100), by = 10),1)) + ylim(0,100)
