library(stringr)
library(ggplot2)
library(dplyr)

train.data <- read.csv("~/Notebooks/titanic/data/train.csv", na.strings = c(NA, ""))
train.data$Survived <- factor(train.data$Survived)
train.data$Pclass <- factor(train.data$Pclass)

sapply(train.data, 
       function(df) { 
           sum(is.na(df) == TRUE) / length(df); 
       })

# Impute values.
train.data$Embarked[which(is.na(train.data$Embarked))] <- 'S'
table(train.data$Embarked, useNA = "always")

train.data$Name <- as.character(train.data$Name)
tbl.names <- table(unlist(strsplit(train.data$Name, "\\s+")))
sort(tbl.names[grep("\\.", names(tbl.names))], decreasing = TRUE)

tbl.age.title <- cbind(train.data$Age, str_match(train.data$Name, "[a-zA-z]+\\."))
table(tbl.age.title[is.na(tbl.age.title[,1]), 2])

# Calculate mean age by title.
mean.dr <- mean(train.data$Age[grepl(" Dr\\.", train.data$Name) & !is.na(train.data$Age)])
mean.mr <- mean(train.data$Age[grepl(" Mr\\.", train.data$Name) & !is.na(train.data$Age)])
mean.mrs <- mean(train.data$Age[grepl(" Mrs\\.", train.data$Name) & !is.na(train.data$Age)])
mean.miss <- mean(train.data$Age[grepl(" Miss\\.", train.data$Name) & !is.na(train.data$Age)])
mean.master <- mean(train.data$Age[grepl(" Master\\.", train.data$Name) & !is.na(train.data$Age)])

# Use mean as approximate age, when acutal age is not available. 
train.data$Age[grepl(" Dr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.dr
train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.mr 
train.data$Age[grepl(" Mrs\\.", train.data$Name) & is.na(train.data$Age)] <- mean.mrs 
train.data$Age[grepl(" Miss\\.", train.data$Name) & is.na(train.data$Age)] <- mean.miss 
train.data$Age[grepl(" Master\\.", train.data$Name) & is.na(train.data$Age)] <- mean.master

# Add age group
train.data$AgeGroup <- cut(train.data$Age, seq(0, 100, 10))
survived <- filter(train.data, Survived == 1)
died <- filter(train.data, Survived == 0)
died$Count <- 1

survived.agegroup <- survived %>% 
  group_by(AgeGroup) %>%
  summarise(Count = sum(as.numeric(levels(Survived))[Survived]))

died.agegroup <- died %>%
  group_by(AgeGroup) %>%
  summarise(Count = sum(Count))

survivors <- rbind(survived.agegroup, died.agegroup)

graph.survived.sex <- ggplot(data = train.data, aes(x = train.data$Survived, fill = train.data$Sex))
graph.survived.sex + ggtitle("Survival by Sex") + geom_bar() + scale_fill_discrete(name = "Sex") + scale_x_discrete(name = "Survival", labels = c("0" = "Died", "1" = "Survived")) + scale_y_discrete(name = "Count", limits = seq(0, 550, 50))

graph.survived.class <- ggplot(data = train.data, aes(x = train.data$Survived, fill = train.data$Pclass))
graph.survived.class + ggtitle("Survival by Class") + theme(plot.title = element_text(lineheight = 0.8, face = "bold")) + geom_bar() + scale_fill_discrete(name = "Class") + scale_x_discrete(name = "Survival", labels = c("0" = "Died", "1" = "Survived")) + scale_y_discrete(name = "Count", limits = seq(0, 550, 100))

graph.survived.age <- ggplot(data = survived.agegroup, aes(x = survived.agegroup$AgeGroup, y = survived.agegroup$Count, fill = survived.agegroup$AgeGroup))
graph.survived.age + ggtitle("Survival by Age") + theme(plot.title = element_text(lineheight = 0.8, face = "bold")) + geom_bar(stat = "identity") + scale_fill_discrete(name = "Age Group") + scale_x_discrete(name = "Survival") + scale_y_discrete(name = "Count", limits = seq(0, 100, 10))

graph.died.age <- ggplot(data = died.agegroup, aes(x = died.agegroup$AgeGroup, y = died.agegroup$Count, fill = died.agegroup$AgeGroup))
graph.died.age + ggtitle("Survival by Age") + theme(plot.title = element_text(lineheight = 0.8, face = "bold")) + geom_bar(stat = "identity") + scale_fill_discrete(name = "Age Group") + scale_x_discrete(name = "Survival") + scale_y_discrete(name = "Count", limits = seq(0, 100, 10))

graph.survivors.age <- ggplot(data = survivors, aes(x = survivors$AgeGroup, y = survivors$Count, fill = survivors$AgeGroup))
graph.survivors.age + ggtitle("Survival by Age") + theme(plot.title = element_text(lineheight = 0.8, face = "bold")) + geom_bar(stat = "identity", alpha = 0.75, position = "dodge") + scale_fill_discrete(name = "Age Group") + scale_x_discrete(name = "Survival") + scale_y_discrete(name = "Count", limits = seq(0, 200, 10))
