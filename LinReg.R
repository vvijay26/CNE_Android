#Clear the Environment - To avoid any testing issues.
rm(list = ls())
# install.packages("ggplot2")
# install.packages("sqldf")
# 
library("readxl")
library("ggplot2")
library("sqldf")
#Set the working dir (This will vary depending on which system/directory the files are present)
setwd('D:/Project')
#EDA
cne <- read_xlsx("cne.xlsx")
summary(factor(cne$cat_key))

cne$short_desc_length = nchar(cne$short_desc)

library("MASS")
library("car")
set.seed(100)

colnames(cne)[colnames(cne)=="Total Ratings"] <- "Total_ratings"
cne$iap_min <- ifelse(cne$iap_min==NULL,0,cne$iap_min)
cne$iap_min[is.na(cne$iap_min)] <- 0

# Create some plots
plot_grid(ggplot(cne, aes(Total_ratings,short_desc_length)) + geom_point(colour = "red", size = 1),
ggplot(cne, aes(Total_ratings,cat_keys__002)) + geom_point(colour = "red", size = 1),
ggplot(cne, aes(Total_ratings,cat_keys__001)) + geom_point(colour = "red", size = 1),
ggplot(cne, aes(Total_ratings,iap_min)) + geom_point(colour = "red", size = 1),
          align = "h")

# plot_grid(ggplot(cne, aes(x=short_desc_length,fill=Total_ratings))+ geom_bar(), 
#          ggplot(cne, aes(x=cat_keys__002,fill=Total_ratings))+ geom_bar(),
#          ggplot(cne, aes(x=cat_keys__001,fill=Total_ratings))+ geom_bar()+theme(axis.text.x = element_text(angle = 90)),
#          align = "h")

# Lets scale the variables
cne$short_desc_length[is.na(cne$short_desc_length)] <- 0
cne$short_desc_length <- scale(cne$short_desc_length)
cne$iap_min <- scale(cne$iap_min)
cne$Total_ratings <- scale(cne$Total_ratings)
cne$number_ratings <- scale(cne$number_ratings)

cne$cat_keys__001 <- as.factor(cne$cat_keys__001)
summary(factor(cne$cat_keys__001))
dummy_cne_cat_keys__001 <- data.frame(model.matrix( ~cat_keys__001, data = cne)) #Converting "cat_keys__001" into dummies . 
dummy_cne_cat_keys__001 <- dummy_cne_cat_keys__001[,-1] #This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "cat_keys__001". 
cne$cat_keys__001 <- NULL
cne <- cbind(cne, dummy_cne_cat_keys__001)

cne$cat_keys__002 <- as.factor(cne$cat_keys__002)
levels(cne$cat_keys__002)<-c(1,0) 
cne$cat_keys__002 <- as.numeric(levels(cne$cat_keys__002))[cne$cat_keys__002]



trainindices= sample(1:nrow(cne), 0.7*nrow(cne))
train = cne[trainindices,]
test = cne[-trainindices,]

model_1 <- lm(formula=Total_ratings ~ short_desc_length + iap_min  + cat_keys__002 + cat_keys__001EDUCATION +
	cat_keys__001ENTERTAINMENT+
	cat_keys__001GAME_ACTION+
	cat_keys__001GAME_ADVENTURE+
	cat_keys__001GAME_ARCADE+
	cat_keys__001GAME_CASUAL+
	cat_keys__001GAME_RACING+
	cat_keys__001GAME_SIMULATION+
	cat_keys__001GAME_SPORTS+
	cat_keys__001GAME_STRATEGY+
	cat_keys__001GAME_TRIVIA+
	cat_keys__001MAPS_AND_NAVIGATION+
	cat_keys__001MUSIC_AND_AUDIO+
	cat_keys__001NEWS_AND_MAGAZINES+
	cat_keys__001PERSONALIZATION+
	cat_keys__001PHOTOGRAPHY+
	cat_keys__001PRODUCTIVITY+
	cat_keys__001SHOPPING+
	cat_keys__001SOCIAL+
	cat_keys__001TOOLS+
	cat_keys__001TRAVEL_AND_LOCAL+
	cat_keys__001VIDEO_PLAYERS,data=train)

summary(model_1)

s <- predict(model_1,test)

cor(s,test$Total_ratings)^2 



