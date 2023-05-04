library(dplyr)
library(ggplot2)

#### Data Analysis####

# Import the dataset
df_cs<-read.csv("All_Chargers.csv")

### Data cleaning
# Select only those stations with review and inside the Netherlands
df_cs<- df_cs[df_cs$Ratings.Total>0,]
df_cs<- df_cs[df_cs$Country=="Netherlands" ,]

summary(df_cs)
unique(df_cs$Name) #74 different types of stations
# Provinces:
unique(df_cs$Admin.Area.Level.1) # It says there are 15, however in the Netherlands there are only 12 provinces

# replace "North" with "Noord" and " " with "-" in the Admin.Area.Level.1 column
df_cs$Admin.Area.Level.1 <- gsub("North", "Noord", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub("South", "Zuid", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub(" ", "-", df_cs$Admin.Area.Level.1)

unique(df_cs$Admin.Area.Level.1) # 12 provinces in Netherlands

df_cs_loc<-df_cs%>%
  group_by(Admin.Area.Level.1)%>%
  summarise(obs = n(), max = max(Rating), mean = mean(Rating) ,min = min(Rating))


# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = Admin.Area.Level.1, y = Rating)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Distribution of Reviews by Location",
       x = "Location",
       y = "Review") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(geosphere)

# function to calculate distance between two coordinates using geosphere package
calc_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))/1000000# convert to km
  return(dist)
}

# function to find 3 nearest charging stations based on distance
find_3_nearest <- function(lat, lon, df_cs) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted, n=3))
}

# function to compute average rating of the 3 nearest charging stations
avg_rating_3_nearest <- function(lat, lon, df) {
  nearest_stations <- find_3_nearest(lat, lon, df)
  avg_rating <- mean(nearest_stations$Rating)
  return(avg_rating)
}

# test the find_3_nearest function for Station1
find_3_nearest(df_cs$Latitude[1], df_cs$Longitude[1], df_cs)
# test the avg_rating_3_nearest function for Station1
avg_rating_3_nearest(df_cs$Latitude[1], df_cs$Longitude[1], df_cs)

avg3 <- c()
for (i in 1:nrow(df_cs)){
  # test the avg_rating_3_nearest function for Station1
  avg3[i]<-avg_rating_3_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs)
}


#### Find the closest, save distance####
closest <- function(lat, lon, df_cs) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted$Distance, n=1))
}

cls<-c()
for (i in 1:nrow(df_cs)){
  # avg_rating_3_nearest function for Station1
  cls[i]<-closest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs)
}

#### Calculate how many stations are under 1000m, 500m and 200m

# Import the dataset
df_all<-read.csv("All_Chargers.csv")
df_all<-df_all[df_all$Country=="Netherlands" ,]

# initialize empty vector to store counts
counts_1000 <- c()
counts_500 <- c()
counts_250 <- c()
# loop over each row in the dataframe

for (i in 1:nrow(df_all)) {
  # check if the row has a rating
  if ((df_all$Ratings.Total[i] > 0)) {
    # calculate distance between this row and all other rows
    distances <- apply(df_all[, c("Latitude", "Longitude")], 1, 
                       function(x) calc_distance(df_all$Latitude[i], df_all$Longitude[i], x[1], x[2]))
    # count how many distances are less than 1000m
    count_1000 <- length(df_all[distances < 1000,])
    # count how many distances are less than 500m
    count_500 <- length(df_all[distances < 500,])
    # count how many distances are less than 250m
    count_250 <- length(df_all[distances < 250,])
    # add count to vector
    counts_1000<- c(counts_1000, count_1000)
    # add count to vector
    counts_500<- c(counts_500, count_500)
    # add count to vector
    counts_250<- c(counts_250, count_250)
  }
}


