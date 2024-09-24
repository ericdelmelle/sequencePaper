rm(list = ls())
# the packages
pkgs <- c("dplyr", "dtwclust", "sf", "ggplot2", "reshape2", "knitr", "kableExtra", "ggpubr", "tmap")
sapply(pkgs, library, character.only = TRUE, quietly = TRUE)
library(sf)
library(tidyverse)
library(cluster)
library(factoextra)
library(tidyverse)
library(tidycensus)

# Re-try with LTDB data. Need to read in each year
census20<- read.csv("ltdb_std_2020_fullcount.csv")
census10<- read.csv("LTDB_Std_2010_fullcount.csv")
census00<- read.csv("LTDB_Std_2000_fullcount.csv")
census90<- read.csv("LTDB_Std_1990_fullcount.csv")
census80<- read.csv("LTDB_Std_1980_fullcount.csv")
census70<- read.csv("LTDB_Std_1970_fullcount.csv")



##select the columns & calculate percents
#First 2020
census20 <- census20 %>% rename("TRTID10" = "TRTID2010")
census20$perwhite20 <- census20$nhwt20/census20$pop20
census20$perblack20 <- census20$nhblk20/census20$pop20
census20$perhisp20 <- census20$hisp20/census20$pop20
census20$perasian20 <- census20$asian20/census20$pop20
census20<- census20 %>% select(c("TRTID10","perwhite20", "perblack20", "perhisp20", "perasian20"))

#Now 2010
census10 <- census10 %>% rename("TRTID10" = "tractid")
census10$perwhite10 <- census10$nhwht10/census10$pop10
census10$perblack10 <- census10$nhblk10/census10$pop10
census10$perhisp10 <- census10$hisp10/census10$pop10
census10$perasian10 <- census10$asian10/census10$pop10
census10<- census10 %>% select(c("TRTID10","perwhite10", "perblack10", "perhisp10", "perasian10"))


#Now 2000, need to filter out where pop >0
census00 <- census00 %>% filter (POP00 >0)
census00$perwhite00 <- census00$NHWHT00/census00$POP00
census00$perblack00 <- census00$NHBLK00/census00$POP00
census00$perhisp00 <- census00$HISP00/census00$POP00
census00$perasian00 <- census00$ASIAN00/census00$POP00
census00<- census00 %>% select(c("TRTID10","perwhite00", "perblack00", "perhisp00", "perasian00"))

#1990 
census90 <- census90 %>% filter (POP90 >0)
census90$perwhite90 <- census90$NHWHT90/census90$POP90
census90$perblack90 <- census90$NHBLK90/census90$POP90
census90$perhisp90 <- census90$HISP90/census90$POP90
census90$perasian90 <- census90$ASIAN90/census90$POP90
census90<- census90 %>% select(c("TRTID10","state","county","perwhite90", "perblack90", "perhisp90", "perasian90"))

#Now 1980, need to filter out where pop >0
census80 <- census80 %>% filter (POP80 >0)
census80$perwhite80 <- census80$NHWHT80/census80$POP80
census80$perblack80 <- census80$NHBLK80/census80$POP80
census80$perhisp80 <- census80$HISP80/census80$POP80
census80$perasian80 <- census80$ASIAN80/census80$POP80
census80<- census80 %>% select(c("TRTID10","perwhite80", "perblack80", "perhisp80", "perasian80"))



census_all<- census90 %>% left_join(census00) %>% left_join(., census10) %>% left_join(., census20)%>% left_join(., census80)


#The five counties in NYC are:

#Bronx (FIPS: 36005)
#Kings (Brooklyn, FIPS: 36047)
#New York (Manhattan, FIPS: 36061)
#Queens (FIPS: 36081)
#Richmond (Staten Island, FIPS: 36085)
# Define the FIPS codes for the five NYC counties
nyc_counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond")

# Fetch the 2010 Census tract boundaries
nyc_tracts_2010 <- get_decennial(geography = "tract",
                           variables = "P005003",  
                           state = "NY", 
                           county = nyc_counties, 
                           year = 2010, 
                           geometry = TRUE)

# View the first few rows of the data
head(nyc_tracts_2010)


# the census tract data
tract <- st_read("Tract_2010Census_DP1.shp") 
tract<- tract %>% select("GEOID10")
tract<- rename(tract, TRTID10 = GEOID10)
tract$TRTID10<- as.double(tract$TRTID10)

#Select NYC Counties. These include Bronx County, Kings County (Brooklyn), New York County (Manhattan), Queens County, Richmond County (Staten Island)

census_select <- census_all %>% filter((state == "NY" & county == "Bronx County")|
                              (state == "NY" & county == "Kings County")|
                              (state == "NY" & county == "New York County")|
                              (state == "NY" & county == "Queens County")|
                              (state == "NY" & county == "Richmond County"))
                             

##remove NA values
census_nyc <- na.omit(census_select)%>% select(-state, -county)
census_nyc <- census_nyc %>% select(-state, -county)
#now we need to convert to a long format. We want one variable for each race, but each census tract should be listed as a row for each decade.
#this will give us all the possible racial mixtures for the entire time period.

# Convert the dataframe from wide to long format
census_long <- census_nyc %>%
  pivot_longer(cols = starts_with("per"), 
               names_to = c(".value", "year"), 
               names_pattern = "per(\\w+)(\\d{2})") %>%
  mutate(year = case_when(
    year == "80" ~ 1980,
    year == "90" ~ 1990,
    year == "00" ~ 2000,
    year == "10" ~ 2010,
    year == "20" ~ 2020,
    TRUE ~ as.integer(year)
  ))



##Now do the k-means clustering on all

data_for_clustering <- census_long %>%
  select(white, black, hisp, asian)

# Function to calculate total within-cluster sum of squares for different k
wss <- function(k) {
  kmeans(data_for_clustering, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10
wss_values <- map_dbl(k.values, wss)

# Elbow method plot
plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

# Silhouette method for determining the optimal number of clusters
fviz_nbclust(data_for_clustering, kmeans, method = "silhouette")

# Assume the optimal number of clusters (k) is 3 from the previous steps
set.seed(123)
kmeans_result <- kmeans(data_for_clustering, centers = 3, nstart = 25)

# Add the cluster assignments to the original data
census_long$cluster <- kmeans_result$cluster


# Visualize clusters using PCA for dimensionality reduction
fviz_cluster(kmeans_result, data = data_for_clustering, geom = "point",
             ellipse.type = "norm", show.clust.cent = TRUE,
             palette = "jco", ggtheme = theme_minimal())



# Silhouette Analysis
sil <- silhouette(kmeans_result$cluster, dist(data_for_clustering))
fviz_silhouette(sil)

# Average silhouette width
mean(sil[, "sil_width"])

# Cluster profiles
cluster_profiles <- census_long %>%
  group_by(cluster) %>%
  summarise(across(c(white, black, hisp, asian), mean))

print(cluster_profiles)


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Visualize clusters for each demographic variable
ggplot(census_long, aes(x = as.factor(cluster), y = white, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Distribution of White Population by Cluster", x = "Cluster", y = "White Population (%)") +
  theme_minimal()

ggplot(census_long, aes(x = as.factor(cluster), y = black, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Distribution of Black Population by Cluster", x = "Cluster", y = "Black Population (%)") +
  theme_minimal()

ggplot(census_long, aes(x = as.factor(cluster), y = hisp, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Distribution of Hispanic Population by Cluster", x = "Cluster", y = "Hispanic Population (%)") +
  theme_minimal()

ggplot(census_long, aes(x = as.factor(cluster), y = asian, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Distribution of Asian Population by Cluster", x = "Cluster", y = "Asian Population (%)") +
  theme_minimal()

# Pairwise scatter plots
pairs_plot_data <- census_long %>%
  select(white, black, hisp, asian, cluster)

pairs(pairs_plot_data, col = pairs_plot_data$cluster,
      main = "Pairwise Scatter Plots of Demographic Variables by Cluster")

# Create heatmap table of average values for each cluster

# Calculate the average values for each cluster
cluster_averages <- census_long %>%
  group_by(cluster) %>%
  summarise(
    white = mean(white, na.rm = TRUE),
    black = mean(black, na.rm = TRUE),
    hispanic = mean(hisp, na.rm = TRUE),
    asian = mean(asian, na.rm = TRUE)
  )

print(cluster_averages)

# Load necessary libraries
library(pheatmap)

# Convert the data to a matrix
cluster_matrix <- as.matrix(cluster_averages[,-1]) # Remove the cluster column

# Set row names to cluster numbers
rownames(cluster_matrix) <- cluster_averages$cluster


# Create the heatmap
pheatmap(cluster_matrix, 
         display_numbers = TRUE, 
         number_color = "black",
         fontsize_number = 12,  # Increase the font size of the labels
         color = colorRampPalette(c("blue", "white", "red"))(50), 
         cluster_rows = FALSE, 
         cluster_cols = FALSE,
         main = "Average Demographic Values by Cluster",
         legend = FALSE)

# Define the cluster labels
cluster_labels <- c(
  "1 Hispanic Dominant",
  "2 Asian Significant",
  "3 Black with Hispanic Presence",
  "4 White Dominant",
  "5 White with Asian and Hispanic Presence",
  "6 Black Dominant"
)
rownames(cluster_matrix) <- cluster_labels
names(cluster_labels) <- 1:6

#Now convert to wide format so we have a sequnce for each neighborhood
# Assuming census_long is your data frame
# Convert the data from long to wide format
census_wide <- census_long %>%
  select(TRTID10, year, cluster) %>%
  pivot_wider(names_from = year, values_from = cluster, names_prefix = "cluster_")

# Print the resulting wide-format data frame
print(census_wide)

census_wide <- census_wide %>%
  select(TRTID10, cluster_1980, cluster_1990, cluster_2000, cluster_2010, cluster_2020)

# Ensure the sequence columns are factors
census_wide <- census_wide %>%
  mutate(across(starts_with("cluster_"), as.factor))

# Define the sequences
sequence_data <- seqdef(census_wide[, -1])  # Exclude the TRTID10 column

# Compute the OM distances
costs <- seqcost(sequence_data, method = "TRATE")
om_distances <- seqdist(sequence_data, method = "OMstran", indel = 1, sm = costs$sm, otto = 1)

# Determine the optimal number of clusters using the silhouette method
fviz_nbclust(as.dist(om_distances), FUNcluster = pam, method = "silhouette")

# Adjust the plotting parameters
op <- par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust margins as needed

# Choose the number of clusters (e.g., 4)
num_clusters <- 15
clusters <- cutree(hc, k = num_clusters)

# Add the cluster assignments to the original data
census_wide$sequence_cluster <- clusters

png("sequences.png", width = 600, height = 900)

# Plot the sequences for each cluster
seqIplot(sequence_data, group = census_wide$sequence_cluster, sortv = "from.start")
dev.off()

