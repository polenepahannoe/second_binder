library(tidyverse)
library(Hmisc)
library(ggridges)
library(NHANES)

#The plot
mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  #allows to plot and set the axes
  #+ sign is used as equivalent of %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  #allows to choose the way of representation of data. In this case - point
  geom_point() +
  #allows to adjust the position of the text along x-axis:
  #vjust adjusts how vertically the text is
  #hjust adjusts how horizontally the text is
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#The plot with readable texts
mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  #allows to jitter the points so that they would not stack vertically
  #width is responsible for how far points are from each other
  #alpha is responsible for paleness of the points
  #size is responsible for the size of the points
  geom_jitter(width = .2, alpha = 0.75, size = 2) +
  #makes the graph full white
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  #changes the size of ALL text in the graph
  theme(text = element_text(size = 15)) +
  #labels axes
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (mpg)")

#The Summarised Plot
mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  #allows to summarize data and visualise the summary
  #mean_cl_boot allows to implement non-parametric bootstrap for obtaining 
  #confidence limits for the population mean without assuming normality
  #fun.data summarises y at each x, mean_cl_boot determines the algorithm of summary
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = .5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (mpg)")

#The Finished Plot
mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  #fct_reorder allows to order the manufacturers by the mean cty
  ggplot(aes(x = fct_reorder(manufacturer, .fun = mean, cty), y = cty, colour =
               manufacturer)) +
  stat_summary(fun.data = mean_cl_boot, size = 1.25) +
  geom_jitter(width = 0.1, alpha = .65) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Manufacturer by City Fuel Economy",
       x = "Manufacturer",
       y = "City Fuel Economy (mpg)") +
  #allows to get rid of guides about the colour
  guides(colour = 'none') +
  coord_flip()

#facet_wrap
#cty may vary as a function of the type of vehicle and number size of engine
#facet_wrap allows to build a separate visualisation for each level of the factor
#we are facetting over ignoring SUVs
mpg %>%
  filter(class != "suv") %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = cty, colour = class)) +
  #stat_summary(fun.data = mean_cl_boot, size = 0.25) +
  geom_jitter(width = .5) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Engine Displacement",
       x = "Engine Displacement (litres)",
       y = "City Fuel Economy (mpg)") +
  guides(colour = "none") +
  facet_wrap(~class)

#Scatterplots
mpg %>%
  mutate(class = str_to_upper(class)) %>%
  ggplot(aes(x = cty, y = displ)) +
  geom_point(aes(colour = class)) +
  #adds layer corresponding to fitting a curve to data
  geom_smooth(se = FALSE) +
  theme(text = element_text(size = 13)) +
  theme_minimal() +
  labs(x = "City Fuel Economy (mpg)",
       y = "Engine Displacement (litres)",
       colour = "Vehicle Class")

#Histograms
#histogram of engine sizes (litres; displ) to undestand
#how this variable is distributed
mpg %>%
  ggplot(aes(x = displ)) +
  geom_histogram(binwidth = .5, fill = 'gray') +
  labs(title = "Histogram of Engine Displacement",
       x = "Engine Displacement (litres)",
       y = "Count")
  
#ggridges package
#allows to compare the distributions of engine size
#separated by vehicle class
#the variability among classes becomes visible
mpg %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = fct_reorder(class, .fun = mean, displ))) +
  #allows to plot the density of distribution
  geom_density_ridges(height = .5, aes(fill = class)) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  guides(fill = 'none') +
  labs(x = "Engine Displacement (litres)",
       y = NULL)


ncol(NHANES)
nrow(NHANES)

#tells the number of unique individuals in the dataset
NHANES %>%
  select(ID) %>%
  n_distinct

#clears the data and creates new tidied dataset
NHANES_tidied <- NHANES %>%
  distinct(ID, .keep_all = TRUE)

ncol(NHANES_tidied)
nrow(NHANES_tidied)

#plots the histogram in which each bar represents
#the BMI and the amount of people who have such BMI
NHANES_tidied %>%
  ggplot(aes(x = BMI)) +
  geom_histogram(bins = 100, na.rm = TRUE)

#groups by education and summarises medians
NHANES_tidied %>%
  group_by(Education) %>%
  summarise(median = median(BMI, na.rm = TRUE))

#PLOTTING the Violin Graph
NHANES_tidied %>%
  #filter cases where Education is not missing AND
  #BMI is not missing
  filter(!is.na(Education) & !is.na(BMI)) %>%
  #filtered data passed to ggplot and has no missing data
  ggplot(aes(x = Education, y = BMI, colour = Education)) +
  #caputures the shape of the distribution for each level of
  #education
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  #creates a boxplot for each level of our Education factor
  geom_boxplot(alpha = .5) +
  guides(colour = 'none') +
  labs(title = "Examining the effect of education level on BMI",
       x = "Education level",
       y = "BMI")

#PLOTTING INTERACTION EFFECTS
#to see how Diabetes might interact with Education
#and to capture the nature of the interaction
#expression Education:Diabetes is used when
#the x-axis aesthetic is specified
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Diabetes)) %>%
  ggplot(aes(x =Education:Diabetes, y = BMI, colour = Education)) +
  geom_violin() +
  geom_boxplot(alpha = .5) +
  guides(colour = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(title = "Examining the effect of education level and diabetes on BMI",
       x = "Education level X Diabetes",
       y = "BMI")

#HISTOGRAMS WITH FACET_WRAP()
#to plot this data separately for each education level
#facet_wrap() is used
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>%
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram() +
  guides(fill = 'none') +
  labs(title = "Examining the effect of education level on BMI",
       x = "BMI",
       y = "Number of cases") +
  #scales = 'free' function allows to make distinct
  #y-axes for each of the plot
  facet_wrap(~Education, scales = 'free')

NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>%
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..)) +
  guides(fill = "none") +
  labs(title = "Examining the effect of education level on BMI",
       x = "BMI",
       y = "Density") +
  facet_wrap(~Education)


#Examining the number of babies as a function of Education
NHANES_tidied %>%
  filter(Gender == "female" & !is.na(nBabies) & !is.na(Education)) %>%
  group_by(Education) %>%
  ggplot(aes(x = Education, y = nBabies, colour = Education)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  guides(colour = 'none') +
  labs(title = "Examining the effect of education on number of babies",
       x = "Education",
       y = "Number of babies")



NHANES_tidied %>%
  filter(!is.na(HHIncomeMid) & !is.na(nBabies)) %>%
  ggplot(aes(x = HHIncomeMid, y = nBabies)) +
  geom_point() +
  geom_jitter(alpha = .2, width = 2) +
  geom_smooth(se = FALSE) +
  theme(text = element_text(size = 13)) +
  theme_minimal() +
  labs(x = "Household income",
       y = "Number of Babies")

NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(Diabetes) & !is.na(nBabies)) %>%
  ggplot(aes(x = Education:Diabetes, y = nBabies, colour = Education)) +
  geom_violin() +
  geom_boxplot(alpha = .5, width = .2) +
  guides(colour = 'none') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5)) +
  labs(title = "Examining the effect of education and diabetes on number of babies",
       x = 'Education x Diabetes',
       y = 'Number of Babies')


NHANES_tidied %>%
  filter(!is.na(HHIncomeMid) & !is.na(HomeRooms)) %>%
  ggplot(aes(x = HHIncomeMid, y = HomeRooms)) +
  geom_point(aes(colour = Education)) +
  geom_smooth(se = FALSE) +
  theme(text = element_text(size = 13)) +
  labs(x = "Household Income",
       y = "Number of rooms in house",
       colour = "Education level")
  

#Scatterplots
mpg %>%
  mutate(class = str_to_upper(class)) %>%
  ggplot(aes(x = cty, y = displ)) +
  geom_point(aes(colour = class)) +
  #adds layer corresponding to fitting a curve to data
  geom_smooth(se = FALSE) +
  theme(text = element_text(size = 13)) +
  theme_minimal() +
  labs(x = "City Fuel Economy (mpg)",
       y = "Engine Displacement (litres)",
       colour = "Vehicle Class")

NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(HHIncomeMid)) %>%
  group_by(Education) %>%
  ggplot(aes(x = HHIncomeMid, fill = Education)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..)) +
  guides(fill = 'none') +
  labs(title = "Examining the effect of education level on Household income",
       x = "Household income",
       y = "Density") +
  #scales = 'free' function allows to make distinct
  #y-axes for each of the plot
  facet_wrap(~Education, scales = 'free') 
  

#Examining the effect of general health on sleep
NHANES_tidied %>% 
  filter(!is.na(HealthGen) & !is.na(SleepHrsNight)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = SleepHrsNight, fill = HealthGen)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..)) +
  guides(fill = 'none') + 
  labs( title = "Examining the effect of general health on sleep", 
        x = "Number of Hours of Sleep per Night", 
        y = "Density") + 
  facet_wrap(~ HealthGen)
