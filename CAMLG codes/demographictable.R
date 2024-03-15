# Load dplyr for data manipulation
library(dplyr)
library(table1)

# Read data
msaAll <- read.csv("msa.csv")

# Compute demographic statistics first
Total_Participants <- nrow(msaAll)
Male <- sum(msaAll$Sex == 'Male')
Female <- sum(msaAll$Sex == 'Female')
Percent_Male <- (Male / Total_Participants) * 100
Percent_Female <- (Female / Total_Participants) * 100
Avg_Age <- mean(msaAll$Age)
SD_Age <- sd(msaAll$Age)
Avg_Age_Onset <- mean(na.omit(msaAll$PDCAgeOnset))
SD_Age_Onset <- sd(na.omit(msaAll$PDCAgeOnset))

# Create the summary data frame
demographics_summary <- data.frame(
  Total_Participants,
  Male,
  Female,
  Percent_Male,
  Percent_Female,
  Avg_Age,
  SD_Age,
  Avg_Age_Onset,
  SD_Age_Onset
)

# Save the summary data frame to a CSV file
write.csv(demographics_summary, "demographics_summary.csv")

# PC vs NC
# Create a demographics table
demographics_table <- msaAll %>%
  group_by(SheetDx) %>%
  summarise(
    N = n(),
    Avg_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Female = sum(Sex == "Female", na.rm = TRUE),
    Male = sum(Sex == "Male", na.rm = TRUE)
  )

# Create a demographics table for NC vs PD diagnostics
tbl_NCvsPD <- table1(~ Age + Sex | SheetDx, data = msaAll)

# Save as a CSV
write.csv(tbl_NCvsPD, "table1_NCvsPD.csv")

# Print out the table
print(tbl_NCvsPD)



library(dplyr)
library(table1)

# Read data
msaAll <- read.csv("msa.csv")

# Remove rows with NA in ConsensusDx
msaAll <- msaAll[!is.na(msaAll$ConsensusDx), ]

# Create a demographics table for within-PD diagnostics
demographics_table_within_PD <- msaAll %>%
  group_by(ConsensusDx) %>%
  summarise(
    N = n(),
    Avg_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Avg_Age_Onset = mean(PDCAgeOnset, na.rm = TRUE),
    Median_Age_Onset = median(PDCAgeOnset, na.rm = TRUE),
    Female = sum(Sex == "Female", na.rm = TRUE),
    Male = sum(Sex == "Male", na.rm = TRUE)
  )

# Print out the table
print(demographics_table_within_PD)

# Create a demographics table for within-PD diagnostics including Race and Age of Onset
tbl_within_PD <- table1(~ Age + Age + Sex | ConsensusDx, data = msaAll)

# Save the table as a CSV file
write.csv(tbl_within_PD, "table1_within_PD_with_race_and_onset.csv")

# Save the table as a PNG image including Race and Age of Onset
png("table1_within_PD_with_race_and_onset.png", width = 800, height = 600)
print(tbl_within_PD)
dev.off()
