#--------------------------------------
#install all necessary packages

my_packages <- c("tidyverse", "plotly", "readxl","dplyr","tidyr","writexl") # Specify your packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed)                               # Install not installed packages

lapply(my_packages, library, character.only = TRUE)                                     # Load all packages

#---------------------------------------
#Read in Dummy Data

Raw_Data <- read_excel(paste(fp, "LSOA 2022 Age-Sex Popuations.xlsx", sep=""))

#-----------------------------------
#Perform any data manipulation needed

# Exclude the total populations from the analysis
Raw_Data <- Raw_Data[, -which(names(Raw_Data) == "Total")]

# Don't pivot the LSOA and Local Authority names and codes
cols_to_exclude <- c("LAD 2021 Code","LAD 2021 Name","LSOA 2021 Code","LSOA 2021 Name")

# 'Unpivot' the data (i.e. make the data frame long and thin rather than short and wide)
Raw_Data2 <- Raw_Data %>%
  pivot_longer(
    cols = !matches(cols_to_exclude),
    names_to = c("Sex and Age"), # Separate column names
    values_to = c("Population")
  )

# Extract the first character from the 'Sex and Age' column and create a new Sex column using it
Raw_Data3 <- Raw_Data2 %>%
  mutate(Sex = substr(`Sex and Age`, 1, 1),
         `Sex and Age` = substr(`Sex and Age`, 2, nchar(`Sex and Age`))) %>%

# As what was the Sex and Age column now just contains age information, re-name it and also set it as a numeric data type
rename(Age = `Sex and Age`)

Raw_Data3$Age <- as.numeric(Raw_Data3$Age)

# Put the ages into 5-year age bands
Raw_Data3$Age <- case_when(
            Raw_Data3$Age >= 0 & Raw_Data3$Age <= 4 ~ "0-4",
            Raw_Data3$Age >= 5 & Raw_Data3$Age <= 9 ~ "5-9",
            Raw_Data3$Age >= 10 & Raw_Data3$Age <= 14 ~ "10-14",
            Raw_Data3$Age >= 15 & Raw_Data3$Age <= 19 ~ "15-19",
            Raw_Data3$Age >= 20 & Raw_Data3$Age <= 24 ~ "20-24",
            Raw_Data3$Age >= 25 & Raw_Data3$Age <= 29 ~ "25-29",
            Raw_Data3$Age >= 30 & Raw_Data3$Age <= 34 ~ "30-34",
            Raw_Data3$Age >= 35 & Raw_Data3$Age <= 39 ~ "35-39",
            Raw_Data3$Age >= 40 & Raw_Data3$Age <= 44 ~ "40-44",
            Raw_Data3$Age >= 45 & Raw_Data3$Age <= 49 ~ "45-49",
            Raw_Data3$Age >= 50 & Raw_Data3$Age <= 54 ~ "50-54",
            Raw_Data3$Age >= 55 & Raw_Data3$Age <= 59 ~ "55-59",
            Raw_Data3$Age >= 60 & Raw_Data3$Age <= 64 ~ "60-64",
            Raw_Data3$Age >= 65 & Raw_Data3$Age <= 69 ~ "65-69",
            Raw_Data3$Age >= 70 & Raw_Data3$Age <= 74 ~ "70-74",
            Raw_Data3$Age >= 75 & Raw_Data3$Age <= 79 ~ "75-79",
            Raw_Data3$Age >= 80 & Raw_Data3$Age <= 84 ~ "80-84",
            Raw_Data3$Age >= 85 & Raw_Data3$Age <= 89 ~ "85-89",
            Raw_Data3$Age >= 90 ~ "90+")

# Sum the populations into the 5-year age and sex bands
Raw_Data4 <- Raw_Data3 %>% group_by(`LAD 2021 Code`,`LAD 2021 Name`,`LSOA 2021 Code`, 
                                    `LSOA 2021 Name`,`Age`,`Sex`) %>%
                           summarize(Pop = sum(Population)) %>%
                           ungroup()

# Create separate outputs for males and females (needed as the data frame has too many rows to be exported into Excel directly)
Raw_Data4_Males <- Raw_Data4 %>% filter(Sex == "M")
Raw_Data4_Females <- Raw_Data4 %>% filter(Sex == "F")

# Create Excel files with the final outputs in (these can be copied into the same file as two tabs)
write_xlsx(Raw_Data4_Females,paste(fp,"LSOA 2022 Age-Sex Popuations Output Females.xlsx", sep=""))
write_xlsx(Raw_Data4_Males,paste(fp,"LSOA 2022 Age-Sex Popuations Output Males.xlsx", sep=""))
