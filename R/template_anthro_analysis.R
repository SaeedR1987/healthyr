

rm(list = ls())

library(healthyr)

df <- proc_anthro1
head(df)

report <- create_anthro_quality_report(df, grouping = "enum")
report

df2 <- raw_fsl1

# TEMPLATE ANALYSIS DOCUMENT

# Setup ####

library(tidyverse)
library(healthyr)

# Step 1: Load your Dataset ####

df <- raw_anthro1

# Step 2: Format Your Dataset ####

df2 <- format_nut_health_indicators(df = df,
                                    hhid = "KEY", 
                                    date_of_dc = "today", 
                                    cluster = "cluster_id", 
                                    enum = "enum",
                                    sex_var = "child_sex", 
                                    age_months_var = "age", 
                                    date_of_birth = "birthdate", 
                                    weight_var = "weight", 
                                    height_var = "height", 
                                    muac_var = "muac", 
                                    oedema_var = "edema",
                                    )

# Step 3: Create a Quality Summary Report ####



# Step 4: Evaluate Data with Visualizations ####


# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####



# Step 6: Analyse Survey Results ####








