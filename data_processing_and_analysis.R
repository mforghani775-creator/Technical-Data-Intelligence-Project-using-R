# ----------------------------------------------------------
# Technical Challenge – ELC Data – IS
# Author: Maryam Forghaniallahabadi
# Purpose: Load, clean, and join ELC quality, funded,
#          deprivation, and rurality data for analysis
# ----------------------------------------------------------

# These Library needed for the run 
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(readr)  

# 0.Load raw data files 
quality_raw <- read_csv("Dataset 1 - CareInspecorateSettingGrades.csv")
funded_raw  <- read_csv("Dataset 2 - Funded ELC settings list with locations.csv")
dz_raw      <- read_csv("Dataset 3 - DataZone Rurality and Deprivation Category Lookups.csv")
ur8_raw     <- read_csv("Dataset 4.csv") 

# 1.Checking the names and the structure of Data
names(quality_raw)
str(quality_raw)

names(funded_raw)
str(funded_raw)

names(dz_raw)
str(dz_raw)

names(ur8_raw)
str(ur8_raw)

# 2. Standardize column names
quality <- quality_raw %>% clean_names()
funded  <- funded_raw %>% clean_names()
dz      <- dz_raw %>% clean_names()
ur8     <- ur8_raw %>% clean_names()

# 2.1. Check the names after Standardize
names(quality)
names(funded)
names(dz)
names(ur8)

# 3. Clean Dataset 1 — Inspection Quality ~~~ & assigning correct variables as below
#~~	as.factor: specifies categorical non-ordinal covariates (explanatory variables)
#~~	as.ordered: specifies categorical ordered (ordinal) covariates
#~~	as.numeric: specifies continuous or integer interval covariates

quality <- quality %>% 
  mutate(
    cs_number = as.character(cs_number),
    
    min_grade = as.numeric(min_grade),
    max_grade = as.numeric(max_grade),
    kq_setting = as.numeric(kq_setting),
    kq_staff_team = as.numeric(kq_staff_team),
    kq_leadership = as.numeric(kq_leadership),
    kq_care_play_and_learning = as.numeric(kq_care_play_and_learning),
    registered_places = as.numeric(registered_places),
    
    rad_sat_score = factor(
      rad_sat_score,
      levels = c("LOW","MEDIUM","HIGH"),
      ordered = TRUE),
    
    grade_spread = as.factor(grade_spread),
    
    service_type <- as.factor(service_type),
    care_service <- as.factor(care_service),
    
    date_reg = dmy(date_reg),
    publication_of_latest_grading = dmy(publication_of_latest_grading))

# 3.1. Confirm successful conversion
str(quality)

# 4. cleaning dataset 2

funded <- funded %>% 
  mutate(
    cs_number = as.character(cs_number),
    data_zone = as.character(data_zone),
    local_authority = as.factor(local_authority),
    lat = as.numeric(lat),
    lon = as.numeric(lon))

# 4.1. Confirm successful conversion
str(funded)

# 5. cleaning dataset 3
dz <- dz %>% 
  mutate(
    data_zone = as.character(data_zone),
    ur8_code = factor(ur8_code,
                  levels = 1:8,
                  ordered = TRUE),
    simd2020v2_decile = factor(
      as.numeric(simd2020v2_decile),
      levels = 1:10,
      ordered = TRUE))  # ← important field

# 5.1. Confirm successful conversion
str(dz)

# 6. cleaning dataset 4
ur8 <- ur8 %>%
  mutate(
    ur8_code = factor(ur8_code,
                      levels = 1:8,
                      ordered = TRUE),
    ur8_name = as.factor(ur8_name))

str(ur8)


#...~~~important checking the dublicates

# 7. Check duplicates before join
sum(duplicated(quality$cs_number))      # from dataset 1
sum(duplicated(funded$cs_number))       # from dataset 2
sum(duplicated(dz$data_zone))           # from dataset 3
sum(duplicated(ur8$ur8_code))           # from dataset 4

quality %>% filter(duplicated(cs_number))
funded  %>% filter(duplicated(cs_number))
dz      %>% filter(duplicated(data_zone))
ur8     %>% filter(duplicated(ur8_code))


# 8. Multi-stage joins to build master analytical dataset
# 8.1. Keep only funded + inspected settings
elc_joined <- funded %>% 
  inner_join(quality, by = "cs_number")   

# 8.2. keeps all funded grade records, adds SIMD + UR8
elc_joined <- elc_joined %>%
  left_join(dz, by = "data_zone")   


# 8.3. Add UR8 category names (Dataset 4)
elc_joined <- elc_joined %>%
  left_join(ur8, by = "ur8_code")

# 8.4 check structure
str(elc_joined)

# 8.5. preview first 10 rows
head(elc_joined, 10)

# 8.6. check the numbers, joined and funded should be the same
nrow(quality)
nrow(funded)
nrow(elc_joined)  

# 8.7. Remove junk column 
elc_joined <- elc_joined %>%
  select(-starts_with("x1"))

head(elc_joined, 10)

#~~~~~~~~~~~............~~~~~~~~~~~~~~~................~~~~~~~~~~~~~~~~~~~~~~~~
# 9. Categorise quality into 3 performance bands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
elc_joined <- elc_joined %>%
  mutate(
    quality_band = case_when(
      max_grade >= 5               ~ "High (5–6)",
      max_grade >= 3 & max_grade < 5 ~ "Medium (3–4)",
      max_grade < 3                ~ "Low (1–2)",
      TRUE ~ NA_character_),
    quality_band = factor(
      quality_band,
      levels = c("Low (1–2)", "Medium (3–4)", "High (5–6)"),
      ordered = TRUE))


write.csv(elc_joined, "elc_joined.csv", row.names = FALSE)

# 10. Summary statistics + initial look
# 10.1. SIMD Distribution
table(elc_joined$simd2020v2_decile, useNA="ifany")
# 10.2. Urban–Rural Classification Distribution
table(elc_joined$ur8_name, useNA="ifany")
# 10.3. Summary of quality dataset (raw inspection grades)
summary(select(quality, min_grade, max_grade, rad_sat_score))
# 10.4. Summary of joined dataset (after merging everything)
summary(select(elc_joined, max_grade, quality_band, simd2020v2_decile, ur8_name))


#------------- Overall distribution of grades and bands -----------------------
# 11. DISTRIBUTION ANALYSIS — Quality band summaries, LA patterns

quality_overall <- elc_joined %>%
  summarise(
    n_settings   = n(),
    mean_grade   = mean(max_grade, na.rm = TRUE),
    median_grade = median(max_grade, na.rm = TRUE),
    min_grade    = min(max_grade, na.rm = TRUE),
    max_grade    = max(max_grade, na.rm = TRUE)
  )

quality_overall

# 11.1 Count & % within each quality category
quality_band_summary <- elc_joined %>%
  filter(!is.na(quality_band)) %>%
  count(quality_band) %>%
  mutate(pct = 100 * n / sum(n))

quality_band_summary

prop.table(table(elc_joined$quality_band)) * 100


#------------- Quality by Local Authority ------------------------------------------

la_quality <- elc_joined %>%
  filter(!is.na(max_grade)) %>%
  group_by(local_authority) %>%
  summarise(
    n_settings = n(),
    mean_grade = mean(max_grade, na.rm = TRUE),
    pct_high   = mean(quality_band == "High (5–6)",   na.rm = TRUE) * 100,
    pct_medium = mean(quality_band == "Medium (3–4)", na.rm = TRUE) * 100,
    pct_low    = mean(quality_band == "Low (1–2)",    na.rm = TRUE) * 100
  ) %>%
  arrange(desc(mean_grade))

# Top 5 and bottom 5 LAs by mean grade
la_top5    <- la_quality %>% slice_head(n = 5)
la_bottom5 <- la_quality %>% slice_tail(n = 5)

la_top5
la_bottom5


#------------- Quality by SIMD Decile (deprivation gradient as 1 = most deprived, 10 = least) --------------

simd_quality <- elc_joined %>%
  filter(!is.na(simd2020v2_decile), !is.na(quality_band)) %>%
  group_by(simd2020v2_decile, quality_band) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(simd2020v2_decile) %>%
  mutate(pct = 100 * n / sum(n))

simd_quality


ggplot(simd_quality,
       aes(x = simd2020v2_decile, y = pct, fill = quality_band)) +
  geom_col(position = "fill") +
  labs(x = "SIMD decile (1 = most deprived, 10 = least)",
       y = "Proportion of settings",
       fill = "Quality band",
       title = "Quality of funded ELC settings by deprivation level")


#------------- Quality by Urban–Rural classification (UR8) -------------------------

ur8_quality <- elc_joined %>%
  filter(!is.na(ur8_name), !is.na(quality_band)) %>%
  group_by(ur8_name, quality_band) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ur8_name) %>%
  mutate(pct = 100 * n / sum(n))

ur8_quality


ggplot(ur8_quality,
       aes(x = ur8_name, y = pct, fill = quality_band)) +
  geom_col(position = "fill") +
  coord_flip() +
  labs(x = "Urban–rural category",
       y = "Proportion of settings",
       fill = "Quality band",
       title = "Quality of funded ELC settings across the urban–rural spectrum")


#------------- Outliers — focus on low-quality settings -------------------------------------

low_quality_settings <- elc_joined %>%
  filter(quality_band == "Low (1–2)") %>%
  select(local_authority, service_name, service_type,
         max_grade, quality_band, simd2020v2_decile, ur8_name)

low_quality_settings


#------------- Capacity effect — do larger settings score higher? -----------------------------

size_quality <- elc_joined %>%
  filter(!is.na(registered_places)) %>%
  group_by(quality_band) %>%
  summarise(
    median_places = median(registered_places),
    mean_places   = mean(registered_places),
    n = n())

size_quality


# 12. GRAPHICS — High-value visual summaries
# 12.1. Overall Quality Distribution
ggplot(elc_joined, aes(x = quality_band, fill = quality_band)) +
  geom_bar() +
  labs(title = "Overall Quality Distribution of Funded ELC Settings",
       x = "Quality Band",
       y = "Number of Settings") +
  scale_fill_brewer(palette="Set2")


# 12.2. Quality by Local Authority — Which LAs perform best?
ggplot(la_quality, aes(x = reorder(local_authority, mean_grade), y = mean_grade)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title="Mean Quality Grade by Local Authority",
       x="Local Authority", y="Average Grade (1–6)")


# 12.3. Heatmap of deprivation × rurality × quality
heat <- elc_joined %>%
  filter(!is.na(simd2020v2_decile), !is.na(ur8_name)) %>%
  group_by(simd2020v2_decile, ur8_name) %>%
  summarise(avg_grade = mean(max_grade, na.rm=TRUE), .groups="drop")

ggplot(heat, aes(x=simd2020v2_decile, y=ur8_name, fill=avg_grade)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title="Quality by Deprivation and Urban–Rural Classification",
       x="SIMD Decile (1 = most deprived)",
       y="Urban–Rural Category",
       fill="Mean Grade")


# 12.4. Scatter — Does setting size affect quality?
#first check how many missing values
sum(is.na(elc_joined$max_grade))
sum(is.na(elc_joined$registered_places))

elc_plot <- elc_joined %>%
  filter(!is.na(registered_places), !is.na(max_grade))

ggplot(elc_plot, aes(x = registered_places, y = max_grade, color = quality_band)) +
  geom_jitter(alpha=0.5) +
  geom_smooth(method="loess", se=FALSE, color="black") +
  labs(title="Setting Size vs Quality Grades",
       x="Registered Places (Capacity)",
       y="Inspection Grade (1–6)")

#Look at a few of missing values
elc_joined %>%
  filter(is.na(max_grade)) %>%
  select(local_authority, service_name, quality_band) %>%
  head()


# 12.5. Trends over Time ,Has quality improved/worsened over years?
elc_joined <- elc_joined %>%
  mutate(year = year(publication_of_latest_grading))

year_quality <- elc_joined %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(
    mean_grade = mean(max_grade, na.rm = TRUE),
    n = n()
  )

ggplot(year_quality, aes(x = year, y = mean_grade)) +
  geom_line(size=1.2, color="darkblue") +
  geom_point(size=2) +
  labs(title="Trend in ELC Inspection Grades Over Time",
       x="Year", y="Average Grade")


# 12.6. Providers comparison — Public vs Private vs LA run
provider_quality <- elc_joined %>%
  group_by(service_type) %>%
  summarise(
    mean_grade = mean(max_grade, na.rm=TRUE),
    pct_high = mean(quality_band=="High (5–6)")*100,
    n=n()
  ) %>% arrange(desc(mean_grade))

ggplot(provider_quality, aes(x=reorder(service_type, mean_grade), y=mean_grade)) +
  geom_col(fill="forestgreen") +
  coord_flip() +
  labs(title="Average Quality by Provider Type",
       x="Provider Type", y="Mean Grade")


# 12.7. Are smaller settings more likely to be high-quality?
ggplot(elc_joined, aes(x=registered_places, fill=quality_band)) +
  geom_density(alpha=0.4) +
  labs(title="Distribution of Setting Size Across Quality Bands",
       x="Capacity", y="Density")


###~~~~~~~~STATISTICAL MODELLING: A regression model to check the effect~~~~~~~~~~~~~~~
#Which factor best predicts quality? SIMD? Rurality? Size?
# ANOVA p-value tells if quality varies by SIMD

model_simd <- aov(max_grade ~ simd2020v2_decile, data = elc_joined)
summary(model_simd)   


model_ur8 <- aov(max_grade ~ ur8_name, data = elc_joined)
summary(model_ur8)

#proper p value
options(scipen=999)

model_size <- lm(max_grade ~ registered_places, data = elc_plot)
summary(model_size)

#I tested whether setting size predicts inspection quality using linear regression. 
#The relationship was statistically significant (p = 0.043), however the R² value (0.0016) indicates that capacity explains less than 1% of the variation in quality — meaning size has very little practical influence.
#This reinforces a key finding: ELC quality in Scotland is broadly consistent and not strongly driven by setting size.
