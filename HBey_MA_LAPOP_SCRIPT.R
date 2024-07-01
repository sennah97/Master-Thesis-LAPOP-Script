



#############################################
################ Hannes Bey #################
######## Master Thesis Script LAPOP #########
#############################################

## load packages for LCA
library(tidyverse)
library(stargazer)
library(haven)
library(texreg)
library(systemfit)
library(car)   
library(mfx)
library(sjlabelled)
library(nnet)
library(psych)
library(ggplot2)
library(mitools)
library(aod)
library(mice)
library(estimatr)
library(forcats)

## import dataset EVS 2008 (original stata file)
BRA_merge_20072023 <- read_sav("BRA_merge_2007-2023_LAPOP_AmericasBarometer_v1.0_w.sav")
dat <- BRA_merge_20072023

#### DATA PREPARATION ####
## Checking for which variables I have sufficient data (I also checked whether the number of NAs is acceptable)
# IV: Religion 
table(dat$q3, dat$wave, useNA = "always") # religion 2008
table(dat$q3c, dat$wave, useNA = "always") # religion 2010 - 2016
table(dat$q3cn, dat$wave, useNA = "always") # religion 2018 + 2023
table(dat$q5a, dat$wave, useNA = "always") # Attending religious services 2008-2012; 2016-2018)

# Mediator: Redistributive attitudes
table(dat$redist1, dat$wave, useNA = "always") # government must spend more to help the poor 2018
table(dat$redist2a, dat$wave, useNA = "always") # okay for rich to pay high taxes 2018 
table(dat$redist3, dat$wave, useNA = "always") # most unemployed could find a job if they want 2018
table(dat$ros1, dat$wave, useNA = "always") # state should own the most important industries 2008-2016
table(dat$ros4, dat$wave, useNA = "always") # state should introduce firm policies to reduce inequality 2008-2018 + 2023
table(dat$ros6, dat$wave, useNA = "always") # state is more responsible for health 2010-12

# Mediator: Dexual morality 
table(dat$w14a, dat$wave, useNA = "always") # abortion justified 2012-2018 + 2023)
table(dat$d6, dat$wave, useNA = "always") # same-sex marriage approval 2010-2018 + 2023

# DV: Party Choice 
table(dat$vb3n, dat$wave, useNA = "always") # party choice 2023
table(dat$vb3n_18, dat$wave, useNA = "always") # party choice 2018
table(dat$vb3n_16, dat$wave, useNA = "always") # party choice 2016
table(dat$vb3n_14, dat$wave, useNA = "always") # party choice 2014
table(dat$vb3_12, dat$wave, useNA = "always") # party choice 2012
table(dat$vb3_10, dat$wave, useNA = "always") # party choice 2010
table(dat$vb3_08, dat$wave, useNA = "always") # party choice 2010

# Control Variables
table(dat$edre, dat$wave, useNA = "always") #  education 2023
table(dat$etid, dat$wave, useNA = "always") # ethnicity 2006-2021
table(dat$q1, dat$wave, useNA = "always") # gender 2006-2018
table(dat$sexin, dat$wave, useNA = "always") # gender 2023
table(dat$prov, dat$wave, useNA = "always") # state 2008-2018 + 2023
table(dat$q2, dat$wave, useNA = "always") # age 2006-2023
table(dat$q10new_12, dat$wave, useNA = "always") # household income 2012
table(dat$q10, dat$wave, useNA = "always") # household income 2008-2010
table(dat$q10new_14, dat$wave, useNA = "always") # household income 2014
table(dat$q10new_16, dat$wave, useNA = "always") # household income 2016
table(dat$q10new_18, dat$wave, useNA = "always") # household income 2018
table(dat$q10inc, dat$wave, useNA = "always") # household income 2023
table(dat$estratosec, dat$wave, useNA = "always") # city size



####################
# DATA PREPARATION #
####################



## select variables 
dat_cl <- dat %>% dplyr::select(idnum, wave, estratopri, prov, ur, q1, q2, etid, ed,  # demographics: region, state, urban/rural, gender, age, ethnicity, education
                                q3, q3c, q3cn, q3c, q5a, q5b, cp6, # religion 2008, 2010 - 2016, 2018 + 2023; church attendance, importance religion, attendance at religious meetings
                                redist1, redist2a, redist3, ros4,  # attitudes to redistribution
                                w14a, d6, d5, # sexual morality
                                vb3n, vb3n_18, vb3n_16, vb3n_14, vb3_12, vb3_10, l1, # party choice + ideology
                                q10new_18, q10new_16, q10new_14, q10new_12, q10, # income
                                b20, b20a,  # confidence in catholic church, confidence in protestant church
                                municipio10, estratosec) # municipalities

dat_cl <- dat_cl %>% dplyr::rename(id = idnum, region = estratopri, state = prov,ethnicity = etid, res = ur, gender = q1, # control variables
                                   age = q2, edu_years = ed, ethnicity = etid, # control variables
                                   chrch_att =  q5a, rel08 = q3, rel1016 = q3c, rel1823 = q3cn, rel_imp = q5b, rel_meet = cp6, # religion 
                                   gov_prov = redist1, tax_rich = redist2a, unem_job = redist3, gov_ineq = ros4, # attitudes to redistribution
                                   abor = w14a, sam_sex = d6, hom_pol = d5, # sexual morality
                                   income18 = q10new_18, income16 = q10new_16, income14 = q10new_14, income12 = q10new_12, income_0810 = q10, # income
                                   par_choc21 = vb3n, par_choc18 = vb3n_18, par_choc16 = vb3n_16, par_choc14 = vb3n_14, 
                                   par_choc12 = vb3_12, par_choc10 = vb3_10, idlgy = l1,
                                   conf_cath = b20, conf_ev = b20a, city_size = estratosec)


## getting an overview of the data 
head(dat_cl)

## creating one religion variable for the period 2008-2018
# inspecting haven labels 
get_labels(dat_cl$rel08)
get_labels(dat_cl$rel1016)
get_labels(dat_cl$rel1823)

# recoding variables as factor
dat_cl <- dat_cl %>% 
  mutate_at(
    c('rel08', 'rel1016', 'rel1823', 'wave'), as.factor)

# recoding labels into 
dat_cl <- dat_cl %>%
  mutate(
    religion1 = dplyr::recode(rel08, '1' = 'catholic', '2' = 'evangelical', '4' = 'non_allign',
                              '5' = 'evangelical', .default = 'other'),
    religion2 = dplyr::recode(rel1016, '1' = 'catholic', '2' = 'evangelical', '4' = 'non_allign',
                              '5' = 'evangelical', .default = 'other'),
    religion3 = dplyr::recode(rel1823, '1' = 'catholic', '2' = 'evangelical', '4' = 'non_allign',
                              '5' = 'evangelical', .default = 'other'))
dat_cl <- dat_cl %>% 
  mutate(
    rel_dom = coalesce(religion1, religion2, religion3))  

dat_cl$rel_dom

# change reference group to non_alligned
dat_cl <- within(dat_cl, rel_dom <- relevel(rel_dom, ref = "other")) 

## create common income variable 
# standardize all income variables
dat_cl <- dat_cl %>%
  mutate(across(c(income18, income16, income14, income12, income_0810), ~ scale(.x, center = TRUE, scale = TRUE)))

dat_cl <- dat_cl %>%
  mutate(across(c(income18, income16, income14, income12, income_0810), ~ as.vector(.x)))

# merge all income variables into one standardized variable
dat_cl <- dat_cl %>% 
  mutate(
    income = coalesce(income18, income16, income14, income12, income_0810)) 

table(dat_cl$income, useNA = "always")

# reverse ordering church attendance and importance of religion so that higher values indicated more frequent attendance.
dat_cl <- dat_cl %>% mutate_at(c('chrch_att', 'rel_imp', 'rel_meet'), as.numeric)
dat_cl <- dat_cl %>%
  mutate(
    chrch_att = dplyr::recode(chrch_att, '1' = '5', '2' = '4', '3' = '3', '4' = '2', '5' = '1'),
    rel_imp = dplyr::recode(rel_imp, '1' = '4', '2' = '3', '3' = '2', '4' = '1'), 
    rel_meet = dplyr::recode(rel_meet, '1' = '4', '2' = '3', '3' = '2', '4' = '1'))

# recoding as numeric
dat_cl <- dat_cl %>% mutate_at(c('age', 'edu_years', 'gender', 'income',
                                 'ethnicity', 'res', 'chrch_att', 'rel_imp', 
                                 'abor', 'sam_sex', 'gov_prov', 'gov_ineq', 'income18',
                                 'tax_rich', 'unem_job', 'rel_meet'), as.numeric)

## recoding categorical variables
dat_cl <- dat_cl %>%
  mutate(
    gender = dplyr::recode(gender, '1' = '1', '2' = '0' ),# 1= male; 0 = female
    ethnicity = dplyr::recode(ethnicity, '1' = '0', '4' = '1', '5'= '2',
                               '3'= '3', '7'= '4', '1506' = '4'), # 0= white; 1 = black, 2 = mixed, 3 = indigenous, 4 = other 
    res = dplyr::recode(res, '1' = '1', '2' = '0' ,)) # 0=rural, 1=urban

# create separate factor variables (I am keeping both factor and numeric version to run logstic and linear probability models)
dat_cl <- dat_cl %>% 
  mutate(
    gender = as.factor(gender), # 1= male; 0 = female
    ethnicity = as.factor(ethnicity), # 0= white; 1 = black, 2 = mixed, 3 = indigenous, 4 = other 
    res = as.factor(res), # 0=rural, 1=urban
    rel_dom = as.factor(rel_dom),
    abor = as.factor(abor), 
    state = as.factor(state), 
    city_size = as.factor(city_size)) 


# adding factor levels
levels(dat_cl$gender) <- c("female", "male")
levels(dat_cl$ethnicity) <- c("white", "black", "mixed", "indigenous", "other")
levels(dat_cl$res) <- c("rural", "urban")
levels(dat_cl$abor) <- c("Justified", "Unjustified")
levels(dat_cl$city_size) <- c("big", "medium", "small")

## recode party choice for each year 
## 2018
# Recoding party choice into factor variable
dat_cl <- dat_cl %>% 
  mutate(par_choc18_new = as.factor(par_choc18))

# saving party choice labels as character vector
par_choc18_lbs <- get_labels(dat_cl$par_choc18)
par_choc18_lbs

# passing party choice labels to new factor variable
levels(dat_cl$par_choc18_new) <- c(par_choc18_lbs)
table(dat_cl$par_choc18_new)

# create dataframe for 2014 (based on 2010 data)
part_idlgy18 <- data.frame(
  par_choc18_new = c("Ninguno fue a votar pero dejó la boleta en blanco", "Ninguno (anuló su voto)" , "Jair Bolsonaro (PSL)", "Fernando Haddad (PT)" , "Ciro Gomes (PDT)", 
                 "Geraldo Alckmin (PSDB)", "João Amoêdo (NOVO)", "Cabo Daciolo (PATRI)","Marina Silva (REDE)", "Álvaro Dias (PODEMOS)",
                 "Guilherme Boulos (PSOL)", "Otro" ,"DK" ,"NR", "N/A"),
  ideology_score18 = c(NA, NA, 8.11, 2.97, 3.92, 7.11, 8.13, 8.55, 4.77, 7.24, 1.28, NA, NA, NA, NA)
)

# merge datasets 
dat_cl <- merge(dat_cl, part_idlgy18, by = "par_choc18_new", all.x = TRUE)
hist(dat_cl$ideology_score18)

## 2016
# Recoding party choice into factor variable
dat_cl <- dat_cl %>% 
  mutate(par_choc16_new = as.factor(par_choc16))

# saving party choice labels as character vector
par_choc16_lbs <- get_labels(dat_cl$par_choc16)
par_choc16_lbs

# passing party choice labels to new factor variable
levels(dat_cl$par_choc16_new) <- c(par_choc16_lbs)
table(dat_cl$par_choc16_new, useNA = "always")

# create dataframe for 2016 (based on 2010 data)
part_idlgy16 <- data.frame(
  par_choc16_new = c("Ninguno (fue a votar pero dejó la boleta en blanco", "Ninguno (anuló su voto)" , "Dilma Rousseff (PT)", "Aécio Neves (PSDB)" , "Marina Silva (PSB)", 
                     "Otro" ,"DK" ,"NR", "N/A"),
  ideology_score16 = c(NA, NA, 2.9, 4.6, 3.0, NA, NA, NA, NA)
)

# merge datasets 
dat_cl <- merge(dat_cl, part_idlgy16, by = "par_choc16_new", all.x = TRUE)

## 2012
dat_cl <- dat_cl %>% 
  mutate(par_choc12_new = as.factor(par_choc12))

# saving party choice labels as character vector
par_choc12_lbs <- get_labels(dat_cl$par_choc12)
par_choc12_lbs

# passing party choice labels to new factor variable
levels(dat_cl$par_choc12_new) <- c(par_choc12_lbs)
table(dat_cl$par_choc12_new, useNA = "always")

# create dataframe for 2012 (based on 2010 data)
part_idlgy12 <- data.frame(
  par_choc12_new = c("None (Blank ballot)", "dilma( pt, pmdb, pdt, pcdob, psb, pr, prb, ptn, psc, ptc)",  
                     "josé serra (psdb, dem, ptb, pps, pmn, pt do b)" , "marina silva (pv)", "plínio sampaio (psol)",
                     "levy fidélix (prtb)", "josé m. eymael (psdc)", 
                     "Otro" ,"DK" ,"NR", "N/A"),
  ideology_score12 = c(NA, 2.9, 4.6, 3.5, 1.4, 5.3, 5.4, NA, NA, NA, NA)
)

# merge datasets 
dat_cl <- merge(dat_cl, part_idlgy12, by = "par_choc12_new", all.x = TRUE)
hist(dat_cl$ideology_score12)

### combine into a single vote ideology variable 
dat_cl <- dat_cl %>% 
  mutate(
    party_idlgy = coalesce(ideology_score12, ideology_score16, ideology_score18))     
hist(dat_cl$party_idlgy)

# create PT or left vote variable 
dat_cl <- dat_cl %>%
  mutate(
    PT_or_left = if_else(party_idlgy >= 3.0, 0, 1),
    PT_or_left = as.factor(PT_or_left)
  )

# create final dataframe focused in 2010, 2016, and 2018 elections
dat_final <- dat_cl %>% filter(wave == 2012 | wave == 2016 | wave == 2018) %>%  # select relevant years
  dplyr::select(wave, state, chrch_att, rel_dom, age, gender, ethnicity, # select relevant variables 
                income, edu_years, PT_or_left, abor, sam_sex, gov_ineq, city_size)



####################################
# Main Analysis: Regression Models #
####################################




## Impute data for regression analysis 
# Perform the MICE imputation
dat_final_imp <- mice::mice(dat_final, m = 20, print = FALSE, seed = 12345)

# inspecting results of missing values: no trends detected in imputed means 
plot(dat_final_imp) 

#### Descriptives #### 

## create complete dataset
dat_final_imp_comp <- mice::complete(dat_final_imp)

# put income on the same scale as other variables 
dat_final_imp_comp <- dat_final_imp_comp %>%
  mutate(income = scales::rescale(income, to = c(0, 7)))

# create dummy for church attendance
dat_final_imp_comp <- dat_final_imp_comp %>%
  mutate(chr_att_gr = case_when(
    chrch_att <= 2   ~ "Less than once a month",          
    chrch_att >= 3 ~ "At least once a month")) %>%
  mutate(chr_att_gr = as.factor(chr_att_gr)) 

## Mean Plot by religious denomination 
# Group means continious variables
group_means <- dat_final_imp_comp %>%
  group_by(rel_dom) %>%
  summarise(mean_chrch_att = mean(chrch_att),
            SD_chrch_att = SD(chrch_att),
            mean_income = mean(income),
            SD_income = SD(income),
            mean_sam_sex = mean(sam_sex),
            SD_sam_sex = SD(sam_sex),
            mean_gov_ineq = mean(gov_ineq),
            SD_gov_ineq = SD(gov_ineq),
            rel_dom_n = n())
  
# group sizes religion
group_sizes <- data.frame(
  rel_dom = c("catholic", "evangelical", "non_allign"),
  n = c(2464, 1331, 393)  # 
)

# pivot group means to long format
group_means_long <- group_means %>%
  filter(rel_dom != "other") %>%  
  dplyr::select(rel_dom, mean_sam_sex, mean_gov_ineq, mean_chrch_att)  %>%
  pivot_longer(
    cols = -rel_dom,  
    names_to = "variable",
    values_to = "mean"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  left_join(group_sizes, by = "rel_dom") 

# pivot standard deviations to long format
group_SDs_long <- group_means %>%
  filter(rel_dom != "other") %>%  
  dplyr::select(rel_dom, SD_sam_sex, SD_gov_ineq, SD_chrch_att, rel_dom_n)  %>%
  pivot_longer(
    cols = -rel_dom,  
    names_to = "variable",
    values_to = "SD"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  dplyr::select(-measure)

# merge SDs and means again
df_plots <- inner_join(group_means_long, group_SDs_long, by = c("rel_dom", "metric"))

# calculating se and confidence intervals for error bars
df_plots <- df_plots %>%
  mutate(
    se = SD / sqrt(n),
    se_lower = mean - se,
    se_upper = mean + se,
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1),
    ci_lower = mean - ic,
    ci_upper = mean + ic
  )  %>% 
  mutate(ci_upper = case_when(
    rel_dom == "non_allign" & metric == "sam_sex" ~ 7, # capping CI at 7 (because scale does not extend further)
    TRUE ~ ci_upper))

# relabel variables and labels
df_plots <- df_plots %>%
  rename(Religious_Denomination = rel_dom) %>%
  mutate(
    Religious_Denomination = fct_recode(Religious_Denomination,
                         "Catholic" = "catholic",
                         "Evangelical" = "evangelical",
                         "Non-Aligned" = "non_allign"),
    metric = fct_recode(metric,
                        "Same-sex marriage acceptance" = "sam_sex",
                        "Support for redistributive policies" = "gov_ineq",
                        "Church Attendance" = "chrch_att"))

# filter out church attendance (only included to check differences but not included in final draft)
df_plots <- df_plots  %>%
  filter(metric != "Church Attendance")
# construct mean plot
plot <- ggplot(df_plots, aes(x = mean, y = metric, fill = Religious_Denomination)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Religious Denominations",
       x = "",
       y = "",
       fill = "Religious Denomination") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(0, 7),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 7, by = 1)) +
  theme_minimal() 
plot

## Proportion Plot
# Proportions for categorical variables 
proportions <- dat_final_imp_comp %>%
  filter(rel_dom != "other") %>%
  group_by(rel_dom) %>%
  summarize(
    count_PT_or_left = sum(PT_or_left == 1),  
    total_responses1 = n(),                 
    Left_vote = count_PT_or_left / total_responses1,  
    count_abor = sum(abor == "Justified"),  
    total_responses2 = n(),                  
    Abortion_Justified = count_abor / total_responses2 
  ) %>%
  pivot_longer(
    cols = c(Left_vote, Abortion_Justified),
    names_to = "proportion_type",
    values_to = "value"
  ) %>%
  # Calculating the error bars using normal approximation
  mutate(
    n = ifelse(proportion_type == "prop_proportion1", total_responses1, total_responses2),
    se = sqrt(value * (1 - value) / n),
    lower = value - 1.96 * se,
    upper = value + 1.96 * se
  )

prop_plot_df <- proportions %>%
  rename(Religious_Denomination = rel_dom) %>%
  mutate(
    Religious_Denomination = fct_recode(Religious_Denomination,
                                        "Catholic" = "catholic",
                                        "Evangelical" = "evangelical",
                                        "Non-Aligned" = "non_allign"),
    proportion_type = fct_recode(proportion_type,
                                 "Vote for left party" = "Left_vote",
                                 "Abortion is justified when
                                  mother's health is at risk" = "Abortion_Justified")
    )


plot_prop <- ggplot(prop_plot_df, aes(x = value, y = proportion_type, fill = Religious_Denomination)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, position = position_dodge(width = 0.7)) +
  labs(title = "Comparison of Proportions by Religious Denomination",
       x = "",
       y = "",
       fill = "Religious Denomination") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(0, 1),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 1, by = 0.1)) +
  theme_minimal()
plot_prop


### Plots for church attendance 
## group means
group_means <- dat_final_imp_comp %>%
  group_by(chr_att_gr) %>%
  summarise(mean_income = mean(income),
            SD_income = SD(income),
            mean_sam_sex = mean(sam_sex),
            SD_sam_sex = SD(sam_sex),
            mean_gov_ineq = mean(gov_ineq),
            SD_gov_ineq = SD(gov_ineq),
            chr_att_gr_n = n())

# group sizes religion
group_sizes <- data.frame(
  chr_att_gr = c("At least once a month", "Less than once a month"),
  n = c(3000, 1529)  # 
)

# pivot group means to long format
group_means_long <- group_means %>%
  dplyr::select(chr_att_gr, mean_sam_sex, mean_gov_ineq)  %>%
  pivot_longer(
    cols = -chr_att_gr,  
    names_to = "variable",
    values_to = "mean"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  left_join(group_sizes, by = "chr_att_gr") 

# pivot standard deviations to long format
group_SDs_long <- group_means %>%
  dplyr::select(chr_att_gr, SD_sam_sex, SD_gov_ineq,chr_att_gr_n)  %>%
  pivot_longer(
    cols = -chr_att_gr,  
    names_to = "variable",
    values_to = "SD"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  dplyr::select(-measure)

# merge SDs and means again
df_plots <- inner_join(group_means_long, group_SDs_long, by = c("chr_att_gr", "metric"))

# calculating se and confidence intervals for error bars
df_plots <- df_plots %>%
  mutate(
    se = SD / sqrt(n),
    se_lower = mean - se,
    se_upper = mean + se,
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1),
    ci_lower = mean - ic,
    ci_upper = mean + ic
  )  

# relabel variables and labels
df_plots <- df_plots %>%
  rename(Church_Attendance = chr_att_gr) %>%
  mutate(
    metric = fct_recode(metric,
                        "Same-sex marriage acceptance" = "sam_sex",
                        "Support for redistributive policies" = "gov_ineq"))
# construct mean plot
plot <- ggplot(df_plots, aes(x = mean, y = metric, fill = Church_Attendance)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Church Attendance",
       x = "",
       y = "",
       fill = "Church Attendance") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(0, 7),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 7, by = 1)) +
  theme_minimal() 
plot


# Proportions for categorical variables 
proportions <- dat_final_imp_comp %>%
  group_by(chr_att_gr) %>%
  summarize(
    count_PT_or_left = sum(PT_or_left == 1),  
    total_responses1 = n(),                 
    Left_vote = count_PT_or_left / total_responses1,  
    count_abor = sum(abor == "Justified"),  
    total_responses2 = n(),                  
    Abortion_Justified = count_abor / total_responses2 
  ) %>%
  pivot_longer(
    cols = c(Left_vote, Abortion_Justified),
    names_to = "proportion_type",
    values_to = "value"
  ) %>%
  # Calculating the error bars using normal approximation
  mutate(
    n = ifelse(proportion_type == "prop_proportion1", total_responses1, total_responses2),
    se = sqrt(value * (1 - value) / n),
    lower = value - 1.96 * se,
    upper = value + 1.96 * se
  )

prop_plot_df <- proportions %>%
  rename(Church_Attendance = chr_att_gr) %>%
  mutate(proportion_type = fct_recode(proportion_type,
                                 "Vote for left party" = "Left_vote",
                                 "Abortion is justified when
                                  mother's health is at risk" = "Abortion_Justified"))


plot_prop <- ggplot(prop_plot_df, aes(x = value, y = proportion_type, fill = Church_Attendance)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, position = position_dodge(width = 0.7)) +
  labs(title = "Comparison of Proportions by Church Attendance",
       x = "",
       y = "",
       fill = "Church Attendance") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(0, 1),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 1, by = 0.1)) +
  theme_minimal()
plot_prop


#### Prepare Models 
## Complete the imputed datasets and store them in a list
dat_final_imp_sets <- mice::complete(dat_final_imp, action = "long", include = TRUE) %>%
  group_by(.imp) %>%
  nest()

# create separate IV variables for sam_sex and gov_ineq and scale them from 0 to 1; create factor variable for abortion attitudes
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(data = map(data, ~ .x %>%
                      mutate(gov_ineq_IV = scale(gov_ineq, center = FALSE, scale = max(abs(gov_ineq), na.rm = TRUE)),
                             sam_sex_IV = scale(sam_sex, center = FALSE, scale = max(abs(sam_sex), na.rm = TRUE)),
                             abor_IV = as.factor(abor),
                             chrch_att_IV = scale(chrch_att, center = FALSE, scale = max(abs(chrch_att), na.rm = TRUE)))))

# creating dichotomous item for church attendance
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(data = map(data, ~ .x %>%
                      mutate(chr_att_gr = case_when(
                        chrch_att <= 2   ~ "Less than once a month",          
                        chrch_att >= 3 ~ "At least once a month")) %>%
                      mutate(chr_att_gr = as.factor(chr_att_gr)))) 


# specific numeric predictors to standardize 
cols_to_standardize_IV <- c("chrch_att", "age",  "edu_years", "sam_sex", "gov_ineq", "income")

# rescale all numeric predictors according to Gelman rules
dat_final_imp_sets <- dat_final_imp_sets %>% 
  mutate(data = map(data, ~ .x %>%
                      mutate(across(all_of(cols_to_standardize_IV), ~ (. - mean(., na.rm = TRUE)) / (2 * sd(., na.rm = TRUE))))))


# set non_allign as reference group 
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(data = map(data, ~ .x %>%
                      mutate(rel_dom = relevel(rel_dom, ref = "non_allign"),
                             abor_IV = relevel(abor_IV, ref = "Unjustified"),
                             abor = relevel(abor, ref = "Unjustified"),
                             chr_att_gr = relevel(chr_att_gr, ref = "Less than once a month"))))


#### Linear Probability Models
# create separate model for lm analysis 
dat_final_imp_sets_lm <- dat_final_imp_sets

# set catholic as reference group  #### ONLY AFTER RUNNING WITH NON_ALLIGN AS REFERENCE GROUP
# dat_final_imp_sets <- dat_final_imp_sets %>%
#  mutate(data = map(data, ~ .x %>%
#                      mutate(rel_dom = relevel(rel_dom, ref = "catholic"))))

# set catholic as reference group
dat_final_imp_sets_lm <- dat_final_imp_sets_lm %>%
  mutate(data = map(data, ~ .x %>%
                      mutate(rel_dom = relevel(rel_dom, ref = "non_allign"))))

# create separate IV variables for sam_sex and gov_ineq and scale them from 0 to 1; create factor variable for abortion attitudes
dat_final_imp_sets_lm <- dat_final_imp_sets_lm %>%
  mutate(data = map(data, ~ .x %>%
                      mutate(PT_or_left = as.numeric(PT_or_left) - 1, # subtracting by -1 so that 1/2 become 0/1
                             abor_IV = as.numeric(abor_IV)-1)))

# run regression models
dat_final_imp_sets_lm <- dat_final_imp_sets_lm %>%
  mutate(
    # Model 0: base models 
    vt_base1 = map(data, ~ lm_robust(PT_or_left ~  rel_dom, data = .)),
    vt_base2 = map(data, ~ lm_robust(PT_or_left ~  chrch_att + rel_dom + income,  data = .)),
    # Model 1: with controls
    vt_cntrl1 = map(data, ~ lm_robust(PT_or_left ~ rel_dom + income + age + gender + ethnicity  + edu_years + city_size + wave + state, data = .)),
    vt_cntrl2 = map(data, ~ lm_robust(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity + edu_years + city_size + wave + state, data = .)),
    # Model 2: with interactions
    vt_int1 = map(data, ~ lm_robust(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity  + ethnicity + edu_years + city_size + wave + state + rel_dom*income, data = .)),
    vt_int2 = map(data, ~ lm_robust(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity + ethnicity  + edu_years + city_size  + wave + state + chrch_att*income, data = .)),
    # Model 3: Mediation same_sex marriage (with chrch_att)
    sam_sex = map(data, ~ lm_robust(sam_sex_IV ~ chrch_att + rel_dom + income + age + gender + ethnicity  + edu_years + city_size + wave + state, data = .)),
    vt_sam_sex = map(data, ~ lm_robust(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity  + edu_years + city_size + sam_sex + wave + state, data = .)),
    # Model 3: Mediation abortion
    abor = map(data, ~ lm_robust(abor_IV ~ chrch_att +  rel_dom + income + age + gender + ethnicity  + edu_years + city_size + wave + state, data = .)),
    vt_abor = map(data, ~ lm_robust(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity  + edu_years + city_size + abor + wave + state, data = .)),
    # Model 4: Mediation model gov_ineq
    gov_ineq = map(data, ~ lm_robust(gov_ineq_IV ~ chrch_att + rel_dom + income + age + gender + ethnicity + edu_years + city_size + wave  + state, data = .)),
    vt_gov_ineq = map(data, ~ lm_robust(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity + edu_years + city_size + gov_ineq + wave + state, data = .)),
    # Model 5: Predicting Church Attendance 
    chrch_att = map(data, ~ lm_robust(chrch_att_IV ~ rel_dom + income + age + gender + ethnicity + edu_years + city_size + wave  + state, data = .))
  )

### Model output
## vt_base1
vt_base1_pol <- pool(dat_final_imp_sets_lm$vt_base1)
vt_base1_pol_t <- broom::tidy(vt_base1_pol)
vt_base1_pol_t
# extract estimates, standard errors and p-values 
vt_base1_pol_t <- vt_base1_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "\n(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_base1_pol_t <- vt_base1_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_base1_pol_t, file = "vt_base1_pol_t.csv")
vt_base1_pol_t

## vt_base2
vt_base2_pol <- pool(dat_final_imp_sets_lm$vt_base2)
vt_base2_pol_t <- broom::tidy(vt_base2_pol)
vt_base2_pol_t
# extract estimates, standard errors and p-values 
vt_base2_pol_t <- vt_base2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_base2_pol_t <- vt_base2_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_base2_pol_t, file = "vt_base2_pol_t.csv")
vt_base2_pol_t

## vt_cntrl 1
vt_cntrl1_pol <- pool(dat_final_imp_sets_lm$vt_cntrl1)
vt_cntrl1_pol_t <- broom::tidy(vt_cntrl1_pol)
vt_cntrl1_pol_t
# extract estimates, standard errors and p-values 
vt_cntrl1_pol_t <- vt_cntrl1_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_cntrl1_pol_t <- vt_cntrl1_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_cntrl1_pol_t, file = "vt_cntrl1_pol_t.csv")
vt_cntrl1_pol_t

# R squared
mean(vt_cntrl1_pol$glanced$r.squared)

## vt_cntrl 2 (with chrch_att)
vt_cntrl2_pol <- pool(dat_final_imp_sets_lm$vt_cntrl2)
vt_cntrl2_pol_t <- broom::tidy(vt_cntrl2_pol)
vt_cntrl2_pol_t

# extract estimates, standard errors and p-values 
vt_cntrl2_pol_t_out <- vt_cntrl2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_cntrl2_pol_t_out <- vt_cntrl2_pol_t_out %>% dplyr::select(term, estimate)
write.csv(vt_cntrl2_pol_t_out, file = "vt_cntrl2_pol_t_out.csv")
vt_cntrl2_pol_t_out

# R squared
mean(vt_cntrl2_pol$glanced$r.squared)

# prepare graph for comparing religious groups
vt_cntrl2_pol_t$pred_prob <- 1/(1+ exp(-vt_cntrl2_pol_t$estimate))
vt_cntrl2_pol_t$pred_prob_SE <-  (exp(-vt_cntrl2_pol_t$estimate) / (1 + exp(-vt_cntrl2_pol_t$estimate))^2) * vt_cntrl2_pol_t$std.error
vt_cntrl2_pol_t

# R squared
mean(vt_cntrl2_pol$glanced$r.squared)

## vt_int1
vt_int1_pol <- pool(dat_final_imp_sets_lm$vt_int1)
vt_int1_pol_t <- broom::tidy(vt_int1_pol)
vt_int1_pol_t
# extract estimates, standard errors and p-values 
vt_int1_pol_t <- vt_int1_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_int1_pol_t <- vt_int1_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_int1_pol_t, file = "vt_int1_pol_t.csv")
vt_int1_pol_t

# R squared
mean(vt_int1_pol$glanced$r.squared)

## vt_int2
vt_int2_pol <- pool(dat_final_imp_sets_lm$vt_int2)
vt_int2_pol_t <- broom::tidy(vt_int2_pol)
vt_int2_pol_t

# extract estimates, standard errors and p-values 
vt_int2_pol_t <- vt_int2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_int2_pol_t <- vt_int2_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_int2_pol_t, file = "vt_int2_pol_t.csv")
vt_int2_pol_t

# R squared
mean(vt_int2_pol$glanced$r.squared)

## sam_sex
sam_sex_pol <- pool(dat_final_imp_sets_lm$sam_sex)
sam_sex_pol_t <- broom::tidy(sam_sex_pol)
sam_sex_pol_t
# extract estimates, standard errors and p-values 
sam_sex_pol_t <- sam_sex_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
sam_sex_pol_t <- sam_sex_pol_t %>% dplyr::select(term, estimate)
write.csv(sam_sex_pol_t, file = "sam_sex_pol_t.csv")
sam_sex_pol_t

# R squared
mean(sam_sex_pol$glanced$r.squared)

## vt_sex_mor
vt_sam_sex_pol <- pool(dat_final_imp_sets_lm$vt_sam_sex)
vt_sam_sex_pol_t <- broom::tidy(vt_sam_sex_pol)
vt_sam_sex_pol_t
# extract estimates, standard errors and p-values 
vt_sam_sex_pol_t <- vt_sam_sex_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_sam_sex_pol_t <- vt_sam_sex_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_sam_sex_pol_t, file = "vt_sam_sex_pol_t.csv")
vt_sam_sex_pol_t

# R squared
mean(vt_sam_sex_pol$glanced$r.squared)

## abortion 
abor_pol <- pool(dat_final_imp_sets_lm$abor)
abor_pol_t <- broom::tidy(abor_pol)
abor_pol_t
# extract estimates, standard errors and p-values 
abor_pol_t <- abor_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
abor_pol_t <- abor_pol_t %>% dplyr::select(term, estimate)
write.csv(abor_pol_t, file = "abor_pol_t.csv")
abor_pol_t

# R squared
mean(abor_pol$glanced$r.squared)


## abortion voting 
vt_abor_pol <- pool(dat_final_imp_sets_lm$vt_abor)
vt_abor_pol_t <- broom::tidy(vt_abor_pol)
vt_abor_pol_t
# extract estimates, standard errors and p-values 
vt_abor_pol_t <- vt_abor_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_abor_pol_t <- vt_abor_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_abor_pol_t, file = "vt_abor_pol_t.csv")
vt_abor_pol_t

# R squared
mean(vt_abor_pol$glanced$r.squared)

## gov_ineq 
gov_ineq_pol <- pool(dat_final_imp_sets_lm$gov_ineq)
gov_ineq_pol_t <- broom::tidy(gov_ineq_pol)
gov_ineq_pol_t
# extract estimates, standard errors and p-values 
gov_ineq_pol_t <- gov_ineq_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
gov_ineq_pol_t <- gov_ineq_pol_t %>% dplyr::select(term, estimate)
write.csv(gov_ineq_pol_t, file = "gov_ineq_pol_t.csv")
gov_ineq_pol_t

# R squared
mean(gov_ineq_pol$glanced$r.squared)


## voting government inequality
vt_gov_ineq_pol <- pool(dat_final_imp_sets_lm$vt_gov_ineq)
vt_gov_ineq_pol_t <- broom::tidy(vt_gov_ineq_pol)
vt_gov_ineq_pol_t
# extract estimates, standard errors and p-values 
vt_gov_ineq_pol_t <- vt_gov_ineq_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_gov_ineq_pol_t <- vt_gov_ineq_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_gov_ineq_pol_t, file = "vt_gov_ineq_pol_t.csv")
vt_gov_ineq_pol_t

# R squared
mean(vt_gov_ineq_pol$glanced$r.squared)

## church attendance
chrch_att_pol <- pool(dat_final_imp_sets_lm$chrch_att)
chrch_att_pol_t <- broom::tidy(chrch_att_pol)
chrch_att_pol_t
# extract estimates, standard errors and p-values 
chrch_att_pol_t <- chrch_att_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
chrch_att_pol_t <- chrch_att_pol_t %>% dplyr::select(term, estimate)
write.csv(chrch_att_pol_t, file = "chrch_att_pol_t.csv")
chrch_att_pol_t

# R squared
mean(chrch_att_pol$glanced$r.squared)

####### Addtional Robustness Checks ###### 
#### Robustness Check: Logistic Regression 
### Run logistic regression models
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(
    # Model 0: base models 
    vt_base1 = map(data, ~ glm(PT_or_left ~  rel_dom, data = ., family = binomial)),
    vt_base2 = map(data, ~ glm(PT_or_left ~  chrch_att + rel_dom + income,  data = ., family = binomial)),
    # Model 1: with controls
    vt_cntrl1 = map(data, ~ glm(PT_or_left ~ rel_dom + income + age + gender + ethnicity  + edu_years + city_size + wave + state, data = ., family = binomial)),
    vt_cntrl2 = map(data, ~ glm(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity + edu_years + city_size + wave + state, data = ., family = binomial)),
    # Model 2: with interactions
    vt_int1 = map(data, ~ glm(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity  + ethnicity + edu_years + city_size + wave + state + rel_dom*income, data = ., family = binomial)),
    vt_int2 = map(data, ~ glm(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity + ethnicity  + edu_years + city_size  + wave + state + chrch_att*income, data = ., family = binomial)),
    # Model 3: Mediation same_sex marriage (with chrch_att)
    sam_sex = map(data, ~ lm(sam_sex_IV ~ chrch_att + rel_dom + income + age + gender + ethnicity  + edu_years + city_size + wave + state, data = .)),
    vt_sam_sex = map(data, ~ glm(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity  + edu_years + city_size + sam_sex + wave + state, data = ., family = binomial)),
    # Model 3: Mediation abortion
    abor = map(data, ~ glm(abor_IV ~ chrch_att +  rel_dom + income + age + gender + ethnicity  + edu_years + city_size + wave + state, data = ., family = binomial)),
    vt_abor = map(data, ~ glm(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity  + edu_years + city_size + abor + wave + state, data = ., family = binomial)),
    # Model 4: Mediation model gov_ineq
    gov_ineq = map(data, ~ lm(gov_ineq_IV ~ chrch_att + rel_dom + income + age + gender + ethnicity + edu_years + city_size + wave  + state, data = .)),
    vt_gov_ineq = map(data, ~ glm(PT_or_left ~ chrch_att + rel_dom + income + age + gender + ethnicity + edu_years + city_size + gov_ineq + wave + state, data = ., family = binomial))
  )

## vt_base1
vt_base1_pol <- pool(dat_final_imp_sets$vt_base1)
vt_base1_pol_t <- broom::tidy(vt_base1_pol)

# extract estimates, standard errors and p-values 
vt_base1_pol_t <- vt_base1_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "\n(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_base1_pol_t <- vt_base1_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_base1_pol_t, file = "vt_base1_pol_t.csv")
vt_base1_pol_t

## vt_base2
vt_base2_pol <- pool(dat_final_imp_sets$vt_base2)
vt_base2_pol_t <- broom::tidy(vt_base2_pol)
vt_base2_pol_t
# extract estimates, standard errors and p-values 
vt_base2_pol_t <- vt_base2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_base2_pol_t <- vt_base2_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_base2_pol_t, file = "vt_base2_pol_t.csv")
vt_base2_pol_t


## vt_cntrl 1
vt_cntrl1_pol <- pool(dat_final_imp_sets$vt_cntrl1)
vt_cntrl1_pol_t <- broom::tidy(vt_cntrl1_pol)
vt_cntrl1_pol_t
# extract estimates, standard errors and p-values 
vt_cntrl1_pol_t <- vt_cntrl1_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "\n(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_cntrl1_pol_t <- vt_cntrl1_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_cntrl1_pol_t, file = "vt_cntrl1_pol_t.csv")
vt_cntrl1_pol_t

## vt_cntrl 2 (with chrch_att)
vt_cntrl2_pol <- pool(dat_final_imp_sets$vt_cntrl2)
vt_cntrl2_pol_t <- broom::tidy(vt_cntrl2_pol)
vt_cntrl2_pol_t

# extract estimates, standard errors and p-values 
vt_cntrl2_pol_t_out <- vt_cntrl2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_cntrl2_pol_t_out <- vt_cntrl2_pol_t_out %>% dplyr::select(term, estimate)
write.csv(vt_cntrl2_pol_t_out, file = "vt_cntrl2_pol_t_out.csv")
vt_cntrl2_pol_t_out

# prepare graph for comparing religious groups
vt_cntrl2_pol_t$pred_prob <- 1/(1+ exp(-vt_cntrl2_pol_t$estimate))
vt_cntrl2_pol_t$pred_prob_SE <-  (exp(-vt_cntrl2_pol_t$estimate) / (1 + exp(-vt_cntrl2_pol_t$estimate))^2) * vt_cntrl2_pol_t$std.error
vt_cntrl2_pol_t

# prepare dataframe with predicted probabilities and SEs
data <- data.frame(
  Category = c("Non_alligned", "Catholics", "Evangelicals"),
  Coefficient = c(0.5819405, 0.6099957, 0.5140230),
  SE = c(0.05832597, 0.04437849, 0.05115945)
)

# plot 
ggplot(data, aes(x = Category, y = Coefficient, ymin = Coefficient - SE, ymax = Coefficient + SE)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  geom_hline(yintercept = 0.4015910, linetype = "dashed", color = "red") +  # red line at level of intercept
  labs(title = " ", y = "Probability to vote for left party", x = "") 

## vt_int1
vt_int1_pol <- pool(dat_final_imp_sets$vt_int1)
vt_int1_pol_t <- broom::tidy(vt_int1_pol)
vt_int1_pol_t
# extract estimates, standard errors and p-values 
vt_int1_pol_t <- vt_int1_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_int1_pol_t <- vt_int1_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_int1_pol_t, file = "vt_int1_pol_t.csv")
vt_int1_pol_t

## vt_int2
vt_int2_pol <- pool(dat_final_imp_sets$vt_int2)
vt_int2_pol_t <- broom::tidy(vt_int2_pol)
vt_int2_pol_t

# extract estimates, standard errors and p-values 
vt_int2_pol_t <- vt_int2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_int2_pol_t <- vt_int2_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_int2_pol_t, file = "vt_int2_pol_t.csv")
vt_int2_pol_t

## sam_sex
sam_sex_pol <- pool(dat_final_imp_sets$sam_sex)
sam_sex_pol_t <- broom::tidy(sam_sex_pol)
sam_sex_pol_t
# extract estimates, standard errors and p-values 
sam_sex_pol_t <- sam_sex_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
sam_sex_pol_t <- sam_sex_pol_t %>% dplyr::select(term, estimate)
write.csv(sam_sex_pol_t, file = "sam_sex_pol_t.csv")
sam_sex_pol_t

## vt_sex_mor
vt_sam_sex_pol <- pool(dat_final_imp_sets$vt_sam_sex)
vt_sam_sex_pol_t <- broom::tidy(vt_sam_sex_pol)
vt_sam_sex_pol_t
# extract estimates, standard errors and p-values 
vt_sam_sex_pol_t <- vt_sam_sex_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_sam_sex_pol_t <- vt_sam_sex_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_sam_sex_pol_t, file = "vt_sam_sex_pol_t.csv")
vt_sam_sex_pol_t

## abortion 
abor_pol <- pool(dat_final_imp_sets$abor)
abor_pol_t <- broom::tidy(abor_pol)
abor_pol_t
# extract estimates, standard errors and p-values 
abor_pol_t <- abor_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
abor_pol_t <- abor_pol_t %>% dplyr::select(term, estimate)
write.csv(abor_pol_t, file = "abor_pol_t.csv")
abor_pol_t

## abortion voting 
vt_abor_pol <- pool(dat_final_imp_sets$vt_abor)
vt_abor_pol_t <- broom::tidy(vt_abor_pol)
vt_abor_pol_t
# extract estimates, standard errors and p-values 
vt_abor_pol_t <- vt_abor_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_abor_pol_t <- vt_abor_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_abor_pol_t, file = "vt_abor_pol_t.csv")
vt_abor_pol_t


## gov_ineq 
gov_ineq_pol <- pool(dat_final_imp_sets$gov_ineq)
gov_ineq_pol_t <- broom::tidy(gov_ineq_pol)
gov_ineq_pol_t
# extract estimates, standard errors and p-values 
gov_ineq_pol_t <- gov_ineq_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
gov_ineq_pol_t <- gov_ineq_pol_t %>% dplyr::select(term, estimate)
write.csv(gov_ineq_pol_t, file = "gov_ineq_pol_t.csv")
gov_ineq_pol_t

## voting government inequality

vt_gov_ineq_pol <- pool(dat_final_imp_sets$vt_gov_ineq)
vt_gov_ineq_pol_t <- broom::tidy(vt_gov_ineq_pol)
vt_gov_ineq_pol_t
# extract estimates, standard errors and p-values 
vt_gov_ineq_pol_t <- vt_gov_ineq_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
vt_gov_ineq_pol_t <- vt_gov_ineq_pol_t %>% dplyr::select(term, estimate)
write.csv(vt_gov_ineq_pol_t, file = "vt_gov_ineq_pol_t.csv")
vt_gov_ineq_pol_t
