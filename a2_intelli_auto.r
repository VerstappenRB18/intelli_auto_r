####################### Installed Packages #######################
library(readxl)
library(tidyverse)
library(caret)
library(pROC)
library(broom)
library(car)
library(forcats)
library(sjPlot)
library(gtsummary)
library(gt)
library(DescTools)
library(reshape2)
library(corrplot)
library(polycor)
library(MASS)
library(lmtest)
library(randomForest)
library(scatterplot3d)
####################### Installed Packages #######################

####################### Reading Dataset #######################
employee_data <- read_excel("a2_dataset.xlsx")
####################### Reading Dataset #######################

####################### Data Pre-Processing #######################
employee_data <- employee_data %>%
  # Exclude rows with missing values
  drop_na() %>%
  # Rename columns
  dplyr::rename(
    id_num = IdNum,
    working_hours = WorkHrs,
    occupation = Occupn,
    age = Age,
    education_years = EducYrs,
    sex = Sex,
    earners = Earners,
    pre_tax_income = PreTaxInc,
    pre_tax_family_income = PreTaxFamInc,
    job_satisfaction = JobSat,
    rich_work = RichWork,
    job_characteristics = JobChar,
    get_ahead = GetAhead,
    trauma = Trauma,
    union_member = MemUnion,
    working_years = WrkYears,
    employment_years = EmpYears,
    number_promotions = NumPromo,
    future_promotion = FutPromo,
    sex_promotion = SexPromo,
    career_advancement = Advances,
    decision_involvement = IDecide,
    budget_participation = OrgMoney,
    proud_organization = ProudOrg,
    stay_organization = StayOrg,
    mgn_emp_relationship = UnManRel,
    coworker_relationship = CoWrkRel,
    schooling = Schooling,
    training = Training,
    aware_industry_4_0 = AwareI4.0,
    engagement = Engagement
  ) %>%
  # Transform values to fit the context of the question
  mutate(
    # Correct working_years to ensure age - working_years = 16
    working_years = if_else(age - working_years < 16, age - 16, working_years),
    # Correct employment_years to ensure emp_years < working_years
    employment_years = if_else(employment_years > working_years,
      working_years, employment_years
    ),
    # Validate pre_tax_income and pre_tax_family_income based on earners
    pre_tax_family_income = if_else(earners == 1, pre_tax_income,
      pre_tax_family_income
    ),
    # Categorize employment_years directly
    over_15_emp_years = if_else(employment_years > 15, "yes", "no"),
    over_15_emp_years = as.factor(over_15_emp_years),
    # Recode the occupation column
    occupation = case_when(
      occupation == "1.Manag" ~ "managerial",
      occupation == "2.Prof'nal" ~ "professional",
      occupation == "3.Tech/Sales" ~ "technical/sales",
      occupation == "4.Admin" ~ "admin support",
      occupation == "5.Service" ~ "service",
      occupation == "6.Prod'n" ~ "production",
      occupation == "7.Laborer" ~ "laborer",
    ),
    # Recode the sex column
    sex = if_else(sex == "Male", "male", "female"),
    # Recode the job_satisfaction column
    job_satisfaction = case_when(
      job_satisfaction == "4.Very Dissat" ~ 1,
      job_satisfaction == "3.Little Dissat" ~ 2,
      job_satisfaction == "2.Mod Sat" ~ 3,
      job_satisfaction == "1.Very Sat" ~ 4
    ),
    # Recode the rich_work column
    rich_work = case_when(
      rich_work == "1.Yes" ~ 3,
      rich_work == "2.No" ~ 1,
      rich_work == "3.Not Sure" ~ 2
    ),
    # Recode the job_characteristics column
    job_characteristics = case_when(
      job_characteristics == "1.High Inc" ~ "high income",
      job_characteristics == "2.Not Fired" ~ "not fired",
      job_characteristics == "3.Flex Hours" ~ "flexible hours",
      job_characteristics == "4.Opp Advance" ~ "advancement opportunities",
      job_characteristics == "5.Enjoy Work" ~ "enjoy work"
    ),
    # Recode the get_ahead column
    get_ahead = case_when(
      get_ahead == "1.Work" ~ "work",
      get_ahead == "2.Luck" ~ "luck",
      get_ahead == "3.Work&Luck" ~ "both"
    ),
    # Recode the union_member column
    union_member = if_else(union_member == "Yes Union", "yes", "no"),
    # Recode the future_promotion column
    future_promotion = case_when(
      future_promotion == "5.V Unlikely" ~ 1,
      future_promotion == "4.Unlikely" ~ 2,
      future_promotion == "3.Not sure" ~ 3,
      future_promotion == "2.Likely" ~ 4,
      future_promotion == "1.V Likely" ~ 5
    ),
    # Recode the sex_promotion column
    sex_promotion = case_when(
      sex_promotion == "1.Better" ~ 3,
      sex_promotion == "2.Worse" ~ 1,
      sex_promotion == "3.No Effect" ~ 2
    ),
    # Recode the career_advancement column
    career_advancement = case_when(
      career_advancement == "4.Lost" ~ 1,
      career_advancement == "3.Same" ~ 3,
      career_advancement == "2.Steady" ~ 2,
      career_advancement == "1.Rapid" ~ 4
    ),
    # Recode the decision_involvement column
    decision_involvement = case_when(
      decision_involvement == "1.Always" ~ 4,
      decision_involvement == "2.Much" ~ 3,
      decision_involvement == "3.Some" ~ 2,
      decision_involvement == "4.Never" ~ 1
    ),
    # Recode the organization_money column
    budget_participation =
      ifelse(budget_participation == "Yes Budget", "yes", "no"),
    # Recode the proud_organization column
    proud_organization = case_when(
      proud_organization == "4.No Proud" ~ 1,
      proud_organization == "3.Ind Proud" ~ 2,
      proud_organization == "2.Some Proud" ~ 3,
      proud_organization == "1.V Proud" ~ 4
    ),
    # Recode the stay_organization column
    stay_organization = case_when(
      stay_organization == "5.V Unlikely" ~ 1,
      stay_organization == "4.Unlikely" ~ 2,
      stay_organization == "3.Not Sure" ~ 3,
      stay_organization == "2.Likely" ~ 4,
      stay_organization == "1.V Likely" ~ 5
    ),
    # Recode the un_manager_relationship column
    mgn_emp_relationship = case_when(
      mgn_emp_relationship == "5.V Bad" ~ 1,
      mgn_emp_relationship == "4.Bad" ~ 2,
      mgn_emp_relationship == "3.So So" ~ 3,
      mgn_emp_relationship == "2.Good" ~ 4,
      mgn_emp_relationship == "1.V Good" ~ 5
    ),
    # Recode the coworker_relationship column
    coworker_relationship = case_when(
      coworker_relationship == "5.V Bad" ~ 1,
      coworker_relationship == "4.Bad" ~ 2,
      coworker_relationship == "3.So So" ~ 3,
      coworker_relationship == "2.Good" ~ 4,
      coworker_relationship == "1.V Good" ~ 5
    ),
    # Recode the schooling column
    schooling = case_when(
      schooling == "4.Not Imp'tant" ~ 1,
      schooling == "3.Some Imp'tant" ~ 2,
      schooling == "2.Imp'tant" ~ 3,
      schooling == "1.V Imp'tant" ~ 4
    ),
    # Recode the training column
    training = case_when(
      training == "4.Not Imp'tant" ~ 1,
      training == "3.Some Imp'tant" ~ 2,
      training == "2.Imp'tant" ~ 3,
      training == "1.V Imp'tant" ~ 4
    ),
    # Recode the aware_industry_4_0 column
    aware_industry_4_0 = ifelse(aware_industry_4_0 == "Yes", "yes", "no")
  )

employee_data <- employee_data %>%
  mutate_at(
    vars(
      occupation,
      sex,
      job_characteristics,
      get_ahead,
      union_member,
      budget_participation,
      aware_industry_4_0,
      job_satisfaction,
      rich_work,
      future_promotion,
      sex_promotion,
      career_advancement,
      decision_involvement,
      stay_organization,
      proud_organization,
      mgn_emp_relationship,
      coworker_relationship,
      schooling,
      training
    ),
    as.factor
  ) %>%
  dplyr::select(
    -c(id_num)
  )

employee_data <- employee_data %>%
  distinct()

mean_age <- mean(employee_data$age, na.rm = TRUE)
std_age <- sd(employee_data$age, na.rm = TRUE)

processed_data <- employee_data %>%
  mutate(
    age_group = case_when(
      age < (mean_age - std_age) ~ "lower-aged",
      age >= (mean_age - std_age) & age <= (mean_age + std_age) ~ "middle-aged",
      age > (mean_age + std_age) ~ "higher-aged"
    ),
    earners_group = case_when(
      earners == 1 ~ "one",
      earners == 2 ~ "two",
      earners >= 3 ~ "three or more"
    ),
    education_group = case_when(
      education_years < 12 ~ "less hs",
      education_years == 12 ~ "hs grad",
      education_years >= 13 & education_years <= 16 ~ "college",
      education_years > 16 ~ "adv degree"
    )
  )

processed_data <- processed_data %>%
  mutate(
    age_group = factor(age_group,
      levels = c("lower-aged", "middle-aged", "higher-aged")
    ),
    earners_group = factor(earners_group,
      levels = c("one", "two", "three or more")
    ),
    education_group = factor(education_group,
      levels = c("less hs", "hs grad", "college", "adv degree")
    ),
    rich_work = recode_factor(
      factor(rich_work),
      `1` = "no",
      `2` = "unsure",
      `3` = "yes"
    ),
    rich_work = factor(rich_work, levels = c("no", "unsure", "yes")),
    sex_promotion = recode_factor(
      factor(sex_promotion),
      `1` = "worse",
      `2` = "no effect",
      `3` = "better"
    ),
    sex_promotion = factor(
      sex_promotion,
      levels = c("worse", "no effect", "better")
    ),
    decision_involvement = recode_factor(
      factor(decision_involvement),
      `1` = "never",
      `2` = "sometimes",
      `3` = "much of the time",
      `4` = "always"
    ),
    decision_involvement = factor(
      decision_involvement,
      levels = c("never", "sometimes", "much of the time", "always")
    ),
    job_satisfaction = recode_factor(
      factor(job_satisfaction),
      `1` = "very dissatisfied",
      `2` = "little dissatisfied",
      `3` = "moderately satisfied",
      `4` = "very satisfied"
    ),
    job_satisfaction = factor(
      job_satisfaction,
      levels = c(
        "very dissatisfied", "little dissatisfied",
        "moderately satisfied", "very satisfied"
      )
    ),
    has_promoted = ifelse(number_promotions > 0, "yes", "no"),
    has_promoted = as.factor(has_promoted),
    future_promotion = fct_collapse(
      future_promotion,
      "unlikely" = c("1", "2"),
      "unsure" = c("3"),
      "likely" = c("4", "5")
    ),
    career_advancement = fct_collapse(
      career_advancement,
      "no" = c("1", "2"),
      "yes" = c("3", "4")
    ),
    proud_organization = fct_collapse(
      proud_organization,
      "not proud/neutral" = c("1", "2"),
      "somewhat proud" = c("3"),
      "very proud" = c("4")
    ),
    stay_organization = fct_collapse(
      stay_organization,
      "unlikely" = c("1", "2"),
      "unsure" = c("3"),
      "likely" = c("4", "5")
    ),
    mgn_emp_relationship = fct_collapse(
      mgn_emp_relationship,
      "bad" = c("1", "2"),
      "so-so" = c("3"),
      "good" = c("4"),
      "very good" = c("5")
    ),
    coworker_relationship = fct_collapse(
      coworker_relationship,
      "bad" = c("1", "2"),
      "so-so" = c("3"),
      "good" = c("4"),
      "very good" = c("5")
    ),
    schooling = fct_recode(schooling,
      "unimportant" = "1",
      "somewhat important" = "2",
      "important" = "3",
      "very important" = "4"
    ),
    training = fct_recode(training,
      "unimportant" = "1",
      "somewhat important" = "2",
      "important" = "3",
      "very important" = "4"
    )
  )

remove_cols <- nearZeroVar(processed_data,
  names = TRUE,
  freqCut = 2, uniqueCut = 20
)

remove_cols

processed_data <- processed_data %>%
  dplyr::select(
    -c(
      working_hours, get_ahead, trauma,
      number_promotions, proud_organization
    )
  )

str(processed_data)
####################### Data Pre-Processing #######################

####################### Exploratory Data Analysis #######################

# Checking the distribution of engagement scores
ggplot(processed_data, aes(x = engagement)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Engagement Levels",
    x = "Engagement",
    y = "Frequency"
  ) +
  theme_minimal()

# Checking correlations between engagement and other numerical features
num_vars <- processed_data %>%
  dplyr::select(
    age, pre_tax_income, pre_tax_family_income, engagement, earners,
    working_years, education_years, employment_years,
  ) %>%
  cor()

corrplot(num_vars, type = "upper", order = "hclust")

# Select only categorical variables
categorical_vars <- processed_data %>%
  dplyr::select(
    job_satisfaction, occupation, training, sex, earners_group,
    education_group, rich_work, job_characteristics, union_member,
    has_promoted, future_promotion, sex_promotion,
    career_advancement, decision_involvement, stay_organization,
    mgn_emp_relationship, coworker_relationship, schooling,
    aware_industry_4_0, age_group, budget_participation
  )

# Initialize an empty matrix to store the results
n <- ncol(categorical_vars)
cramer_v_matrix <-
  matrix(NA, n, n,
    dimnames = list(names(categorical_vars), names(categorical_vars))
  )

# Calculate Cramér's V for all pairs of categorical variables
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    tryCatch({
      cramer_v_matrix[i, j] <-
        CramerV(categorical_vars[[i]], categorical_vars[[j]], conf.level = NA)
      cramer_v_matrix[j, i] <- cramer_v_matrix[i, j]
    }, error = function(e) {
      cramer_v_matrix[i, j] <- 0
      cramer_v_matrix[j, i] <- 0
    })
  }
}

diag(cramer_v_matrix) <- 1

corrplot(as.matrix(cramer_v_matrix),
         type = "upper", order = "hclust",
         tl.cex = 0.8,
         mar = c(0, 0, 2, 2))

run_aov_tests <- function(response_var, categorical_vars) {
  p_values <-
    data.frame(
      variable = character(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )

  for (cat_var in colnames(categorical_vars)) {
    formula <- as.formula(paste(response_var, "~", cat_var))
    aov_test <- aov(formula, data = processed_data)
    p_value <- summary(aov_test)[[1]][["Pr(>F)"]][1]
    p_values <- p_values %>%
      dplyr::add_row(variable = cat_var, p_value = p_value)
  }
  return(p_values)
}

p_values_age <- run_aov_tests("age", categorical_vars)

ggplot(
  p_values_age,
  aes(x = reorder(variable, -p_value), y = p_value)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 1) +
  coord_flip() +
  labs(
    title = "P-values from ANOVA tests (Age vs. Categorical Variables)",
    x = "Categorical Variables",
    y = "P-value"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

p_values_engagement <- run_aov_tests("engagement", categorical_vars)

ggplot(
  p_values_engagement,
  aes(x = reorder(variable, -p_value), y = p_value)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 1) +
  coord_flip() +
  labs(
    title = "P-values from ANOVA tests (Engagement vs. Categorical Variables)",
    x = "Categorical Variables",
    y = "P-value"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = -0.5, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

p_values_wrk_years <- run_aov_tests("working_years", categorical_vars)

ggplot(
  p_values_wrk_years,
  aes(x = reorder(variable, -p_value), y = p_value)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 1) +
  coord_flip() +
  labs(
    title = "P-values from ANOVA tests (Working Years vs. Categorical Variables)", #nolint
    x = "Categorical Variables",
    y = "P-value"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.margin = ggplot2::margin(t = 20, r = 50, b = 20, l = 20)
  )

job_char_counts <- processed_data %>%
  group_by(job_characteristics) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)


job_char_counts <- job_char_counts %>%
  arrange(desc(percentage)) %>%
  mutate(
    job_characteristics = factor(
      job_characteristics, levels = job_characteristics
    )
  )

ggplot(
  job_char_counts, aes(x = "", y = percentage, fill = job_characteristics)
) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(
    title = "Which one of the following job characteristics is most important to you?", #nolint
    fill = "Job Characteristics"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8,
            fontface = "bold") +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.01, size = 20),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.x = unit(0.3, "cm"),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10)
  )

ggsave("a2_photos/job_char_pie.jpg", width = 16, height = 16)

job_satisfaction_count <- processed_data %>%
  group_by(job_satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(
  job_satisfaction_count, aes(x = "", y = percentage, fill = job_satisfaction)
) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(
    title = "On the whole, how satisfied are you with your job?", #nolint
    fill = "Job Satisfaction"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 7.5,
            fontface = "bold") +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.x = unit(0.3, "cm"),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10)
  )

decision_involvement_count <- processed_data %>%
  group_by(decision_involvement) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(
  decision_involvement_count,
  aes(x = "", y = percentage, fill = decision_involvement)
) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(
    title = "Does your job allow you to take part in making decisions that affect your work?", #nolint
    fill = "Decision Involvement"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8,
            fontface = "bold") +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.01, size = 20),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.x = unit(0.3, "cm"),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10)
  )

mgn_emp_count <- processed_data %>%
  group_by(mgn_emp_relationship) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(
  mgn_emp_count,
  aes(x = "", y = percentage, fill = mgn_emp_relationship)
) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(
    title = "In general, how would you describe relations in your workplace between management and employees?", #nolint
    fill = "Relationship Level"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8,
            fontface = "bold") +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.25, size = 20),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.x = unit(0.3, "cm"),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10)
  )

coworker_count <- processed_data %>%
  group_by(coworker_relationship) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(
  coworker_count,
  aes(x = "", y = percentage, fill = coworker_relationship)
) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(
    title = "In general, how would you describe relations in your workplace between coworkers and colleagues?", #nolint
    fill = "Relationship Level"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8,
            fontface = "bold") +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.25, size = 20),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.x = unit(0.3, "cm"),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10),
    aspect.ratio = 1
  )

training_count <- processed_data %>%
  group_by(training) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(
  training_count,
  aes(x = "", y = percentage, fill = training)
) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  labs(
    title = "How important was your formal on-the-job training to the job you do now?", #nolint
    fill = "Importance of Training"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8,
            fontface = "bold") +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.001, size = 20),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.x = unit(0.3, "cm"),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10)
  )

ggsave("a2_photos/training_pie.jpg", width = 12, height = 10)
####################### Exploratory Data Analysis #######################

####################### Question 1 #######################
# Figure 1: Age Group vs Engagement
figure_1 <-
  ggplot(processed_data, aes(x = age_group, y = engagement, fill = age_group)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  labs(
    title = "Engagement by Age Group",
    subtitle = "Distribution of Engagement Scores Across Different Age Groups",
    x = "Age Group",
    y = "Engagement"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA)
  )

figure_1

# Figure 2: Age Group vs Working Years
figure_2 <- ggplot(
  processed_data,
  aes(x = age_group, y = working_years, fill = age_group)
) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  labs(
    title = "Working Years by Age Group",
    subtitle = "Distribution of Working Years Across Different Age Groups",
    x = "Age Group",
    y = "Working Years"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA)
  )

figure_2

# Figure 3: Age Group vs Employment Years
figure_3 <- ggplot(
  processed_data,
  aes(x = age_group, y = employment_years, fill = age_group)
) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  labs(
    title = "Employment Years by Age Group",
    subtitle = "Distribution of Employment Years Across Different Age Groups",
    x = "Age Group",
    y = "Employment Years"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA)
  )

figure_3

# Figure 4: Age Group vs Pre-Tax Income
figure_4 <- ggplot(
  processed_data,
  aes(x = age_group, y = pre_tax_income, fill = age_group)
) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  labs(
    title = "Pre-Tax Income by Age Group",
    subtitle = "Distribution of Pre-Tax Income Across Different Age Groups",
    x = "Age Group",
    y = "Pre-Tax Income"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA)
  )

figure_4

# Figure 5: Age Group vs Awareness of Industry 4.0
figure_5 <-
  ggplot(processed_data, aes(x = age_group, fill = aware_industry_4_0)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("yes" = "#0073C2FF", "no" = "#EFC000FF")) +
  labs(
    title = "Awareness of Industry 4.0 by Age Group",
    subtitle = "Comparison of Awareness Levels across Age Groups",
    x = "Age Group",
    y = "Count",
    fill = "Aware of Industry 4.0"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey85", linetype = "dashed")
  )

figure_5

# Figure 6: Age Group vs Future Promotion
figure_6 <-
  ggplot(processed_data, aes(x = age_group, fill = future_promotion)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "In the Next Five Years, How Likely Are You To Be Promoted?",
    subtitle = "Comparison of Promotion Likelihood across Age Groups",
    x = "Age Group",
    y = "Count",
    fill = "Promotion Likelihood"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey85", linetype = "dashed")
  )

figure_6
####################### Question 1 #######################

####################### Question 2 #######################
# Plot Working Years histogram
ggplot(processed_data, aes(x = working_years)) +
  geom_histogram(bins = 20, aes(fill = after_stat(count)), color = "black") +
  scale_fill_gradient(low = "navy", high = "skyblue") +
  labs(
    title = "Distribution of Working Years",
    x = "Working Years", y = "Frequency"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

ggplot(processed_data, aes(x = age, y = working_years)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Age and Working Years",
       x = "Age",
       y = "Working Years") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Working Years and Employment Years
ggplot(processed_data, aes(x = employment_years, y = working_years)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Employment Years and Working Years",
       x = "Employment Years",
       y = "Working Years") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Create the 3D scatterplot
s3d <- scatterplot3d(
  processed_data$age,
  processed_data$employment_years, processed_data$working_years,
  main = "3D Scatter Plot: Age, Employment Years, and Working Years",
  xlab = "Age",
  ylab = "Employment Years",
  zlab = "Working Years",
  pch = 16, color = "darkblue", grid = TRUE, box = FALSE,
  angle = 55, cex.symbols = 1.2
)
# Fit a linear model
fit <- lm(working_years ~ age + employment_years, data = processed_data)
# Add the regression plane to the plot
s3d$plane3d(
  fit, draw_polygon = TRUE, lty = "dotted", col = "lightgray", lwd = 2
)

model_1 <- lm(working_years ~ age, data = processed_data)
summary(model_1)
based_on_age <- predict(model_1)
# Create a new dataframe with predicted values based on the model
res_1 <- resid(model_1)
residuals_data <- data.frame(Fitted_Values = fitted(model_1), Residuals = res_1)

# Residual Plot
ggplot(residuals_data, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(alpha = 0.7, color = "#0073C2", size = 4) +
  geom_smooth(
    method = "loess", color = "darkorange",
    linetype = "solid", se = FALSE, linewidth = 1.2
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "red", linewidth = 1
  ) +
  labs(
    title = "Residuals vs Fitted (Working Years vs Age)",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, size = 22, color = "#34495E"
    ),
    axis.title.x = element_text(face = "bold", size = 18, color = "#34495E"),
    axis.title.y = element_text(face = "bold", size = 18, color = "#34495E"),
    axis.text = element_text(size = 14, color = "#34495E"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(15, 15, 15, 15)
  )

# Working year ~ Employment Years model
model_2 <- lm(working_years ~ employment_years, data = processed_data)
summary(model_2)

# Create Residual plots: model 2
res_2 <- resid(model_2)
residuals_data <- data.frame(Fitted_Values = fitted(model_2), Residuals = res_2)

ggplot(residuals_data, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(alpha = 0.7, color = "#0073C2", size = 4) +
  geom_smooth(
    method = "loess", color = "darkorange",
    linetype = "solid", se = FALSE, linewidth = 1.2
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "red", linewidth = 1
  ) +
  labs(
    title = "Residuals vs Fitted (Working Years vs Employment Years)",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, size = 22, color = "#34495E"
    ),
    axis.title.x = element_text(face = "bold", size = 18, color = "#34495E"),
    axis.title.y = element_text(face = "bold", size = 18, color = "#34495E"),
    axis.text = element_text(size = 14, color = "#34495E"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(15, 15, 15, 15)
  )

ggplot(processed_data, aes(x = age, y = working_years)) +
  geom_point(alpha = 0.8, size = 3, shape = 19) +
  geom_line(aes(y = based_on_age), linewidth = 1.5, linetype = "solid") +
  labs(
    title = "Predicted vs Actual Working Years by Age",
    x = "Age",
    y = "Working Years",
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, size = 22, color = "#333333"
    ),
    axis.title.x = element_text(face = "bold", size = 16, color = "#333333"),
    axis.title.y = element_text(face = "bold", size = 16, color = "#333333"),
    axis.text.x = element_text(size = 14, color = "#333333"),
    axis.text.y = element_text(size = 14, color = "#333333"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

model_3 <- lm(working_years ~ age * sex, data = processed_data)
summary(model_3)

# Calculate the residuals from model_3
residuals_model_3 <- residuals(model_3)

# Create the histogram using ggplot2
ggplot(data = data.frame(residuals_model_3), aes(x = residuals_model_3)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Residuals (Model 3)",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

# Create Residual plots: model 2
res_3 <- resid(model_3)
residuals_data <- data.frame(
  Fitted_Values = fitted(model_3), Residuals = res_3
)

# Residual Plot
ggplot(
  residuals_data, aes(x = Fitted_Values, y = Residuals)
) +
  geom_point(alpha = 0.7, color = "#0073C2", size = 4) +
  geom_smooth(
    method = "loess", color = "darkorange",
    linetype = "solid", se = FALSE, linewidth = 1.2
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "red", linewidth = 1
  ) +
  labs(
    title = "Residuals vs Fitted (Multiple Model)",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, size = 22, color = "#34495E"
    ),
    axis.title.x = element_text(face = "bold", size = 18, color = "#34495E"),
    axis.title.y = element_text(face = "bold", size = 18, color = "#34495E"),
    axis.text = element_text(size = 14, color = "#34495E"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(15, 15, 15, 15)
  )

# Coefficients from the linear model
coefficients <- coef(model_3)

# Intercept and slopes for females (sex = female)
intercept_female <- coefficients["(Intercept)"]
slope_female <- coefficients["age"]

# Intercept and slopes for males (sex = male)
intercept_male <- coefficients["(Intercept)"] + coefficients["sexmale"]
slope_male <- coefficients["age"] + coefficients["age:sexmale"]

# Display the formulas
cat(
  "Female Formula: working_years =",
  intercept_female, "+", slope_female, "* age\n"
)
cat(
  "Male Formula: working_years =",
  intercept_male, "+", slope_male, "* age\n"
)

# Create a new data frame with the input for prediction
new_data <- data.frame(age = c(40, 40), sex = c("female", "male"))

# Predict working years based on the model
predicted_working_years <- predict(model_3, newdata = new_data)

# Display the predictions
predicted_working_years

predicted_multiple <- predict(model_3)

ggplot(processed_data, aes(x = age, y = working_years, color = sex)) +
  geom_point(alpha = 0.8, size = 3, shape = 19) +
  geom_line(aes(y = predicted_multiple), linewidth = 1.5, linetype = "solid") +
  scale_color_manual(values = c("female" = "#E41A1C", "male" = "#377EB8")) +
  labs(
    title = "Predicted vs Actual Working Years by Age and Gender",
    x = "Age",
    y = "Working Years",
    color = "Gender"
  ) +
  facet_grid(. ~ sex, labeller = labeller(sex = label_both), space = "free_x") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, size = 22, color = "#333333"
    ),
    axis.title.x = element_text(face = "bold", size = 16, color = "#333333"),
    axis.title.y = element_text(face = "bold", size = 16, color = "#333333"),
    axis.text.x = element_text(size = 14, color = "#333333"),
    axis.text.y = element_text(size = 14, color = "#333333"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.text.x = element_text(size = 16, face = "bold", color = "#333333"),
    strip.background = element_rect(fill = "gray95", color = "gray90"),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

####################### Question 2 #######################

####################### Question 3 #######################
# Engagement Descriptive Statistics
engagement_stat <- processed_data %>%
  summarise(
    mean = mean(engagement, na.rm = TRUE),
    median = median(engagement, na.rm = TRUE),
    sd = sd(engagement, na.rm = TRUE),
    min = min(engagement, na.rm = TRUE),
    max = max(engagement, na.rm = TRUE)
  )
print(engagement_stat)

# Engagement Histogram
ggplot(processed_data, aes(x = engagement)) +
  geom_histogram(bins = 20, aes(fill = after_stat(count)), color = "black") +
  scale_fill_gradient(low = "navy", high = "skyblue") +
  labs(
    title = "Distribution of Employee Engagement Levels",
    x = "Engagement Levels",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, size = 20, color = "#333333"
    ),
    axis.title.x = element_text(face = "bold", size = 16, color = "#333333"),
    axis.title.y = element_text(face = "bold", size = 16, color = "#333333"),
    axis.text = element_text(size = 14, color = "#333333"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

# Employment Years and Engagement
ggplot(
  processed_data,
  aes(x = employment_years, y = engagement)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              color = "darkblue", linewidth = 1.5) +
  labs(
    title = "Relationship between Employment Years and Engagement",
    x = "Employment Years",
    y = "Engagement"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, size = 20, color = "#333333"
    ),
    axis.title.x = element_text(face = "bold", size = 16, color = "#333333"),
    axis.title.y = element_text(face = "bold", size = 16, color = "#333333"),
    axis.text = element_text(size = 14, color = "#333333"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

# Multiple Linear Regression
# Build the initial model for data
full_model <- lm(engagement ~ . - over_15_emp_years, data = processed_data)
summary(full_model)
# Check for multicollinearity using VIF
vif_values <- vif(full_model)
print(vif_values)
# Stepwise Model
step_model <- step(full_model, direction = "both")
summary(step_model)
print(step_model)
# Random Forest
rf_model <- randomForest(
  engagement ~ rich_work + working_years + union_member +
    employment_years + career_advancement + decision_involvement +
    mgn_emp_relationship + aware_industry_4_0, data = processed_data
)
importance(rf_model)
varImpPlot(rf_model)
# Adjusted Linear Reg model
data_reg <- lm(
  engagement ~ employment_years + union_member + rich_work,
  data = processed_data
)
summary(data_reg)
print(data_reg)
# Check for multicollinearity using VIF
vif_values <- vif(data_reg)
print(vif_values)
#### PREDICTION
# Predict engagement using the adjusted model
predicted_engagement <- predict(data_reg)
# Create a summary of predicted engagement
summary(predicted_engagement)

# Create the scatter plot
ggplot(processed_data, aes(x = employment_years, y = engagement)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(
    aes(y = predicted_engagement), method = "lm",
    se = FALSE, color = "#0073C2FF", linetype = "solid", linewidth = 1
  ) +
  labs(
    title = "Comparison of Actual and Predicted Engagement vs Employment Years",
    subtitle = "Impact of Employment Years, Union Membership and Desire to Continue Working After Becoming Rich", #nolint
    x = "Employment Years",
    y = "Engagement",
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
    axis.title.x = element_text(face = "bold", size = 16, color = "#333333"),
    axis.title.y = element_text(face = "bold", size = 16, color = "#333333"),
    axis.text.x = element_text(size = 14, color = "#333333"),
    axis.text.y = element_text(size = 14, color = "#333333"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )
####################### Question 3 #######################

####################### Question 4 #######################
ggplot(processed_data, aes(x = over_15_emp_years, y = engagement)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  coord_flip() +
  labs(
    x = "Over 15 Employment Years",
    y = "Engagement"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA)
  )

processed_data$aware_industry_4_0 <-
  relevel(processed_data$aware_industry_4_0, ref = "yes")

# Create a summary of the counts for each level of aware_industry_4_0
aware_counts <- processed_data %>%
  group_by(aware_industry_4_0) %>%
  summarise(count = n())

# Create a professional pie chart
ggplot(aware_counts, aes(x = "", y = count, fill = aware_industry_4_0)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = pi) +
  scale_fill_manual(values = c("yes" = "blue", "no" = "red")) +
  labs(
    title = "Awareness of Industry 4.0 among Employees",
    fill = "Awareness Level"
  ) +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8,
            fontface = "bold") +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.x = unit(0.3, "cm"),
    plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10)
  )

# Split the data into training and testing sets
log_1 <- glm(
  over_15_emp_years ~ engagement + working_years + training +
    schooling + career_advancement + aware_industry_4_0,
  data = processed_data,
  family = "binomial"
)

# Create a new data frame for the two employees
new_employees <- data.frame(
  engagement = c(1, 5),
  working_years = c(20, 20),
  training = factor("very important", levels = levels(processed_data$training)),
  schooling = factor(
    "very important", levels = levels(processed_data$schooling)
  ),
  career_advancement = factor(
    "yes", levels = levels(processed_data$career_advancement)
  ),
  aware_industry_4_0 = factor(
    "yes", levels = levels(processed_data$aware_industry_4_0)
  )
)

# Calculate the predicted probabilities
predicted_probabilities <-
  predict(log_1, newdata = new_employees, type = "response")

# Convert the probabilities to percentages
predicted_percentages <- predicted_probabilities * 100

# Print the predicted percentages
predicted_percentages

summary(log_1)

logLik(log_1)

anova(log_1)

exp(coef(log_1))

confint(log_1)

predicted_probabilities <- predict(log_1, type = "response")

predicted_classes <- ifelse(predicted_probabilities > 0.5, "yes", "no")

confusion_matrix <-
  confusionMatrix(
    as.factor(predicted_classes),
    as.factor(processed_data$over_15_emp_years),
    positive = "yes"
  )

print(confusion_matrix)

# Create ROC object
roc_obj <- roc(processed_data$over_15_emp_years, predicted_probabilities)
auc_value <- round(roc_obj$auc, 4)
# Plot ROC curve
ggroc(roc_obj, legacy.axes = TRUE) +
  ggtitle(paste("ROC Curve for Logistic Model - AUC:", round(auc_value, 3))) +
  xlab("False Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(linewidth = 0.8, color = "grey90"),
    panel.grid.minor = element_blank()
  )

plot_4 <- plot_model(log_1,
  type = "pred",
  terms = c("engagement [all]", "training", "aware_industry_4_0"),
  ci.lvl = NA,
  line.size = 1.5,
) +
  labs(
    y = "Probability of Over 15 Employment Years",
    x = "Engagement",
    title = "Predicted Probability of Long-Term Employment",
    subtitle = "Influence of Engagement and Importance of Formal Training Between Awareness Levels Of Industry 4.0", # nolint
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(title = "Importance of Formal Training"))

plot_3 <- plot_model(log_1,
  type = "pred",
  terms = c("engagement [all]", "training"),
  ci.lvl = NA,
  line.size = 1.5,
) +
  labs(
    y = "Probability of Over 15 Employment Years",
    x = "Engagement",
    title = "Predicted Probability of Long-Term Employment",
    subtitle = "Influence of Engagement and Importance of Formal Training", # nolint
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(title = "Importance of Formal Training"))

report <- tbl_regression(log_1, exponentiate = TRUE)

report

report_tbl <- as_gt(report)
####################### Question 4 #######################