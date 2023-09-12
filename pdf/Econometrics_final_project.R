setwd('/Users/teddyfoley/Desktop/metrics')
library('tidyverse')
library(ggplot2)
library(sjPlot)
library(stargazer)
library(estimatr)
library(texreg)

#Part 1

hds <- c('hd2011.csv','hd2012.csv','hd2013.csv','hd2014.csv','hd2015.csv','hd2016.csv')
sfas <- c('sfa1011.csv','sfa1112.csv','sfa1213.csv','sfa1314.csv','sfa1415.csv','sfa1516.csv')

data <- data.frame(matrix(ncol = 0, nrow = 0))
for (x in 1:length(hds)){
  hd <- tibble(read_csv(hds[x]))
  sfa <- tibble(read_csv(sfas[x]))
  temp_data <- inner_join(hd, sfa, by = "UNITID") 
  temp_data <- add_column(temp_data, 'year' = 2009+x, .before = 'UNITID')
  if(nrow(data) < 1){
    data <- temp_data
  } else{
    data <- bind_rows(data, temp_data)
  }
}

#Part 2

num_years <- length(unique(data$year))

data <- data %>%
  arrange(year, UNITID) %>%
  filter(STABBR == "TN") %>%
  filter(ICLEVEL == 1 | ICLEVEL == 2) %>%
  filter(CYACTIVE == 1) %>%
  group_by(UNITID) %>%
  mutate(row_count = n()) %>%
  filter(row_count == num_years) %>%
  ungroup() %>%
  select(-row_count) %>%
  rename(ID_IPEDS = UNITID) %>%
  rename(enroll_ftug = SCUGFFN) %>%
  rename(grant_federal = FGRNT_T) %>%
  rename(grant_state = SGRNT_T) %>%
  mutate(degree_bach = case_when(ICLEVEL == 1 ~ 1, ICLEVEL != 1 ~ 0)) %>%
  mutate(public = case_when(CONTROL == 1 ~ 1, CONTROL != 1 ~ 0)) %>%
  group_by(ID_IPEDS) %>%
  mutate(change = sum(public)) %>%
  filter(change %% 6 == 0) %>%
  ungroup() %>%
  select(-change) %>%
  select(year, ID_IPEDS, degree_bach, public, enroll_ftug, grant_state, grant_federal)
#Drop institutions that only offer graduate degrees


#Part 3

public2 <- NULL
public4 <- NULL
private2 <- NULL
private4 <- NULL
total <- NULL
for (x in 1:6){
  year_data = filter(data, year == 2009+x)
  public2 <- pull(summarize(filter(year_data, public == 1 & degree_bach == 0),mean(grant_state)))
  public4 <- pull(summarize(filter(year_data, public == 1 & degree_bach == 1),mean(grant_state)))
  private2 <- pull(summarize(filter(year_data, public == 0 & degree_bach == 0),mean(grant_state)))
  private4 <- pull(summarize(filter(year_data, public == 0 & degree_bach == 1),mean(grant_state)))
  total <- c(total,public2,public4,private2,private4)
}

df <- data.frame(years=rep(c(2010,2011,2012,2013,2014,2015),each=4),
                 name=rep(c("Public 2","Public 4","Private 2","Private 4"),6),
                 avgs = total)

#graph generated for HW
ggplot(data=df, aes(y=avgs, x=years)) + 
  labs(title="Funding Across Different Types of Universities", 
       y = "Average State + Local Funding", x = "Year") + 
  geom_line(aes(colour=name))


#Part 4
#Specify a regression model of your choice that you believe will generate the 
#most accurate estimate of the causal effect of the Tennessee Promise program on 
#enrollment at public, two-year colleges. Estimate this model and present your 
#findings in a table.

#USE DIFF IN DIFF
data$pub2 = ifelse(data$public == 1 & data$degree_bach == 0, 1, 0)
data$pub4 = ifelse(data$public == 1 & data$degree_bach == 1, 1, 0)
data$priv2 = ifelse(data$public == 0 & data$degree_bach == 0, 1, 0)
data$priv4 = ifelse(data$public == 0 & data$degree_bach == 1, 1, 0)
data$time= ifelse(data$year>= 2015, 1, 0)
data$did= data$time*data$pub2

did_data = filter(data, pub2+priv2 == 1)

didreg= lm(enroll_ftug ~ pub2 + time + did, did_data)
summary(didreg)
stargazer(didreg, title="Regression Results", type="latex")

#Part 5 - Discussion, no code

#Part 6

did_data1415 = filter(did_data, year == 2014 | year == 2015)
basic_reg = lm(enroll_ftug ~ pub2 + time + did, data=did_data1415)
robust_reg = lm_robust(enroll_ftug ~ pub2 + time + did, se_type='HC2', data=did_data1415)
cluster_reg = lm_robust(enroll_ftug ~ pub2 + time + did, clusters = ID_IPEDS, se_type='CR0', data=did_data1415)

summary(basic_reg)
summary(robust_reg)
summary(cluster_reg)

texreg(list(robust_reg,cluster_reg), include.ci = FALSE)


#Part 7
# Duplicate your dataset, generating a new college ID which takes on a different 
# value for the same college in the two copies. Estimate the same regression and 
# report in the same table as in (a) your estimate of β with both a 
# heteroskedasticity-robust standard error and a standard error that clusters by 
# the new college ID. Compute the ratio of standard errors on D_it here relative 
# to those you obtained in (a). Are you surprised by the answer?

data2 <- did_data1415
data2$ID_IPEDS <- data2$ID_IPEDS + 1000000
bigdata <- bind_rows(did_data1415, data2)

summary(basic_reg)
summary(robust_reg)
summary(cluster_reg)

texreg(list(robust_reg,cluster_reg), include.ci = FALSE)

#Part 8
# Estimate the same regression in the duplicated dataset but now cluster the 
# standard errors by the original college ID. Compare this standard error to the 
# one you obtained in part 8. You do not need to add anything to your regression 
# table for this question.

bigdata$ID_IPEDS <- bigdata$ID_IPEDS%%1000000
basic_reg = lm(enroll_ftug ~ pub2 + time + did, data=bigdata)
robust_reg = lm_robust(enroll_ftug ~ pub2 + time + did, se_type='HC2', data=bigdata)
cluster_reg = lm_robust(enroll_ftug ~ pub2 + time + did, clusters = ID_IPEDS, se_type='CR0', data=bigdata)

summary(basic_reg)
summary(robust_reg)
summary(cluster_reg)

texreg(list(robust_reg,cluster_reg), include.ci = FALSE)

#Part 9
# Return to the original, non-duplicated dataset from part 8. Run a simulation, 
# similar to that from the Bertrand et al. (2004) paper we discussed in class, 
# which randomly re- assigns Publici across colleges (holding Yit fixed). 
# Re-estimate the regression in each of these “placebo samples,” with both 
# heteroskedasticity-robust and clustered standard errors, and record whether 
# you can reject the null hypothesis of β = 0 at the 1%, 5%, and 10% confidence 
# level in each. Repeat this simulation 500 times and report in a new table the 
# average rejection rates for the two standard error estimators and three 
# confidence levels. What do you conclude?

t_robust = NULL
t_cluster = NULL
for (x in 1:500){
  reassigned_data <- did_data1415 %>%
    group_by(ID_IPEDS) %>%
    mutate(public = sample(0:1,1))
  reassigned_data$did= reassigned_data$time*reassigned_data$public
  robust_reg2 = lm_robust(enroll_ftug ~ pub2 + time + did, se_type='HC2', data=reassigned_data)
  cluster_reg2 = lm_robust(enroll_ftug ~ pub2 + time + did, clusters = ID_IPEDS, se_type='CR0', data=reassigned_data)
  
  robust_coefficients <- tidy(coef(summary(robust_reg2))["did", ])
  cluster_coefficients <- tidy(coef(summary(cluster_reg2))["did", ])
  
  t_robust <- c(t_robust, sapply(robust_coefficients[2], as.numeric)[3])
  t_cluster <- c(t_cluster, sapply(cluster_coefficients[2], as.numeric)[3])
}

#put all this in latex table
length(which(abs(t_robust) > 2.326 )) #1%
length(which(abs(t_robust) > 1.645 )) #5%
length(which(abs(t_robust) > 1.282)) #10%
mean(t_robust) #mean

length(which(abs(t_cluster) > 2.326 )) #1%
length(which(abs(t_cluster) > 1.645 )) #5%
length(which(abs(t_cluster) > 1.282)) #10%
mean(t_cluster) #mean


#Part 10
# Transform the non-duplicated dataset to be in first-differences: construct a 
# new dataset where each row corresponds to a college and one of the variables 
# is the difierence in Yit between 2014 and 2015 (call this △Yi). Estimate the 
# bivariate regression of △Yi on Publici with heteroskedasticity-robust standard 
# errors and report this in the table from above. Dis- cuss how this estimate 
# and standard error compares with those in estimated in the non-first 
# differenced dataset.

stats2014 <- filter(did_data1415, did_data1415$year == 2014)

diff_data <- filter(did_data1415, did_data1415$year == 2015)
diff_data$grant_state <- diff_data$grant_state - stats2014$grant_state
diff_data$enroll_ftug <- diff_data$enroll_ftug - stats2014$enroll_ftug

diff_reg = lm_robust(enroll_ftug ~ public, se_type = 'HC2', diff_data)
summary(diff_reg)

texreg(diff_reg, include.ci = FALSE)

#Part 11
# Repeat your simulation exercise on this first-differenced specification, using 
# heteroskedasticity- robust standard errors. Report the average rejection rates 
# in the same table as in (d). Discuss your findings: why do you get difierent 
# answers here than you did before with heteroskedasticity-robust standard errors?

t_diff_robust = NULL
for (x in 1:500){
  reassigned_data <- diff_data %>%
    group_by(ID_IPEDS) %>%
    mutate(public = sample(0:1,1))
  reassigned_data$did= reassigned_data$time*reassigned_data$public
  robust_reg3 = lm_robust(enroll_ftug ~ public, se_type='HC2', data=reassigned_data)
  robust_coefficients <- tidy(coef(summary(robust_reg3))["public", ])
  t_diff_robust <- c(t_diff_robust, sapply(robust_coefficients[2], as.numeric)[3])
}

#put all this in latex table
length(which(abs(t_diff_robust) > 2.326 )) #1%
length(which(abs(t_diff_robust) > 1.645 )) #5%
length(which(abs(t_diff_robust) > 1.282)) #10%
mean(t_diff_robust) #mean






