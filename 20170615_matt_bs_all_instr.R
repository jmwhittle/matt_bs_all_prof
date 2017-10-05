library(tidyverse); theme_set(theme_minimal())
library(readxl)
library(lme4)
library(plotly)


######################### DATA #########################
data  <- read_xlsx("100118gradeanalysisbyinstructor.xlsx")

# this ended up being wrong too.
# this is the cum_gpa field that brandon and abbey fixed for this study. 
data.2 <- read_csv("grade_analysis_by_instructor_20170727.csv") %>%
 select(ACADEMIC_PERIOD, STUDENT_ID, CRN, PRIOR_CUM_GPA)

# cleaning variables
data$term <- data$`Academic Period`
data$student_id <- data$ID
data$white <- ifelse(data$Ethnicity == "Caucasian", 1, 0)
data$first_gen <- ifelse(data$`First Generation` == "Y", 1, 0)
data$vetern <- ifelse(data$`Veteran Indicator` == "Y", 1, 0)
data$pell <- ifelse(data$`Pell Recipient` == "Y", 1, 0)
data$male <- ifelse(data$Gender == "Male", 1, 0)
data$term_age <- data$`Age at End of Course`
data$online <- ifelse(data$Campus == "SLCC Online", 1, 0)
data$passed <- ifelse(data$`Higher than 'C-'` == "Y", 1, 0)
data$term_grade <- data$`Final Grade`
data$credit_attempted <- data$`Total Registered Credits in Term`
data$instructor_id <- data$`Instructor ID`
data$course_id <- data$`Course Identification`

data.2$ACADEMIC_PERIOD <- as.character(data.2$ACADEMIC_PERIOD)
data.2$CRN <- as.character(data.2$CRN)

data <- data %>% 
  left_join(data.2, by = c("Academic Period" = "ACADEMIC_PERIOD", 
                           "ID" = "STUDENT_ID",
                           "CRN" = "CRN"))

# This was based on the misspecified field in the student_term table that marie/rochelle are fixing
#gpa_data <- read_tsv("cum_gpa_addition_20170726.tsv")
#gpa_data$ENROLL_TERM <- as.character(gpa_data$ENROLL_TERM)
#data <- data %>% left_join(gpa_data, by = c("ID" = "BANNER_ID", "term" = "ENROLL_TERM"))

##### creating a gpa factor variable
data$grade_bin <- ifelse(is.na(data$PRIOR_CUM_GPA) == T, "no_cum_gpa",
                         ifelse(data$PRIOR_CUM_GPA > 2.99999, "3+", 
                                ifelse(data$PRIOR_CUM_GPA > 1.99999, "2-3", "not passing")))
# data %>% group_by(instructor_id) %>% tally() %>% View()
# data %>% group_by(instructor_id) %>% 
#   select(instructor_id, term_grade) %>%
#   ggplot() + 
#   geom_bar(aes(as.factor(term_grade), col = instructor_id)) + 
#   theme_minimal()  + 
#   guides(color = "none") 

# have to drop the "W" otherwise classes that have been canceled sneak into the data. 
# ended up keeping 'course_id != "FIN2220"' since it still resulted in to fails and the 
# instructor has over 1800 enteries.
mlm.data <- data %>% filter(term_grade != "W" & is.na(instructor_id) == FALSE) %>% 
  dplyr::select(instructor_id, course_id, student_id, 
                                   white, male, term_age,
                                   vetern, pell, online, 
                                   credit_attempted, term, term_grade, passed, PRIOR_CUM_GPA, grade_bin) 
# 157255 student/course records. from spring 2010 to fall 2016. with 359 unique instructor ids
# 281 unique course. CIS 1020 and FIN 1050 were the most represented with 17621 and 14883 unique
# student/course records. student/course records with a "W" were removed (this makes pass "rates" look
# better). The removal of "W" was necessary since some courses had only "W" leading me to believe that
# these courses were cancelled for some reason. 


############################## ADDING ADJUNCT INFO #############################

# requesting from HR
# hr_request <- mlm.data %>% 
# group_by(instructor_id, term) %>% 
#   tally() %>% 
#   dplyr::select(instructor_id, term)
# write_csv(hr_request, "BU_instructors.csv")

# working with debbie's data


############################## MODELS ##########################################
#simple MLM with only an instructor level plus covariates 
# glmer.1L <- glmer(passed ~ white + male + term_age + 
#                    pell + online + credit_attempted + 
#                      (1|instructor_id), data = mlm.data, family = binomial(link = "logit"))
# 
# saveRDS(glmer.1L, "glmer_1.rds")
# glmer.1L <- readRDS("glmer_1.rds")
# summary(glmer.1L)
# lme4::coef(glmer.1L) # combined models, or individual models for the combinations of levels.
# lme4::fixef(glmer.1L) # the unmodeled coefficents. 
# lme4::ranef(glmer.1L) # the modeled instructor-level errors. the adjustment need to fixef coefficents to get back to coef output.
# library(arm)
# adding a course level to glmer.1L
# glmer.2L <- glmer(passed ~ rescale(white) + rescale(male) + rescale(term_age) +
#                     rescale(pell) + rescale(online) + rescale(credit_attempted) +
#                     as.factor(grade_bin) +
#                     (1|course_id) + (1|instructor_id),
#                   data = mlm.data, family = binomial(link = "logit"))
# saveRDS(glmer.2L, "glmer_2.rds")
glmer.2L <- readRDS("glmer_2.rds")

summary(glmer.2L)
lme4::coef(glmer.2L) # combined models, or individual models for the combinations of levels.
lme4::fixef(glmer.2L) # the unmodeled coefficents. 
lme4::ranef(glmer.2L) #

rand.ef.m2 <- lme4::ranef(glmer.2L, condVar = T)
# plot.2.in <- plot(rand.ef.m2$instructor_id$`(Intercept)`)
# plot.2.in
# abline(h=mean(rand.ef.m2$instructor_id$`(Intercept)`), lty = 1)
# abline(h=(mean(rand.ef.m2$instructor_id$`(Intercept)`) + 2*sd(rand.ef.m2$instructor_id$`(Intercept)`)), lty = 3)
# abline(h=(mean(rand.ef.m2$instructor_id$`(Intercept)`) - 2*sd(rand.ef.m2$instructor_id$`(Intercept)`)), lty = 3)
# 
# plot.2.ci <- plot(rand.ef.m2$course_id$`(Intercept)`)
# plot.2.ci
# abline(h=mean(rand.ef.m2$course_id$`(Intercept)`), lty = 1)
# abline(h=(mean(rand.ef.m2$course_id$`(Intercept)`) + 2*sd(rand.ef.m2$course_id$`(Intercept)`)), lty = 3)
# abline(h=(mean(rand.ef.m2$course_id$`(Intercept)`) - 2*sd(rand.ef.m2$course_id$`(Intercept)`)), lty = 3)

# histogram - ggplot - plotly
# gghist.2l.inst <- rand.ef.m2$instructor_id %>% ggplot() + 
#   geom_histogram(aes(`(Intercept)`)) + 
#   theme_minimal() + 
#   geom_vline(xintercept =mean(rand.ef.m2$instructor_id$`(Intercept)`),
#              color = "Red") + 
#   geom_vline(xintercept =mean(rand.ef.m2$instructor_id$`(Intercept)`) + 2*sd(rand.ef.m2$instructor_id$`(Intercept)`)) + 
#   geom_vline(xintercept =mean(rand.ef.m2$instructor_id$`(Intercept)`) - 2*sd(rand.ef.m2$instructor_id$`(Intercept)`)) + 
#   geom_vline(xintercept =mean(rand.ef.m2$instructor_id$`(Intercept)`) + sd(rand.ef.m2$instructor_id$`(Intercept)`),
#              color = "Blue") + 
#   geom_vline(xintercept =mean(rand.ef.m2$instructor_id$`(Intercept)`) - sd(rand.ef.m2$instructor_id$`(Intercept)`),
#              color = "Blue")
# 
# ggplotly(gghist.2l.inst)

# instructor as a random variable while fixing course_id. 
# should compare these two distribution to each other. 
# also this breaks R...
# glmer.1L.cf <- glmer(passed ~ rescale(white) + rescale(male) + rescale(term_age) + 
#                        rescale(pell) + rescale(online) + rescale(credit_attempted) + as.factor(course_id) +
#                        (1|instructor_id), data = mlm.data, family = binomial(link = "logit"))
# 
# lme4::coef(glmer.1L.cf) # combined models, or individual models for the combinations of levels.
# lme4::fixef(glmer.1L.cf) # the unmodeled coefficents. 
# lme4::ranef(glmer.1L.cf) #
# After talking to jessie and keith I don't think this is necessary.
# it is unlikely 'one off' classes will impact the random effects for instructors much. 

# testing this method: https://stackoverflow.com/questions/27787875/options-for-caterpillar-plots-in-lme4-grouping-by-factor-to-visually-identify-t
#################### instructor level random effects intercepts ###############
#################### caterpilar plots #########################################
library(boot)
rr1 <- rand.ef.m2
rr2 <- data.frame(instructor = rownames(rr1[[1]]), 
                  intercept = unname(rr1[[1]]), 
                  se = sqrt(c(attr(rr1[[1]], "postVar")))) %>% 
  arrange(desc(intercept))

rr2$instructor <- factor(rr2$instructor, levels = rr2$instructor[order(rr2$intercept)])

class.n <- mlm.data %>% group_by(instructor_id) %>% tally()
class.n$instructor <- as.factor(class.n$instructor_id)

rr3 <- rr2 %>% left_join(class.n, by = "instructor")
rr3$instructor <- factor(rr3$instructor, levels = rr3$instructor[order(rr3$intercept)])
rr3$intercept.fere <- inv.logit(rr3$intercept + glmer.2L@beta[1])
rr3$ymin <- inv.logit((rr3$intercept + glmer.2L@beta[1]) -1.96*rr3$se)
rr3$ymax <- inv.logit((rr3$intercept + glmer.2L@beta[1]) +1.96*rr3$se)

rr.sd <- sd(inv.logit(rr3$intercept + glmer.2L@beta[1]))
rr.mean <- mean(inv.logit(rr3$intercept + glmer.2L@beta[1]))

## caterpilar plots
caterpiler.plot <- rr3 %>% 
  ggplot(aes(x =instructor, 
             y = intercept.fere, 
             ymin=ymin, 
             ymax=ymax)) + 
  geom_pointrange(aes(col = n)) + 
   geom_hline(yintercept = rr.mean) + 
   geom_hline(yintercept = rr.mean + 1.96*rr.sd) + 
   geom_hline(yintercept = rr.mean - 1.96*rr.sd) + 
   theme(axis.text.x=element_blank(), 
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_blank(),
         panel.background = element_blank()) + 
  labs(col = "Number of students") +
  geom_text(aes(y = rr.mean - 1.96*rr.sd, label="-2 SD", x = 77)) + 
  geom_text(aes(y = rr.mean + 1.96*rr.sd, label="+2 SD", x = 75)) +
  geom_text(aes(y = rr.mean, label="Mean", x = 12)) + ylab("") + ggtitle("Caterpillar plots for all instructors")
caterpiler.plot
saveRDS(caterpiler.plot, "caterpiler_plot.rds")

#ggplotly(caterpiler.plot, height = 1000, width = 1000)

# max to 1000
cat.1k <- rr3 %>% filter(n > 999) %>%
  ggplot(aes(x =instructor, 
             y = intercept.fere, 
             ymin=ymin, 
             ymax=ymax)) + 
  geom_pointrange(aes(col = n)) + 
  geom_hline(yintercept = rr.mean) + 
  geom_hline(yintercept = rr.mean + 1.96*rr.sd) + 
  geom_hline(yintercept = rr.mean - 1.96*rr.sd) + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) + 
  labs(col = "Number of students") +
  geom_text(aes(y = rr.mean - 1.96*rr.sd + .01, label="-2 SD", x = 1)) + 
  geom_text(aes(y = rr.mean + 1.96*rr.sd + .01, label="+2 SD", x = 1)) +
  geom_text(aes(y = rr.mean + .01, label="Mean", x = 1)) + ylab("") + ggtitle("Instructors with over 1000 student records")
cat.1k
saveRDS(cat.1k, "cat_1k.rds")

#ggplotly(cat.1k)


# 999 to 500
cat.500 <- rr3 %>% filter(n < 1000 & n > 499) %>% 
  ggplot(aes(x =instructor, 
             y = intercept.fere, 
             ymin=ymin, 
             ymax=ymax)) + 
  geom_pointrange(aes(col = n)) + 
  geom_hline(yintercept = rr.mean) + 
  geom_hline(yintercept = rr.mean + 1.96*rr.sd) + 
  geom_hline(yintercept = rr.mean - 1.96*rr.sd) + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) + 
  labs(col = "Number of students") +
  geom_text(aes(y = rr.mean - 1.96*rr.sd + .01, label="-2 SD", x = 1)) + 
  geom_text(aes(y = rr.mean + 1.96*rr.sd + .01, label="+2 SD", x = 1)) +
  geom_text(aes(y = rr.mean + .01, label="Mean", x = 1)) + ylab("") + ggtitle("Instructors with 500-999 student records")
cat.500
saveRDS(cat.500, "cat_500.rds")

#ggplotly(cat.500)

#499 to 100
cat.100 <- rr3 %>% filter(n < 500 & n > 99) %>% 
  ggplot(aes(x =instructor, 
             y = intercept.fere, 
             ymin=ymin, 
             ymax=ymax)) + 
  geom_pointrange(aes(col = n)) + 
  geom_hline(yintercept = rr.mean) + 
  geom_hline(yintercept = rr.mean + 1.96*rr.sd) + 
  geom_hline(yintercept = rr.mean - 1.96*rr.sd) + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) + 
  labs(col = "Number of students") +
  geom_text(aes(y = rr.mean - 1.96*rr.sd + .01, label="-2 SD", x = 2)) + 
  geom_text(aes(y = rr.mean + 1.96*rr.sd + .01, label="+2 SD", x = 2)) +
  geom_text(aes(y = rr.mean + .01, label="Mean", x = 2)) + ylab("") + ggtitle("Instructors with 100-499 student records")
cat.100
saveRDS(cat.100, "cat_100.rds")

#ggplotly(cat.100)

# less than 100
cat.0 <- rr3 %>% filter(n < 100) %>% 
  ggplot(aes(x =instructor, 
             y = intercept.fere, 
             ymin=ymin, 
             ymax=ymax)) + 
  geom_pointrange(aes(col = n)) + 
  geom_hline(yintercept = rr.mean) + 
  geom_hline(yintercept = rr.mean + 1.96*rr.sd) + 
  geom_hline(yintercept = rr.mean - 1.96*rr.sd) + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) + 
  labs(col = "Number of students") +
  geom_text(aes(y = rr.mean - 1.96*rr.sd + .01, label="-2 SD", x = 3)) + 
  geom_text(aes(y = rr.mean + 1.96*rr.sd + .01, label="+2 SD", x = 3)) +
  geom_text(aes(y = rr.mean + .01, label="Mean", x = 3)) + ylab("") + ggtitle("Instructors with under 100 student records")
cat.0
saveRDS(cat.0, "cat_0.rds")

#ggplotly(cat.0, height = 1500)

################### comparison histograms #########################
raw.inst <- mlm.data %>%
group_by(instructor_id) %>% 
  summarise(pct.pass = mean(passed))

rr3.comp <- rr3 %>% left_join(raw.inst, by = c("instructor_id" = "instructor_id"))

#raw hist
raw.hist <- rr3.comp %>% ggplot() + 
  geom_histogram(aes(pct.pass, fill = "yellow", alpha = .7)) + 
  xlab("Percent passed by instructor") + 
  scale_fill_manual(values = "#ffcd00") + 
  geom_vline(xintercept = median(rr3.comp$pct.pass), linetype = "dashed", color = "#833921") + 
  geom_text(aes(x = median(rr3.comp$pct.pass) + .02, y = 31, label = "median 0.85")) +
  ggtitle("Raw instructor pass rates") + 
  guides(fill = "none", alpha = "none")

adj.hist <- rr3.comp %>% ggplot() + 
  geom_histogram(aes(intercept.fere, fill = "blue", alpha = .7)) + 
  xlab("Percent passed by instructor") + 
  scale_fill_manual(values = "#00abe1") + 
  geom_vline(xintercept = median(rr3.comp$intercept.fere), color = "#003865") + 
  geom_text(aes(x = median(rr3.comp$intercept.fere) + .015, y = 31, label = "median 0.78")) + 
  ggtitle("Adjusted instructor pass rates")+ 
  guides(fill = "none", alpha = "none")


comb.hist <- rr3.comp %>% ggplot() + 
  geom_histogram(aes(intercept.fere, fill = "blue", alpha = .5)) + 
  geom_histogram(aes(pct.pass, fill = "yellow", alpha = .5)) +
  xlab("Percent passed by instructor") + 
  scale_fill_manual(values = c("#00abe1", "#ffcd00")) + 
  geom_vline(xintercept = median(rr3.comp$pct.pass), linetype = "dashed", color = "#833921") + 
  geom_vline(xintercept = median(rr3.comp$intercept.fere), color = "#003865") + 
  ggtitle("Raw vs. Adjsuted instructor pass rates")+ 
  guides(fill = "none", alpha = "none")

#plots
raw.hist
adj.hist
comb.hist

caterpiler.plot
cat.1k
cat.500
cat.100

median(rr3.comp$intercept.fere) # 0.7849906
mean(rr3.comp$intercept.fere) # 0.7773661

ggplotly(cat.1k)
ggplotly(cat.500)
ggplotly(cat.100)
#################### BASIC OUTLINE OF THE DATA ######################
###### instructor #########
# median.pct.pass <- mlm.data %>% 
#   filter(term == "201640" |
#            term == "201540" |
#            term == "201440" |
#            term == "201620" |
#            term ==  "201520" |
#            term == "201420") %>% 
#   group_by(instructor_id) %>% 
#   summarise(pct.pass = mean(passed)) 
# 
# median.pct <- median(median.pct.pass$pct.pass)
# sd.pct.pass <- sd(median.pct.pass$pct.pass)
# mean.pct.pass <- mean(median.pct.pass$pct.pass)
# 
# inst.hist <- mlm.data %>% 
#   filter(term == "201640" |
#            term == "201540" |
#            term == "201440" |
#            term == "201620" |
#            term ==  "201520" |
#            term == "201420") %>% 
#   group_by(instructor_id) %>% 
#   summarise(pct.pass = mean(passed)) %>%
#   arrange(pct.pass) %>%
#   ggplot() +
#   geom_histogram(aes(pct.pass, col = pct.pass, alpha = .5), bins = 50) +
#   theme_minimal() +
#   geom_vline(xintercept = median.pct, colour = "blue") +
#   geom_text(aes(x = 0.8261965 - 0.1, label="Median: 0.83", y = 17),  colour = "blue") +
#   xlab("Percent passed by instructor") +
#   ggtitle("Pass percentage by instructor Spring 2014 - Fall 2016") + 
#   guides(alpha = "none")
# inst.hist
# saveRDS(inst.hist, "inst_hist.rds")

####### course ###########
median.pass.class <- mlm.data %>% filter(term == "201640" | 
                                         term == "201540" | 
                                         term == "201440" | 
                                         term == "201620" | 
                                         term ==  "201520" | 
                                         term == "201420") %>% 
  group_by(course_id) %>% 
  summarise(pct.pass = mean(passed)) 

median.pass.class <- median.pass.class %>% filter(pct.pass > 0.25)

median.pct.cl <- median(median.pass.class$pct.pass)
sd.pct.pass.cl <- sd(median.pass.class$pct.pass)
mean.pct.pass.cl <- mean(median.pass.class$pct.pass)

course.hist <- mlm.data %>% filter(term == "201640" | 
                      term == "201540" | 
                      term == "201440" | 
                      term == "201620" | 
                      term ==  "201520" | 
                      term == "201420") %>% 
  group_by(course_id) %>% 
  summarise(pct.pass = mean(passed)) %>% filter(pct.pass > 0) %>%
  ggplot() +
  geom_histogram(aes(pct.pass, col = pct.pass, alpha = .5), bins = 50) +
  theme_minimal() +
  geom_vline(xintercept = median.pct.cl, colour = "blue") +
  geom_text(aes(x = 0.8658147 - .1, label="Median: 0.87", y = 27),  colour = "blue") +
  xlab("Percent passed by course") +
  ggtitle("Pass percentage by course Spring 2014 - Fall 2016") + 
  guides(alpha = "none")
course.hist
saveRDS(course.hist, "course_hist.rds")

ggplotly(course.hist)

##################### EXPORTING GRAPHS ################
# tiff(filename = "caterpilar.tiff", width = 1000, height = 1000)
# caterpiler.plot #yes I know caterpilars like a's
# dev.off()
# 
# tiff("cat_1k.tiff", width = 720, height = 720)
# cat.1k
# dev.off()
# 
# tiff("cat_500.tiff", width = 720, height = 720)
# cat.500
# dev.off()
# 
# tiff("cat_100.tiff", width = 720, height = 720)
# cat.100
# dev.off()
# 
# tiff("cat_0.tiff", width = 720, height = 720)
# cat.0
# dev.off()
# 
# tiff("course_hist.tiff")
# course.hist
# dev.off()
# 
# tiff("inst_hist.tiff")
# inst.hist
# dev.off()

# png("course_hist.png", res = 150, width = 720, height = 720)
# course.hist
# dev.off()
# 
# png("inst_hist.png", res = 140, width = 720, height = 720)
# inst.hist
# dev.off()
# 
# png("caterpil_plot.png", res = 100, width = 1000, height = 720)
# caterpiler.plot
# dev.off()
# 
# png("cat_1k.png", res = 100, width = 1000, height = 720)
# cat.1k
# dev.off()
# 
# png("cat_500.png", res = 100, width = 1000, height = 720)
# cat.500
# dev.off()
# 
# png("cat_100.png", res = 100, width = 1000, height = 720)
# cat.100
# dev.off()
# 
# png("cat_0.png", res = 100, width = 1000, height = 720)
# cat.0
# dev.off()


#################### Cater plots for ########################### 


# Bin gpa rounded to .1 with a no-courses taken as a bin
# course pass level with cov. at course level. 1 level instructor still keep course random effect
# 
# course.data <- data %>% 
#   filter(term_grade != "W" & is.na(instructor_id) == FALSE & is.na(Ethnicity) == FALSE) %>%
#   group_by(`Academic Period`, `Course Identification`, `Instructor ID`, `Time of Day`) %>% 
#   summarise(avg.age = mean(term_age), 
#                pct.male = mean(male), 
#                pct.passed = mean(passed),
#                pct.white = mean(white), 
#                pct.vet = mean(vetern), 
#                avg.cr = mean(credit_attempted),
#                pct.first = mean(first_gen),
#                pct.pell = mean(pell),
#                pct.online = mean(online),
#             avg.prior_cum_gpa = mean(PRIOR_CUM_GPA),
#                n.class = n()) 
# course.data %>% ggplot() + geom_density(aes(pct.passed))
# course.data %>% ggplot() + geom_density(aes(pct.white))
# course.data %>% ggplot() + geom_density(aes(pct.vet))
# course.data %>% ggplot() + geom_density(aes(pct.male))
# course.data %>% ggplot() + geom_density(aes(avg.age))
# course.data %>% ggplot() + geom_density(aes(avg.cr))
# course.data %>% ggplot() + geom_density(aes(pct.first))
# course.data %>% ggplot() + geom_density(aes(pct.pell))
# course.data %>% ggplot() + geom_density(aes(pct.online))
# course.data %>% ggplot() + geom_density(aes(avg.gpa))
# 
# glmer.course <- lmer(pct.passed ~ pct.white + pct.male + avg.age + 
#                     pct.pell + pct.online + avg.cr + avg.prior_cum_gpa +
#                     pct.vet + pct.first + as.factor(`Course Identification`) + 
#                       (1|`Instructor ID`), data = course.data)
# 
# # plotting the results for course.
# rand.course <- lme4::ranef(glmer.course, condVar = T)
# 
# rc.sd <- sd(boot::inv.logit(rand.course$`Instructor ID`$`(Intercept)`))
# rc.mean <- mean(boot::inv.logit(rand.course$`Instructor ID`$`(Intercept)`))
# 
# rc1 <- rand.course
# rc2 <- data.frame(instructor = rownames(rc1[[1]]), 
#                   intercept = unname(rc1[[1]]), 
#                   se = sqrt(c(attr(rc1[[1]], "postVar")))) %>% 
#   arrange(desc(intercept))
# 
# rc2$instructor <- factor(rc2$instructor, levels = rc2$instructor[order(rc2$intercept)])
# 
# class.n <- course.data %>% group_by(`Instructor ID`) %>% tally()
# class.n$instructor <- as.factor(class.n$`Instructor ID`)
# 
# rc3 <- rc2 %>% left_join(class.n, by = "instructor")
# rc3$instructor <- factor(rc3$instructor, levels = rc3$instructor[order(rc3$intercept)])
# 
# caterpiler.plot <- rc3 %>% 
#   ggplot(aes(x =instructor, 
#              y = boot::inv.logit(intercept), 
#              ymin=boot::inv.logit(intercept - 1.96*se), 
#              ymax=boot::inv.logit(intercept + 1.96*se))) + 
#   geom_pointrange(aes(col = n)) + 
#   geom_hline(yintercept = rc.mean) + 
#   geom_hline(yintercept = rc.mean + 1.96*rc.sd) + 
#   geom_hline(yintercept = rc.mean - 1.96*rc.sd) + 
#   theme(axis.text.x=element_blank(), 
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.background = element_blank()) + 
#   labs(col = "Number of students") +
#   geom_text(aes(y = rc.mean - 1.96*rc.sd, label="-2 SD", x = 10)) + 
#   geom_text(aes(y = rc.mean + 1.96*rc.sd, label="+2 SD", x = 10)) +
#   geom_text(aes(y = rc.mean, label="Mean", x = 12)) + ylab("") + ggtitle("Caterpillar plots for all instructors")
# caterpiler.plot
# 
# course.plot.2 <- rc3 %>% filter(n > 2) %>%
#   ggplot(aes(x =instructor, 
#              y = intercept, 
#              ymin=intercept - 1.96*se, 
#              ymax=intercept + 1.96*se)) + 
#   geom_pointrange(aes(col = n)) + 
#   geom_hline(yintercept = rc.mean) + 
#   geom_hline(yintercept = rc.mean + 1.96*rc.sd) + 
#   geom_hline(yintercept = rc.mean - 1.96*rc.sd) + 
#   theme(axis.text.x=element_blank(), 
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.background = element_blank()) + 
#   labs(col = "Number of courses") + 
#   geom_text(aes(y = rc.mean - 1.96*rc.sd, label="-2 SD", x = 77)) + 
#   geom_text(aes(y = rc.mean + 1.96*rc.sd, label="+2 SD", x = 75)) +
#   geom_text(aes(y = rc.mean, label="Mean", x = 12)) + ylab("") + ggtitle("Caterpillar plots for all instructors")
# course.plot.2
# 
# rc3 %>% ggplot() + geom_histogram(aes(intercept)) + 
#   geom_vline(xintercept = rc.mean) + 
#   geom_vline(xintercept = rc.mean + 1.96*rc.sd) + 
#   geom_vline(xintercept = rc.mean - 1.96*rc.sd)
# 
# rc3 %>% ggplot() + geom_point(aes(x = se, y = n, col = intercept))
# rc3 %>% ggplot() + geom_point(aes(x = intercept, y = n, col = se))

###############################################################################
###############################################################################
###############################################################################

#plots
raw.hist
adj.hist
comb.hist

caterpiler.plot
cat.1k
cat.500
cat.100

median(rr3.comp$intercept.fere) # 0.7849906
mean(rr3.comp$intercept.fere) # 0.7773661

ggplotly(cat.1k)
ggplotly(cat.500)
ggplotly(cat.100)