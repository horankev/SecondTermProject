#################################

# trying multilevel model with lme4 from lmer...
# for different regions on conservative % vote
# first, using cars_mean, then deprived_mean, then health_bad_both, then age_18_44, then qual_mean, then retired


######
# from: https://benwhalley.github.io/just-enough-r/extending-traditional-rm-anova.html
slope.model <- lmer(con_19 ~ cars_mean + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ cars_mean + (cars_mean | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_cars_mean <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=cars_mean, color=region)) +
  # add axis label
  xlab("cars_mean") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(02,1.8), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ deprived_mean + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ deprived_mean + (deprived_mean | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_deprived_mean <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=deprived_mean, color=region)) +
  # add axis label
  xlab("deprived_mean") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(0.5,1.4), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ health_bad_both + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ health_bad_both + (health_bad_both | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_health_bad_both <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=health_bad_both, color=region)) +
  # add axis label
  xlab("health_bad_both") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(2.4,12), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ age_18_44 + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ age_18_44 + (age_18_44 | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_age_18_44 <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=age_18_44, color=region)) +
  # add axis label
  xlab("age_18_44") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(2.4,12), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ qual_mean + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ qual_mean + (qual_mean | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_qual_mean <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=qual_mean, color=region)) +
  # add axis label
  xlab("qual_mean") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(1.2,5.4), expand=c(0,0)) +
  scale_y_continuous(limits=c(-100, 100))

##

slope.model <- lmer(con_19 ~ retired + (1|region),  data=sel_vars_2019_sep_scot)

random.slope.model <- lmer(con_19 ~ retired + (retired | region), data=sel_vars_2019_sep_scot)
summary(random.slope.model)

lmerTest::ranova(random.slope.model)

a <-  data_frame(
  model = "random.slope",
  fitted = predict(random.slope.model),
  residual = residuals(random.slope.model))
b <- data_frame(
  model = "random.intercept",
  fitted = predict(slope.model),
  residual = residuals(slope.model))
residual.fitted.data <- bind_rows(a,b)

residual.fitted.data %>%
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~model)

p_retired <-
  ranef(random.slope.model)$region %>%
  # implicitly convert them to a dataframe and add a column with the subject number
  rownames_to_column(var="region") %>%
  # plot the intercept and slobe values with geom_abline()
  ggplot(aes()) +
  geom_abline(aes(intercept=`(Intercept)`, slope=retired, color=region)) +
  # add axis label
  xlab("retired") + ylab("Residual RT") +
  # set the scale of the plot to something sensible
  scale_x_continuous(limits=c(4.2,26)) +
  scale_y_continuous(limits=c(-100, 100))

##
p_cars_mean + p_deprived_mean + p_health_bad_both + p_age_18_44 + p_qual_mean + p_retired + plot_layout(ncol=3)

#########################
sel_vars_2019$winner_19 <- ifelse(sel_vars_2019$winner_19=="Conservative",0,
                                  ifelse(sel_vars_2019$winner_19=="Labour",1,
                                         ifelse(sel_vars_2019$winner_19=="Liberal Democrat",2,
                                                ifelse(sel_vars_2019$winner_19=="Scottish National Party",3,
                                                       ifelse(sel_vars_2019$winner_19=="Plaid Cymru",4,
                                                              ifelse(sel_vars_2019$winner_19=="Green",5,
                                                                     6))))))
b <- gam(list(winner_19~s(unemployed)+s(qual_mean),~s(unemployed)+s(qual_mean),
              ~s(unemployed)+s(qual_mean),~s(unemployed)+s(qual_mean),
              ~s(unemployed)+s(qual_mean),~s(unemployed)+s(qual_mean)),
         family=multinom(K=6),data=sel_vars_2019)
plot(b,pages=1)
gam.check(b)

summary(b)
