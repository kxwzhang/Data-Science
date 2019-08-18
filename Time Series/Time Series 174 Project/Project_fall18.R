library(readxl)
health <- read_excel("Downloads/health.xlsx")
View(health)
attach(health)
library(Hmisc)
# Full model
deathrate = health$X1
docav = health$X2
hosav = health$X3
income = health$X4
density = health$X5
mod <- lm((deathrate) ~ docav + hosav + income + density, data = health)

# Lets see a correlation matrix as a first step to get an idea of what we're working with
pairs(deathrate ~ docav + hosav + income + density)

# We see here that density has the smalles p value, so we will begin stepwise regression with this
anova(mod)
summary(mod)
# Now we begin stepwise regression with our first predictor as density
mod0 = lm(deathrate ~ 1)
add1(mod0, ~.+ docav + hosav + income + density, test = 'F')



# Adding income because if we use density first, we get no other relationships
mod1 = lm(deathrate ~ income)
add1(mod1, ~.+ docav + hosav + density, test = 'F')
ggplot(health, aes(x=docav, y=deathrate, color=deathrate))+geom_point()+scale_color_gradientn(colours=rainbow(3))
summary(mod1)
#QQ Plot
qq1 = plot(mod1,c(1,2))
shapiro.test(mod1$residuals)

# Now adding density
mod2 = lm(deathrate ~ income + density)
add1(mod2, ~.+ docav + hosav, test = 'F')
ggplot(health, aes(x=hosav, y=deathrate, color=deathrate))+geom_point()+scale_color_gradientn(colours=rainbow(3))
summary(mod2)
#QQ Plot
qq2 = plot(mod2,c(1,2))


# Adding doctor availability although it is borderline significant considering alpha = .15
mod3 = lm(deathrate ~ income + density + docav)
add1(mod3, ~.+ hosav)
ggplot(health, aes(x=income, y=deathrate, color=deathrate))+geom_point()+scale_color_gradientn(colours=rainbow(3))
summary(mod3)
#QQ Plot
qq3 = plot(mod3,c(1,2))
# We see here that hospital availability has no relatonship with deathrate or other predictors?



# So now we have our full model including deathrate on income, density, and doctor availability.
full.mod = lm(deathrate ~ income + density + docav)
# ____________________________________________________________________________________________

# Let us apply AIC method to compare the resulting model created using stepwise

mod0 = lm(deathrate ~ 1)
mod.upper = lm(deathrate ~ docav + hosav + income + density)
step(mod0, scope = list(lower = mod0, upper = mod.upper))
# Well this didnt give us shit. Oh well lets move on...
# __________________________________________________________________________________________

# Lets start messing with transformation for our full model we got from stepwise
boxcox(deathrate ~ income + density + docav)

# We see from from boxcox lambda = 2 lies within out 95% confidence interval, so lets apply it to y
trans.mod = lm((deathrate)^2 ~ income + density + docav)
pairs((deathrate)^2 ~ income + density + docav)

