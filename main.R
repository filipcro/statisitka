#ucitavanje biblioteka
library("ggplot2")

# ucitavanje podataka
DepressionDataSet <- read.delim("./depression-and-the-internet.txt")

# Izbacivanje NA vrijednosti iz podataka (izgubljeno je 14 redova)
DepressionDataSet = na.omit(DepressionDataSet)


# promjena imena stupaca koji imaju losa imena
names(DepressionDataSet)[names(DepressionDataSet) == 'Household.income...000.'] = 'HouseholdIncome'
names(DepressionDataSet)[names(DepressionDataSet) == 'Household.size'] = 'HouseholdSize'
names(DepressionDataSet)[names(DepressionDataSet) == 'Internet.use..mean.hours.per.week.'] = 'InternetUse'
names(DepressionDataSet)[names(DepressionDataSet) == 'Race..white...1..minority...0.'] = 'Race'

# promjena rase u factor
DepressionDataSet$Race<- as.factor( DepressionDataSet$Race )
levels(DepressionDataSet$Race) = c('NonWhite', 'White')


# Ovo je zadatak a)
summary(DepressionDataSet)

ggplot(DepressionDataSet, aes(HouseholdIncome) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(HouseholdIncome)), color="blue", linetype="dashed", size=1) +
  xlab('Household income (in 1000)') +
  ylab('')

ggplot(DepressionDataSet, aes(InternetUse) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(InternetUse)), color="blue", linetype="dashed", size=1) +
  xlab('Internet use') +
  ylab('')

ggplot(DepressionDataSet, aes(DepressionBefore) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(DepressionBefore)), color="blue", linetype="dashed", size=1) +
  xlab('Depression before') +
  ylab('')

ggplot(DepressionDataSet, aes(DepressionAfter) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(DepressionAfter)), color="blue", linetype="dashed", size=1) +
  xlab('Depression after') +
  ylab('')


ggplot(DepressionDataSet, aes(HouseholdIncome) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(HouseholdIncome)), color="blue", linetype="dashed", size=1) +
  xlab('Household income (in 1000)') +
  ylab('')

ggplot(DepressionDataSet, aes(HouseholdSize) ) +
  geom_bar() +
  xlab('Household size') +
  ylab('')

#Ovo je zadatak b)
cor(DepressionDataSet[, c("InternetUse", "DepressionBefore", "DepressionAfter", "HouseholdIncome", "HouseholdSize")])

ggplot(DepressionDataSet, aes(x = InternetUse, y = DepressionAfter)) + geom_point()
ggplot(DepressionDataSet, aes(x = DepressionBefore, y = DepressionAfter)) + geom_point()

#Ovo je zadatak c)
# u isti dataframe je dodana stupac Difference
DepressionDataSet$Difference = DepressionDataSet$DepressionAfter  - DepressionDataSet$DepressionBefore

# opis dobivene varijable
summary(DepressionDataSet$Difference)

ggplot(DepressionDataSet, aes(Difference) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(Difference)), color="blue", linetype="dashed", size=1) +
  xlab('Depression after - Depression before') +
  ylab('')


#Ovo je zadatak d)
shapiro.test(DepressionDataSet$DepressionBefore)
shapiro.test(DepressionDataSet$DepressionAfter)
shapiro.test(DepressionDataSet$InternetUse)
shapiro.test(DepressionDataSet$HouseholdIncome)
shapiro.test(DepressionDataSet$HouseholdSize)

shapiro.test(DepressionDataSet$Difference)

ggplot(DepressionDataSet, aes(Difference) ) + geom_density()

ggplot(DepressionDataSet, aes(sample = Difference) ) + stat_qq() + stat_qq_line()

#Ovo je zadatak e)
ggplot(data = DepressionDataSet, aes(x = Gender, y = Difference)) +
  geom_boxplot()

ggplot(DepressionDataSet, aes(sample = Difference, colour = factor(Gender))) +
  stat_qq() +
  stat_qq_line()

wilcox.test(DepressionDataSet$Difference~DepressionDataSet$Gender)

###
ggplot(DepressionDataSet, aes(x=Difference, fill=Gender, color=Gender)) + geom_histogram(position="identity")
  geom_histogram() +
  theme(legend.position="top")

cor(DepressionDataSet[, c("InternetUse", "HouseholdIncome", "HouseholdSize", "Difference")])
ggplot(DepressionDataSet, aes(x = HouseholdIncome, y = Difference)) + geom_point()


ggplot(DepressionDataSet, aes(DepressionBefore) ) +
  geom_histogram(bins = 12, na.rm = TRUE) 

  ggplot(DepressionDataSet, aes(DepressionAfter) ) +
  geom_histogram(bins = 10, fill="white", alpha=0.5, position="identity", na.rm = TRUE) 
  
ggplot(DepressionDataSet, aes(x = "", y = HouseholdIncome)) + geom_boxplot() + coord_flip()