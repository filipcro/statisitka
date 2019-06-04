#ucitavanje biblioteka
library('ggplot2')
library('car')

# ucitavanje podataka
DepressionDataSet <- read.delim('./depression-and-the-internet.txt')

# Izbacivanje NA vrijednosti iz podataka (izgubljeno je 14 redova)
DepressionDataSet = na.omit(DepressionDataSet)


# promjena imena stupaca koji imaju losa imena
names(DepressionDataSet)[names(DepressionDataSet) == 'Household.income...000.'] = 'HouseholdIncome'
names(DepressionDataSet)[names(DepressionDataSet) == 'Household.size'] = 'HouseholdSize'
names(DepressionDataSet)[names(DepressionDataSet) == 'Internet.use..mean.hours.per.week.'] = 'InternetUse'
names(DepressionDataSet)[names(DepressionDataSet) == 'Race..white...1..minority...0.'] = 'Race'

# promjena rase u factor
DepressionDataSet$Race<- as.factor( DepressionDataSet$Race )
levels(DepressionDataSet$Race) = c('nonWhite', 'white')


# Ovo je zadatak a)
summary(DepressionDataSet)

ggplot(DepressionDataSet, aes(HouseholdIncome) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(HouseholdIncome)), color='blue', linetype='dashed', size=1) +
  xlab('Household income (in 1000)') +
  ylab('')

ggplot(DepressionDataSet, aes(InternetUse) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(InternetUse)), color='blue', linetype='dashed', size=1) +
  xlab('Internet use') +
  ylab('')

ggplot(DepressionDataSet, aes(DepressionBefore) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(DepressionBefore)), color='blue', linetype='dashed', size=1) +
  xlab('Depression before') +
  ylab('')

ggplot(DepressionDataSet, aes(DepressionAfter) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(DepressionAfter)), color='blue', linetype='dashed', size=1) +
  xlab('Depression after') +
  ylab('')


ggplot(DepressionDataSet, aes(HouseholdIncome) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(HouseholdIncome)), color='blue', linetype='dashed', size=1) +
  xlab('Household income (in 1000)') +
  ylab('')

ggplot(DepressionDataSet, aes(HouseholdSize) ) +
  geom_bar() +
  xlab('Household size') +
  ylab('')

#Ovo je zadatak b)
cor(DepressionDataSet[, c('InternetUse', 'DepressionBefore', 'DepressionAfter', 'HouseholdIncome', 'HouseholdSize')])

ggplot(DepressionDataSet, aes(x = InternetUse, y = DepressionAfter)) + geom_point()
ggplot(DepressionDataSet, aes(x = DepressionBefore, y = DepressionAfter)) + geom_point()
ggplot(DepressionDataSet, aes(x = HouseholdSize, y = HouseholdIncome)) + geom_point()

#Ovo je zadatak c)
# u isti dataframe je dodana stupac Difference
DepressionDataSet$Difference = DepressionDataSet$DepressionAfter  - DepressionDataSet$DepressionBefore

# opis dobivene varijable
summary(DepressionDataSet$Difference)

ggplot(DepressionDataSet, aes(Difference) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(Difference)), color='blue', linetype='dashed', size=1) +
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

#testiranje normalnosti po grupama
tapply(DepressionDataSet$Difference, DepressionDataSet$Gender, shapiro.test)

ggplot(DepressionDataSet, aes(sample = Difference, colour = Gender)) +
  stat_qq() +
  stat_qq_line()

ggplot(DepressionDataSet, aes(x=Difference, color = Gender) ) + geom_density()


#parametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Gender, var)
#Brown–Forsythe test
leveneTest(DepressionDataSet$Difference, DepressionDataSet$Gender, data = DepressionDataSet)

#neparametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Gender, median)
wilcox.test(DepressionDataSet$Difference~DepressionDataSet$Gender)


#Ovo je zadatak f)
ggplot(data = DepressionDataSet, aes(x = Race, y = Difference)) +
  geom_boxplot()

#testiranje normalnosti po grupama
ggplot(DepressionDataSet, aes(sample = Difference, colour = Race)) +
  stat_qq() +
  stat_qq_line()

ggplot(DepressionDataSet, aes(x=Difference, color = Race) ) + geom_density()

tapply(DepressionDataSet$Difference, DepressionDataSet$Race, shapiro.test)

#parametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Race, var)
#Brown–Forsythe test
leveneTest(DepressionDataSet$Difference, DepressionDataSet$Race, data = DepressionDataSet)

#neparametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Race, median)
wilcox.test(DepressionDataSet$Difference~DepressionDataSet$Race)


#Ovo je zadatak g)
ggplot(data = DepressionDataSet, aes(x = Age, y = Difference)) +
  geom_boxplot()

#testiranje normalnosti po grupama
ggplot(DepressionDataSet, aes(sample = Difference, colour = Age)) +
  stat_qq() +
  stat_qq_line()

ggplot(DepressionDataSet, aes(x=Difference, color = Age) ) + geom_density()

tapply(DepressionDataSet$Difference, DepressionDataSet$Age, shapiro.test)

#parametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Age, var)
#Brown–Forsythe test
leveneTest(DepressionDataSet$Difference, DepressionDataSet$Age, data = DepressionDataSet)

#neparametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Age, median)
wilcox.test(DepressionDataSet$Difference~DepressionDataSet$Age) # postoji signifikantna razlika


######
cor(DepressionDataSet[, c('InternetUse', 'HouseholdIncome', 'HouseholdSize', 'Difference')])
