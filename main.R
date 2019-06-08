#ucitavanje biblioteka
library('ggplot2')
library('car')
library('MASS')

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



#############################
##### Ovo je zadatak a) #####
#############################
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



#############################
##### Ovo je zadatak b) #####
#############################
cor(DepressionDataSet[, c('InternetUse', 'DepressionBefore', 'DepressionAfter', 'HouseholdIncome', 'HouseholdSize')])

ggplot(DepressionDataSet, aes(x = InternetUse, y = DepressionAfter)) + geom_point()
ggplot(DepressionDataSet, aes(x = DepressionBefore, y = DepressionAfter)) + geom_point()
ggplot(DepressionDataSet, aes(x = HouseholdSize, y = HouseholdIncome)) + geom_point()



#############################
##### Ovo je zadatak c) #####
#############################
# u isti dataframe je dodana stupac Difference
DepressionDataSet$Difference = DepressionDataSet$DepressionAfter  - DepressionDataSet$DepressionBefore

# opis dobivene varijable
summary(DepressionDataSet$Difference)

ggplot(DepressionDataSet, aes(Difference) ) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=mean(Difference)), color='blue', linetype='dashed', size=1) +
  xlab('Depression after - Depression before') +
  ylab('')



#############################
##### Ovo je zadatak d) #####
#############################
#prema Shapiro testu uzorci nisu iz normalne distribucije
shapiro.test(DepressionDataSet$DepressionBefore)
shapiro.test(DepressionDataSet$DepressionAfter)
shapiro.test(DepressionDataSet$InternetUse)
shapiro.test(DepressionDataSet$HouseholdIncome)
shapiro.test(DepressionDataSet$HouseholdSize)
shapiro.test(DepressionDataSet$Difference)

#iz grafova mozemo zakljuciti da Difference blizu normalne distribucije
ggplot(DepressionDataSet, aes(Difference) ) + geom_density()
ggplot(DepressionDataSet, aes(sample = Difference) ) + stat_qq() + stat_qq_line()



#############################
##### Ovo je zadatak e) #####
#############################
ggplot(data = DepressionDataSet, aes(x = Gender, y = Difference)) +
  geom_boxplot()

#testiranje normalnosti po grupama
#Difference nije normalna po grupama Gender
tapply(DepressionDataSet$Difference, DepressionDataSet$Gender, shapiro.test)

ggplot(DepressionDataSet, aes(sample = Difference, colour = Gender)) +
  stat_qq() +
  stat_qq_line()

ggplot(DepressionDataSet, aes(x=Difference, color = Gender) ) + geom_density()


#parametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Gender, var)
#Brown–Forsythe test
leveneTest(DepressionDataSet$Difference, DepressionDataSet$Gender, data = DepressionDataSet)
#prihvacamo hipotezu o jednakosti varijanca, ali nisu bili zadovoljeni uvjeti za provodenje testa

#neparametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Gender, median)
wilcox.test(DepressionDataSet$Difference~DepressionDataSet$Gender)
#prihvacamo hipotezu da su medijani jednaki



#############################
##### Ovo je zadatak f) #####
#############################
ggplot(data = DepressionDataSet, aes(x = Race, y = Difference)) +
  geom_boxplot()

#testiranje normalnosti po grupama
ggplot(DepressionDataSet, aes(sample = Difference, colour = Race)) +
  stat_qq() +
  stat_qq_line()

ggplot(DepressionDataSet, aes(x=Difference, color = Race) ) + geom_density()

tapply(DepressionDataSet$Difference, DepressionDataSet$Race, shapiro.test)
#Difference nije normalna po grupama Race

#parametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Race, var)
#Brown–Forsythe test
leveneTest(DepressionDataSet$Difference, DepressionDataSet$Race, data = DepressionDataSet)
#prihvacamo hipotezu o jednakosti varijanca, ali nisu bili zadovoljeni uvjeti za provodenje testa

#neparametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Race, median)
wilcox.test(DepressionDataSet$Difference~DepressionDataSet$Race)
#prihvacamo hipotezu da su medijani jednaki



#############################
##### Ovo je zadatak g) #####
#############################
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
#prihvacamo hipotezu o jednakosti varijanca, ali nisu bili zadovoljeni uvjeti za provodenje testa

#neparametarski test
tapply(DepressionDataSet$Difference, DepressionDataSet$Age, median)
wilcox.test(DepressionDataSet$Difference~DepressionDataSet$Age) 
# postoji signifikantna razlika, pa prihvacamo da medijani nisu jednaki



#############################
##### Ovo je zadatak h) #####
#############################
ggplot(data = DepressionDataSet, aes(x = interaction(Age, Gender), y = InternetUse)) +
  geom_boxplot()

tapply(DepressionDataSet$InternetUse, list(DepressionDataSet$Age, DepressionDataSet$Gender), median)
tapply(DepressionDataSet$InternetUse, list(DepressionDataSet$Age, DepressionDataSet$Gender), var)


ggplot(DepressionDataSet, aes(sample=InternetUse, color = interaction(Age, Gender)) ) +
  stat_qq() + 
  stat_qq_line()

tapply(DepressionDataSet$InternetUse, interaction(DepressionDataSet$Age, DepressionDataSet$Gender), shapiro.test)
#nisu zadovoljene pretpostavke za provodenje anove

#ANOVA
ANOVA.model = aov(InternetUse ~ Age * Gender, data = DepressionDataSet)
summary(ANOVA.model) 
#postoji signifikantna razlika po grupi godine
TukeyHSD(ANOVA.model)
#postoji signifikantnarazlika izmedu grupa Teen:male i Adult:male, kao i grupa Teen:male i Adult:female

kruskal.test(InternetUse ~ interaction(Age, Gender), data=DepressionDataSet) 
#postoji signifikantna razlika izmdeu grupa



#############################
##### Ovo je zadatak i) #####
#############################

# modificirano grupiranje spremljeno u ModifiedHouseholdSize
DepressionDataSet$ModifiedHouseholdSize = as.factor(DepressionDataSet$HouseholdSize)
levels( DepressionDataSet$ModifiedHouseholdSize )= c("1","2","3","4","5","5")
DepressionDataSet$ModifiedHouseholdSize = as.integer(DepressionDataSet$ModifiedHouseholdSize)

ggplot(data = DepressionDataSet, aes(group = ModifiedHouseholdSize, y=Difference)) +
  geom_boxplot()

tapply(DepressionDataSet$Difference, DepressionDataSet$ModifiedHouseholdSize, shapiro.test)
#nisu zadovoljene pretpostavke parametarskog testa (samo je grupa '2' normalna)

kruskal.test(Difference ~ ModifiedHouseholdSize, data=DepressionDataSet) 
# ne postoji signifikantna razlika

#############################
##### Ovo je zadatak j) #####
#############################

#izrada dummy varijabla 
ModelsDataSet = subset(DepressionDataSet, select = c(Difference, InternetUse, HouseholdIncome, HouseholdSize))
ModelsDataSet$IsFemale = as.integer(DepressionDataSet$Gender == 'female')
ModelsDataSet$IsWhite = as.integer(DepressionDataSet$Race == 'white')
ModelsDataSet$IsAdult = as.integer(DepressionDataSet$Age == 'Adult')

#pregled korelacije izmedu varijabli
cor(ModelsDataSet)

#linearni model sa svim varijablama
Linear.model.all = lm(Difference ~ ., ModelsDataSet)
summary(Linear.model.all)

#prazan linearni model
Linear.model.empty = lm(Difference ~ 1, ModelsDataSet)

#metode izbora varijabli
stepAIC(Linear.model.empty, direction = 'forward', scope=list(upper=Linear.model.all,lower=Linear.model.empty))
stepAIC(Linear.model.all, direction = 'backward')
stepAIC(Linear.model.empty, direction = 'both',  scope=list(upper=Linear.model.all,lower=Linear.model.empty))

#sve metode izbora varijabli daju isti rezultat
Linear.model = lm(formula = Difference ~ HouseholdSize + IsAdult + InternetUse, data = ModelsDataSet)
summary(Linear.model)



#############################
##### Ovo je zadatak k) #####
#############################
ggplot(fortify(Linear.model), aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line()
