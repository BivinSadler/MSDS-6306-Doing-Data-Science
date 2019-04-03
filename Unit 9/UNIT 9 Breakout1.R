train_perc = .75
train_indices = sample(seq(1,500,length = 500),round(train_perc*500))
train = U9BO1[train_indices,]
test = U9BO1[-train_indices,]

U9BO1$X2 = U9BO1$X^2
U9BO1$X3 = U9BO1$X^3
U9BO1$X4 = U9BO1$X^4
U9BO1$X5 = U9BO1$X^5

fitTrain = lm(Y~X, data = train)
predsTest = predict(fitTrain, newdata = test)
ASEholderTest1 = sum((predsTest - test$Y)^2)/(length(test$Y))
ASEholderTest1

fitTrain = lm(Y~X + X2, data = train)
predsTest = predict(fitTrain, newdata = test)
ASEholderTest2 = sum((predsTest - test$Y)^2)/(length(test$Y))
ASEholderTest2

fitTrain = lm(Y~X + X2 + X3, data = train)
predsTest = predict(fitTrain, newdata = test)
ASEholderTest3 = sum((predsTest - test$Y)^2)/(length(test$Y))
ASEholderTest3

fitTrain = lm(Y~X + X2 + X3 + X4, data = train)
predsTest = predict(fitTrain, newdata = test)
ASEholderTest4 = sum((predsTest - test$Y)^2)/(length(test$Y))
ASEholderTest4


fitTrain = lm(Y~X + X2 + X3 + X4 + X5, data = train)
predsTest = predict(fitTrain, newdata = test)
ASEholderTest5 = sum((predsTest - test$Y)^2)/(length(test$Y))
ASEholderTest5

dfASE = data.frame(ASE = c(ASEholderTest1,ASEholderTest2,ASEholderTest3,ASEholderTest4,ASEholderTest5 ), Deg = c(1,2,3,4,5))
dfASE %>% filter(Deg > 2) %>% ggplot(aes(x = factor(Deg), y = ASE)) + geom_bar(stat = "identity")
dfASE %>% filter(Deg > 2) %>% ggplot(aes(x = factor(Deg), y = ASE)) + geom_bar(stat = "identity") + coord_cartesian(ylim = c(.9,.95))