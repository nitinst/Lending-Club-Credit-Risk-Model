library(ROCR)
predicted <- plogis(predict(model, test_data_trnsfrmd))  # predicted
library(InformationValue)
optCutOff <- optimalCutoff(test_data_trnsfrmd$good_bad, predicted)[1] +0.4
misClassError(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
plotROC(test_data_trnsfrmd$good_bad, predicted)
AUROC_value <- AUROC(test_data_trnsfrmd$good_bad, predicted)
# Concordance(test_data_trnsfrmd$good_bad, predicted)
sensitivity(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
specificity(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
confusionMatrix(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
gini_coef <- 2 * AUROC_value - 1

ks_plot(test_data_trnsfrmd$good_bad, predicted)
ks_coeff <- ks_stat(test_data_trnsfrmd$good_bad, predicted)
