# CS513Project
Collected sample data from Kaggle, got a data set of type of text, size of 5572. Converted it to utf-8 format. Counted the mean of the length of string, the number of uppercase alphabet, the number of exclamation mark and so on for ham and spam, generated a data frame for them; drew a related data table. Converted ham and spam into corpora and plain text documents, drew word cloud for each of them.Constructed a sparse matrix to eliminate low-frequency vocabularies in text, calculated the frequency of each word. Merged this sparse matrix and previous data frame to generate a data set for modeling. Used oversampling to augment data set and used 5-fold-cv to cross validate, got the training set of size of 5404 and the testing set of size of 2316. Trained data with popular classification methods such as k-Nearest Neighbors, Random Forest, CART, Naive Bayes, Artificial Neural Network respectively; Created confusion matrices to show the predictive effect of each model and recorded the corresponding consumption time. Analyzed and evaluated the advantages and disadvantages of each model, chose Random Forest model as our main method at last. Got the accuracy as 99.34% and the consumption of time as 43.62 seconds.
