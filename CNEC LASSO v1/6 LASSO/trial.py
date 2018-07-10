import pandas as pd

from sklearn import datasets, linear_model
from sklearn.model_selection import train_test_split

from matplotlib import pyplot as plt

libraries = pd.read_csv("libraries.csv", header=None)
target = pd.read_csv("target.csv", header=None)

X_train, X_test, y_train, y_test = train_test_split(libraries, target, test_size=.5)

lm = linear_model.LinearRegression()

model = lm.fit(X_train, y_train)
predictions = lm.predict(X_test)
print(predictions)