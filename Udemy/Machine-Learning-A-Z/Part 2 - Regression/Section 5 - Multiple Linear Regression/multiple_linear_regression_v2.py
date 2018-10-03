# Multiple Linear Regression

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('50_Startups.csv')
X = dataset.iloc[:, :-1].values
y = dataset.iloc[:, -1].values

# Encoding categorical data
# Encoding the Independent Variable
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X = LabelEncoder()
X[:, 3] = labelencoder_X.fit_transform(X[:, 3])
onehotencoder = OneHotEncoder(categorical_features = [3])
X = onehotencoder.fit_transform(X).toarray()

# Avoiding the Dummy Variable Trap
X = X[:, 1:]

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

# Feature Scaling
"""from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)
sc_y = StandardScaler()
y_train = sc_y.fit_transform(y_train)"""

# Fitting Multiple Linear Regression to the Training Set 
from sklearn.linear_model import LinearRegression
regressor = LinearRegression()
regressor.fit(X_train, y_train)

# Predicting the Test set results
y_pred = regressor.predict(X_test)

# Building the optimal model using Backward Elimination
import statsmodels.formula.api as sm
def backward_elimination(X, y, sl):
    """
    X: the data matrix with the independent variables
    y: the matrix of the dependent variables
    sl: statistical level, by default the user should add 0.05 (5%)
    """
    X = np.append(arr=np.ones((len(X),1)).astype(int), values=X, axis=1)
    while(True):
        regressor_OLS = sm.OLS(y,X).fit()
        ind = np.argmax(regressor_OLS.pvalues)
        max_pvalue = regressor_OLS.pvalues[ind]
        if max_pvalue > sl:
            X = np.delete(X, ind, axis=1)
        else:
            print(regressor_OLS.summary())
            X = np.delete(X, 0, axis=1)
            return X

def backward_elimination2(X, y, sl):
    """
    X: the data matrix with the independent variables (predictors)
    y: the matrix of the dependent variable (target)
    sl: statistical level, by default the user should add 0.05 (5%)
    """
    X = np.append(arr=np.ones((len(X),1)).astype(int), values=X, axis=1)
    
    while(True):
        regressor_OLS = sm.OLS(y,X).fit()
        ind = np.argmax(regressor_OLS.pvalues)
        max_pvalue = regressor_OLS.pvalues[ind]    
        
        if max_pvalue > sl:
            actual_adj_rsquared = regressor_OLS.rsquared_adj
            X_temp = np.delete(X, ind, axis=1)
            next_regressor_OLS = sm.OLS(y,X_temp).fit()
            next_adj_rsquared = next_regressor_OLS.rsquared_adj    
            if(actual_adj_rsquared > next_adj_rsquared):
                X = np.delete(X, 0, axis=1)
                print(regressor_OLS.summary())
                return X
            else:
                X = np.delete(X, ind, axis=1)
        else:
            print(regressor_OLS.summary())
            X = np.delete(X, 0, axis=1)
            return X
        
###############################################
"""
X = np.append(arr=np.ones((len(X),1)).astype(int), values=X, axis=1)
X_opt = X[:, [0,1,2,3,4,5]]
regressor_OLS = sm.OLS(y,X_opt).fit()
regressor_OLS.summary()

X_opt = np.delete(X_opt, 2, axis=1)
regressor_OLS = sm.OLS(y,X_opt).fit()
regressor_OLS.summary()

X_opt = np.delete(X_opt, 1, axis=1)
regressor_OLS = sm.OLS(y,X_opt).fit()
regressor_OLS.summary()

X_opt = np.delete(X_opt, 2, axis=1)
regressor_OLS = sm.OLS(y,X_opt).fit()
regressor_OLS.summary()

X_opt = np.delete(X_opt, 2, axis=1)
regressor_OLS = sm.OLS(y,X_opt).fit()
regressor_OLS.summary()
"""

SL = 0.05 # level of statistical significance
X_opt = backward_elimination(X, y, SL)



















