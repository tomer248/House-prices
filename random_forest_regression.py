import numpy as np
import pandas as pd
import matplotlib as plt

#spliting the train dataset
train = pd.read_csv('train.csv')
x_train = train.iloc[:,:-1].values
y_train = train.iloc[:, -1].values
x_test = pd.read_csv('test.csv')
#taking care of missing data
from sklearn.impute import  SimpleImputer
imputer = SimpleImputer(missing_values=np.nan, strategy='most_frequent')
imputer.fit(x_train)
x_train = imputer.transform(x_train)

imputer = SimpleImputer(missing_values=np.nan, strategy='most_frequent')
imputer.fit(x_test)
x_test= imputer.transform(x_test)
x_train = x_train[:,1:]
x_test = x_test[:,1:]
#encoding categorical data
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder
ct = ColumnTransformer(transformers=[('encoder',
                                     OneHotEncoder(),[11])],remainder='passthrough')
x_train=np.array(ct.fit_transform(x_train))
x_test = np.array(ct.fit_transform(x_test))


from sklearn.preprocessing import LabelEncoder
label_encoder = LabelEncoder()
for col in range(25,103):
    x_train[:,col] = label_encoder.fit_transform(x_train[:,col])
    x_test[:,col] = label_encoder.fit_transform(x_test[:,col])


from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
x_train[:,25:] = sc.fit_transform(x_train[:,25:])
x_test[:,25:]= sc.fit_transform(x_test[:,25:])
x_train=pd.DataFrame(x_train)
x_train.to_csv('python_to_R.csv')
x_test= pd.DataFrame(x_test)
x_test.to_csv('python_R_test')