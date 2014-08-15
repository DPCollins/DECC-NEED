from __future__ import division
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.utils import shuffle
from sklearn.cross_validation import train_test_split

plt.close('all')

pd.options.display.mpl_style = 'default'

# /////////////////////////////////  Data Prep   /////////////////////////////////////////

# Load NEED data which is trimmed to keep possibly importnat features

Lon = pd.read_csv('Lon_latest.csv')
del Lon['Unnamed: 0']
del Lon['Unnamed: 0.1']
del Lon['Property Age (H)']
del Lon['ENERGY_CONSUMPTION_CURRENT']
del Lon['Postcode']
# del Lon['ROOFINSULATIONTHICKENESS']
del Lon['EIMD_RANK']

del Lon['WALLCONSTRUCTION']
del Lon['WALL_CONS']

f = {'A':1, 'B':2,'C':3,'D':4,'E':5,'F':6,'G':7}  # Energy Efficiency Band Conversion

f2 = {'ND':10,'NI':11,'100mm':5,'150mm':6,'200mm':7,'250mm':8,'75mm':4,'50mm':3,'300mm+':9,'12mm':1,'25mm':2,'0':0, '12':12 } # Roof insulation Thickness

Lon['ENERGY_EFFICIENCY_BAND'] = Lon['ENERGY_EFFICIENCY_BAND'].apply(lambda x: f[x])

Lon['ROOFINSULATIONTHICKNESS'] = Lon['ROOFINSULATIONTHICKNESS'].replace(np.NaN, '12')
Lon['ROOFINSULATIONTHICKNESS'] = Lon['ROOFINSULATIONTHICKNESS'].apply(lambda x: f2[x])

### T.symmetric_difference(S) useful in finding difference between two sets

# Insulation/Wall types 


# ////////////////////////////////////////////////////////////////////////////////////////


WT = ['CWI', 'LI', 'SolidWI']

mask = (Lon['CWI'] + Lon['LI'] == 2)    # Check for subset of properties with both Cav Wall I and Loft Insulation
Lon[mask]



# #####################################################################################

fig, axes = plt.subplots(nrows=1, ncols=2)
Lon['ECons2012'].hist(ax=axes[0], bins=100).set_title('Electricity Consumption 2012')
Lon['GCons2012'].hist(ax=axes[1], bins=100).set_title('Gas Consumption 2012')

# #####################################################################################
fig, axes = plt.subplots(nrows=1, ncols=2)

Lon['Number of Bedrooms (H)'].value_counts().plot(kind='bar', ax=axes[0]).set_title('Number of Bedrooms')
Lon['Number of Adults in Household (H'].value_counts().plot(kind='bar', ax=axes[1]).set_title('Number of Adults in Household')


# ***************************************************************************************

# Descriptive Stats on Cavity Wall Insulation and Loft Insulation with breakdowns by 
#Prop Age and Type

# ***************************************************************************************

Lon['Count'] = 1


LI = Lon['LI'].value_counts()
CW = Lon['CWI'].value_counts()

print "% households with LI is: ", (100*LI[1]/(LI[0]+LI[1]))
print "% households with CWI is: ", (100*CW[1]/(CW[0]+CW[1]))


# ' This looks at breakdown of properites with CWI by Property Age and Type'


SS = Lon.groupby(['PROP_AGE','CWI']).sum()
print SS['Count']

SSType = Lon.groupby(['PROP_TYPE','CWI']).sum()
print SSType['Count']

LL = Lon.groupby(['PROP_AGE','LI']).sum()
print LL['Count']

LLType = Lon.groupby(['PROP_TYPE','LI']).sum()
print LLType['Count']

########################################################################


OA_SUM = Lon.groupby('OA_2011').sum()

OA_CWI = OA_SUM['CWI']  # total props with CWI in Output area
OA_LI = OA_SUM['LI']	# total props with LI in Output area

Households = OA_SUM['Count'] # total households in Output area


OA_grp = Lon.groupby('OA_2011').median()
OA_grp['%CWI'] = np.round(1000*OA_CWI/Households,0)/10
OA_grp['%LI'] = np.round(1000*OA_LI/Households,0)/10

# #####################################################################################

fig, axes = plt.subplots(nrows=1, ncols=2)
OA_grp['%CWI'].hist(ax=axes[0], bins=20).set_title('% Households within Census OA with CWI')
OA_grp['%LI'].hist(ax=axes[1], bins=20).set_title('% Households within Census OA with LI')

########################################################################



########################################################################
#					     Classifier Test  
########################################################################

loop = []

for i in range(0,4):
	
	print "Pass Number %i of 10" %(i)
	
	clf = RandomForestClassifier(n_estimators=100)

	u = Lon.dropna()

	cols = set(u.columns)
	rest = cols.symmetric_difference(['CWI', 'OA_2011', "GCons2012","ECons2012"])
	# rest = cols.symmetric_difference(['CWI'])


	input = u[list(rest)]
	idx = range(0,len(input))

	# Downsample negative cases for CWI (ration CWI:No_CWI ~ 1:10)

	indices = np.where(u.CWI == 0)[0]
	rng = np.random.RandomState(10)
	rng.shuffle(indices)
	n_pos = (u.CWI == 1).sum()

	df = u.drop(u.index[indices[n_pos:]])

	u2 = u.drop(df.index)  # CWI - ALL 0. remainder of dataset after undersampling

	# u2Input = u2[list(rest)] # ALL 0 Case
	# u2Target = u2['CWI'] # All 0!


	# Train Random Forest on undersampled data

	X = df[list(rest)]
	y = df['CWI'] 

	X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)
	est = clf.fit(X_train, y_train)
	Pred1 = clf.predict(X_test)

	# Plot Variable importance

	VI = pd.DataFrame(est.feature_importances_)
	VI.index = X.columns
	VI = VI.sort(columns=[0], ascending=False)
	VI.plot(kind='barh')


	yy =  pd.crosstab(y_test, Pred1, rownames=["Actual"], colnames=["Pred"])
	Acc_Test =  np.sum(y_test == Pred1) / float(len(y_test))

	Recall = yy[0][0]/(yy[0][0]+yy[1][0])
	Precision = yy[0][0]/(yy[0][0]+yy[0][1])

	Fmeasure = (2 * Precision * Recall)/(Precision + Recall)

	print 75*'*'
	print "Accuracy of Classification (downsampled): ", Acc_Test
	print "F-measure: ", Fmeasure
	print 75*'*'

	############################################################################

	# Test Trained RF on Rest of Data - All 0s due to sampling used in training step 

	############################################################################


	u2Input = u2[list(rest)] # ALL 0 Case
	u2Target = u2['CWI'] # All 0!

	Pred = clf.predict(u2Input)
	Pred_PC = clf.predict_proba(u2Input)

	xx =  pd.crosstab(u2Target, Pred, rownames=["Actual"], colnames=["Pred"])

	print xx
	ACC2 =  np.sum(u2Target == Pred) / float(len(u2Target))

	print 75*'*'
	print "Accuracy of Classification (Complete 0 set): ", ACC2 
	print 75*'*'

	print "Class Probabilities"

	T = pd.DataFrame(Pred_PC)
	T.index = u2Input.index

	T['GTruth'] = 0
	T['Pred'] = Pred

	T_edge = T[T[1]>0.75]

	EDGE = u2.ix[T_edge.index]	

	# Find proportion of households in OA that are classed as EDGE

	Miss = EDGE['OA_2011'].value_counts()

	OAs = Lon['OA_2011'].value_counts()

	OAprop = pd.DataFrame(Miss)
	OAprop.index = OAprop.index

	OAprop['Households'] = OAs
	OAprop['proportion'] = (100*OAprop[0])/OAprop['Households']
# 	Sort = OAprop.sort(column='proportion',ascending=False)

# 	Filt = Sort[(Sort['proportion']>30) & (Sort['Households']>20)] # this is somewhat arbitrary maybe don't filt for heatmap

# 	Filt2 = Sort[(Sort['Households']>20)]

	if i == 0:
		FiltTOT = OAprop
	else:
		name = 'Prop'+str(i)
		FiltTOT[name] = OAprop['proportion']
	
	# VVT XX2
# 	rec = list(Filt2.index[:20].values)
	
# 	loop.append(rec)

