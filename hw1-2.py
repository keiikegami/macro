# import relevant package
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from math import pow
% matplotlib inline

# make the three time series
# original series
original = [100*(CPI[i] - CPI[i-1])/CPI[i-1] for i in range(1, len(CPI))]
original_series = original[12:]
#ma
ma_series = [sum([original[i-j] for j in range(12)])/12 for i in range(12, len(original))]
#log
log_series = [100*(pow(1+(np.log(CPI[i]) - np.log(CPI[i-12])), 1/12)-1) for i in range(13, len(CPI))]

# plot the data
date = data[["year"]].values[:, 0][13:]
plt.figure(figsize=(20,10))
plt.plot(date, original_series, label = 'original')
plt.plot(date, ma_series, label = 'MA')
plt.plot(date, log_series, label = 'Log')
plt.legend()
plt.xlabel("date")
plt.ylabel("inflation_rate")
plt.savefig('inflation.png')
plt.show()

# csv output
df = pd.DataFrame({"date": date, "original":original_series, "ma":ma_series, "log":log_series})
df.to_csv("corrected_inflation.csv")