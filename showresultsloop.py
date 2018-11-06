import dnest4.classic as dn4
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

#dn4.postprocess()

dn4.postprocess(plot=False)

posterior_sample = dn4.my_loadtxt("posterior_sample.txt")
colnames = dn4.load_column_names("posterior_sample.txt")["colnames"]
indices = dn4.load_column_names("posterior_sample.txt")["indices"]

# Load the data and compute log "returns"
quantities = np.loadtxt("data.txt")
log_returns = np.diff(np.log(quantities))
data = {"y": log_returns, "N": len(log_returns)}

# Plot data
plt.plot(quantities, "ko-")
plt.xlabel("Month")
plt.ylabel("Revenue (\$)")

# Predictions of average monthly income over the next year
monthly_income_predictions = np.empty(posterior_sample.shape[0])

# Compute and plot up to 100 predictions
for i in range(0, posterior_sample.shape[0]):
    start = indices["ypred0"]
    end = indices["ypred0"]
    while True:
        try:
            if colnames[end][0:5] != "ypred":
                end -= 1
                break
        except:
            end -= 1
            break
        end += 1
    prediction = [quantities[-1]]
    for j in range(start, end + 1):
        prediction.append(prediction[-1]*np.exp(posterior_sample[i, j]))
    monthly_income_predictions[i] = np.mean(prediction)

    if i < 100:
        # Plot prediction
        plt.plot(np.arange(len(quantities)-1,
                           len(quantities)+len(prediction)-1),
                 prediction, "ro-", alpha=0.1)

plt.ylim([0.0, 3.0*quantities.max()])

plt.savefig('pred.png')
plt.close()

plt.hist(np.log10(monthly_income_predictions), 1000, density=True)
plt.xlabel("$\\log_{10}($Monthly income over next year$)$")

mip = pd.DataFrame(data = monthly_income_predictions)


# Quantiles for predictions
ss = np.sort(monthly_income_predictions)
quantiles = [2.5, 5.0, 10.0, 16.0, 25.0, 50.0, 75.0, 84.0, 90.0, 95.0, 97.5]
values = []
for q in quantiles:
    index = int(q/100.0*len(monthly_income_predictions))
    values.append(ss[index])

# Print quantiles
print("Predictions for average income per month, over next 12 months:")
df = pd.DataFrame({"percentiles": quantiles, "values": np.round(values, 2)})
df.index.name = None # Remove row names
print(df)

mip.to_csv('mip.csv')
prediction.to_csv('prediction.csv')
df.to_csv('percentiles.csv')

plt.savefig('hist.png')
plt.close()



