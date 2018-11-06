import numpy as np
import matplotlib.pyplot as plt
import dnest4.builder as bd

# Load the data and compute log "returns"
#quantities = np.genfromtxt("data.txt", delimiter = ",", dtype = 'int32')
quantities = np.loadtxt("data.txt")
t = np.array(range(len(quantities) + 12))
# log_returns = np.diff(np.log(quantities))
data = {"y": quantities, "N": len(quantities), "t": t }

# Create the model
model = bd.Model()

# Parameters of AR(1) distribution with t innovations for data with trend
# params for t distribution
model.add_node(bd.Node("beta0",  bd.Uniform(0.0, 10000.0)))

model.add_node(bd.Node("log_sigma", bd.Uniform(-5.0, 5.0)))
model.add_node(bd.Node("sigma", bd.Delta("exp(log_sigma)")))

#model.add_node(bd.Node("log_nu", bd.Uniform(-1.0, 5.0)))
#model.add_node(bd.Node("nu", bd.Delta("exp(log_nu)")))

# prior for AR1 coef changed by Saurabh
#model.add_node(bd.Node("log_L", bd.Uniform(-10.0, 10.0)))
#model.add_node(bd.Node("L", bd.Delta("exp(log_L)")))

model.add_node(bd.Node("alpha", bd.Normal(0, 1)))

# prior for linear trend added by Saurabh
model.add_node(bd.Node("beta1", bd.Normal(0, 100)))

# Conditional prior for the data
for i in range(1, data["N"]):
	name = "y{index}".format(index=i)
	# trend added by Saurabh
	mean = "beta0 + beta1*t{index} + alpha*(y{past} - beta0 - beta1*t{past})".format(index=i, past=i-1)
	model.add_node(bd.Node(name,
                           bd.Normal(mean, "sigma"),
                           observed=True))

# trend added by Saurabh
# Predict next 12 months
mean0 = "beta0 + beta1*t{index2} + alpha*(y{last} - beta0 - beta1*t{last})".format(index2=24, last=data["N"]-1)
ypred = bd.Node("ypred0", bd.Normal(mean0, "sigma"))
model.add_node(ypred)
for i in range(1, 12):
	meani = "beta0 + beta1*t{index3} + alpha*(ypred{k} - beta0 - beta1*t{index4} )".format(index3=i+24, k=i-1, index4=i+23)
	ypred = bd.Node("ypred{i}".format(i=i),	
                    bd.Normal(meani, "sigma"))
	model.add_node(ypred)

# print predictions on the screen
print(ypred)
	
# Create the C++ code
bd.generate_h(model, data)
bd.generate_cpp(model, data)

# Compile the C++ code so it's ready to go
import os
os.system("make")

