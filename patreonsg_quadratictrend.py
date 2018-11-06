import numpy as np
import matplotlib.pyplot as plt
import dnest4.builder as bd

# Load the data and compute log "returns"
#quantities = np.genfromtxt("data.txt", delimiter = ",", dtype = 'int32')
quantities = np.loadtxt("data.txt")
t = np.array(range(len(quantities) + 12))
# log_returns = np.diff(np.log(quantities))
data = {"y": quantities, "N": len(quantities), "t": t , "tt": np.square(t)}

# Create the model
model = bd.Model()

# Parameters of AR(1) distribution with t innovations for data with trend
# params for t distribution
model.add_node(bd.Node("mu",  bd.Uniform(0, 10000)))

model.add_node(bd.Node("log_beta", bd.T(-2.0, 1.0, 2.0)))
model.add_node(bd.Node("beta", bd.Delta("exp(log_beta)")))

model.add_node(bd.Node("log_nu", bd.Uniform(-1.0, 5.0)))
model.add_node(bd.Node("nu", bd.Delta("exp(log_nu)")))

# prior for AR1 coef changed by Saurabh
#model.add_node(bd.Node("log_L", bd.Uniform(-10.0, 10.0)))
#model.add_node(bd.Node("L", bd.Delta("exp(log_L)")))
model.add_node(bd.Node("alpha", bd.Normal(0, 1)))

# prior for linear trend added by Saurabh
model.add_node(bd.Node("beta2", bd.Uniform(-5000.0, 5000.0)))

# prior for quadratic trend added by Saurabh
model.add_node(bd.Node("beta3", bd.Uniform(-1000.0, 1000.0)))

# Conditional prior for the data
for i in range(1, data["N"]):
	name = "y{index}".format(index=i)
	# trend added by Saurabh
	mean = "mu + beta2*t{index} + beta3*tt{index} + alpha*(y{past} - mu - beta2*t{index} - beta3*tt{index} )".format(index=i, past=i-1)
	model.add_node(bd.Node(name,
                           bd.T(mean, "beta", "nu"),
                           observed=True))

# trend added by Saurabh
# Predict next 12 months
mean0="mu + beta2*t{index2} + beta3*tt{index2}  + alpha*(y{last} -mu -beta2*t{index2})".format(index2=24, last=data["N"]-1)
ypred=bd.Node("ypred0", bd.T(mean0, "beta", "nu"))
model.add_node(ypred)
for i in range(1, 12):
	meani = "mu + beta2*t{index3} + beta3*tt{index3} + alpha*(ypred{k}-mu - beta2*t{index3} - beta3*tt{index3}  )".format(index3=i+24, k=i-1)
	ypred = bd.Node("ypred{i}".format(i=i),	
                    bd.T(meani, "beta", "nu"))
	model.add_node(ypred)

# print predictions on the screen
print(ypred)
	
# Create the C++ code
bd.generate_h(model, data)
bd.generate_cpp(model, data)

# Compile the C++ code so it's ready to go
import os
os.system("make")

