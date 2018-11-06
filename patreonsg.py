import numpy as np
import matplotlib.pyplot as plt
import dnest4.builder as bd

# Load the data and compute log "returns"
#quantities = np.genfromtxt("data.txt", delimiter = ",", dtype = 'int32')
quantities = np.loadtxt("data.txt")
log_returns = np.diff(np.log(quantities))
data = {"y": log_returns, "N": len(log_returns)}

# Create the model
model = bd.Model()

# Parameters of AR(1) distribution with t innovations for log-returns
model.add_node(bd.Node("mu",  bd.T(0.0, 0.1, 2.0)))
model.add_node(bd.Node("log_beta", bd.T(-2.0, 1.0, 2.0)))
model.add_node(bd.Node("beta", bd.Delta("exp(log_beta)")))
model.add_node(bd.Node("log_L", bd.Uniform(-10.0, 10.0)))
model.add_node(bd.Node("L", bd.Delta("exp(log_L)")))
model.add_node(bd.Node("alpha", bd.Delta("exp(-1.0/L)")))
model.add_node(bd.Node("log_nu", bd.Uniform(-1.0, 5.0)))
model.add_node(bd.Node("nu", bd.Delta("exp(log_nu)")))

# Conditional prior for the data
for i in range(1, data["N"]):
    name = "y{index}".format(index=i)
    mean = "mu + alpha*(y{past} - mu)".format(past=i-1)
    model.add_node(bd.Node(name,
                           bd.T(mean, "beta", "nu"),
                           observed=True))

# Predict next 12 months
ypred = bd.Node("ypred0", bd.T("mu + alpha*(y{last}-mu)".format(last=data["N"]-1), "beta", "nu"))
model.add_node(ypred)
for i in range(1, 12):
    ypred = bd.Node("ypred{i}".format(i=i),
                    bd.T("mu + alpha*(ypred{k}-mu)".format(k=i-1),
                         "beta", "nu"))
    model.add_node(ypred)

# print predictions to text file
print(ypred)
	
# Create the C++ code
bd.generate_h(model, data)
bd.generate_cpp(model, data)

# Compile the C++ code so it's ready to go
import os
os.system("make")

