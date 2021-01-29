source("model.R")  # Load your code.

# This script expects the dataset as a sys.args argument.
input_dataset = 'training.csv'  # The default value.

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 1) {
	input_dataset = args[1]
}

# Load the dataset.
train_data = read.csv(input_dataset)

# Create a model, train it, then save it.
Xdata = within(train_data, rm('claim_amount'))
ydata = train_data['claim_amount']

model = fit_model(Xdata, ydata)

save_model(model)