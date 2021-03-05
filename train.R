source("model.R")  # Load your code.

# This script expects the dataset as a sys.args argument.
input_dataset = 'training.csv'  # The default value.

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 1) {
	input_dataset = args[1]
}

# Load the dataset.
train_data = read.csv(input_dataset) %>% as_tibble()

newbusiness_idpolicy <- train_data %>% distinct(id_policy) %>% slice_sample(n = 10)
full_data <- train_data
train_data <- train_data %>% anti_join(newbusiness_idpolicy) %>% 
  slice_sample(n = 1e4)

# Create a model, train it, then save it.
Xdata = within(train_data, rm('claim_amount'))
ydata = train_data['claim_amount']

x_raw = Xdata
y_raw = ydata

model = fit_model(x_raw = Xdata, y_raw = ydata)

save_model(model)