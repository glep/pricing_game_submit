source("model.R")  # Load your code.

# This script expects the dataset as a sys.args argument.
input_dataset = 'training.csv'  # The default value.

args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 1) {
	input_dataset = args[1]
}

# Load the dataset.
train_data = read.csv(input_dataset) %>% as_tibble()

# Run only to quickly test the code on a small sample
if (FALSE) {
  full_data <- train_data
  train_data <- train_data %>% 
    inner_join(train_data %>% distinct(id_policy) %>% slice_sample(n = 1e4))
  # train_data <- full_data
}

# Create a model, train it, then save it.
Xdata = within(train_data, rm('claim_amount'))
ydata = train_data['claim_amount']

x_raw = Xdata
y_raw = ydata

model = fit_model(x_raw = Xdata, y_raw = ydata)

save_model(model)