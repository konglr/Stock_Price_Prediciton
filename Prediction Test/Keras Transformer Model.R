library(keras)

# Define the model
model <- keras_model_sequential()
model %>%
  layer_transformer_block(
    num_heads = 8,
    residual_dropout = 0.1,
    attention_dropout = 0.1,
    use_masking = TRUE,
    vanilla_attention = FALSE,
    # Set the input shape to (sequence_length, feature_dim)
    input_shape = c(sequence_length, feature_dim)
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = "linear")

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error",
  metrics = list("mean_absolute_error")
)