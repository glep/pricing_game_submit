#!/bin/bash

export DATASET_PATH=training.csv

Rscript predict.R

WEEKLY_EVALUATION=true Rscript predict.R
