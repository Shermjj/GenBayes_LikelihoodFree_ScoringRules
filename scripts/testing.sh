#!/bin/bash

# inference with different outlier proportion and location

model=normal_location_misspec

# set up folders:
inference_folder=inferences_start_from_prior
observation_folder=observations
true_posterior_folder=true_posterior


EPSILON_VALUES=( 0.1 0.2 )
LOCATION_VALUES=( 3 5 7 10 20 )


# TRUE POSTERIOR:
CORES=1
N_SAMPLES=10000
BURNIN=10000


# LFI
echo Inference with my methods

burnin=40000
n_samples=20000
n_samples_per_param=500
n_samples_in_obs=100
NGROUP=50
prop_size=2.0

# PLOTS
echo PLOTS
# Figures 5 and 11
python3 scripts/plot_location_normal_misspec.py $model \
        --inference_folder $inference_folder \
        --observation_folder $observation_folder \
        --n_samples $n_samples \
        --n_samples_per_param $n_samples_per_param \
        --burnin $burnin \
        --true_posterior_folder $true_posterior_folder

