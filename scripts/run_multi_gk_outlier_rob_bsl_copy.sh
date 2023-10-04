#!/bin/bash

# increasing number of samples in observation

model=g-and-k

# set up folders:
inference_folder=inferences
inference_folder=inferences
observation_folder=observations

burnin=0
n_samples=10
n_samples_per_param=500
NGROUP=500

# loop over METHODS:

METHODS=( SyntheticLikelihood )

N_SAMPLES_IN_OBS=( 1 10 50 100 500 1000)

PROPSIZES_BSL=( 1 1 1 1 1 1 )

FOLDER=results/${model}/${inference_folder}/
for ((k=0;k<${#METHODS[@]};++k)); do
for ((k2=0;k2<${#N_SAMPLES_IN_OBS[@]};++k2)); do

    method=${METHODS[k]}
    n_samples_in_obs=${N_SAMPLES_IN_OBS[k2]}

    echo $method $n_samples_in_obs

     if [[ "$method" == "SyntheticLikelihood" ]]; then
            PROPSIZE=${PROPSIZES_BSL[k2]}
     fi

    python scripts/inference.py \
    $model  \
    $method  \
    --n_samples $n_samples  \
    --burnin $burnin  \
    --n_samples_per_param $n_samples_per_param \
    --n_samples_in_obs $n_samples_in_obs \
    --inference_folder $inference_folder \
    --observation_folder $observation_folder \
    --load \
    --prop_size $PROPSIZE \
    --n_group $NGROUP \
     >${FOLDER}out_MCMC_${method}_${n_samples_in_obs}

done
done
