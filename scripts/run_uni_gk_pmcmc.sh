#!/bin/bash

# increasing number of samples in observation
model=univariate_g-and-k

# set up folders:
inference_folder=inferences
inference_folder=inferences
observation_folder=observations

# now do inference with our methods
echo Inference

burnin=0
n_samples=110000
n_samples_per_param=500
NGROUP=500

# loop over METHODS:

METHODS=( EnergyScore )

N_SAMPLES_IN_OBS=( 1 10 50 100 500 1000)

PROPSIZES_ENERGY=( 1 1 0.4 0.2 0.2 0.2)

FOLDER=results/${model}/${inference_folder}/
for ((k=0;k<${#METHODS[@]};++k)); do
for ((k2=0;k2<${#N_SAMPLES_IN_OBS[@]};++k2)); do

        method=${METHODS[k]}
        n_samples_in_obs=${N_SAMPLES_IN_OBS[k2]}

        echo $method $n_samples_in_obs

     if [[ "$method" == "EnergyScore" ]]; then
            PROPSIZE=${PROPSIZES_ENERGY[k2]}
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
        --n_groups $NGROUP \
         >${FOLDER}out_MCMC_${method}_${n_samples_in_obs}

    done
done
