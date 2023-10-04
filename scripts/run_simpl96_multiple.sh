#!/bin/bash
model=SimpLorenz96
# set up folders:
observation_folder=observations
# ABC inference:
ABC_inference_folder=ABC_inference
ABC_method=ABC
ABC_n_samples=1000
ABC_n_samples_per_param=10
ABC_steps=25
n_samples_in_obs=10
for i in {1..10}
do
        echo "RUNNING EXPERIMENT ${i}"
        FOLDER="results/${model}_${i}/"
        python scripts/abc_inference.py \
                $model  \
                $ABC_method  \
                --root_folder "results/SimpLorenz96_${i}" \
                --n_samples $ABC_n_samples  \
                --n_samples_per_param $ABC_n_samples_per_param \
                --n_samples_in_obs $n_samples_in_obs \
                --inference_folder $ABC_inference_folder \
                --observation_folder $observation_folder \
                --seed 1234 \
                --steps $ABC_steps \
        &>${FOLDER}out_MCMC_${ABC_method}_${n_samples_in_obs}
done