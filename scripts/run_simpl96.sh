#!/bin/bash
model=SimpLorenz96
# set up folders:
inference_folder=inferences
observation_folder=observations
# ABC inference:
ABC_inference_folder=ABC_inference
ABC_method=ABC
ABC_n_samples=1000 #impt
ABC_n_samples_per_param=10
ABC_steps=25
n_samples_in_obs=1
python scripts/abc_inference.py \
        $model  \
        $ABC_method  \
        --n_samples $ABC_n_samples  \
        --n_samples_per_param $ABC_n_samples_per_param \
        --n_samples_in_obs $n_samples_in_obs \
        --inference_folder $ABC_inference_folder \
        --observation_folder $observation_folder \
        --seed 1234 \
        --steps $ABC_steps 