# Generalized Bayesian Likelihood-Free Inference Using Scoring Rules Estimators 

Code for the paper: __Generalized Bayesian Likelihood-Free Inference Using Scoring Rules Estimators__,
which can be found [here](https://arxiv.org/abs/2104.03889).

## Instructions

All experiments and plots from the paper can be reproduced using the code provided here. 

The content of this repository is as follows: 

- `src` contains source code for the models, scoring rules and various utilities
- `scripts` contains Python and bash scripts to run the different experiments.
- `mg1_code` contains Matlab code to perform exact MCMC inference for the M/G/1 model, as described in Shestopaloff and Neal, ["On bayesian inference for the M/G/1 queue with efficient MCMC sampling"](https://arxiv.org/abs/1401.5548), 2014. This was kindly provided by the authors, except for the file `mg1_code/do_runs_mine.sh` which was added by me.

### Reproducing the experiments

We provide bash scripts calling the Python scripts with the same parameters used in the paper. All figures in the paper can be generated by calling those scripts.

For instance, the MA2 experiment may be run by calling: 

    ./scripts/MA2.sh

Additionally, running: 

    ./scripts/run.sh

reproduces all experiments by calling the different bash scripts. 

The scripts work on a single core. However, runnning everything on one single core can be slow; parallelization can be used to run different inferences at the same time and thus reducing computing time. 

## New Experiments
(tested with python 3.7.15)
Run 
    ./scripts/run_simpl96.sh
to run the updated lorenz96 experiments with ABC
    
### Requirements
The inference routines with the Scoring Rules are implemented using the [`ABCpy` Python package](https://github.com/eth-cscs/abcpy).

All requirements are listed in the `requirements.txt` file. You can install those with: 

    pip install -r requirements.txt

### Tests
We provide some tests for our source code. To run them, do:
     
    python -m unittest src/tests.py

## Citation
Please use the following `.bib` entry:

    @misc{pacchiardi2021generalized,
          title={Generalized Bayesian Likelihood-Free Inference Using Scoring Rules Estimators}, 
          author={Lorenzo Pacchiardi and Ritabrata Dutta},
          year={2021},
          eprint={2104.03889},
          archivePrefix={arXiv},
          primaryClass={stat.ME}
    }