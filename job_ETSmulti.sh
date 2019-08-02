#!/bin/bash
#$ -N IRT_parallel
#$ -j y
#$ -t 1-10
#$ -tc 10

EVAL_DIR=/home/research/tmp/tgong/updnone
cd $EVAL_DIR
/opt/python/2.7/bin/python IRT_model.py 3 $SGE_TASK_ID 1 1 72000 30 > IRT_model_${SGE_TASK_ID}.log 2>&1
