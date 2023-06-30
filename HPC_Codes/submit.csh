#!/bin/csh
module load R

if ($#argv != 1) then
        echo "Usage: You need to feed two arguments to this program which is"
        echo "the number of years and the number of models. For example,"
        echo "./R_loops.csh 2 2"
        exit 1
endif

# Specify the number of fixed A value and the batch number
set numBatch = $1
# Initialize batch loop counter
set batch = 1
while ($batch <= $numBatch)

    while ($batch <= $numBatch)

       echo "Submit job A = $A and batch = $batch"

       bsub -n 1 -W 1800 -q stat -oo out.fixa=$A.batch=$batch -eo err.fixa=$A.batch=$batch "/usr/local/usrapps/bjreich/hyang23/smoke/bin/Rscript 8_FIXA.R $batch"

       @ batch++

    end

    @ A++
end 
