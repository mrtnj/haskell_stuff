-- boot
-- This is martin's first step towards learning a little Haskell: a small
-- script for doing bootstrapping.
-- I felt bootstrapping would be a nice task to try: not completely trivial
-- but not immensely complex either.
-- Will eventually take two samples in some file format (csv?) and
-- make a bootstrap comparison of means.

import System.Random

-- Input and seeding of the rng will be done with some monad later
sample = [10,20,30]
randomNumbers = take 1000 (randomRs (0, (length sample - 1)) (mkStdGen 42))


-- Make bootstrap replicates in two steps: make random draws of the data
-- (with replacement), then apply them to the data to get bootstrap
-- replicates.

generateBootstrap originalData nBootstraps =
    applyShuffleOrder originalData
        (generateShuffleOrder (length originalData) randomNumbers nBootstraps)

generateShuffleOrder nData randomNumbers nBootstraps = 
    if nBootstraps > 0 then 
        [take nData randomNumbers] ++
          generateShuffleOrder nData (drop nData randomNumbers)(nBootstraps-1)
    else []

-- Apply shuffle to a single bootstrap replicate.
applyShuffle x shuffle = 
    if shuffle == [] then
        []
    else
        [x !! head shuffle] ++ applyShuffle x (tail shuffle)
        
        
applyShuffleOrder x shuffleOrder =
    if shuffleOrder == [] then
        []
    else
        [applyShuffle x (head shuffleOrder)] ++
          applyShuffleOrder x (tail shuffleOrder)



          
-- Quite useless main function
main = do
    print "boot"
    print "will eventually bootstrap, if martin knows his stuff"
    let boot = generateBootstrap sample 10
    print boot