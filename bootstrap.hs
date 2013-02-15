-- boot
-- This is martin's first step towards learning a little Haskell: a small
-- script for doing bootstrapping.
-- I felt bootstrapping would be a nice task to try: not completely trivial
-- but not immensely complex either.
-- Will eventually take two samples in some file format (csv?) and
-- make a bootstrap comparison of means.

import System.Random
import Data.List.Split
import Data.List

-- Input and seeding of the rng will be done with some monad later
-- sample = [10,20,30]


-- Make bootstrap replicates in two steps: make random draws of the data
-- (with replacement), then apply them to the data to get bootstrap
-- replicates.

generateBootstrap originalData nBootstraps randomNumbers =
    applyShuffles originalData
        (generateShuffles (length originalData) randomNumbers nBootstraps)


generateShuffles nData randomNumbers 0 = []
generateShuffles nData randomNumbers nBootstraps = 
    [take nData randomNumbers] ++
        generateShuffles nData (drop nData randomNumbers)(nBootstraps-1)
    
    
-- Apply shuffle to a single bootstrap replicate.
applyShuffles x shuffles =
    map applyShuffle shuffles
        where applyShuffle  = map (x !!)
    

    
    
-- Functions for parsing lines into a sample
-- someData = ["1,10 ", "1,20", "1,30"]

parseSample inData =
    map parseLine (splitOn "\n" inData)
        where parseLine = castNumbers . splitOn "," . filter (/= ' ')

splitSample inData =
    map f inData
        where f x = (x !! 0, x !! 1)
        
castNumbers xs =
    map readNumber xs
        where readNumber x = read x::Int
        
separateSamples =
    map snd


    
-- Functions to calculate statistics on bootstrap replicates

mean x = realToFrac (sum x) / genericLength x

bootstrapMeans =
    map mean

-- Rough quantile function. (Note: this is probably not the best estimate
-- of the quantile.)
quantile q xs =
    sort xs !! (round (q * (genericLength xs - 1)))

bootstrapQuantiles xs =
    (quantile 0.025 xs, quantile 0.975 xs)
    


-- Quite useless main function
main = do
    print "boot"
    print "will eventually bootstrap, if martin knows his stuff"
    fileName <- getLine
    inData <- readFile fileName
    let aSample = splitSample (parseSample inData)
    let a = separateSamples aSample
    print a
    generator <- getStdGen
    let randomNumbers = take (100 * length a) (randomRs (0, (length a - 1)) generator)
    let boot = generateBootstrap a 100 randomNumbers
    let means = bootstrapMeans boot
    let estimate = mean means
    let quantiles = bootstrapQuantiles means
    --print boot
    --print means
    print estimate
    print quantiles
    
    