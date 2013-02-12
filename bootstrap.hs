-- boot
-- This is martin's first step towards learning a little Haskell: a small
-- script for doing bootstrapping.
-- I felt bootstrapping would be a nice task to try: not completely trivial
-- but not immensely complex either.
-- Will eventually take two samples in some file format (csv?) and
-- make a bootstrap comparison of means.

import System.Random
import Data.List.Split

-- Input and seeding of the rng will be done with some monad later
sample = [10,20,30]
randomNumbers = take 1000 (randomRs (0, (length sample - 1)) (mkStdGen 42))


-- Make bootstrap replicates in two steps: make random draws of the data
-- (with replacement), then apply them to the data to get bootstrap
-- replicates.

generateBootstrap originalData nBootstraps =
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
someData = ["1,10 ", "1,20", "1,30"]

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

        

-- Quite useless main function
main = do
    print "boot"
    print "will eventually bootstrap, if martin knows his stuff"
    inData <- readFile "testdata.txt"
    --let lines = parseFile inData
    let aSample = splitSample (parseSample inData)
    let a = separateSamples aSample
    let boot = generateBootstrap a 10
    print boot
    
    