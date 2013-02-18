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


-- Functions for bootstrap replicates

-- Make bootstrap replicates in two steps: make random draws of the data
-- (with replacement), then apply them to the data to get bootstrap
-- replicates.
generateBootstrap :: [Float] -> Int -> [Int] -> [[Float]]
generateBootstrap originalData nBootstraps randomNumbers =
    applyShuffles originalData
        (generateShuffles (length originalData) randomNumbers nBootstraps)


-- Take the appropriate number of random draws to generate the shuffle
-- for a single bootstrap replicate.
generateShuffles :: Int -> [Int] -> Int -> [[Int]]
generateShuffles nData randomNumbers 0 = []
generateShuffles nData randomNumbers nBootstraps = 
    [take nData randomNumbers] ++
        generateShuffles nData (drop nData randomNumbers)(nBootstraps-1)
    
    
-- Apply shuffle to a single bootstrap replicate.
applyShuffles :: [Float] -> [[Int]] -> [[Float]]
applyShuffles x shuffles =
    map applyShuffle shuffles
        where applyShuffle  = map (x !!)
    

    
    
-- Functions for parsing lines into a sample
parseSample :: String -> [Float]
parseSample = separateSamples . formatSample . parseFile

-- Parse a file of data.
parseFile :: String -> [[Float]]
parseFile inFile =
    map parseLine (splitOn "\n" inFile)
        where parseLine = castNumbers . splitOn "," . filter (/= ' ')

-- Read lists of strings to Float
castNumbers :: [String] -> [Float]
castNumbers xs =
    map readNumber xs
        where readNumber x = read x::Float
        
-- Split lists of data into tuples of (indicator, sample value)
formatSample :: [[a]] -> [(a,a)]
formatSample inData =
    map f inData
        where f x = (x !! 0, x !! 1)

-- Take the sample values out of tuples
separateSamples :: [(a, a)] -> [a]
separateSamples =
    map snd
    
    
    
-- Functions to calculate statistics on bootstrap replicates

-- Mean of a list
mean :: [Float] -> Float
mean x = realToFrac (sum x) / genericLength x

bootstrapMeans :: [[Float]] -> [Float]
-- Apply means to bootstrap replicates
bootstrapMeans =
    map mean

-- Rough quantile function. (Note: this is probably not the best estimate
-- of the quantile.)
quantile :: Float -> [Float] -> Float
quantile q xs =
    sort xs !! (round (q * (genericLength xs - 1)))

-- Pull out (0.025 and 0.975) quantiles
bootstrapQuantiles :: [Float] -> (Float, Float)
bootstrapQuantiles xs =
    (quantile 0.025 xs, quantile 0.975 xs)
    
    
    
-- Quite useless main function
main = do
    print "boot"
    print "will eventually bootstrap, if martin knows his stuff"
    fileName <- getLine
    inData <- readFile fileName
    let aSample = parseSample inData
    print aSample
    generator <- getStdGen
    let randomNumbers = take (100 * length aSample) (randomRs (0, (length aSample - 1)) generator)
    let boot = generateBootstrap aSample 100 randomNumbers
    let means = bootstrapMeans boot
    let estimate = mean aSample
    let quantiles = bootstrapQuantiles means
    --print boot
    --print means
    print estimate
    print quantiles
    
    