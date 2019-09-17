{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( someFunc
    ) where

import System.Console.GetOpt

import Control.Monad

import Development.Shake
import Development.Shake.FilePath

import Data.List
import Data.List.Split
import Data.List.Index
import Data.Strings

import Data.Maybe

-- /dagsdato/2019 - 0915 - cis/Grade 9_ CS/ Liv Atlan - SYS_77837/IMG_019748.jpg
-- /doneshooting/naerum_skole/cr2/0A/10.SYS_1234111.1.CC.005.cr2
--
-- input:
-- sessionstype
-- photographee
-- shootingtype
-- newdirectory
 

data Doneshooting = Doneshooting
    { school :: String
    , grade :: String
    , session :: String
    , sys :: String
    , shooting :: String
    , photographer :: String
    } deriving Show


toCr2 :: Int -> Doneshooting -> FilePath
toCr2 i doneshooting@(Doneshooting {..}) = school </> "cr2" </> grade </> fileName
    where
        extension = ".cr2"
        fileName = toFileName i extension doneshooting


toJpg :: Int -> Doneshooting -> FilePath
toJpg i doneshooting@(Doneshooting {..}) = school </> "_webshop" </> fileName
    where
        extension = ".jpg"
        fileName = toFileName i extension doneshooting


toFileName :: Int -> String -> Doneshooting -> FilePath
toFileName i extension (Doneshooting {..}) = session ++ "." ++ sys ++ "." ++ shooting ++ "." ++ photographer ++ "." ++ index ++ extension
    where
        index = strPadLeft '0' 3 (show i)


toDoneshooting :: Photographer -> FilePath -> Doneshooting
toDoneshooting (Photographer {..}) x = Doneshooting 
    { school = drop 14 (paths !! 0)
    , grade = paths !! 1
    , session = "10"
    , sys = toSys paths
    , shooting = "1"
    , photographer = id
    } where paths = splitDirectories x
    

toSys :: [String] -> String 
toSys paths = "SYS" ++ "_" ++ (splitOn "_" (paths !! 2) !! 1)

data Photographer = Photographer { id :: String } deriving Show

flags = [Option "" ["photographer"] ( ReqArg (\x -> Right $ Photographer { id = x }) "ID") "photographer ID"]

someFunc :: IO ()
someFunc = shakeArgsWith (shakeOptions {shakeFiles="_build", shakeThreads = 0 }) flags $ \ flags _ -> return $ Just $ do
    let dir1 = "./"
    let dir2 = dir1 </> "backed"
    let photographer = fromJust $ foldl (\acc x -> Just x) Nothing flags

    
    files <- liftIO $ getDirectoryFilesIO dir1 ["//*.cr2"] 

    let bySys = groupBy (\x y -> toSys (splitDirectories x) == (toSys (splitDirectories y))) files

    forM_ bySys $ \ sys -> do
        ifor_ sys $ \ index' x -> do
            let index = index' + 1
            let doneshooting = toDoneshooting photographer x
            let cr2 = dir2 </> toCr2 index doneshooting
            let jpg = dir2 </> toJpg index doneshooting
            want [cr2, jpg]

            cr2 %> \f -> do 
                copyFile' (dir1 </> x) f                                                  

            jpg %> \f -> do 
                copyFile' (dir1 </> x -<.> "jpg") f                                                    
