module FndQueens where


import System.Random
import Data.Char
import Debug.Trace
import Data.List
import Control.Monad
import qualified Data.Set as Set
import Control.Concurrent.STM
import Control.Concurrent





type Board a        = ((a -> a -> Bool) -> Bool)


usedboard           :: (Num a, Ord a, Eq a) => a -> a -> Board a -> Board a
usedboard x y b     = \k -> k x y || b (\x2 y2 -> ikillYouQueen x y x2 y2 || k x2 y2)
userboard 0 0 _     = const False 

ub0                 = userboard 0 0 undefined --stub. the board dim is [1..8]X[1..8]
makeUB []           = ub0
makeUB ((x, y) : t) = usedboard x y $ makeUB t  

checkboard ub       = not $ ubb (\x y -> False)
                    where ubb = usedboard 99 99 ub  --final stub


ikillYouQueen x1 y1 x2 y2  
                    | x1==x2 || y1==y2= True
                    | x1<x2 && y1<y2  = gogoUp x1 y1 x2 y2--go up form 1
                    | x1<x2 && y1>y2  = gogoDown x1 y1 x2 y2 --go down from 1
                    | y1>y2 = gogoUp x2 y2 x1 y1 --go up from 2
                    | otherwise = gogoDown x2 y2 x1 y1--go down from 2


gogoDown 9 _ _ _    = False
gogoDown x1 y1 x2 y2= x1==x2 && y1==y2 || gogoDown (x1+1) (y1-1) x2 y2
             
gogoUp 9 _ _ _      = False
gogoUp x1 y1 x2 y2  = x1==x2 && y1==y2 || gogoUp (x1+1) (y1+1) x2 y2




makeSearchList      :: [(Int, Int)]
makeSearchList      = zip (randomRs (1,8) $ mkStdGen 81) (randomRs (1,8) $ mkStdGen 18)


findQueens1 [] _    =   [] 
findQueens1 ql ub   =   let (x,y): qln = ql
                            ubn = usedboard x y ub 
                        in if checkboard ubn
                            then (x, y) : findQueens1 ql ubn
                            else findQueens1 qln ub


findQueens2 shared ql fl dr = do 
                        s <- mReadSet
                        if length mFindQueenL >= 8 && not (mCheckMember s)
                           then do
                                mSetUpdate s
                                findQueens2 shared (drop dr makeSearchList) [] dr 
                           else findQueens2 shared (drop 64 ql) (init. tail $ mFindQueenL) dr 

                        where  mSetInsert    = Set.insert . Set.fromList
                               mCheckMember  = Set.member . Set.fromList $ mFindQueenL
                               mSetUnion     = (flip Set.union) . (mSetInsert mFindQueenL) 
                               mReadSet      = atomically $ readTVar shared
                               mFindQueenL   = findQueens1 (take 64 ql) (makeUB fl) ++ fl  
                               mSetUpdate    = atomically . (modifyTVar shared) . mSetUnion


fqhelper 0 _ _ =  []
fqhelper th dr sh =  (findQueens2 sh makeSearchList [] dr) : fqhelper (th-1) (dr+111) sh


fqWorker  = do 
               let thdCount = 6
               shared <- atomically $ newTVar Set.empty
               t  <- mapM forkIO $ fqhelper thdCount 0 shared 
               forkIO $ 5000 `timesDo` (do 
                                      milliSleep 100
                                      s <- atomically $ readTVar shared
                                      when (length s >= 92) $ mapM  killThread t >>  myThreadId >>=killThread)
               return shared
                     
               where timesDo = replicateM_
                     milliSleep = threadDelay . (*) 1000
           

findQueens          :: Set.Set (Set.Set (a, a)) -> [[(a,a)]] 
findQueens          =  foldMap (\x-> [Set.toList x])



    

