module FndQueens where


import System.Random
import Data.Char
import Debug.Trace
import Data.List
import Control.Monad
import Control.Concurrent
import qualified Data.Set as Set



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



makeQueensList      :: [(Int, Int)]
makeQueensList      = zip (randomRs (1,8) $ mkStdGen 81) (randomRs (1,8) $ mkStdGen 18)


findQueens1 [] _    =   [] 
findQueens1 ql ub   =   let (x,y): qln = ql
                            ubn = usedboard x y ub 
                        in if checkboard ubn
                            then (x, y) : findQueens1 ql ubn
                            else findQueens1 qln ub



--findQueens2 ql t s |  trace ("TRACE IS ==>> " ++ show r) False = undefined
findQueens2 ql t s f  
                     | f >= 92 = s
                     | otherwise = 
                           let tm = t ++ (findQueens1 (take 64 ql) $ makeUB t)
                           in if length tm >= 8 && not (mSM tm s)
                                 then findQueens2 makeQueensList [] (mSI tm s) (f + 1)
                                 else findQueens2 (drop 64 ql) (init. tail $ tm) s f
                              where mSI  = Set.insert . Set.fromList
                                    mSM  = Set.member . Set.fromList
                                                   
                                                   

fqHelper            =  findQueens2 makeQueensList [] Set.empty 0
findQueens          =  foldMap (\x-> [Set.toList x]) fqHelper  



--mapToLetters      = liftM (map $ \(x,y) ->(chr(ord 'A' + x - 1), y)) 

--printQL :: (Show a) => [a] -> IO ()
--printQL []        = print "End!"
--printQL (x:xs)    = print x >> printQL xs  



--main = do
--    print "Searching.."
--    printQL . mapToLetters $ findQueens  
    

