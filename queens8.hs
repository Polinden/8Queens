-- {-# OPTIONS_GHC -Wall #-}
import System.Random
import Data.Char
import Debug.Trace
import Data.List


type Board a        = ((a -> a -> Bool) -> Bool)

usedboard           :: (Num a, Ord a, Eq a) => a -> a -> Board a -> Board a
usedboard x y b     = \k -> k x y || b (\x2 y2 -> ikillYouQueen x y x2 y2 || k x2 y2)
userboard 0 0 _     = const False 

ub0                 = userboard 0 0 undefined --stub. the board dim is [1..8]X[1..8]
makeUB []           = ub0
makeUB ((x, y) : t) = usedboard x y $ makeUB t  

checkboard ub       = not $ ubb (\x y -> False)
                    where ubb = usedboard 99 99 ub  --final stub


--ikillYou x1 y1 x2 y2 = x1==x2 && y1==y2 --it was a test
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
makeQueensList      = zip (randomRs (1,8) $ mkStdGen 18) (randomRs (1,8) $ mkStdGen 28)
--makeQueensList    = [(x, y) | x<-[1..8], y<-[1..8]]
--shflQL (q:ql)     = ql ++ [q]


findQueens1 [] _    =   [] 
findQueens1 ql ub   =   let (x,y): qln = ql
                            ubn = usedboard x y ub 
                        in if checkboard ubn
                            then (x, y) : findQueens1 ql ubn
                            else findQueens1 qln ub

findQueens2 ql t    =  let tm = findQueens1 (take 64 ql) $ makeUB t
                       in if (length $ trace ("-> "++show tm) tm) < 8
                                then findQueens2 (drop 64 ql) []
                                else tm

findQueens = findQueens2 makeQueensList []


mapToLetters = map $ \(x,y) ->(chr(ord 'A' + x - 1), y) 



main = do
    print "Searching.."
    let r = findQueens
    print $ mapToLetters r  

