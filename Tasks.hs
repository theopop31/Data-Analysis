
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Data.Maybe

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
op :: Read a => String -> [a] -> [a]
op x acc
  | null (reads x :: [(Float, String)]) == True = acc
  | otherwise = (fst (head (reads x))) : acc

string_to_float :: [String] -> [Float]
string_to_float = foldr op []

averageSteps :: [String] -> Float
averageSteps l = Data.List.sum (string_to_float l) / 8

f :: [String] -> [String]
f [] = []
f (x:y) = [x, printf "%.2f" (averageSteps y)]

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name", "Average Number of Steps"] : tail (map f m)


-- Task 2

-- Number of people who have achieved their goal:

get_passed_people_num :: Table -> Int
get_passed_people_num m = length (filter (\x -> (averageSteps x)*8 >=1000) (tail m))


-- Percentage of people who have achieved their:
getNumberOfPeople :: Table -> Float
getNumberOfPeople m = fromIntegral (length m)

get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral(get_passed_people_num m)) / getNumberOfPeople m


-- Average number of daily steps
rowSum :: [String] -> Float
rowSum m = Data.List.sum (string_to_float m)

getTotalSteps :: Table -> Float
getTotalSteps m = foldl (+) 0.0 (map (rowSum) m)

get_steps_avg :: Table -> Float
get_steps_avg m = (getTotalSteps (tail m)) / (getNumberOfPeople (tail m))


-- Task 3
getStepsPerHTable :: Table -> [[Float]]
getStepsPerHTable [] = []
getStepsPerHTable (x:xs) = string_to_float(x) : (getStepsPerHTable xs)

getStepsPerH :: [[Float]] -> [Float]
getStepsPerH [] = []
getStepsPerH [x] = x
getStepsPerH (x:xs) = zipWith (+) x (getStepsPerH xs)

divideSteps :: [Float] -> Table -> [Float]
divideSteps l m = map (/(getNumberOfPeople (tail m))) l

convertStepsPerH :: [Float] -> [String]
convertStepsPerH [] = []
convertStepsPerH (x:xs) = (printf "%.2f" x) : convertStepsPerH xs

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : [(convertStepsPerH (divideSteps (getStepsPerH (getStepsPerHTable (tail m))) m))]


-- Task 4

getMinutesTable :: Table -> [[Float]]
getMinutesTable [] = []
getMinutesTable (x:xs) = string_to_float(tail (tail(tail x))) : getMinutesTable xs

convertIntToString :: [Int] -> [String]
convertIntToString [] = []
convertIntToString (x:xs) = (printf "%d" x) : convertIntToString xs

getColumn :: Int -> [[a]] -> [a]
getColumn i = map (!! (i - 1))

activeR1 :: [Float] -> Int
activeR1 c = length (filter (<50) (filter (>=0) c))

activeR2 :: [Float] -> Int
activeR2 c = length (filter (<100) (filter (>=50) c))

activeR3 :: [Float] -> Int
activeR3 c = length (filter (<500) (filter (>=100) c))

active :: [[Float]] -> Int -> [Int]
active t i = (activeR1 (getColumn i t)) : ((activeR2(getColumn i t)):((activeR3 (getColumn i t)) : []))


get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] : [("VeryActiveMinutes" : (convertIntToString (active (getMinutesTable (tail m)) 1))),
                                                              ("FairlyActiveMinutes" : (convertIntToString (active (getMinutesTable (tail m)) 2))),
                                                              ("LightlyActiveMinutes" : (convertIntToString (active (getMinutesTable (tail m)) 3)))]


-- Task 5
getTotalStepsList :: Table -> [Int]
getTotalStepsList [] = []
getTotalStepsList l = map floor (string_to_float (getColumn 2 l))

createPairs :: [String] -> [Int] -> [(String, Int)]
createPairs [] _ = []
createPairs _ [] = []
createPairs (x:xs) (y:ys) = (x,y) : createPairs xs ys

myCompare :: (String, Int) -> (String, Int) -> Ordering
myCompare (x1, y1) (x2, y2)
          | y1 < y2 = GT
          | y1 > y2 = LT
          | otherwise = compare x1 x2

decodePairs :: [(String, Int)] -> [[String]]
decodePairs [] = []
decodePairs ((a, b) : xs) = (a : [printf "%d" b]) : decodePairs xs

pairs :: Table -> [(String, Int)]
pairs m = sortBy myCompare (createPairs (getColumn 1 (tail m)) (getTotalStepsList m))

get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : (decodePairs (pairs m))


-- Task 6
computeAverage :: Int -> [Float] -> Float
computeAverage i l = ((l!!i) + (l !! (i+1)) + (l !! (i+2)) + (l !! (i+3))) / 4

get4H :: [[Float]] ->Int -> [Float]
get4H t i = map (computeAverage i) t

computeDifference :: [Float] -> [Float] -> [Float]
computeDifference l1 l2 = map (abs) (zipWith (-) l1 l2)

constructQuadruple :: [String] -> [Float] -> [Float] -> [Float] -> [(String, Float, Float, Float)]
constructQuadruple [] _ _ _ = []
constructQuadruple _ [] _ _ = []
constructQuadruple _ _ [] _ = []
constructQuadruple _ _ _ [] = []
constructQuadruple (x1:xs1) (x2:xs2) (x3:xs3) (x4:xs4) = (x1,x2,x3,x4) : constructQuadruple xs1 xs2 xs3 xs4

myCompare2 :: (String, Float, Float, Float) -> (String, Float, Float, Float) -> Ordering
myCompare2 (x1,x2,x3,x4) (y1,y2,y3,y4)
            | x4 < y4 = LT
            | x4 > y4 = GT
            | otherwise = compare x1 y1

average4H :: Table -> Int -> [Float]
average4H t i = get4H (getStepsPerHTable(tail t)) i

quadruples :: Table -> [(String, Float, Float, Float)]
quadruples t = sortBy myCompare2 (constructQuadruple (getColumn 1 (tail t)) (average4H t 0) (average4H t 4) (computeDifference (average4H t 0) (average4H t 4) ) )

deconstructQuadruple :: [(String, Float, Float, Float)] -> [[String]]
deconstructQuadruple [] = []
deconstructQuadruple ((x1, x2, x3, x4) :xs) = (x1 : [(printf "%.2f" x2), (printf "%.2f" x3), (printf "%.2f" x4)]) : deconstructQuadruple xs


get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : deconstructQuadruple (quadruples m)


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f m)


get_sleep_total :: Row -> Row
get_sleep_total r =  [(head r), (printf "%.2f" (rowSum (tail r)))]

{-
    TASK SET 2
-}

-- Task 1

getColumnNumber :: ColumnName -> Row -> Int
getColumnNumber n t
  | n == head t = 0
  | length t == 0 = -1
  | otherwise = 1 + getColumnNumber n (tail t)

myCompare3 :: Row -> Row -> Int -> Ordering
myCompare3 c1 c2 cNum
  | string_to_float [(c1 !! cNum)] == [] = if (c1 !! cNum == c2 !! cNum) then compare (head c1) (head c2) else compare (c1 !! cNum) (c2 !! cNum)
  | (head (string_to_float [(c1 !! cNum)])) == (head (string_to_float [(c2 !! cNum)])) = compare (head c1) (head c2)
  | otherwise = compare (head (string_to_float [(c1 !! cNum)])) (head (string_to_float [(c2 !! cNum)]))

tsort :: ColumnName -> Table -> Table
tsort column table
  | getColumnNumber column (head table) < 0 = [[]]
  | otherwise = (head table) : sortBy (\c1 c2 -> myCompare3 c1 c2 (getColumnNumber column (head table)) ) (tail table)

-- Task 2
checkHeader :: Table -> Table -> Bool
checkHeader t1 t2
  | head t1 == head t2 = True
  | otherwise = False

addColumns :: Table -> Table -> Table
addColumns t1 t2 = t1 ++ (tail t2)

vunion :: Table -> Table -> Table
vunion t1 t2
  | checkHeader t1 t2 == True = addColumns t1 t2
  | otherwise = t1

-- Task 3
generateEmptyRow :: Int -> Row -> Row
generateEmptyRow 0 r = r
generateEmptyRow n r = generateEmptyRow (n-1) ("" : r)

addEmptyRows :: Int -> Table -> Table
addEmptyRows 0 t = t
addEmptyRows n t = addEmptyRows (n-1) (t ++ [(generateEmptyRow (length (head t)) [])])

hunion :: Table -> Table -> Table
hunion t1 t2
  | length t1 > length t2 = hunion t1 (addEmptyRows (length t1 - length t2) t2)
  | length t1 < length t2 = hunion (addEmptyRows (length t2 - length t1) t1) t2
  | otherwise = zipWith (++) t1 t2

-- Task 4
commonHeaders :: ColumnName -> Row -> Row -> Row
commonHeaders s h1 = filter (\x -> x `elem` h1 && x /= s)

removeElem :: Eq a => a -> [a] -> [a]
removeElem _ [] = []
removeElem x (y:ys) = if (x == y) then removeElem x ys else y : removeElem x ys

replaceInRow :: Int -> Value -> Row -> Row
replaceInRow index elem row = (fst split) ++ elem : (tail (snd split))
                              where split = splitAt index row
replaceValues :: [Int] -> Row -> Row -> Row
replaceValues common r1 r2
  | null common = r1
  | (r2 !! (head common)) /= "" = replaceValues (tail common) ((replaceInRow (head common) (r2 !! (head common)) r1)) r2
  | otherwise = replaceValues (tail common) r1 r2
removeDuplicates :: [Int] -> Row -> Row
removeDuplicates n r
  | null n = r
  | otherwise = removeDuplicates (tail n) (removeElem (r !! (head n)) r)

searchForRow :: ColumnName -> [ColumnName] -> Row -> Table -> Row
searchForRow name header row t
  | null t = []
  | (row !! (getColumnNumber name header)) `elem` (head t) = (replaceValues indexList row (head t)) ++ (removeDuplicates indexList modifiedRow)
  | otherwise = searchForRow name header row (tail t)
         where indexList = map (\x -> getColumnNumber x header) (commonHeaders name header (head t))
               modifiedRow = (removeElem (row !! (getColumnNumber name header)) (head t))

join :: ColumnName -> Table -> Table -> Table 
join key_column t1 t2 = map (\x -> searchForRow key_column header x (tail t2)) (tail t1)
                        where header = head t1

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = header : (filter (\x -> x /= []) (join key_column t1 t2))
                        where header = head t1 ++ removeElem ((head t1) !! (getColumnNumber key_column (head t1))) (head t2)

-- Task 5
oneRow :: (Row -> Row -> Row) -> Row -> Table -> [Row]
oneRow f r t
  | length t > 0 = (f r (head t)) : (oneRow f r (tail t))
  | otherwise = []

allRows :: (Row -> Row -> Row) -> Table -> Table -> Table
allRows f t1 t2
  | length t1 > 0 = (oneRow f (head t1) t2) ++ (allRows f (tail t1) t2)
  | otherwise = []

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : (allRows new_row_function (tail t1) (tail t2))

-- Task 6
getColumnIndexes :: [ColumnName] -> Table -> [Int]
getColumnIndexes xs t = map (\x -> (getColumnNumber x (head t)) + 1) xs

columnsList :: [ColumnName] -> Table -> [Row]
columnsList n t = map (\x -> getColumn x t) (getColumnIndexes n t)

listToTable :: Row -> Table
listToTable [] = []
listToTable (x:xs) = [x] : (listToTable xs)

convertColumnList :: Table -> [Table]
convertColumnList t = map listToTable t

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = foldl (hunion) (head tableList) (tail tableList)
                                  where tableList = convertColumnList (columnsList columns_to_extract t)

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t
  | getColumnNumber key_column (head t) < 0 = [[]]
  | otherwise = (head t) : (filter (\x -> (condition (x !! column))) (tail t))
                            where column = getColumnNumber key_column (head t)

-- Task 8 TO_DO


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult

toTable :: QResult -> Table
toTable (Table t) = t
toTable (List t) = [t]

query = FromTable physical_activity
qTT = toTable(eval query)

instance Eval Query where
    eval (FromTable t) = Table t
    eval (AsList colname (FromTable t)) = List (tail (getColumn (getColumnNumber colname (head t) + 1) t))
    eval (Sort s (FromTable t)) = Table (tsort s t)
    eval (ValueMap op (FromTable t)) = Table (vmap op t)
    eval (RowMap op colnames (FromTable t)) = Table (rmap op colnames (tail t))
    eval (VUnion (FromTable t1) (FromTable t2)) = Table (vunion t1 t2)
    eval (HUnion (FromTable t1) (FromTable t2)) = Table (hunion t1 t2)
    eval (TableJoin colname (FromTable t1) (FromTable t2)) = Table (tjoin colname t1 t2)
    eval (Cartesian op colnames (FromTable t1) (FromTable t2)) = Table (cartesian op colnames t1 t2)
    eval (Projection colnames (FromTable t)) = Table (projection colnames t)
    eval (Filter cond (FromTable t)) = Table (head t : (filter (feval (head t) cond)  (tail t)))
    eval (Graph op (FromTable t)) = Table (makeGraph (sortGraph $ flattenTables $ removeElem [] (tableConversion t op)))

-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval h (Eq colname ref) = (\r -> (head (string_to_float [r !! cNum])) == ref)
                            where cNum = getColumnNumber colname h
    feval h (Lt colname ref) = (\r -> (head (string_to_float [r !! cNum])) < ref)
                            where cNum = getColumnNumber colname h
    feval h (Gt colname ref) = (\r -> (head (string_to_float [r !! cNum])) > ref)
                            where cNum = getColumnNumber colname h
    feval h (In colname list) = (\r -> (head (string_to_float [r !! cNum])) `elem` list)
                            where cNum = getColumnNumber colname h
    feval h (FNot cond) = (\r -> not ((feval h cond) r))
    feval h (FieldEq colname1 colname2) = (\r -> (head (string_to_float [r !! cNum1])) == (head (string_to_float [r !! cNum2])))
                                            where cNum1 = getColumnNumber colname1 h
                                                  cNum2 = getColumnNumber colname2 h

instance FEval String where
    feval h (Eq colname ref) = (\r -> (r !! cNum) == ref)
                                where cNum = getColumnNumber colname h
    feval h (Lt colname ref) = (\r -> (r !! cNum) < ref)
                                where cNum = getColumnNumber colname h
    feval h (Gt colname ref) = (\r -> (r !! cNum) > ref)
                                where cNum = getColumnNumber colname h
    feval h (In colname list) = (\r -> (r !! cNum) `elem` list)
                                where cNum = getColumnNumber colname h
    feval h (FNot cond) = (\r -> not ((feval h cond) r))
    feval h (FieldEq colname1 colname2) = (\r -> (r !! cNum1) == (r !! cNum2))
                                            where cNum1 = getColumnNumber colname1 h
                                                  cNum2 = getColumnNumber colname2 h


-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

rowConversion :: Row -> EdgeOp -> Table -> Table
rowConversion c op t
  | null t = []
  | head c == head (head t) = rowConversion c op (tail t)
  | op c (head t) /= Nothing = [(head c), (head (head t)), (fromJust (op c (head t)))] : convertedRow
  | otherwise = convertedRow
              where convertedRow = rowConversion c op (tail t)

tableConversion :: Table -> EdgeOp -> [Table]
tableConversion t1 op
  | null t1 = []
  | otherwise = (rowConversion (head t1) op (tail t1)) : (tableConversion (tail t1) op)

flattenTables :: [Table] -> Table
flattenTables t
  | null t = []
  | otherwise = head t ++ flattenTables (tail t)

sortRow :: Row -> Row
sortRow r
  | (head r) > (head (tail r)) = (head (tail r)) : (head r) : (tail (tail r))
  | otherwise = r
sortGraph :: Table -> Table
sortGraph t = map (\r -> sortRow r) t

makeGraph :: Table -> Table
makeGraph t = ["From", "To", "Value"] : t

-- 3.5
similarities_query :: Query
similarities_query = FromTable eight_hours

-- 3.6 (Typos)
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = []
