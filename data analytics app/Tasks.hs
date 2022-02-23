{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tasks where
import Data.List
import Data.Array 
import Data.Ord 
import Dataset
import Text.Printf
type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
compute_exam_grades :: Table -> Table
aua :: Table -> Table
aua [] = []
aua (y:xs) =  ((head y) : printf "%.2f" ((fromInteger(foldr (+) 0 $ map(\x -> if x == "" then 0 else read x :: Integer) $ tail $ reverse $ tail y )) / 4 + ((\x -> if x == "" then 0 else read x :: Float) $ head $ reverse $ tail y) ):[]) : aua (xs) 
compute_exam_grades x =  (head(head x) : ("Punctaj Exam":[])) : aua (tail x)





-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Integer
task2 :: Table -> Integer
task2 [] = 0
task2 (y:xs) = if (((\x -> read x :: Float) $ head $ reverse y)) > 2.5 then (1 + task2(xs)) else (0 + task2(xs)  )
get_passed_students_num x = task2 (tail(compute_exam_grades x))






-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage x  =  fromInteger(get_passed_students_num x ) / (fromInteger(toInteger(length x) - 1))







-- Average exam grade
get_exam_avg :: Table -> Float
task23 :: Table -> Float
task23 [] = 0
task23 (y:xs) = ((\x -> read x :: Float) $ head $ reverse y) + task23(xs)
get_exam_avg x  = (task23(tail(compute_exam_grades x ))) / (fromInteger(toInteger(length x) - 1))








-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
task24:: Table -> Int
task24 [] = 0
task24 (x:xs) = if (foldr (+) 0 $ map(\x -> if x == "" then 0 else read x ::Float)  $ tail $ tail $ tail $ tail $ reverse $ tail $ tail x) >= 1.5 then 1 + task24(xs) else 0 + task24(xs)     


get_passed_hw_num x = task24(tail x)




-- Task 3
get_avg_responses_per_qs :: Table -> Table
task3:: Table -> [Integer]
trex:: Table -> [Float]
toRow :: Table -> Row
header :: Row -> Row
task3 (x:[]) = (map(\x -> if x == "" then 0 else read x :: Integer)  $ tail $ reverse $ tail x)
task3 (x:xs) = zipWith (+) (map(\x -> if x == "" then 0 else read x :: Integer)  $ tail $ reverse $ tail x) (task3(xs))
trex x = (map    (\z -> ( fromInteger(z)    /      (fromInteger(toInteger(length x) - 1)) )) $ (task3 (tail x)))
toRow y = reverse $ map(\x -> printf "%.2f" x ) $ (trex y) 
header x = reverse $ tail $ reverse $ tail x
get_avg_responses_per_qs x =  (header(head x )):((toRow x):[])




-- Task 4
rand :: Integer -> Row -> Row
rand 1 x = tail x
rand n x = tail (rand (n-1) x )
get_exam_summary :: Table -> Table
qunu :: Table ->Integer -> [Integer]
qunu [] n = [0,0,0]
qunu (x:xs) n = zipWith (+)(if y == "0" then [1,0,0] else if y == "1" then [0,1,0] else if y == "2" then [0,0,1]else [1,0,0] ) (qunu(xs) n)
             where y = head $ (rand n x)			 			 			 			 			 
get_exam_summary x = ["Q","0","1","2"] : ("Q1" : (map(\x -> show x) $ (qunu (tail x) 1))) :  ("Q2" : (map(\x -> show x) $ (qunu (tail x) 2))) : ("Q3" : (map(\x -> show x) $ (qunu (tail x) 3))) : ("Q4" :(map(\x -> show x) $ (qunu (tail x) 4))) :  ("Q5": (map(\x -> show x) $ (qunu (tail x) 5))) : (("Q6" : (map(\x -> show x) $ (qunu (tail x) 6))) : [])




-- Task 5
get_ranking :: Table -> Table
get_ranking x = (head $ compute_exam_grades x) : (sortBy (\xs ys -> compare(tail xs,head xs)(tail ys,head ys)) $ (tail $ compute_exam_grades x)) 


-- Task 6
get_exam_diff_table :: Table -> Table
task :: Table -> Table
task [] = []
task (y:xs) =  ((head y) :  printf "%.2f" ((fromInteger(foldr (+) 0 $ map(\x -> if x == "" then 0 else read x :: Integer) $ tail $ reverse $ tail y )) / 4 - ((\x -> if x == "" then 0 else read x :: Float) $ head $ reverse $ tail y) + ((\x -> if x == "" then 0 else read x :: Float) $ head $ reverse $ tail y) ) :   printf "%.2f" ( ((\x -> if x == "" then 0 else read x :: Float) $ head $ reverse $ tail y) )           : printf "%.2f" (abs$(fromInteger(foldr (+) 0 $ map(\x -> if x == "" then 0 else read x :: Integer) $ tail $ reverse $ tail y )) / 4 - ((\x -> if x == "" then 0 else read x :: Float) $ head $ reverse $ tail y) ):[]) : task (xs) 
get_exam_diff_table x =  (head(head x) : ("Punctaj interviu":"Punctaj scris":"Diferenta":[])) :  (sortBy (\xs ys -> compare(head $ reverse xs,head xs)(head $ reverse ys,head ys)) $ (task (tail x)))


-- SET 2




helper :: String -> String ->Row
helper "" "" = [""]
helper "" current = [current]
helper (x:xs) current	
     | x == ','     = current : helper xs ""
     | otherwise    = helper xs (current ++ [x])

helper2 :: String -> String ->Table
helper2 "" "" = []
helper2 "" current = (helper "" current) :[[]]
helper2 (x:xs) current	
     | (x == '\n')   =  (helper  current "") : helper2 xs ""
	 | (x == '\\')   = (helper  current "") : helper2 xs ""
     | otherwise    = helper2 xs (current ++ [x])

read_csv :: CSV -> Table
read_csv "" = [[]]
read_csv str = (helper2 (str++"\\") "") 

join :: String -> Row -> String
join sep xs = foldr  (\a b->  if (b=="" && a=="-") then b else if b=="-" then a else a++sep++b ) "" (xs ++ ["-"])
write_csv :: Table -> CSV
write_csv [[]] =  ""
write_csv (x:[]) =  (reverse $ tail $ reverse $ join "," x) 
write_csv (x:xs) =  (reverse $ tail $ reverse $ join "," x) ++"\n" ++ write_csv xs



--TASK 1

headerul :: Row -> String -> Int
headerul a b = head $ filter ((== b) . (a !!)) [0..]

createList :: Table -> Int -> Row
createList [[]] y  =  []
createList ([x]) y  =  [(x!!y)] 
createList (x:xs) y = (x !!  y) : (createList xs y)

as_list :: String -> Table -> [String]
as_list x y = createList (tail y)  (headerul (head y) x)  


--TASK 2
tsort :: String -> Table -> Table
tsort a b = (head b) : (sortBy (\xs ys -> compare( (xs!!(headerul (head b) a)),head xs)((ys!!(headerul (head b) a)),head ys)) $ ( tail b))



--TASK 3
vmap :: (Value -> Value) -> Table -> Table
vmap f ([x]) = (map f x) :[]
vmap f (x:xs) = (map f x) : (vmap f xs) 

--TASK 4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmapHelp :: (Row -> Row) -> [String] -> Table -> Table
get_hw_grade_total :: Row -> Row
rmapHelp f y ([x]) = f x :[]
rmapHelp f y (x:xs) = (f x) : (rmapHelp f y xs)
rmap f y x = y : (rmapHelp f y (tail x)) 
get_hw_grade_total x = (head x) : ( printf "%.2f"  (foldr (+) 0 $ map(\x -> if x == "" then 0 else read x ::Float) $ tail $ tail x)) : []



--TASK 5
vunion :: Table -> Table -> Table
uniune :: Table -> Table -> Table
uniune x [] = x
uniune [] y = y
uniune [x] y = x : y
uniune (x:xs) y = x : (uniune xs y)
vunion x y = if (head x == head y) then uniune ( x) (tail y) else x 



--TASK 6
hunion :: Table -> Table -> Table
ramas :: Table -> Row -> Table
ramas [x] y = (x ++ y) : []
ramas (x:xs) y =  (x ++ y) : ramas xs y 
hunion [x] [y] = (x ++ y) : []
hunion x [y] = ((head x) ++ y) : (ramas (tail x) (map (\z -> "") $ y  ))
hunion [y] x =((head x) ++ y) :  (ramas (tail x) (map (\z -> "") $ y  ))
hunion (x:xs) (y:ys) = (x ++ y) : hunion xs ys 



--TASK 7
tjoin :: String -> Table -> Table -> Table
getindex :: Row -> String -> Int
getindex a b = head $ filter ((== b) . (a !!)) [0..]

ceva :: String -> Table -> Row
ceva x y =if null (filter p y) == True then (tail $ (map (\z -> "") $ head y  )) else   (filter (\e -> e/=x) $ head $ (filter p y))
     where p z = head z == x

nistejoin :: Table -> Table -> Int -> Table
nistejoin [x] y indice = (x ++ (ceva (x!!indice) y)) :[]
nistejoin (x:xs) y indice = (x ++ (ceva (x!!indice) y)) : (nistejoin xs y indice)    
tjoin str x y = nistejoin x y (getindex (head x) str)



--TASK 8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cart2 :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cart :: (Row -> Row -> Row) -> [String] -> Row -> Table -> Table
cart f str x [y] = (f x y) : []
cart f str x (y:ys) = (f x y) : (cart f str x ys) 
cart2 f str [x] y = (cart f str x y) 
cart2 f str (x:xs) y = uniune (cart f str x y)  (cart2 f str xs y) 
cartesian f str x y = str : (cart2 f str (tail x) (tail y))


--TASK 9
projection :: [String] -> Table -> Table
project :: [String] -> Table -> Table
project [x] y = (as_list x y):[[]]
project (x:xs) y = (as_list x y) : (project xs y)
projection x y = x : transpose (project x y)


--ETAPA 3

 
type EdgeOp = Row -> Row -> Maybe Value
data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

 
data QResult = CSV CSV | Table Table | List [String] 
instance Show QResult where
     show (Table x) = write_csv x
     show (List x) = show x
     show (CSV x) = show x
class Eval a where
    eval :: a -> QResult
 
instance Eval Query where
     eval (FromCSV x) = Table (read_csv x)
     eval (ToCSV x) =  CSV  (show  $  eval (x))
     eval (AsList x y) = List( as_list x $ (read_csv $ show $ eval(y)))
     eval (Sort x y) = Table( tsort x $ (read_csv $ show $ eval(y)))
     eval (ValueMap f y) = Table(vmap f (read_csv $ show $ eval(y)))
     eval (RowMap f x y) = Table(rmap f x (read_csv $ show $ eval(y))) 
     eval (VUnion x y) = Table(vunion (read_csv $ show $ eval(x)) (read_csv $ show $ eval(y)))
     eval (HUnion x y) = Table(hunion (read_csv $ show $ eval(x)) (read_csv $ show $ eval(y)))
     eval (TableJoin a x y) = Table(tjoin a (read_csv $ show $ eval(x)) (read_csv $ show $ eval(y)))
     eval (Cartesian f a x y) = Table(cartesian f a (read_csv $ show $ eval(x)) (read_csv $ show $ eval(y)))
     eval (Projection x y) = Table(projection x (read_csv $ show $ eval(y)))
     eval (Filter a x) = Table $ ((head $ (read_csv $ show $ eval(x))) : (transpose (projection (extra (head $ (read_csv $ show $ eval(x))) (tail $ (read_csv $ show $ eval(x))) a) (transpose $ (read_csv $ show $ eval(x))))))
     eval (Graph cond x) = Table$["From","To","Value"]:(reverse$(graf cond (tail $ read_csv $ show $ eval(x)) (tail $tail $ read_csv $ show $ eval(x))))
     
   

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String 
isFloatin :: String -> [Float]-> Bool
isStringin :: String -> [String] -> Bool       
isFloatin _ [] = False
isFloatin x (y : ys) = if ((x/="")&&(read x ::Float) == y) then True else isFloatin x ys
isStringin _ [] = False
isStringin x (y : ys) = if x == y then True else isStringin x ys
type FilterOp = Row -> Bool 
class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp
    extra :: [String] -> Table -> (FilterCondition a)->[String]
instance FEval Float where
     feval x (Eq y z) b = if (read (b !!(getindex x y))::Float) == z then True else False 
     feval x (Lt y z) b = if (read (b !!(getindex x y))::Float) < z then True else False 
     feval x (Gt y z) b = if (read(b !!(getindex x y))::Float) > z then True else False 
     feval x (In y z) b = if (isFloatin (b !!(getindex x y)) (z))== True then True else False
     feval x (FNot cond) b = not (feval x cond b)
     feval x (FieldEq y z ) b = if (read (b !!(getindex x y))::Float) == (read (b !!(getindex x z))::Float) then True else False 
     extra x [y] a = if (feval x a y) == True then ((y!!0) : []) else []
     extra x (y:ys) a = if (feval x a y) == True then ((y!!0) : (extra x ys a)) else  extra x ys a  
instance FEval String where
     feval x (Eq y z) b = if (b !!(getindex x y)) == z then True else False 
     feval x (Lt y z) b = if (b !!(getindex x y)) < z then True else False 
     feval x (Gt y z) b = if (b !!(getindex x y)) > z then True else False
     feval x (In y z) b = if (isStringin (b !!(getindex x y)) z) == True then True else False
     feval x (FNot cond) b = not (feval x cond b)
     feval x (FieldEq y z ) b = if (b !!(getindex x y)) == (b !!(getindex x z)) then True else False 
     extra x [y] a = if (feval x a y) == True then ((y!!0) : []) else []
     extra x (y:ys) a = if (feval x a y) == True then ((y!!0) : (extra x ys a)) else  extra x ys a  

graf ::EdgeOp -> Table -> Table -> Table
graf2 ::EdgeOp -> Row -> Table -> Table
graf2 cond x [y] = case cond x y of
                    Just n -> if (head y > head x) then (((head x) : (head y) : n :[]): []) else (((head y) : (head x) : n :[]): [])
                    Nothing -> []

graf2 cond x (y:ys) = case cond x y of
                         Just n -> if (head y > head x) then (((head x) : (head y) : n : []) : graf2 cond x ys) else (((head y) : (head x) : n : []) : graf2 cond x ys) 
                         Nothing -> graf2 cond x ys 
graf cond (x:xs) [y] = graf2 cond x [y]
graf cond (x:xs) (y:ys) = uniune   ( (graf cond xs ys))(reverse $ (graf2 cond x (y:ys)))

similarities_query :: Query

sumegal :: Row -> Row -> Integer
sumegal [x] [y] = if(x==y) then 1 else 0
sumegal (x:xs) (y:ys) = if(x==y) then (1 + sumegal xs ys) else (0 + sumegal xs ys)
similar_mai_mic_10 :: EdgeOp
similar_mai_mic_10 (x:xs) (y:ys)
          | (sumegal xs ys >=5 && sumegal xs ys <=9 && x/="" && y /= "") = Just (show(sumegal xs ys))
          | otherwise = Nothing
similar_mai_mare_10 :: EdgeOp
similar_mai_mare_10 (x:xs) (y:ys)
          | (sumegal xs ys >=10 && x/="" && y /= "") = Just (show(sumegal xs ys))
          | otherwise = Nothing          
similarities_query = VUnion (Sort "Value" (Graph similar_mai_mic_10 (FromCSV lecture_grades_csv))) (Sort "Value"(Graph similar_mai_mare_10 (FromCSV lecture_grades_csv)))

editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table ! (m,n)
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
    
    table :: Array (Int,Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
    
    dist (0,j) = j
    dist (i,0) = i
    dist (i,j) = minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]
correct_table :: String -> CSV -> CSV -> CSV
correct_col :: Row -> Row -> Row
replace :: Value -> Int -> Row -> Row
replace x min [y] = if editDistance x y < min then y : [] else []
replace x min (y:ys) = if editDistance x y < min then y : replace x (editDistance x y) ys else replace x min ys
correct_col [x] y = if elem x y == True then x : [] else (head $ tail $ replace x 500 y) : []
correct_col (x:xs) y = if elem x y == True then x : correct_col xs y else (head $ reverse $ replace x 500 y) : correct_col xs y 
correct_table col x y  =  write_csv $ (head $ read_csv x) : (transpose $ ( (correct_col (as_list col $ read_csv x) (as_list col $ read_csv y)):(as_list "Email" $ read_csv x) :[]))




get_hw_grade_2 :: Row -> Row
get_hw_grade_2 x = (head x) : ( printf "%.2f"  (foldr (+) 0 $ map(\x -> if x == "" then 0 else read x ::Float) $ tail x)) : []
lecture_grade :: Row -> Row
lecture_grade x = (head x) :( printf "%.2f"  (2*(foldr (+) 0 $ map(\x -> if x == "" then 0 else read x ::Float) $ tail x)/fromInteger(toInteger(length x) - 1) )) : []

ignore :: Table -> Table
ignore (x:xs) = if ((head x) =="") == False then (x:xs) else ignore xs

grades :: CSV -> CSV -> CSV -> CSV -> CSV
nume_email_punctajcurs :: CSV -> CSV -> CSV-> Table
nume_punctajteme_email_punctajcurs :: CSV -> CSV -> CSV-> Table
nume_punctajteme_punctajcurs_punctajexam :: CSV -> CSV -> CSV -> CSV -> Table

total :: Table -> Table
total [a] = (\x ->if (((if (x!!1) == "" then 0 else read (x!!1) ::Float) + (if (x!!2) == "" then 0 else read (x!!2) ::Float) < 2.5) || ((if (x!!3) == "" then 0 else read (x!!3) ::Float) < 2.5)) then (("4.00":[]):[]) else ((( printf "%.2f"(minimum[((if (x!!1) == "" then 0 else read (x!!1) ::Float)+(if (x!!2) == "" then 0 else read (x!!2) ::Float)),5] + (if (x!!3) == "" then 0 else read (x!!3) ::Float)) ):[]):[]) ) $ a
total (a:xs) = (\x ->if (((if (x!!1) == "" then 0 else read (x!!1) ::Float) + (if (x!!2) == "" then 0 else read (x!!2) ::Float) < 2.5) || ((if (x!!3) == "" then 0 else read (x!!3) ::Float) < 2.5)) then (("4.00":[]):total xs) else ((( printf "%.2f"(minimum[((if (x!!1) == "" then 0 else read (x!!1) ::Float)+(if (x!!2) == "" then 0 else read (x!!2) ::Float)),5] + (if (x!!3) == "" then 0 else read (x!!3) ::Float)) ):[]):total xs) ) $ a

nume_email_punctajcurs w x z = tjoin "Email" (read_csv $ (correct_table "Nume" w x)) (rmap lecture_grade [ "Email","Punctaj Curs"]  ([ "header"]:(ignore $ tail $ tsort "Email" (read_csv z))))
nume_punctajteme_email_punctajcurs w x z = tjoin "Nume" (tsort "Nume" (rmap get_hw_grade_2 ["Nume", "Punctaj Teme"] $ read_csv x)) (nume_email_punctajcurs w x z)   
nume_punctajteme_punctajcurs_punctajexam w x y z = projection ["Nume","Punctaj Teme","Punctaj Curs","Punctaj Exam"] (tjoin "Nume" (nume_punctajteme_email_punctajcurs w x z)  ( compute_exam_grades  $ read_csv y))
grades w x y z = write_csv $  (hunion (nume_punctajteme_punctajcurs_punctajexam w x y z) (("Punctaj Total" :[]) : (total $ tail $ (nume_punctajteme_punctajcurs_punctajexam w x y z))))
