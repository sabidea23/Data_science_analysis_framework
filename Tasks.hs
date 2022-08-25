
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

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

--compute the average number of steps in the 8 days for a person
average_steps :: Row -> String
average_steps steps = printf "%.2f" (sum (float_rows (tail steps)) / 8)

--turns a list of strings into a float list
float_rows :: Row -> [Float]
float_rows = map read

--compute everyone's average number of steps per given day 
compute_average_steps :: Table -> Table
compute_average_steps = foldr op []
                    where op row acc
                            | head row == "Name" = ["Name", "Average Number of Steps"]: acc
                            | otherwise = [head row, average_steps row]:acc

-- Task 2

--create a float list with the total number of steps 
steps_list :: Table -> [Float]
steps_list m = map ((*8) . (read . last)) (tail m)

--compute how many people have at least 1000 steps given the total number of steps
get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr op 0 (steps_list $ compute_average_steps m)
                    where op steps acc
                                | steps > 1000 = 1 + acc
                                | otherwise = acc

--percentage of people who have achieved their goal 
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral (get_passed_people_num m) :: Float) /
                                (fromIntegral (length $ steps_list m ) :: Float)

--average number of daily steps for all people, compute as the ratio 
--between the total number of steps and the total number of people
get_steps_avg :: Table -> Float
get_steps_avg m = sum (steps_list (compute_average_steps m)) /
                (fromIntegral (length $ steps_list m ) :: Float)

-- Task 3

trans:: Table -> Table
trans ([]:_) = []
trans x = map head x : trans (map tail x)

average_steps_day :: Row -> String
average_steps_day steps = printf "%.2f" (sum (float_rows steps) /
                            fromIntegral (length steps) :: Float)

--compute the average number of steps for the every hour in the eight_hours table
get_avg_steps_per_h_aux :: Table -> Row
get_avg_steps_per_h_aux m  = foldr op [] (tail $ trans $ tail m)
                    where op row acc =  average_steps_day row : acc

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : [get_avg_steps_per_h_aux m]

-- Task 4

--check for a category of activity how many people are in the indicated ranges
count_range :: Row -> [Integer]
count_range m = foldr op [0, 0, 0] (float_rows m)
            where op el acc
                    | el >= 0 && el < 50 = head acc + 1 : tail acc
                    | el >= 50 &&  el < 100 = head acc:((acc !! 1) + 1):tail (tail acc)
                    | otherwise = head acc: head (tail acc):((acc !! 2) + 1): tail (tail $ tail acc)

--build a list with 3 lines, on each line is the number of people in each 
--range of a category of activity
get_activ_summary_aux :: Table -> Table
get_activ_summary_aux m =  foldr op [] (tail $ tail $ tail $ transpose $ tail m)
                        where op row acc = map show (count_range row) : acc

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] :
                    transpose (["VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes"] : transpose (get_activ_summary_aux m))

-- Task 5

-- orders two rows by comparing their last element and if they are equal,
-- then by comparing their first element(name)
ascending_sort :: Row -> Row -> Ordering
ascending_sort row1 row2
            | head (head row1) < head (head row2) = LT
            | head (head row1) > head (head row2) = GT
            | otherwise = if head row1 < head row2 then LT else GT

get_ranking_select :: Table -> Table
get_ranking_select = map (take 2)

get_ranking :: Table -> Table
get_ranking m = ["Name", "Total Steps"] : sortBy ascending_sort (tail $ get_ranking_select m)

-- Task 6

--compute the difference between the average of the first 4 hours and the last 4 hours
diff_hours :: Row -> Float
diff_hours row = abs $ read (average_four_hours $ reverse row) - read (average_four_hours $ tail row)

--compute the sum of the first 4 items in a list
sum_up_to_four_el :: Int -> [Float] -> Float
sum_up_to_four_el 4       _  = 0
sum_up_to_four_el counter [] = 0
sum_up_to_four_el counter (x:xs) = x + sum_up_to_four_el (counter + 1) xs

--compute the average of 4 elements of a list 
average_four_hours :: Row -> String
average_four_hours row = printf "%.2f" (sum_up_to_four_el 0 (float_rows row) / 4)

--build a row from the final table
steps_row :: Row -> Row
steps_row row = [head row, printf "%.2f" (read (average_four_hours $ tail row) :: Float),
                printf "%.2f" (read (average_four_hours $ reverse row) :: Float), (printf "%.2f".diff_hours) row]

--create the unsorted table with the names of the people, 
--the average of the first 4 days, the last 4 days and the difference
unordered_diff_table :: Table -> Table
unordered_diff_table m = foldr op [] (tail m)
                                where op row acc = steps_row row:acc

--build the final table and sort using the previous get_tabeltion
get_steps_diff_table :: Table -> Table
get_steps_diff_table m =  ["Name", "Average first 4h", "Average last 4h", "Difference"] :
                sortBy ascending_sort (unordered_diff_table m)

-- Task 7

-- applies the given get_tabeltion to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (map f)

-- Task 8

min_slept_total :: Row -> String
min_slept_total row = printf "%.2f" (sum $ float_rows $ tail row)

-- applies a get_tabeltion to all entries (rows) in a table
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map get_sleep_total (tail m)

--returns a row with 2 values: email, total number of minutes slept that week.
get_sleep_total :: Row -> Row
get_sleep_total r = [head r, min_slept_total r]

{-
    TASK SET 2
-}

-- Task 1

--sort in ascending order for columns with numeric values and lexicographical for strings
--if the value is missing (“”), then it is considered before all values
ascending_sort' :: Int -> Row -> Row -> Ordering
ascending_sort' pos row1 row2
            |  (row1 !! pos) == "" &&  (row2 !! pos) /= ""  = LT
            |  (row1 !! pos) /= "" &&  (row2 !! pos) == ""  = GT
            |  (read (row1 !! pos) :: Double) < (read (row2 !! pos) :: Double) = LT
            |  (read (row1 !! pos) :: Double) > (read (row2 !! pos) :: Double) = GT
            | otherwise = if head row1 <= head row2 then LT else GT

--returns the index of the column which has a certain name
number_column :: Row -> String -> Int -> Int
number_column [] _ _ = 0
number_column col name index = if head col == name then index else number_column (tail col) name (index + 1)

--sort the tail of the tabel and adds the head with the keys for each column
tsort :: ColumnName -> Table -> Table
tsort column table = head table : sortBy (ascending_sort' (number_column (head table) column 0)) (tail table)

-- Task 2

--checks if all elements of two rows are identical
identical_rows :: Row -> Row -> Bool
identical_rows [] _ = False
identical_rows _[] = False
identical_rows [x] [y]  = x == y
identical_rows row1 row2 = (head row1 == head row2) && identical_rows (tail row1) (tail row2)

--if the header rows are identical than table2 is added at the end of table1
--else table1 is returned
vunion :: Table -> Table -> Table
vunion t1 t2
        | identical_rows (head t1) (head t2) = t1++tail t2
        | otherwise = t1

-- Task 3

--creates a table with n identical rows
add_rows :: Int -> Row -> Table
add_rows 0 row = []
add_rows n row = row : add_rows (n-1) row

--creates a row with n identical elements
add_elems :: Int -> String -> Row
add_elems 0 str = []
add_elems n str = str : add_elems (n-1) str

--takes tables t1 and t2 and extends each row of t1 with a row of t2 
hunion :: Table -> Table -> Table
hunion t1 t2
          |length t1 > length t2 = transpose (transpose t1++
          transpose (t2 ++ add_rows (length t1 - length t2) (add_elems (length (head t2)) "")))
          |length t1 < length t2 = transpose (transpose (t1 ++
          add_rows (length t2 - length t1) (add_elems (length (head t1)) ""))++ transpose t2)
          |otherwise = transpose (transpose t1 ++ transpose t2)

-- Task 4

--if the element with index i1 from the row is equal to an element with
--index i2 from any row of the table then their content is concatenated
--else, the row becomes empty
merge_row ::Int -> Int -> Row-> Row -> Table -> Row
merge_row i1 i2 row erow [] = []
merge_row i1 i2 row erow t = if (head t !! i2) == (row !! i1)
                                then row ++ rem_el (row !! i1) (head t)
                                else merge_row i1 i2 row erow (tail t)

--aplies the get_tabeltion above for each row in the first table
merge_table :: Int -> Int -> Table -> Table -> Table
merge_table i1 i2 [] t2  = []
merge_table i1 i2 t1 t2 = merge_row i1 i2 (head t1) (add_elems (length (head t2) - 1) "") t2:
                          merge_table i1 i2 (tail t1) t2

--remove empty rows from the table
filter_empty :: Table -> Table
filter_empty t = head t : filter (\row -> if length row == 0 then False else True) (tail t)

--removes an element from a row
rem_el :: String -> Row -> Row
rem_el str = filter (/= str)

--if an entry from table 1 has the same value for the key as an entry from table 2, 
--their contents must be merged. If there are other columns with the same name in both tables,
--then the value from t2 overrides the value from t1, unless it is null 
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = filter_empty ((head t1 ++ rem_el key_column (head t2)):
                merge_table (number_column (head t1) key_column 0) 
                (number_column (head t2) key_column 0) (tail t1) (tail t2))

-- Task 5

----the operation performed on a row and a table
row_op :: (Row -> Row -> Row) -> Row -> Table -> Table
row_op f row [] =  []
row_op f row t = f row (head t) : row_op f row (tail t)

--compute the cartesian product of 2 tables
--apply the given operation on each entry in t1 with each entry in t2.
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian new_row_get_tabeltion new_column_names t1 t2  = new_column_names : operation new_row_get_tabeltion (tail t1) (tail t2)
                            where operation new_row_get_tabeltion [] _ = []
                                  operation new_row_get_tabeltion t1 t2  = row_op new_row_get_tabeltion (head t1) t2 ++
                                                       operation new_row_get_tabeltion (tail t1) t2

-- Task 6

--adds the elements with the given index to the output
as_list :: String -> Table -> Row
as_list name t = map (\row -> row !! number_column (head t) name 0) (tail t)

--get_tabeltion which extracts the specified columns 
extract  :: [String] -> Table -> Table
extract [] _ = []
extract header t = (head header : as_list (head header) t) : extract (tail header) t

--extract from a table those columns specified by name in the first argument
projection :: [String] -> Table -> Table
projection columns_to_extract t =  transpose $ extract columns_to_extract t

-- Task 7  

--filter rows based on a given condition applied to a specified column.
filterTable :: (Value -> Bool) -> String -> Table -> Table
filterTable condition key_column t = head t : filter (\row -> condition 
                                (row !! number_column (head t) key_column 0)) (tail t)

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
 
data QResult = Table Table | List [String]

--filters a table according to a condition
filter_table :: FEval a => FilterCondition a -> Table -> Table
filter_table cond table = (head table) : (foldr (\row acc -> if (feval (head table) cond) row == True 
                                then row : acc else acc) [] (tail table))

get_tabel :: QResult -> Table
get_tabel (Table t) = t 

instance Eval Query where
    eval (FromTable t) = Table t
    eval (AsList colname query) = List (as_list colname (get_tabel (eval query)))
    eval (Sort colname query) = Table (tsort colname (get_tabel (eval query)))
    eval (ValueMap op query) = Table (vmap op (get_tabel (eval query)))
    eval (RowMap op colnames query) = Table (rmap op colnames (get_tabel  (eval query)))
    eval (VUnion query1 query2) = Table (vunion (get_tabel (eval query1))
                                                (get_tabel (eval query2)))
    eval (HUnion query1 query2) = Table (hunion (get_tabel (eval query1))
                                                (get_tabel (eval query2)))
    eval (TableJoin colname query1 query2) = Table (tjoin colname (get_tabel (eval query1))
                                                                 (get_tabel (eval query2)))
    eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames 
                                                       (get_tabel (eval query1))
                                                       (get_tabel (eval query2)))
    eval (Projection colnames query) = Table (projection colnames (get_tabel (eval query)))
    eval (Filter cond query) = Table (filter_table cond (get_tabel (eval query)))
    eval (Graph edge_op (FromTable t)) = Table (nub (["From", "To", "Value"]:(concat_lines (tail t) edge_op)))
                            where   concat_lines [x] _    = []
                                    concat_lines [] _    = []
                                    concat_lines (x:xs) op = (check_row x xs op) ++ (concat_lines xs op)
                                     where check_row _ [] _ = []
                                           check_row row (x:xs) op = case (edge_op row x) of 
                                                                        (Just value) -> if ((head row) < head x)
                                                                            then ((head row):(head x):[value]) : (check_row row xs op)
                                                                            else ((head x):(head row):[value]) : (check_row row xs op)
                                                                        Nothing ->  (check_row row xs op)

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

found_el :: Row -> String -> Bool
found_el [] _ = False
found_el row name  = if head row == name then True else found_el (tail row) name

found_el_float :: [Float] -> String -> Bool
found_el_float [] _ = False
found_el_float row name  = if name == "" then if head row == 0 then True 
                                                else found_el_float (tail row) name
                            else if head row == (read name :: Float) then True 
                                                else found_el_float (tail row) name

instance FEval Float where
    feval header (Eq colname ref) = \row -> (if (read (row !! (number_column header colname 0)) :: Float) 
                                            == ref then True else False)
    feval header (Lt colname ref) = \row -> (if (read (row !! (number_column header colname 0)) :: Float) 
                                            < ref then True else False)
    feval header (Gt colname ref) = \row -> (if (read (row !! (number_column header colname 0)) :: Float) 
                                            > ref then True else False)
    feval header (In colname list) = \row -> found_el_float list (row !! (number_column header colname 0))
    feval header (FNot cond) = \row ->(if (feval header cond) row == True then False else True)
    feval header (FieldEq colname1 colname2) = \row -> (if (read (row !! (number_column header colname1 0)) :: Float)
                                 == (read (row !! (number_column header colname2 0)) :: Float) then True else False)

instance FEval String where
    feval header (Eq colname ref) = \row -> (if (row !! (number_column header colname 0)) == ref 
                                            then True else False)
    feval header (Lt colname ref) = \row -> (if (row !! (number_column header colname 0)) < ref 
                                            then True else False)
    feval header (Gt colname ref) = \row -> (if (row !! (number_column header colname 0)) > ref 
                                            then True else False)
    feval header (In colname list) = \row -> found_el list (row !! (number_column header colname 0))
    feval header (FNot cond) = \row ->(if (feval header cond) row == True then False else True)
    feval header (FieldEq colname1 colname2) = \row -> (if (row !! (number_column header colname1 0)) ==
                                              (row !! (number_column header colname2 0)) then True else False)

-- 3.4
type EdgeOp = Row -> Row -> Maybe Value

-- 3.5
similarities_query :: Query
similarities_query = case (eval (Graph edge_op_similaritie (FromTable eight_hours))) of
                            Table t -> (Sort "Value"(FromTable t))

-- go through all strings in both rows and count 
-- the equal values from the same column
check_values :: Row -> Row -> Int
check_values (x:xs) (y:ys) 
    | x == y = 1 + (check_values xs ys)
    | otherwise = (check_values xs ys)
check_values [] _ = 0
check_values _ [] = 0

-- edge operation function for the Graph
edge_op_similaritie :: Row -> Row -> Maybe String
edge_op_similaritie r1 r2 
    | ((check_values r1 r2) >= 5) = Just (show (check_values r1 r2))
    | otherwise = Nothing

-- 3.6 (Typos)
--find distance between two strings
str_dist :: (Eq a) => [a] -> [a] -> Int
str_dist a b = d m n
  where (m, n) = (length a, length b)
        d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = listArray bounds
               [d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))

--finds the best match in a row with the given string
best_match :: String -> Row -> String
best_match name list = foldr (\x acc -> if (str_dist x name) < (str_dist acc name)  then x else acc) [] list

--replaces a certain value in a row
replace_value :: Row -> String -> String -> Row
replace_value row wrong_val val = map (\x -> if x == wrong_val then val else x) row 

correct_table_aux :: String -> Table -> Table -> Table
correct_table_aux val [] t2 = []
correct_table_aux val t1 t2 = if (found_el (as_list val t2) ((head t1) !! (number_column (head t1) val 0))) == True 
                              then ((head t1): (correct_table_aux val (tail t1) t2)) 
                          else (replace_value (head t1)  ((head t1) !! (number_column (head t1) val 0)) 
                            (best_match ((head t1) !! (number_column (head t1) val 0))  (as_list val t2))):
                           (correct_table_aux val (tail t1) t2) 

correct_table :: String -> Table -> Table -> Table
correct_table val t1 t2 = ["Name","Email"] : tail (correct_table_aux val t1 t2)