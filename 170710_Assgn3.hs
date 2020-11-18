--Q1.

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let sort1 = quicksort [a | a <- xs, a <= x]
      sort2 = quicksort [a | a <- xs, a > x]
  in  sort1 ++ [x] ++ sort2

--Q2. 

has :: (Eq a) => [a] -> a -> Bool
has [] _ = False
has (x:xs) a
  | x == a    = True
  | otherwise = has xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)

--Q3

neighbors :: (Ord a1, Ord a2, Num a1, Num a2) =>a1 -> a2 -> [(a1, a2)]
neighbors i j = lefttop ++ top ++ righttop ++ left ++ right ++ leftbot ++ bot ++ rightbot
                where 
                    right  = if((j+1)<9) then [(i,j+1)] else []                    
                    righttop  = if(i-1>=0 && j+1<9) then [(i-1,j+1)] else []
                    rightbot = if(i+1<9 && j+1<9) then [(i+1,j+1)] else []
                    left = if(j-1>=0) then [(i,j-1)] else []
                    lefttop = if(i-1>=0 && j-1>=0) then [(i-1,j-1)] else []
                    leftbot = if(i+1<9 && j-1>=0) then [(i+1,j-1)] else []
                    bot = if(i+1<9) then [(i+1,j)] else []
                    top = if(i-1>=0) then [(i-1,j)] else []
                    
--Q4.

type Line      = String
type Paragraph = [String]
parify :: [Line] -> [Paragraph]
parify [] = []
parify ls 
  | null first = parify rest          
  | otherwise  = first : parify rest
  where first = takeWhile (/= "") ls 
        rest  = dropWhile (== "") . drop (length first) $ ls
singleParaCount :: Paragraph -> Int
singleParaCount = sum . map lineWordCount
lineWordCount :: Line -> Int
lineWordCount = length . words
wordsPerPara :: String -> [Int]
wordsPerPara = map (singleParaCount) . parify . lines

--Q5.

compose_multiple :: [b -> b] -> b -> b
compose_multiple [] val     = val
compose_multiple (f:fs) val = compose_multiple (init $ f:fs) $ (last $ f:fs) val

--Q6.
--1.
data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving Show

--2.

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Nil = Nil
mapTree f (Node x n1 n2) = Node (f x) (mapTree f n1) (mapTree f n2)

--3.

foldTree :: (a -> b -> b -> b)->b -> BinaryTree a -> b
foldTree _ id Nil     = id
foldTree f id (Node x n1 n2) = f x (foldTree f id n1) (foldTree f id n2)

