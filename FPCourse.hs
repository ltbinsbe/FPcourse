
-- Constanten definities
-- Multiple declarations of a
a = 4
a = 5


-- Datatype definities
type Param1 = String
type Param2 = Int

data Object = Constructor1 Param1 Param2
            | Constructor2 Object [Int]

data IntList = Nil
             | Cons Int List

data List a = Nil -- []
            | Cons a List -- (:)

-- Functie definities
plus1 :: Int -> Int -- (+) :: Int -> Int -> Int
plus1 a     = 1 + a
plus1' a    = (+) 1 a
plus1''     = (+) 1i -- equational reasoning, point-free style


-- Pattern Matching
param2 :: Object -> Param2
param2 (Constructor1 pm1 pm2) = pm2
param2 (Constructor2 obj is ) = param2 obj -- recursie!


lastElemOr :: [Int] -> Int -> Int
lastElemOr [] alt       = alt
lastElemOr [x] _        = x
lastElemOr (x:xs) alt   = lastElemOr xs alt

-- >> slides





-- PHP
function klantenToUpper($klanten) {
    foreach($klanten as $klant)
        $klant['naam'] = strtoupper($klant['naam']);
    return $klanten;
}

-- Haskell
type Klanten = [Klant]
type Naam    = String
data Klant   = Klant Naam Adres Telefoon

klantToUpper :: Klant -> Klant
klantToUpper (Klant naam adres tel) = (Klant (strtoupper naam) adres tel)

klantenToUpper :: Klanten -> Klanten
klantenToUpper (k:ks) = naamToUpper k : klantenToUpper ks
klantenToUpper [] = []

-- >> slides




verdubbel :: [Int] -> [Int]
verdubbel []     = []
verdubbel (x:xs) = (2 * x) : verdubbel xs

map :: Modifier -> [a] -> [b] -- Modifier :: a -> b
map f []     = []
map f (x:xs) = f x : map f xs

verdubbel' = map (*2) 





sommatie :: [Int] -> Int
sommatie []     = 0
sommatie (x:xs) = x + sommatie xs

foldr :: Operator -> Beginwaarde -> [a] -> b -- Operator :: a -> b -> b
foldr f b []     = b -- Beginwaarde :: b
foldr f b (x:xs) = f x (fold f b xs)

som :: [Int] -> Int
som = foldr (+) 0




ones = 1 : ones
vijfOnes = take 5 ones


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes = (map head (iterate multiples [2..]))
    where 
        multiples (x:xs) = filter (isNotDivBy x) xs
        isNotDivBy x y  = rem y x /= 0
