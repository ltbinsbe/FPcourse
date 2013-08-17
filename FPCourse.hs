
-- Multiple declarations of a
a = 4
a = 5

plus1 a     = 1 + a
plus1' a    = (+) 1 a
plus1''     = (+) 1






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


