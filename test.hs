import Point
import City
import Quality
import Link
import Tunel
import Region
import Control.Exception
import GHC.IO



--Point
p1 = newP 3 4; p2 = newP 4 9; p3 = newP 1 3


--Cities
avellaneda = newC "Avellaneda" (newP 3 4) ; lanus = newC "Lanus" (newP 6 8) ; banfield = newC "Banfield" (newP 12 16);
lomasdezamora = newC "Lomas de Zamora" (newP 9 12); caballito = newC "Caballito" (newP 3 4)

--Quality
canalSur = newQ "Canal Sur" 4 8.23; canalOeste = newQ "Canal Oeste" 6 10.27;

--Links
linkSur = newL avellaneda lanus canalSur; linkSur2 = newL avellaneda lomasdezamora canalOeste;
linkOeste = newL avellaneda banfield canalOeste; linkOeste2 = newL banfield lanus canalSur

--Tunel
tunelSur = newT [linkSur, linkSur2] ; tunelOeste = newT [linkOeste , linkOeste2]
--Region
reg1 = newR
reg2 = foundR newR avellaneda
reg3 = foundR (foundR newR avellaneda) lanus
reg4 = linkR reg3 avellaneda lanus canalSur
ciudadesReg= foundR (foundR (foundR (foundR newR avellaneda) lanus) banfield) lomasdezamora
linksReg = linkR (linkR (linkR ciudadesReg avellaneda lanus canalSur) lanus banfield canalSur) banfield lomasdezamora canalSur
buenosaires = tunelR linksReg [avellaneda, lanus, banfield, lomasdezamora]


testF :: Show a => a -> Bool --si la funcion devuelve un error, devuelve True, si no devuelve error, devuelve False.
testF action = unsafePerformIO $ do
    result <- tryJust isException (evaluate action)
    return $ case result of
        Left _ -> True
        Right _ -> False
    where
        isException :: SomeException -> Maybe ()
        isException _ = Just ()


testfoundR :: Region -> City -> Bool
testfoundR region city = testF (foundR region city)

testlinkR:: Region -> City -> City -> Quality -> Bool
testlinkR region city1 city2 quality = testF (linkR region city1 city2 quality)

testtunelR:: Region -> [City] -> Bool
testtunelR region cities = testF (tunelR region cities)

testdelayR:: Region -> City -> City -> Bool
testdelayR region city1 city2 = testF (delayR region city1 city2)

testavailableCapacityForR :: Region -> City -> City -> Bool
testavailableCapacityForR region city1 city2 = testF (availableCapacityForR region city1 city2)

t = [testfoundR reg1 avellaneda, testfoundR reg2 avellaneda, testlinkR ciudadesReg avellaneda lanus canalSur, 
 testlinkR ciudadesReg avellaneda caballito canalSur, 
 testtunelR buenosaires [avellaneda,lanus,  banfield, lomasdezamora],testtunelR linksReg [avellaneda, lanus, banfield],
 connectedR buenosaires avellaneda lomasdezamora,connectedR buenosaires avellaneda lanus, 
 linkedR buenosaires lanus banfield, linkedR buenosaires lanus lomasdezamora,testdelayR buenosaires avellaneda lomasdezamora,
 testdelayR buenosaires avellaneda caballito, testdelayR buenosaires avellaneda lanus,
 testavailableCapacityForR buenosaires lanus banfield,testavailableCapacityForR buenosaires lanus lomasdezamora, 
 testavailableCapacityForR buenosaires lanus caballito]