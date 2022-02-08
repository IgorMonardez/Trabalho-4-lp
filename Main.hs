module Main where
import Data.Char ()
import System.IO ()
import Distribution.ModuleName (main)
import Distribution.Simple (UserHooks(hookedPrograms))

--tipo Data e DataComemorativa
type Data = (Int,Int,Int)
type DataComemorativa = ([Char],Bool,Bool,Int,Int)
type DatasComemorativas = [DataComemorativa]

dia :: Data -> Int
dia (d,_,_) = d

mes :: Data -> Int
mes (_,m,_) = m

ano :: Data-> Int
ano (_,_,a) = a

nome :: DataComemorativa -> [Char]
nome (n,_,_,_,_) = n

feriado :: DataComemorativa -> Bool
feriado (_,f,_,_,_) = f

compara :: Data -> Data -> Int 
compara d1 d2
    |ano d1 > ano d2 = 1
    |ano d2 > ano d1 = -1
    |mes d1 > mes d2 = 1
    |mes d2 > mes d1 = -1
    |dia d1 > dia d2 = 1
    |dia d2 > dia d1 = -1
    |otherwise = 0

toString :: Data -> [Char]
toString d = show (dia d) + "/" + show (mes d) + "/" + show (ano d)

verificaContato :: String -> DatasComemorativas -> Bool
verificaContato nomeDataComemorativa [] = False 
verificaContato nomeDataComemorativa datasComemorativas
    |nomeDataComemorativa == nome (head datasComemorativas) = True
    |otherwise = verificaContato nomeDataComemorativa (tail datasComemorativas)

alterar :: DataComemorativa -> DatasComemorativas -> DatasComemorativas
alterar dataComemorativa [] = []
alterar dataComemorativa datasComemorativas
    | nome dataComemorativa == nome (head datasComemorativas) = dataComemorativa : tail datasComemorativas
    | otherwise = head datasComemorativas : alterar dataComemorativa (tail datasComemorativas)

inserir ::DataComemorativa -> DatasComemorativas -> DatasComemorativas
inserir dataComemorativa [] = [dataComemorativa]
inserir dataComemorativa datasComemorativas
    |verificaContato (nome dataComemorativa) datasComemorativas = alterar dataComemorativa datasComemorativas
    |otherwise = datasComemorativas ++ [dataComemorativa]

remover :: [Char] -> DatasComemorativas -> DatasComemorativas
remover nomeDataComemorativa [] = []
remover nomeDataComemorativa datasComemorativas
    | datasComemorativas == nome(head datasComemorativas) =  tail datasComemorativas
    | otherwise = head datasComemorativas : remover nomeDataComemorativa (tail datasComemorativas)

horasNaoTrabalhadas :: DatasComemorativas -> Int 
horasNaoTrabalhadas [] = 0
horasNaoTrabalhadas datasComemorativas
    |feriado (head datasComemorativas) = 1 + horasNaoTrabalhadas (tail datasComemorativas)
    |otherwise = horasNaoTrabalhadas(tail datasComemorativas)

