module Main where
import Data.Char ()
import System.IO ()
import Distribution.Simple (UserHooks(hookedPrograms))

--tipo Data e DataComemorativa
type Data = (Int,Int,Int)

data DataComemorativa = Comemorativa { dataNome :: [Char],
                                       dataFeriado :: Bool,
                                       dataFeriadoMundial :: Bool,
                                       dataDia :: Int,
                                       dataMes :: Int
                                    }deriving(Show)

type DatasComemorativas = [DataComemorativa]

dia :: Data -> Int
dia (d,_,_) = d

mes :: Data -> Int
mes (_,m,_) = m

ano :: Data-> Int
ano (_,_,a) = a


adicionaDia :: Data -> Int -> Data

adicionaDia (d,m,a) qtd = (mod (d + qtd) 30,m + mod(div (d + qtd) 30) 12,a + div (m + div (d+qtd) 30) 12)
adicionaMes :: Data -> Int -> Data
adicionaMes (d,m,a) qtd = (d,mod (m + qtd) 12,a + div (m+qtd) 12)
adicionaAno :: Data -> Int -> Data
adicionaAno (d,m,a) qtd = (d,m,a + qtd)


compara :: Data -> Data -> Int
compara d1 d2
    |ano d1 > ano d2 = 1
    |ano d2 > ano d1 = -1
    |mes d1 > mes d2 = 1
    |mes d2 > mes d1 = -1
    |dia d1 > dia d2 = 1
    |dia d2 > dia d1 = -1
    |otherwise = 0

comparaDataComemorativa :: Data -> DataComemorativa -> Int
comparaDataComemorativa (d,m,a) dataComemorativa
    |m > dataMes dataComemorativa = 1
    |dataMes dataComemorativa > m = -1
    |d > dataDia dataComemorativa = 1
    |dataDia dataComemorativa > d = -1
    |otherwise = 0

strData :: Data -> String
strData d = show (dia d) ++ "/" ++ show (mes d) ++ "/" ++ show (ano d)

verificaContato :: String -> DatasComemorativas -> Bool
verificaContato nomeDataComemorativa [] = False
verificaContato nomeDataComemorativa datasComemorativas
    |nomeDataComemorativa == dataNome (head datasComemorativas) = True
    |otherwise = verificaContato nomeDataComemorativa (tail datasComemorativas)

alterar :: DataComemorativa -> DatasComemorativas -> DatasComemorativas
alterar dataComemorativa [] = []
alterar dataComemorativa datasComemorativas
    | dataNome dataComemorativa == dataNome (head datasComemorativas) = dataComemorativa : tail datasComemorativas
    | otherwise = head datasComemorativas : alterar dataComemorativa (tail datasComemorativas)

inserir ::DataComemorativa -> DatasComemorativas -> DatasComemorativas
inserir dataComemorativa [] = [dataComemorativa]
inserir dataComemorativa datasComemorativas
    |verificaContato (dataNome dataComemorativa) datasComemorativas = alterar dataComemorativa datasComemorativas
    |otherwise = datasComemorativas ++ [dataComemorativa]

remover :: [Char] -> DatasComemorativas -> DatasComemorativas
remover nomeDataComemorativa [] = []
remover nomeDataComemorativa datasComemorativas
    | nomeDataComemorativa == dataNome (head datasComemorativas) =  tail datasComemorativas
    | otherwise = head datasComemorativas : remover nomeDataComemorativa (tail datasComemorativas)

horasNaoTrabalhadas :: DatasComemorativas -> Int
horasNaoTrabalhadas [] = 0
horasNaoTrabalhadas datasComemorativas
    |dataFeriado (head datasComemorativas) = 8 + horasNaoTrabalhadas (tail datasComemorativas)
    |otherwise = horasNaoTrabalhadas(tail datasComemorativas)

main :: IO()
main = do
    let dataAtual = (09,02,2022)
    let natal = Comemorativa "Natal" True True 25 12
    print (comparaDataComemorativa dataAtual natal)
    let colecaoDatas = inserir natal colecaoDatas
    print (horasNaoTrabalhadas colecaoDatas)
