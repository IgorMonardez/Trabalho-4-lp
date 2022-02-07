type Dia = Int
type Mes = Int
type Ano = Int
type Caracter = Char
type Feriado = Bool
type FeriadoMundial = Bool
type Nome = [Caracter]
data Data = Atual {
              -- |
              dia :: Dia,
              -- |
              mes :: Mes,
              -- |
              ano :: Ano}
          | Comemorativa {
              -- |
              nome :: Nome,
              -- |
              feriado :: Feriado,
              -- |
              feriadoMundial :: FeriadoMundial,
              -- |
              dia :: Dia,
              -- |
              mes :: Mes}

