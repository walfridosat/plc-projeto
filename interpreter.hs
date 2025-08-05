-- interpreter recebe Instrucao, Heap, Estado
-- retorna novo Heap, novo Estado, valor

-- Definições básicas de tipos

type ID = String
type Endereco = Double
type Nome = String

-- Definição dos tipos de variáveis

data Valor = VString String
            | VNum Double 
            | Lambda ID Termo 
            | VAplicar Valor Valor 
            | VBool Bool
            deriving Show

-- Definição da Sintaxe da linguagem

data Termo = Var ID
          | LitNum Double
          | LitBool Bool
          | LitStr String
          | Som Termo Termo
          | If Termo Termo Termo
          | Lam ID Termo
          | Apl Termo Termo
          | Atr ID Termo
          | Seq Termo Termo           
          | New Nome
          | For Termo Termo Termo [Termo] --- inicio, condicao, pos, corpo
          | Maior Termo Termo
          | Igual Termo Termo
          | Menor Termo Termo
          deriving Show

-- Principais definições incluindo: Classe, Objeto, Estado Heap e Programa

type Definicao = (ID,Valor)

type Classe = (Nome, [(ID, Termo)], [(ID, Termo)]) 

data Objeto = ObjClasse Classe | ObjValor Valor

type Programa = [Definicao]

type Heap = [(Endereco,Objeto)]

type Estado = [Definicao]

-- Funções de busca e atualização da Heap e do Estado 

atualiza :: Eq t1 => t1 -> t2 -> [(t1, t2)] -> [(t1, t2)]
atualiza x v [] = [(x,v)]
atualiza variavel valor ((nome, valor_eswtado) : resto) = if nome == variavel then (nome,valor) : resto else (nome,valor_estado) : atualiza variavel valor resto

inHeap :: Eq t => t -> [(t, b)] -> Bool
inHeap variavel ((nome, objeto) : resto) = (nome == variavel) || inHeap variavel resto
inHeap variavel [] = False

getEstado :: ID -> Estado -> Valor
getEstado _ [] = VNum 0
getEstado variavel ((nome, valor):resto)
  | variavel == nome = valor
  | otherwise        = getEstado variavel resto
  
getHeap :: Eq t => t -> [(t, b)] -> b
getHeap variavel ((nome, objeto) : resto) = if nome == variavel then objeto else getHeap variavel resto
getHeap _ [] = error "error: not found in heap"

-- Funções de avaliação

isTrue :: Valor -> Bool
isTrue (VBool True) = True
isTrue (VBool False) = False
isTrue (VNum n) = n>0

eval :: Estado -> Termo -> (Valor, [(ID, Valor)])
eval estado (LitNum n) = (VNum n, estado)
eval estado (LitBool b) = (VBool b, estado)
eval estado (LitStr s) = (VString s, estado)

-- busca e atribuição

eval estado (Var x) = (getEstado x estado, estado)
eval estado (Atr x t) =
  let (v, estado1) = eval estado t
      estado2 = atualiza x v estado1
  in (v, estado2)


-- comparacões Menor, Maior, Igual
eval estado (Menor t u) =
  let (v1, e1) = eval estado t
      (v2, e2) = eval e1 u
  in case (v1,v2) of
    (VNum x, VNum y) -> (VBool (x<y),e2)


eval estado (Maior t u) =
  let (v1, e1) = eval estado t
      (v2, e2) = eval e1 u
  in case (v1,v2) of
    (VNum x, VNum y) -> (VBool (x>y),e2)

eval estado (Igual t u) =
  let (v1, e1) = eval estado t
      (v2, e2) = eval e1 u
  in case (v1,v2) of
    (VNum x, VNum y) -> (VBool (x==y),e2)
    (VBool b1, VBool b2) -> (VBool (b1==b2), e2)
    (VString s1, VString s2) -> (VBool (s1==s2), e2)

eval estado (Som t u) = 
  let (v1, e1) = eval estado t 
      (v2, e2) = eval e1 u 
  in case (v1, v2) of
    (VNum x, VNum y) -> (VNum (x + y), e2)

avaliarLinhas::Estado -> [Termo] -> Estado
avaliarLinhas estado [] = estado
avaliarLinhas estado (linha:resto) =
  let (_, novoEstado) = eval estado linha
  in avaliarLinhas novoEstado resto




  
