-- interpreter recebe Instrucao, Heap, Estado
-- retorna novo Heap, novo Estado, valor

type ID = String
type Endereco = Double
type Nome = String

data Valor = VString | VDouble | Lambda ID Valor | VAplicar Valor Valor | VBool Bool

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
           | For Termo Termo Termo Termo --- inicio, condicao, pos, corpo

type Definicao = (ID,Valor)

type Classe = (Nome, [(ID, Termo)], [(ID, Termo)]) 

type Objeto = Classe | Valor

type None = 0

type Programa = [Definicao]

type Heap = [(Endereco,Objeto)]

type Estado = [Definicao]

-- pegar variavel do estado

atualiza x v [] = [(x,v)]
atualiza variavel valor ((nome, valor_estado) : resto) = if nome == variavel then (nome,valor):resto else (nome,valor_estado) : atualiza variavel valor resto

inHeap variavel ((nome, objeto) : resto) = if nome == variavel then True else inHeap variavel resto
inHeap variavel [] = False

getEstado variavel ((nome, valor) : resto) = if nome == variavel then (if(inHeap valor) then (getHeap valor) else valor) else getEstado variavel resto
getEstado variavel [] = None
getHeap variavel ((nome, objeto) : resto) = if nome == variavel then objeto else getHeap variavel resto

-- eval dito

isTrue :: Valor -> Bool
isTrue (VBool True) = True
isTrue _ = False


eval estado (LitNum n) = (VNum n, estado)
eval estado (LitBool b) = (VBool b, estado)
eval estado (LitStr s) = (VString s, estado)

-- busca e atribuicao

eval estado (Var x) = (getEstado x estado, estado)
eval estado (Atr x t) =
  let (v, estado1) = eval estado t
      estado2 = atualiza x v estado1
  in (v, estado2)


-- comparacoes
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

avaliarLinhas::Estado -> [Termo] -> Estado
avaliarLinhas estado [] = estado
avaliarLinhas estado (linha:resto) =
  let (_, novoEstado) = eval estado linha
  in avaliarLinhas novoEstado resto