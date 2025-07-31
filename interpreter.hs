-- interpreter recebe Instrucao, Heap, Estado
-- retorna novo Heap, novo Estado, valor

type ID = String
type Endereco = Double
type Nome = String

data Valor = String | Double | Lambda ID Valor | Aplicar Valor Valor

type Definicao = (ID,Valor)

type Classe = (Nome,[Definicao])

type Objeto = Classe | Valor

type None = 0

type Programa = [Definicao]

type Heap = [(Endereco,Objeto)]

type Estado = [Definicao]

-- pegar variavel do estado

inHeap variavel ((nome, objeto) : resto) = if nome == variavel then true else inHeap variavel resto
inHeap variavel [] = false

getEstado variavel ((nome, valor) : resto) = if nome == variavel then (if(inHeap valor) then (getHeap valor) else valor) else getEstado variavel resto
getEstado variavel [] = None

getHeap variavel ((nome, objeto) : resto) = if nome == variavel then objeto else getHeap variavel resto
