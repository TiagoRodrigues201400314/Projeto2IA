# Manual Técnico

## Introdução
Este manual técnico documenta o design, a implementação e a análise do programa de inteligência artificial para a implementação do Jogo do Cavalo, em que é possivel jogar Humano Vs Computador ou Computador Vs Computador. O objetivo é fornecer uma visão detalhada do funcionamento interno do programa, incluindo os algoritmos utilizados, a estrutura de dados e as limitações conhecidas.

## Algoritmo Geral
### Descrição do Algoritmo
#### Ficheiro Algoritmo.lisp

##### Esta função cria um estado. 
Cria um estado, sendo esse composto por:
- Tabuleiro
- Pontuação do jogador 1
- Pontuação do jogador 2
```
(defun cria-estado (tabuleiro)
  "Cria um estado inicial para o jogo com o tabuleiro fornecido e pontuação zero para os dois jogadores."
  (list tabuleiro 0 0)
)
```

##### Esta função cria um estado. 
Cria um nó, sendo esse composto por:
- Estado
- Profundidade
- Nó pai
```
(defun cria-no (estado &optional (profundidade 0) (pai nil))
  "Cria um nó no contexto do algoritmo de busca, contendo o estado do jogo, a profundidade atual na árvore de busca e o nó pai."
  (list estado profundidade pai)
)
```

##### Esta função retorna o estado de um nó.
Retorna o estado associado a um nó
```
(defun no-estado(no)
  "Retorna o estado associado a um nó."
  (car no)
)
```

##### Esta função retorna a profundidade de um nó.
Retorna a profundidade de um nó na árvore de busca
```
(defun no-profundidade (no)
  "Retorna a profundidade de um nó na árvore de busca."
  (cadr no)
)
)
```

##### Esta função retorna o nó pai de um nó.
```
(defun no-pai (no)
  "Retorna o nó pai de um dado nó."
  (car (last no))
)
```

##### Esta função incrementa a profundidade de um nó
Cria um novo nó com a profundidade incrementada em relação ao nó fornecido
```
(defun incrementar-profundidade (no)
  "Cria um novo nó com a profundidade incrementada em relação ao nó fornecido."
  (list (no-estado no) (1+ (no-profundidade no)) (no-pai no))
)
```

##### Esta função permite retornar um nó sucessor.
Aplica um operador ao estado do nó para gerar um novo estado, incrementa a profundidade e retorna um novo nó sucessor
```
(defun novo-sucessor (no operador jogador)
  "Aplica um operador ao estado do nó para gerar um novo estado, incrementa a profundidade e retorna um novo nó sucessor."
  (let ((novo-estado (funcall operador (no-estado no) jogador)))
    (cond 
     ((null novo-estado) nil)
     (t (cria-no novo-estado (1+ (no-profundidade no)) no))
    ))
)
```

##### Esta função retorna uma lista de sucessores
Gera todos os nós sucessores aplicando uma lista de operadores ao estado do nó atual.
```
(defun sucessores (no lista-operadores jogador)
  "Gera todos os nós sucessores aplicando uma lista de operadores ao estado do nó atual."
  (cond 
   ((null (no-estado no)) nil)
   (t (remove nil (mapcar (lambda (operador) (novo-sucessor no operador jogador)) lista-operadores)))
  )
)
```

##### Esta função verifica se um nó já existe em uma lista de nós
```
(defun no-existep (no lista)
   "Verifica se um nó já existe em uma lista de nós."
   (not (null (find (no-estado no) (mapcar #'no-estado lista))))
)
```

##### Esta função permite retornar o jogador oposto
Muda o jogador atual para o outro jogador.
```
(defun outro-jogador (jogador)
  "Muda o jogador atual para o outro jogador."
  (cond
   ((eq jogador *jogador1*) *jogador2*)
   (t *jogador1*)
  )
)
```

##### Esta função calcula a soma total de pontos que ainda estão no tabuleiro.
```
(defun pontos-no-tabuleiro (tabuleiro)
  "Calcula a soma total de pontos que ainda estão no tabuleiro."
  (cond
   ((null tabuleiro) 0)
   ((or (equal (car tabuleiro) nil) (equal (car tabuleiro) -1) (equal (car tabuleiro) -2)) (pontos-no-tabuleiro (cdr tabuleiro)))
   (t (+ (car tabuleiro) (pontos-no-tabuleiro (cdr tabuleiro))))
  )
)
```

##### Esta função conta o número total de casas disponíveis no tabuleiro.
```
(defun casas-disponiveis (tabuleiro)
  "Conta o número total de casas disponíveis no tabuleiro."
  (cond
   ((null tabuleiro) 0)
   ((or (equal (car tabuleiro) nil) (equal (car tabuleiro) -1) (equal (car tabuleiro) -2)) (casas-disponiveis (cdr tabuleiro)))
   (t (+ (casas-disponiveis (cdr tabuleiro)) 1))
  )
)
```

##### Esta função retorna a pontuação dos jogadores.
Retorna a pontuação do jogador no estado dado.
```
(defun pontos-jogador (estado jogador)
  "Retorna a pontuação do jogador no estado dado."
  (cond
   ((eq jogador *jogador1*) (cadr estado))
   (t (caddr estado))
  )
)
```

##### Esta função calcula a diferença de pontos entre os jogadores.
Calcula a diferença de pontos entre o jogador e o seu oponente no estado do nó.
```
(defun diferenca-pontos (no jogador)
  "Calcula a diferença de pontos entre o jogador e o seu oponente no estado do nó."
  (- (pontos-jogador (car no) jogador) (pontos-jogador (car no) (outro-jogador jogador)))
)
```

##### Esta função é a função principal responsável pelo algoritmo Alfabeta.
```
(defun alfabeta (no jogador tempo-limite &optional (profundidade *profundidade*) (alfa -9999) (beta 9999) (tempo-final (+ (get-internal-real-time) tempo-limite)) (jogador-max jogador))
  "Função principal do algoritmo Alfabeta"
  (cond 
   ((or (solucaop (car no) jogador)
        (>= (no-profundidade no) profundidade)
        (>= (get-internal-real-time) tempo-final)
        (condicao-vitoria (car no))
        (null no))
    (diferenca-pontos no jogador))
   (t (let ((sucessores (ordenar-nos (sucessores no (lista-movimentos (car no) jogador) jogador) jogador)))
        (cond 
         ((jogador-max jogador jogador-max) (progn
                                              (setf *nos-expandidos* (+ *nos-expandidos* (length sucessores)))
                                              (alfabeta-max sucessores jogador jogador-max tempo-limite profundidade alfa beta tempo-final)))
         (t (progn 
              (setf *nos-expandidos* (+ *nos-expandidos* (length sucessores)))
              (alfabeta-min sucessores jogador jogador-max tempo-limite profundidade alfa beta tempo-final)))
        )))
  )
)
```

##### Esta função é responsável por lidar com os nós MAX.
```
(defun alfabeta-max (sucessores jogador jogador-max tempo-limite profundidade alfa beta tempo-inicio)
  "Função responsável por lidar com os nós MAX"
  (cond 
   ((or (null sucessores)) alfa)
   (t (let* ((no-expandido (car sucessores))
             (valor (alfabeta no-expandido (outro-jogador jogador) jogador-max tempo-limite profundidade alfa beta tempo-inicio))
             (novo-alfa (max alfa valor)))
        (cond 
         ((>= novo-alfa beta) (setf *nos-cortados* (1+ *nos-cortados*)) beta)
         (t (progn
              (setf *jogada-pc* no-expandido)
              (setf *nos-analisados* (1+ *nos-analisados*))
              (max novo-alfa (alfabeta-max (cdr sucessores) jogador tempo-limite profundidade alfa beta tempo-inicio jogador-max))))
        )))
  )
)
```

##### Esta função é responsável por lidar com os nós MIN.
```
(defun alfabeta-min (sucessores jogador jogador-max tempo-limite profundidade alfa beta tempo-inicio)
  "Função responsável por lidar com os nós MIN"
  (cond 
   ((or (null sucessores)) beta)
   (t (let* ((no-expandido (car sucessores))
             (valor (alfabeta no-expandido (outro-jogador jogador) jogador-max tempo-limite profundidade alfa beta tempo-inicio))
             (novo-beta (min beta valor)))
        (cond
         ((<= novo-beta alfa) (setf *nos-cortados* (1+ *nos-cortados*)) alfa)
         (t (progn
              (setf *jogada-pc* no-expandido)
              (setf *nos-analisados* (1+ *nos-analisados*))
              (min novo-beta (alfabeta-min (cdr sucessores) jogador jogador-max tempo-limite profundidade alfa beta tempo-inicio))))
        )))
  )
)
```

##### Esta função avalia um estado do jogo baseado na diferença do número de movimentos possíveis entre o jogador e o adversário.
```
(defun heuristica-no (estado jogador)
  "Avalia um estado do jogo baseado na diferença do número de movimentos possíveis entre o jogador e o adversário."
  (let ((movimentos-jogador (length (calcular-jogadas-possiveis estado jogador)))
        (movimentos-oponente (length (calcular-jogadas-possiveis estado (outro-jogador jogador)))))
    (- movimentos-jogador movimentos-oponente))
)
```

##### Esta função ordena os nós na lista de nós com base na pontuação heurística de cada nó.
```
(defun ordenar-nos (lista-nos jogador)
  "Ordena os nós na lista de nós com base na pontuação heurística de cada nó."
  (sort (copy-list lista-nos) 
        #'(lambda (no1 no2) 
            (> (heuristica-no (car no1) jogador) 
               (heuristica-no (car no2) jogador))))
)
```

#### Ficheiro Jogo.lisp

##### Esta função gera uma lista sequencial de números de 0 até 99..
```
(defun gera-numeros (n)
  "Gera uma lista sequencial de números de 0 até 99."
  (cond
   ((= n 100) nil)
   (t (cons n (gera-numeros (1+ n))))
  )
)
```

##### Esta função embaralha aleatoriamente os elementos de uma lista.
```
(defun embaralha-lista (lista)
  "Embaralha aleatoriamente os elementos de uma lista."
  (cond 
   ((null lista) nil)
   (t (let ((rand-pos (random (length lista))))
        (cons (nth rand-pos lista)
              (embaralha-lista (remove (nth rand-pos lista) lista :count 1)))))
  )
)
```

##### Esta função particiona uma lista em sub-listas de um dado tamanho.
```
(defun particiona-lista (lista tamanho)
  "Particiona uma lista em sub-listas de um dado tamanho."
  (cond 
   ((null lista) nil)
   (t (cons (subseq lista 0 tamanho)
            (particiona-lista (nthcdr tamanho lista) tamanho)))
  )
)
```

##### Esta função a ser chamada para criar tabuleiro aleatório.
```
(defun cria-tabuleiro ()
  "Função a ser chamada para criar tabuleiro aleatório"
  (particiona-lista (embaralha-lista (gera-numeros 0)) 10)
)
```

##### Esta função retorna a posição da casa com a maior pontuação na linha.
```
(defun maior-pontuacao-na-linha (linha &optional coluna maior-pontuacao posicao-maior-pontuacao)
  "Retorna a posição da casa com a maior pontuação na linha."
  (cond 
   ((null linha) posicao-maior-pontuacao)
   ((or (null maior-pontuacao) (> (car linha) maior-pontuacao))
    (maior-pontuacao-na-linha (cdr linha) (1+ coluna) (car linha) coluna))
   (t (maior-pontuacao-na-linha (cdr linha) (1+ coluna) maior-pontuacao posicao-maior-pontuacao))
  )
)
```

##### Esta função coloca o cavalo na melhor posição da primeira ou última linha com base no jogador.
```
(defun primeira-jogada (estado jogador)
  "Coloca o cavalo na melhor posição da primeira ou última linha com base no jogador."
  (let* ((linha-escolhida (cond
                           ((= jogador *jogador1*) (first (car estado)))
                           (t (first (last (car estado))))
                          ))
         (melhor-posicao (maior-pontuacao-na-linha linha-escolhida 0 0 0)))
    (insert-cavalo (cond 
                    ((= jogador *jogador1*) 0)
                    (t (- (length (car estado)) 1))
                   ) melhor-posicao estado jogador))
)
```

##### Esta função calcula o número simétrico.
```
(defun numero-simetrico (numero)
  "Calcula o número simétrico."
  (cond 
   ((and (numberp numero) 
         (>= numero 10)) 
    (let ((dezena (floor numero 10))
          (unidade (mod numero 10)))
      (+ (* unidade 10) dezena)))
   (t nil)
  )
)
```

##### Esta função encontra a posição de um número no tabuleiro e mete a NIL.
```
(defun encontrar-posicao-numero (tabuleiro num)
  "Encontra a posição de um número no tabuleiro e mete a NIL"
  (let ((posicao (position num (reduce #'append tabuleiro))))
    (cond
     (posicao (let* ((linha (floor (/ posicao (length (first tabuleiro)))))
                     (coluna (mod posicao (length (first tabuleiro)))))
                (substituir linha coluna tabuleiro 'nil)))
     (t tabuleiro)))
)
```

##### Esta função verifica se um número é um duplo.
```
(defun numero-duplo (numero)
  "Verifica se um número é um duplo"
  (and (>= numero 11) (<= numero 99) (= (mod numero 10) (floor numero 10)))
)
```

##### Esta função coleta todos os números duplos do tabuleiro.
```
(defun coletar-numeros-duplos (tabuleiro)
  "Coleta todos os números duplos do tabuleiro."
  (remove-if-not #'numero-duplo (flatten tabuleiro))
)
```

##### Esta função transforma uma lista de listas em uma única lista.
```
(defun flatten (lista)
  "Transforma uma lista de listas em uma única lista."
  (cond 
   ((null lista) nil)
   (t (cond
       ((listp (first lista)) (append (flatten (first lista)) (flatten (rest lista))))
       (t (cons (first lista) (flatten (rest lista))))
      ))
  )
)
```

##### Esta função aplica as regras de números simétricos e duplos ao estado do jogo para a jogada humano.
```
(defun aplicar-regra-simetrico-duplo (num estado jogador)
  "Aplica as regras de números simétricos e duplos ao estado do jogo."
  (cond
   ((numero-duplo num) (cond 
                        ((eq *jogador1* jogador) (list (aplicar-regra-duplos (car estado)) (nth 1 estado) (nth 2 estado)))
                        (t (list (aplicar-regra-duplos-computador (car estado)) (nth 1 estado) (nth 2 estado)))
                       ))
   ((numero-simetrico num) (list (encontrar-posicao-numero (car estado) (numero-simetrico num)) (nth 1 estado) (nth 2 estado)))
   (t estado))
)
```

##### Esta função aplica a regra do número simétrico ou do duplo e retorna o estado completo.
```
(defun aplicar-regra-simetrico-duplo-computador (num estado)
  "Aplica a regra do número simétrico ou do duplo e retorna o estado completo."
  (cond
   ((numero-duplo num) (list (aplicar-regra-duplos-computador (car estado)) (nth 1 estado) (nth 2 estado)))
   ((numero-simetrico num) (list (encontrar-posicao-numero (car estado) (numero-simetrico num)) (nth 1 estado) (nth 2 estado)))
   (t estado))
)
```

##### Esta função aplica a regra dos números duplos no tabuleiro para o computador.
```
(defun aplicar-regra-duplos-computador (tabuleiro)
  "Aplica a regra dos números duplos no tabuleiro para o computador."
   (encontrar-posicao-numero tabuleiro (encontrar-maior-duplo tabuleiro))
)
```

##### Esta função busca o maior número duplo disponível no tabuleiro.
```
(defun encontrar-maior-duplo (tabuleiro)
  "Busca o maior número duplo disponível no tabuleiro."
  (labels 
      ((busca (num)
         (cond
          ((< num 11) nil)
          ((find num (reduce #'append tabuleiro)) num)
          (t (busca (- num 11)))))
       )
    (busca 99)
  )
)
```

##### Esta função verifica se a posição i (linha) e j (coluna) está dentro do tabuleiro.
```
(defun valida-posicao (i j)
  "Verifica se a posição está dentro do tabuleiro"
  (and (>= i 0) (< i 10) (>= j 0) (< j 10))
)
```

##### Esta função verifica se a casa na posição (l, c) do tabuleiro contém um número.
```
(defun casa-vazia (l c tabuleiro)
  "Verifica se a casa na posição (l, c) do tabuleiro contém um número."
  (numberp (celula l c tabuleiro))
)
```

##### Esta função verifica todas as possibilidades de jogadas.
```
(defun validacao-jogada (i j tabuleiro jogador)
  "Metodo que verifica todas as possibilidades"
  (cond
   ((and 
     (valida-posicao i j)
     (casa-vazia i j tabuleiro)
     (casa-ameacada i j tabuleiro jogador)
     (jogador-na-posicao i j tabuleiro)) 
    'T)
   (t nil)
  )
)
```

##### Esta função verifica se a casa na posição (i, j) está ameaçada pelo adversário.
```
(defun casa-ameacada (i j tabuleiro jogador)
  "Verifica se a casa na posição (i, j) está ameaçada pelo adversário."
  (let ((adversario (cond ((= jogador *jogador1*) *jogador2*) (t *jogador1*))))
    (let ((posicao-adversario (posicao-cavalo tabuleiro adversario)))
      (cond 
       ((null posicao-adversario) t)
       (t (let ((movimentos-possiveis '((2 1) (1 2) (-1 2) (-2 1) (-2 -1) (-1 -2) (1 -2) (2 -1))))
            (cond 
             ((some #'(lambda (mov)
                        (let ((nova-pos-i (+ (first posicao-adversario) (first mov)))
                              (nova-pos-j (+ (second posicao-adversario) (second mov))))
                          (and (valida-posicao nova-pos-i nova-pos-j)
                               (= nova-pos-i i)
                               (= nova-pos-j j))))
                    movimentos-possiveis)
              nil)
             (t t)
            )))
      )))
)
```

##### Esta função verifica se algum jogador está na posição (i, j) do tabuleiro.
```
(defun jogador-na-posicao (i j tabuleiro)
  "Verifica se algum jogador está na posição (i, j) do tabuleiro."
  (let ((posicao (celula i j tabuleiro)))
    (cond
     ((or (eql posicao *jogador1*) (eql posicao *jogador2*)) nil)
     (t t)
    ))
)
```

##### Esta função calcula e retorna uma lista de jogadas possíveis para o jogador.
```
(defun calcular-jogadas-possiveis (estado jogador)
  "Calcula e retorna uma lista de jogadas possíveis para o jogador."
  (let ((posicao-atual (posicao-cavalo (car estado) jogador))
        (movimentos-possiveis '((2 1) (1 2) (-1 2) (-2 1) (-2 -1) (-1 -2) (1 -2) (2 -1))))
    (remove-if-not #'(lambda (mov)
                       (let ((nova-i (+ (first posicao-atual) (first mov)))
                             (nova-j (+ (second posicao-atual) (second mov))))
                         (validacao-jogada nova-i nova-j (car estado) jogador)))
                   movimentos-possiveis))
)
```

##### Esta função verifica se o cavalo do jogador pode se mover.
```
(defun cavalo-pode-mover (estado jogador)
  "Verifica se o cavalo do jogador pode se mover."
  (let ((posicao-cavalo (posicao-cavalo (car estado) jogador)))
    (some #'(lambda (mov)
              (let ((novo-i (+ (first posicao-cavalo) (first mov)))
                    (novo-j (+ (second posicao-cavalo) (second mov))))
                (validacao-jogada novo-i novo-j (car estado) jogador)))
          '((2 1) (1 2) (-1 2) (-2 1) (-2 -1) (-1 -2) (1 -2) (2 -1))))
)
```

##### Esta função verifica se ambos os cavalos não podem se mover.
```
(defun condicao-vitoria (estado)
  "Verifica se ambos os cavalos não podem se mover."
  (not (or (cavalo-pode-mover estado *jogador1*)
           (cavalo-pode-mover estado *jogador2*)))
)
```

##### Esta função move o cavalo no tabuleiro, aplicando as regras de movimentação.
```
(defun move-cavalo (i j estado jogador &optional (tipo-jogo 2))
  "Move o cavalo no tabuleiro, aplicando as regras de movimentação."
  (let ((posicao-atual (posicao-cavalo (car estado) jogador)))
    (cond 
     ((null posicao-atual) nil)
     (t (let* ((novo-i (+ (first posicao-atual) i))
               (novo-j (+ (second posicao-atual) j))
               (novo-numero (celula novo-i novo-j (car estado)))
               (tabuleiro-sem-cavalo (substituir (first posicao-atual) (second posicao-atual) (car estado) nil)))
          (cond 
           ((validacao-jogada novo-i novo-j (car estado) jogador)
            (let ((novo-estado (atualizar-estado novo-i novo-j tabuleiro-sem-cavalo estado jogador novo-numero tipo-jogo)))
              novo-estado))
           (t nil)
          )))
    ))
)
```

##### Esta função atualiza o estado do jogo após um movimento do cavalo.
```
(defun atualizar-estado (novo-i novo-j tabuleiro-sem-cavalo estado jogador novo-numero tipo-jogo)
  "Atualiza o estado do jogo após um movimento do cavalo."
  (let ((novo-tabuleiro (substituir novo-i novo-j tabuleiro-sem-cavalo jogador))
        (nova-pontuacao (adicionar-pontuacao estado jogador novo-numero)))
  (cond 
   ((eq 2 tipo-jogo) (aplicar-regra-simetrico-duplo-computador novo-numero (list novo-tabuleiro (nth 1 nova-pontuacao) (nth 2 nova-pontuacao))))
   (t (aplicar-regra-simetrico-duplo novo-numero (list novo-tabuleiro (nth 1 nova-pontuacao) (nth 2 nova-pontuacao)) jogador))
  ))
)
```

##### Esta função insere o cavalo no tabuleiro pela primeira vez."
```
(defun insert-cavalo (i j estado jogador &optional (tipo-jogo 1))
  "Move o cavalo no tabuleiro, aplicando as regras de movimentação."
  (cond 
   ((validacao-jogada i j (car estado) jogador)
    (let ((novo-numero (celula i j (car estado))))
      (let ((novo-estado (atualizar-estado i j (car estado) estado jogador novo-numero tipo-jogo)))
        novo-estado)))
   (t nil)
  )
)
```

##### Esta função substitui o elemento na posição (i, j) do tabuleiro pelo valor fornecido.
```
(defun substituir (i j tabuleiro &optional (valor nil))
  "Substitui o elemento na posição (i, j) do tabuleiro pelo valor fornecido."
    (let ((linha (nth i tabuleiro)))
    (let ((nova-linha (substituir-linha j linha valor)))
      (substituir-linha i tabuleiro nova-linha)))
)
```

##### Esta função substitui a linha de índice especificado no tabuleiro pela nova linha fornecida.
```
(defun substituir-linha (indice tabuleiro nova-linha)
  "Substitui a linha de índice especificado no tabuleiro pela nova linha fornecida."
  (cond 
   ((null tabuleiro) nil)
   ((= indice 0) (cons nova-linha (cdr tabuleiro)))
   (t (cons (car tabuleiro) (substituir-linha (1- indice) (cdr tabuleiro) nova-linha)))
  )
)
```

##### Esta função encontra qual a posição do cavalo.
```
(defun posicao-cavalo (tabuleiro jogador)
  "Encontra qual a posição do cavalo"
  (labels ((aux (tab i j)
             (cond
               ((null tab) nil)
               ((null (car tab)) (aux (cdr tab) (1+ i) 0)) 
               ((eql jogador (caar tab)) (list i j)) 
               (t (aux (cons (cdr (car tab)) (cdr tab)) i (1+ j))))))
    (aux tabuleiro 0 0)
  )
)
```

##### Esta função adiciona o valor da célula (i, j) à pontuação do jogador especificado.
```
(defun adicionar-pontuacao (estado jogador novo-numero)
  "Adiciona o valor da célula (i, j) à pontuação do jogador especificado."
  (cond 
   ((= jogador *jogador1*) (list (car estado) (+ (nth 1 estado) novo-numero) (nth 2 estado)))
   ((= jogador *jogador2*) (list (car estado) (nth 1 estado) (+ (nth 2 estado) novo-numero)))
   (t estado)
  )
)
```

##### Esta função determina se um estado de jogo é um estado solução para o jogador especificado.
```
(defun solucaop (estado jogador)
  "Determina se um estado de jogo é um estado solução para o jogador especificado."
  (cond 
   ((eq (length (calcular-jogadas-possiveis estado jogador)) 0) T)
   (t nil)
  )
)
```

##### Estas funções movem o cavalo em todas as possíveis combinações de movimentos de cavalo no xadrez.
```
(defun move-cavalo-2-1 (estado jogador)
  "Movimenta o cavalo do jogador na direção (-2, -1) no estado do jogo."
  (funcall 'move-cavalo -2 -1 estado jogador)
)

(defun move-cavalo-2_1 (estado jogador)
  "Movimenta o cavalo do jogador na direção (-2, 1) no estado do jogo."
  (funcall 'move-cavalo -2 1 estado jogador)
)

(defun move-cavalo-1-2 (estado jogador)
  "Movimenta o cavalo do jogador na direção (-1, -2) no estado do jogo."
  (funcall 'move-cavalo -1 -2 estado jogador)
)

(defun move-cavalo-1_2 (estado jogador)
  "Movimenta o cavalo do jogador na direção (-1, 2) no estado do jogo."
  (funcall 'move-cavalo -1 2 estado jogador)
)

(defun move-cavalo_1-2 (estado jogador)
  "Movimenta o cavalo do jogador na direção (1, -2) no estado do jogo."
  (funcall 'move-cavalo 1 -2 estado jogador)
)

(defun move-cavalo_1_2 (estado jogador)
  "Movimenta o cavalo do jogador na direção (1, 2) no estado do jogo."
  (funcall 'move-cavalo 1 2 estado jogador)
)

(defun move-cavalo_2-1 (estado jogador)
  "Movimenta o cavalo do jogador na direção (2, -1) no estado do jogo."
  (funcall 'move-cavalo 2 -1 estado jogador)
)

(defun move-cavalo_2_1 (estado jogador)
  "Movimenta o cavalo do jogador na direção (2, 1) no estado do jogo."
  (funcall 'move-cavalo 2 1 estado jogador)
)
```

##### Esta função retorna uma lista de movimentos possíveis que o cavalo pode fazer.
```
(defun lista-movimentos (estado jogador)
  "Retorna uma lista de funções de movimento que são possíveis para o estado e jogador dados."
  (let ((jogadas-possiveis (calcular-jogadas-possiveis estado jogador)))
    (mapcar #'(lambda (mov)
                (cond
                  ((equal mov '(2 1)) 'move-cavalo_2_1)
                  ((equal mov '(1 2)) 'move-cavalo_1_2)
                  ((equal mov '(-1 2)) 'move-cavalo-1_2)
                  ((equal mov '(-2 1)) 'move-cavalo-2_1)
                  ((equal mov '(-2 -1)) 'move-cavalo-2-1)
                  ((equal mov '(-1 -2)) 'move-cavalo-1-2)
                  ((equal mov '(1 -2)) 'move-cavalo_1-2)
                  ((equal mov '(2 -1)) 'move-cavalo_2-1)))
            jogadas-possiveis))
)
```

##### Esta função configura uma partida de jogo baseando-se no modo de jogo selecionado pelo jogador.
É a partir desta função que se inicia o jogo.
```
(defun configurar-partida ()
  "Configura uma partida de jogo baseando-se no modo de jogo selecionado pelo jogador."
  (let ((modo-de-jogo (selecionar-modo-de-jogo)))
    (cond 
     ((eq modo-de-jogo 1) (iniciar-jogo 1 (tempo-limite 1) (quem-comeca)))
     ((eq modo-de-jogo 2) (iniciar-jogo 2 (tempo-limite 2)))
    ))
)
```

##### Esta função inicia o jogo com as configurações definidas, incluindo o modo de jogo e o limite de tempo.
```
(defun iniciar-jogo (modo-de-jogo tempo-limite &optional (jogador-inicial *jogador1*))
  "Inicia o jogo com as configurações definidas, incluindo o modo de jogo e o limite de tempo."
  (let ((estado (cria-estado (cria-tabuleiro))))
    (let ((estado-com-jogador1 (primeira-jogada estado *jogador1*)))
      (let ((no-final (cria-no (primeira-jogada estado-com-jogador1 *jogador2*))))
        (loop-jogo no-final jogador-inicial tempo-limite modo-de-jogo))))
)
```

##### Esta função determina qual o vencedor do jogo.
```
(defun determinar-vencedor (estado)
  "Determina o vencedor do jogo."
  (cond 
   ((> (nth 1 estado) (nth 2 estado)) (format t "Jogador 1 venceu com ~A pontos!~%" (nth 1 estado)))
   ((> (nth 2 estado) (nth 1 estado)) (format t "Jogador 2 venceu com ~A pontos!~%" (nth 2 estado)))
   (t (format t "Empate!")))
)
```

##### Esta função aplica um movimento ao estado do jogo e retorna o novo estado.
```
(defun aplicar-movimento (estado movimento-func jogador)
  "Aplica um movimento ao estado do jogo e retorna o novo estado."
  (let ((novo-estado (funcall movimento-func estado jogador)))
    (cond 
     (novo-estado novo-estado)
     (t estado)
    ))
)
```

##### Esta função verifica se o jogador é o jogador que está a fazer a jogada.
```
(defun jogador-max (jogador jogador-max)
  "Verifica se o jogador é o jogador que está a fazer a jogada"
  (eq jogador jogador-max)
)
```

##### Esta função retorna o conteúdo da célula na posição (l, c) do tabuleiro.
```
(defun celula (l c tab)
  "Retorna o conteúdo da célula na posição (l, c) do tabuleiro."
  (nth c (linha l tab))  
)
```

##### Esta função retorna a linha de índice l do tabuleiro.
```
(defun linha (l tab)
  "Retorna a linha de índice l do tabuleiro."
  (nth l tab)
)
```

#### Ficheiro Interact.lisp

##### Esta função lê problemas de um arquivo. O arquivo parece conter vários problemas separados por linhas com "-----".
```
(defun ler-problemas (filename)
  "Chama a função para ler os dados do ficheiro"
  (with-open-file (stream filename) (ler-problemas-aux stream))
)
```

##### Esta função apresenta um ecrã que permite visualizar e selecionar o tipo de jogo.
```
(defun selecionar-modo-de-jogo ()
  "Permite selecionar o tipo de jogo"
  (format t "Escolha o modo de jogo:~%")
  (format t "1. Humano vs Computador~%")
  (format t "2. Computador vs Computador~%")
  (let ((escolha (read)))
    (cond
     ((and (> escolha 0) (< escolha 3)) escolha)
     (t (progn
          (format t "Seleção inválida. Tente novamente.~%")
          (selecionar-modo-de-jogo)))
     ))
)
```

##### Esta função apresenta um ecrã que permite selecionar quem começa o jogo.
```
(defun quem-comeca ()
  "Permite selecionar quem começa o jogo"
  (format t "~%Escolha quem pretende que jogue primeiro:~%")
  (format t "1. Você~%")
  (format t "2. Computador~%")
  (let ((escolha (read)))
    (cond
     ((eq 1 escolha) *jogador1*)
     ((eq 2 escolha) *jogador2*)
     (t (progn
          (format t "Seleção inválida. Tente novamente.~%")
          (quem-comeca)))
     ))
)
```

##### Esta função permite ao utilizador definir o tempo limite de jogo.
```
(defun tempo-limite (tipo-jogo)
  "Permite ao utilizador definir o tempo limite de jogo"
  (cond
   ((eq tipo-jogo 1) (format t "~%Define o tempo limite de jogada do computador, de 1000MS a 5000MS:~%"))
   (t (format t "Define o tempo limite do jogo, de 1000MS a 5000MS:~%"))
  )
  (let ((escolha (read)))
    (cond
     ((and (> escolha 999) (< escolha 5001)) escolha)
     (t (progn
          (format t "Seleção inválida. Tente novamente.~%")
          (tempo-limite tipo-jogo)))
     ))
)
```

##### Esta função lista todas as jogadas possíveis.
```
(defun listar-jogadas (jogadas-possiveis indice)
  "Lista as jogadas possíveis."
  (cond 
   ((null jogadas-possiveis) nil)
   (t (progn
        (format t "~A: (~A, ~A)~%" indice (first (first jogadas-possiveis)) (second (first jogadas-possiveis)))
        (listar-jogadas (rest jogadas-possiveis) (1+ indice))))
  )
)
```

##### Esta função permite ao jogador escolher uma das jogadas possíveis.
```
(defun escolher-jogada (jogadas-possiveis)
  "Permite ao jogador escolher uma das jogadas possíveis."
  (format t "~%Jogadas disponiveis:~%")
  (listar-jogadas jogadas-possiveis 1)
  (format t "Escolha uma jogada: ")
  (let ((escolha (read)))
    (cond 
     ((and (integerp escolha) (<= escolha (length jogadas-possiveis)) (> escolha 0))
      (nth (1- escolha) jogadas-possiveis))
     (t (progn
          (format t "Seleção inválida. Tente novamente.~%")
          (escolher-jogada jogadas-possiveis)))
    ))
)
```

##### Esta função executa a jogada para um jogador humano.
```
(defun jogada-humano (no jogador)
  "Executa a jogada para um jogador humano."
  (let ((jogadas-possiveis (calcular-jogadas-possiveis (car no) jogador)))
    (imprime-tabuleiro (caar no))
    (cond
     ((null jogadas-possiveis) (progn (format t "~%Sem movimentos possíveis.~%") (car no)))
     (t (let ((jogada-escolhida (escolher-jogada jogadas-possiveis)))
          (cria-no (move-cavalo (first jogada-escolhida) (second jogada-escolhida) (car no) jogador 1) (cadr no) no)))
    ))
)
```

##### Esta função executa a jogada para um jogador computador.
```
(defun jogada-computador (no jogador tempo)
  "Executa a jogada para um jogador computador"
  (setf *jogada-pc* no)
  (alfabeta no jogador tempo) 
  ;(imprime-no *jogada-pc*)
  (registrar-jogada  (caar *jogada-pc*) jogador)
)
```

##### Esta é uma função auxiliar, que imprime uma linha do tabuleiro.
```
(defun imprimir-linha (linha)
  "Imprime uma linha do tabuleiro."
  (cond 
   ((null linha) (format t "~%"))
   (t (progn
        (format t "~A " (cond ((null (car linha)) "NIL")
                                    (t (car linha))))
             (imprimir-linha (cdr linha))))
   )
)
```

##### Esta função insere uma mensagem e chama a função para imprimir o tabuleiro.
```
(defun imprime-tabuleiro (tabuleiro)
  "Imprime o tabuleiro linha por linha."
  (format t "~%Estado atual do tabuleiro:~%")
  (imprime-tabuleiro-recursivamente tabuleiro)
)
```

##### Esta função imprime o tabuleiro recursivamente.
```
(defun imprime-tabuleiro-recursivamente (tabuleiro)
  "Imprime o tabuleiro recursivamente."
  (cond 
   ((null tabuleiro) nil)
   (t (progn
        (imprimir-linha (car tabuleiro))
        (imprime-tabuleiro-recursivamente (cdr tabuleiro))))
  )
)
```

##### Esta função executa o loop do jogo alternando entre os jogadores.
```
(defun loop-jogo (no jogador tempo-limite &optional modo-de-jogo)
  "Executa o loop do jogo alternando entre os jogadores."
  (cond
   ((eq modo-de-jogo 2) (loop-jogo-pc no jogador tempo-limite))
   ((condicao-vitoria (car no)) (determinar-vencedor (car no)))
   (t (let ((novo-no (cond
                      ((eq jogador *jogador1*) (registrar-jogada-humano (jogada-humano no jogador)))
                      (t (jogada-computador no jogador tempo-limite))))) 
        (loop-jogo novo-no (outro-jogador jogador) tempo-limite)))
  )
)
```

##### Esta função executa o loop do jogo alternando entre os jogadores computador.
```
(defun loop-jogo-pc (no jogador tempo-limite)
  "Executa o loop do jogo alternando entre os jogadores"
  (cond
   ((condicao-vitoria (car no)) (determinar-vencedor (car no)))
   (t (loop-jogo-pc (jogada-computador no jogador tempo-limite) (outro-jogador jogador) tempo-limite))
  )
)
```

##### Esta função regista as jogadas efetuadas por um jogador computador para o ficheiro log.dat.
```
(defun registrar-jogada (tabuleiro jogador)
  "Regista as jogadas efetuadas por um jogador computador para o ficheiro log.dat"
  (let ((diretorio "C:/LispLogs/"))
    (ensure-directories-exist diretorio)
    (let ((caminho-arquivo (concatenate 'string diretorio "log.dat")))
      (with-open-file (stream caminho-arquivo
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (format stream "Jogador: ~A~%Tabuleiro: ~A~%Nós analisados: ~A~%Cortes: ~A~%~%" jogador tabuleiro *nos-analisados* *nos-cortados*))))
  (setf *nos-analisados* 0)
  (setf *nos-expandidos* 0)
  (setf *nos-cortados* 0)
  *jogada-pc*
)
```

##### Esta função regista as jogadas efetuadas por um jogador humano para o ficheiro log.dat.
```
(defun registrar-jogada-humano (no)
  "Regista as jogadas efetuadas por um jogador humano para o ficheiro log.dat"
  (let ((diretorio "C:/LispLogs/"))
    (ensure-directories-exist diretorio)
    (let ((caminho-arquivo (concatenate 'string diretorio "log.dat")))
      (with-open-file (stream caminho-arquivo
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (format stream "Jogador 1~%Tabuleiro: ~A~%~%" (caar no)))))
  no
)
```

##### Esta função apresenta todos os números duplos do tabuleiro e permite ao usuário escolher um.
```
(defun aplicar-regra-duplos (tabuleiro)
  "Apresenta todos os números duplos do tabuleiro e permite ao usuário escolher um."
  (let ((numeros-duplos (coletar-numeros-duplos tabuleiro)))
    (format t "Números duplos encontrados: ~A~%" numeros-duplos)
    (format t "Por favor, insira um número da lista ou pressione Enter para selecionar o maior: ")
    (let ((entrada (read-line)))
      (cond 
       ((string= entrada "") (aplicar-regra-duplos-computador tabuleiro))
       (t (let ((escolha (parse-integer entrada :junk-allowed t)))
            (cond 
             ((find escolha numeros-duplos) (encontrar-posicao-numero tabuleiro escolha))
             (t (encontrar-posicao-numero tabuleiro (reduce #'max numeros-duplos)))
            )))
      )))
)
```

##### Esta função de testes que umprime o resultado da jogada do computador.
```
(defun imprime-no (jogada)
  "Função de testes que umprime o resultado da jogada do computador"
  (format t "Estado atual do nó:~%")
  (imprime-tabuleiro (caar jogada))
  (format t "Pontuação jogador 1: ~A~%" (pontos-jogador (car jogada) *jogador1*))
  (format t "Pontuação jogador 2: ~A~%" (pontos-jogador (car jogada) *jogador2*))
  (format t "Profundidade: ~A~%" (no-profundidade jogada))
  (format t "Expandidos: ~A~%" *nos-expandidos*)
  (format t "Cortados: ~A~%" *nos-cortados*)
  (format t "Analisados ~A~%" *nos-analisados*)
)
```

## Análise de Complexidade

#### Alfabeta:
A complexidade do algoritmo minimax puro, é O(b^d), onde b é o fator de ramificação da árvore e d é a profundidade da árvore .
Com a poda alfa-beta, a complexidade do tempo de pior caso ainda é O(b^d) porque, na pior situação, o algoritmo ainda precisaria examinar todos os nós. No entanto, a poda alfa-beta reduz significativamente o número de nós que precisam ser examinados. No melhor caso, o algoritmo tem uma complexidade de tempo de O(b^(d/2)), o que é uma melhoria exponencial. No caso médio, espera-se que o algoritmo tenha um desempenho em algum lugar entre o melhor e o pior caso, dependendo da eficácia da poda alfa-beta dada a ordem de avaliação dos nós e a disposição do espaço de busca do jogo.
- Melhor caso: O(b^(d/2)).
- Caso médio: Melhor que O(b^d), mas varia dependendo da eficácia da poda.
- Pior caso: O(b^d)

### Complexidade de Espaço

#### Alfabeta:
No algoritmo alfa-beta, a complexidade de espaço é linear em relação à profundidade da árvore. Isso ocorre porque, em qualquer momento durante a execução do algoritmo, é necessário armazenar apenas um único caminho da raiz até o nó atual na árvore, além de uma pequena quantidade de informação adicional para cada nó ao longo desse caminho (como os valores alfa e beta). Portanto, a complexidade de espaço do algoritmo alfa-beta é O(d), onde d é a profundidade da árvore de busca. Isso significa que a quantidade de memória necessária cresce linearmente com o número de movimentos à frente que o algoritmo está avaliando.

## Estrutura de Dados

### Listas
Listas são usadas para representar uma variedade de coleções de dados, como:
- Tabuleiros de Jogo: O tabuleiro do jogo pode ser representado como uma lista de listas, onde cada sublista representa uma linha do tabuleiro.
- Nós: Um nó é composto por uma lista de listas.
- Caminhos ou Sequências de Movimentos: Uma lista pode representar a sequência de movimentos que o cavalo faz no tabuleiro.

A estrutura de dados utilizada foi as listas. Por exemplo, criar um estado cria uma lista quer contem uma lista de listas (tabuleiro) e a pontuação dos jogadores. A função move-cavalo e outras funções relacionadas manipulam estados para realizar movimentos no tabuleiro e calcular pontuações.

## Limitações e Opções Técnicas
Houve um número elevado de limitações durante o desenvolvimento do programa, essas limitações derivadas na dificuldade de implementação do algoritmo do minimax com cortes alfabeta. Para tentar ultrapassar o maior número de limitações possíveis foi utilizado a pesquisa na internet, analisar os laboratórios que foram realizados nas aulas práticas.
No modo de jogo Computador vs Computador, não consegui descobrir o motivo de não conseguir terminar o jogo, os 2 jogadores fazem jogadas até uma certa etapa, posteriormente deixam de conseguir fazer jogadas, mas no modo Humano vs Computador, o algoritmo funciona conforme esperado.

## Requisitos Não Implementados
Não foi feito a parte para poder participar no campeonato.

## Conclusão
O projeto focado no "Jogo do Cavalo" proporcionou uma oportunidade valiosa para aplicar conceitos avançados de Inteligência Artificial e programação em Lisp, destacando-se a implementação do algoritmo AlfaBeta. Este algoritmo demonstrou ser uma escolha eficaz para a tomada de decisões estratégicas no jogo, permitindo ao computador competir de maneira eficiente contra um oponente humano ou outro computador.
Ao longo do projeto, foram enfrentados diversos desafios, como a representação eficiente do tabuleiro e dos estados do jogo, a manipulação de listas em Lisp, e a implementação de estratégias de corte e avaliação de movimentos. A complexidade temporal e espacial do algoritmo AlfaBeta foi cuidadosamente analisada, garantindo que o programa fosse não apenas eficiente, mas também capaz de tomar decisões rápidas dentro do limite de tempo estabelecido.
Através deste projeto, foi possível aprofundar o entendimento sobre a Teoria de Jogos e a aplicação de algoritmos de procura em espaço de estados. A implementação em Lisp permitiu explorar um paradigma de programação funcional, que se mostrou adequado para o problema em questão devido à sua capacidade de manipulação de listas e estruturas de dados complexas.
A experiência adquirida neste projeto é inestimável, pois além de aprimorar habilidades técnicas, proporcionou uma visão prática de como a Inteligência Artificial pode ser aplicada em contextos de jogos e simulação. O sucesso do algoritmo AlfaBeta em realizar jogadas inteligentes e sua capacidade de adaptar-se a diferentes situações no jogo demonstraram o potencial da IA em ambientes dinâmicos e imprevisíveis.
Em conclusão, este projeto foi um excelente exercício para a aplicação prática de conceitos de Inteligência Artificial, programação em Lisp e Teoria de Jogos, resultando em um programa capaz de jogar o "Jogo do Cavalo" de maneira eficiente e estratégica, tanto contra humanos quanto contra outros programas de computador.
