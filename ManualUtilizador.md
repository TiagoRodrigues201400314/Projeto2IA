# Manual do Utilizador

## Objetivos do Programa

Objetivos do Programa

O programa desenvolvido tem como principais objetivos:
1. Implementar o algoritmo alfaBeta, é uma estratégia de decisão em jogos de dois jogadores. Este algoritmo é crucial para criar um adversário automatizado desafiador, capaz de tomar decisões estratégicas inteligentes durante o jogo.
2. Permitir jogos em dois modos diferentes, Humano vs Computador e Computador vs Computador. No primeiro modo, um jogador humano compete contra o computador, e no segundo, o programa permite que dois computadores joguem entre si. Isso proporciona não só um desafio para os jogadores humanos mas também uma plataforma para testar e avaliar a eficácia das estratégias de IA implementadas.
3. Proporcionar uma experiência de jogo desafiadora e educativa, mas também uma oportunidade de aprendizado sobre algoritmos de inteligência artificial e estratégias de jogo.
   
## Funcionamento Geral

1. Quando o jogo é iniciado, o utilizador pode escolher entre diferentes modos de jogo (Humano vs Computador ou Computador vs Computador). Dependendo do modo selecionado, o programa inicia o ambiente de jogo, que inclui a criação de um tabuleiro e a definição dos jogadores.
2. O programa gera um tabuleiro de jogo, que é uma matriz de números, representando pontos. Cada jogador (humano ou computador) começa o jogo posicionando seu cavalo em uma posição estratégica na primeira ou última linha do tabuleiro, dependendo de qual jogador eles são.
3. Durante o jogo, os jogadores se revezam para mover seus cavalos. O cavalo se move em um padrão em "L" (como no xadrez), e o jogador ganha pontos com base no valor da célula em que o cavalo pousa. O jogo também inclui regras especiais, como números simétricos e duplos, que afetam o tabuleiro e a estratégia de jogo.
4. Quando é a vez do computador jogar, o programa utiliza o algoritmo AlfaBeta para decidir o melhor movimento. Este algoritmo analisa as possíveis jogadas, prevendo os movimentos futuros do adversário e escolhendo a jogada que maximiza a chance de ganhar, enquanto minimiza as possíveis respostas do oponente. Durante este processo, o algoritmo efetua cortes alfa-beta para reduzir o número de caminhos analisados, aumentando a eficiência.
5. No modo Humano vs Computador, o usuário insere sua jogada por meio do teclado, e o computador responde com sua jogada, calculada pelo algoritmo AlfaBeta. No modo Computador vs Computador, ambos os jogadores são controlados pelo computador, e o jogo se desenrola automaticamente.
6. O jogo continua até que um dos jogadores ganhe, o que ocorre quando um dos jogadores não pode mais fazer movimentos válidos ou quando as condições de vitória pré-definidas são atendidas. O programa então determina o vencedor com base nos pontos acumulados ou outros critérios de vitória estabelecidos.
7. Ao longo do jogo, o programa registra várias métricas, como o número de nós analisados, cortados e o tempo gasto em cada jogada. Estes dados são úteis para análise posterior, especialmente para avaliar a eficácia e eficiência do algoritmo AlfaBeta.
8. Após cada jogada, o programa fornece feedback visual e textual ao usuário, mostrando o estado atual do tabuleiro, a pontuação e quaisquer outras informações relevantes.

## Como Usar

Para iniciar o jogo é necessário apenas inserir o comando "(configurar-partida)"

![image](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/cc3c0a0d-f551-47a3-a3cd-e31e58d815a7)

Ao inserir o comando é apresentado no ecrã as duas possibilidades de jogo.
Deverá escrever 1 ou 2 dependendo da escolha que pretender.

![image](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/9fc8270c-90e6-4111-8152-42b39cb65adc)

Ao inserir a escolha é apresentado uma mensagem para introduzir o tempo limite que o computador tem para jogar.
Inserir um numero entre 1000 a 5000, que corresponde ao tempo em milissegundos.

![image](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/a8a78af2-0920-4a73-a695-f999f20cee69)

De seguida é apresentado no ecrã a perguntar quem pretende que começe a jogar.
Deverá escrever 1 ou 2 dependendo da sua escolha.

![image](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/6622d953-9f48-43cf-b335-6cbb9bedea87)

Agora o jogo já começou!
É apresentado no ecrã o estádo atual do tabuleiro e as jogadas disponiveis que tem para jogar, essa lista de jogadas apenas aparece na vez do humano jogar.
Escreva o numero da jogada que pretender, por exemplo, das opções da imagem, para selecionar a jogada (2, 1), que move o cavalo duas linhas para baixo e uma coluna para o lado direiro, tem de escrever 1.
No tabuleiro o jogador 1 é representado como -1, e o jogador 2 como -2.

![image](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/2f8e3d61-37cc-4533-9c42-5f5f53c7eef3)

Depois de selecionar a jogada, o jogador computador, faz a jogada dele, como poderá observar na proxima imagem, que o jogador 2 já moveu o cavalo

![image](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/919754c3-4d76-4819-8593-4e25e23b993d)

O processo se repete até que nenhum dos jogadores se consigam movimentar mais, e quando isso acontece e mostrado uma mensagem a dizer qual foi o jogador que ganhou e a respetiva pontuação.

![Sem título](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/b80f4be7-7739-4d41-a090-e8c513ec1a2d)

Para o modo de jogo Computador vs Computado, quando começar o jogo neste modo, terá que esperar um pouco e quando terminarem o jogo é apresentado a mensagem de quem ganhou.

![image](https://github.com/TiagoRodrigues201400314/Manual-de-Utilizador/assets/100838766/c29911fe-cb95-466d-94b6-b63598e69104)

## Ficheiro de Logs

Será criado um ficheiro log.dat na seguinte localização:
```
C:\LispLogs\
```

Esse ficheiro contem toda a informação das jogadas efetuadas.

O ficheiro tem este formato:

![image](https://github.com/TiagoRodrigues201400314/Projeto2IA/assets/100838766/3885e337-8ba4-4f2a-bef3-4c816dffc5c3)
