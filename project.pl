% Joao Afonso Silva

:- [codigo_comum].

% ------------------------------------------------------------------------------
% 3.1.1   combinacoes_soma(N, Els, Soma, Combs)
% ------------------------------------------------------------------------------

/* Combs e' o resultado de "agrupar" todas as combinacoes possiveis atraves de bagof */
combinacoes_soma(N, Els, Soma, Combs) :-
    bagof(Comb, (combinacao(N, Els, Comb), soma(Comb, Soma)), Combs).

/* Soma e' a soma de todos os elementos de uma lista de numeros */
soma([], 0).
soma([P|R], Soma) :-
    soma(R, SomaRec),
    Soma is SomaRec + P.


% ------------------------------------------------------------------------------
% 3.1.2   permutacoes_soma(N, Els, Soma, Perms)
% ------------------------------------------------------------------------------

/* Perms sao as permutacoes ordenadas das combinacoes  */
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    bagof(Perm, permutacoes_cada_lista(Combs, Perm), Aux),
    append(Aux, AuxAlisado),
    sort(AuxAlisado, Perms).

/* "agrupa" em Perms todas as solucoes (permutacoes de cada combinacao) */
permutacoes_cada_lista(Combs, Perms) :-
    bagof(Perm, (member(L, Combs), permutation(L, Perm)), Perms).


% ------------------------------------------------------------------------------
% 3.1.3   espaco_fila(Fila, Esp, H_V)
% ------------------------------------------------------------------------------

/* se o espaco so tem barras */
espaco_fila(Fila, _, _) :- 
    so_tem_barras(Fila),
    !,
    fail.
/* se o espaco nao e' constituido so por barras mas e' horizontal */
espaco_fila(Fila, Esp, h) :-
    !,
    append([Pref, Aux, Suf], Fila), /* o espaco e' uma truncacao de fila */
    length(Aux, Len), Len >= 1,     /* com comprimento maior ou igual a 1 */
    nao_tem_barra(Aux),
    parede_esquerda(Pref),          /* Pref tem de obedecer 'as regras de parede esquerda */
    parede_direita(Suf),            /* e Suf tem de obedecer 'as regras de parede direita */
    last(Pref, Ultimo),             /* a parede direita tem de ter [_H] como ultimo elemento */
    Ultimo = [_,H],
    faz_espaco(H, Aux, Esp).        /* construtor de espaco */
/* (else) se o espaco nao e' constituido so por barras mas e' vertical */
espaco_fila(Fila, Esp, v) :-
    append([Pref, Aux, Suf], Fila),
    length(Aux, Len), Len >= 1,
    nao_tem_barra(Aux),
    parede_esquerda(Pref),
    parede_direita(Suf),
    last(Pref, Ultimo),
    Ultimo = [V,_],
    faz_espaco(V, Aux, Esp).

/* construtor da estrutura espaco */
faz_espaco(N, Esp, espaco(N, Esp)).

/* seletores da estrutura espaco  */
get_soma(espaco(N, _), N).
get_espaco(espaco(_, Esp), Esp).

/* para nao ter barras uma lista nao pode conter um elemento que seja uma lista */
nao_tem_barra([]).
nao_tem_barra([P|R]) :-
    \+is_list(P),
    nao_tem_barra(R).

/* para so ter barras todos os elementos de uma lista 
tem de ser listas ou entao a lista tem se ser vazia*/
so_tem_barras([]).
so_tem_barras([P|R]) :-
    is_list(P),
    so_tem_barras(R).

/* para ser uma parede esquerda o ultimo elemento  da lista 
tem de ser uma lista ou entao a lista tem de ser vazia */
parede_esquerda([]).
parede_esquerda(L) :-
    last(L, Ultimo),
    is_list(Ultimo).

/* para ser uma parede direita o primeiro elemento da lista
tem de ser uma lista ou entao a lista tem de ser vazia  */
parede_direita([]).
parede_direita(L) :-
    nth0(0, L, Primeiro),
    is_list(Primeiro).


% ------------------------------------------------------------------------------
% 3.1.4   espacos_fila(H_V, Fila, Espacos)
% ------------------------------------------------------------------------------

/* se a fila so tem barras entao o espaco e' vazio */
espacos_fila(_, Fila, []) :- 
    so_tem_barras(Fila),
    !.
/* else obtenho todas as solucoes que satisfazem a condicao de 
espaco retirando espacos potencialmente vazios */
espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos_Aux),
    exclude(==([]), Espacos_Aux, Espacos).


% ------------------------------------------------------------------------------
% 3.1.5   espacos_puzzle(Puzzle, Espacos)
% ------------------------------------------------------------------------------

/* obter todos os espacos verticais e horizontais do puzzle */
espacos_puzzle(Puzzle, Espacos) :-
    mat_transposta(Puzzle, PuzzleTransposto),
    espacos_puzzle_aux(Puzzle, [], EspacosNormal, h),
    espacos_puzzle_aux(PuzzleTransposto, [], EspacosTransposto, v),
    append([EspacosNormal, EspacosTransposto], Espacos).

/* de forma iterativa obter os espacos do puzzle */
espacos_puzzle_aux([], Espacos, Espacos, _).
espacos_puzzle_aux([P|R], Ac, Espacos, H_V) :-
    espacos_fila(H_V, P, EspacosIntermedios),
    append([Ac, EspacosIntermedios], NovoAc),
    espacos_puzzle_aux(R, NovoAc, Espacos, H_V).


% ------------------------------------------------------------------------------
% 3.1.6   espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% ------------------------------------------------------------------------------

/* obtem todos os espacos em Esps com posicoes comuns a Esp excetuando Esp */
espacos_com_posicoes_comuns(Esps, Esp, Esps_com) :-
    espacos_com_posicoes_comuns_aux(Esps, Esp, [], Esps_com_aux),
    exclude(==(Esp), Esps_com_aux, Esps_com).

/* de forma iterativa ir obter os espacos com posicoes comuns */
espacos_com_posicoes_comuns_aux([], _, Esps_com, Esps_com).
/* se Espaco atual tem variaveis em comum com Esp */
espacos_com_posicoes_comuns_aux([espaco(N, Espaco)|R], espaco(NE, Esp), Ac, Esps_com) :-
    \+ \+tem_variaveis_em_comum(Espaco, Esp), /* a dupla negacao evita a unificacao. Obriga a retornar true ou false. */
    !, 
    append([Ac, [espaco(N, Espaco)]], NovoAc),
    espacos_com_posicoes_comuns_aux(R, espaco(NE, Esp), NovoAc, Esps_com).
/* (else) se Espaco atual nao tem variaveis em comum com Esp */
espacos_com_posicoes_comuns_aux([_|R], Esp, Ac, Esps_com) :-
    espacos_com_posicoes_comuns_aux(R, Esp, Ac, Esps_com).

/* para um espaco ter variaveis em comum com outro testar se pelo menos um elemento
do primeiro para alem de ser uma variavel tambem e' elemento do segundo */
tem_variaveis_em_comum([P|_], Esp2) :-
    pertence(P, Esp2),
    var(P).
tem_variaveis_em_comum([_|R], Esp2) :-
    tem_variaveis_em_comum(R, Esp2).

pertence(El, [P|_]) :-
    El == P.
pertence(El, [_|R]) :-
    pertence(El, R).


% ------------------------------------------------------------------------------
% 3.1.7   permutacoes_soma_espacos(Espacos, Perms_soma)
% ------------------------------------------------------------------------------

permutacoes_soma_espacos(Espacos, Perms_soma) :-
    permutacoes_soma_espacos_aux(Espacos, [], Perms_soma).

/* de forma iterativa, para todos os espacos obter a sua lista de permutacoes possiveis */
permutacoes_soma_espacos_aux([], Perms_soma, Perms_soma).
permutacoes_soma_espacos_aux([espaco(Soma, Esp)|R], Ac, Perms_soma) :-
    length(Esp, N),  /* combinacoes de N a N */
    permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
    append([[espaco(Soma, Esp)], [Perms]], EspacoEPerms),
    append([Ac, [EspacoEPerms]], NovoAc),
    permutacoes_soma_espacos_aux(R, NovoAc, Perms_soma).


% ------------------------------------------------------------------------------
% 3.1.8   permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% ------------------------------------------------------------------------------

permutacao_possivel_espaco(Perm, espaco(Soma, Esp), Espacos, Perms_soma) :-
    /* obter as permutacoes possiveis para o espaco em causa, Esp */
    permutacoes_do_espaco(espaco(Soma, Esp), Perms_soma, Permutacoes),
    /* testar cada permutacao possivel para o espaco. Se falhar faz backtrack. */
    member(Perm, Permutacoes),
    length(Perm, Len),
    length(Esp, Len),
    /* obter os espacos com posicoes comuns ao espaco em causa */
    espacos_com_posicoes_comuns(Espacos, espaco(Soma, Esp), Esps_com),
    /* testar se a unificacao da permutacao Perm com o espaco em causa Esp nao criaria preenchimentos 
    dos espacos com posicoes comuns a Esp que seriam impossiveis de concretizar. */
    espacos_possiveis(Perm, Esp, Esps_com, Perms_soma).

permutacoes_do_espaco(espaco(Soma, Esp), Perms_soma, Permutacoes) :-
    member(L, Perms_soma),
    L = [espaco(Soma, Esp), Permutacoes].

/* para testar se os espacos resultantes da unificacao de Perm com Esp sao possiveis e' preciso 
testar para cada espaco resultante se este tem pelo menos uma permutacao que unifique com ele, 
ja com os elementos "novos" resultantes da unificacao de Perm com Esp */    
espacos_possiveis(_, _, [], _).
espacos_possiveis(Perm, Esp, [espaco(Soma, Espaco)|R], Perms_soma) :-
    permutacoes_do_espaco(espaco(Soma, Espaco), Perms_soma, Permutacoes),
    Perm = Esp,  /* unificacao para obter os elementos "novos" que vao ser testados */
    /* para cada espaco verificar se e' possivel */
    espaco_possivel(Espaco, Permutacoes),
    espacos_possiveis(Perm, Esp, R, Perms_soma).

/* um espaco e' possivel se unificar com pelo menos uma das permutacoes a que lhe correspondem */
espaco_possivel(Espaco, [P|_]) :-
    \+ \+Espaco = P,  /* de novo a dupla negacao da unificacao simula uma "falsa unificacao", */
    !.                /* ou seja, se for possivel unificar sucede sem unificar, c.c. falha. */
espaco_possivel(Espaco, [_|R]) :-
    espaco_possivel(Espaco, R).


% ------------------------------------------------------------------------------
% 3.1.9   permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% ------------------------------------------------------------------------------

/* obtem todas as permutacoes possiveis para um espaco */
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    get_espaco(Esp, Espaco),
    findall(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Aux),
    sort(Aux, Aux2),  /* remove solucoes repetidas */
    append([[Espaco], [Aux2]], Perms_poss).


% ------------------------------------------------------------------------------
% 3.1.10   permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% ------------------------------------------------------------------------------

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_possiveis_espacos_aux(Espacos, Espacos, [], Perms_poss_esps).

/* obtem de forma iterativa todas as permutacoes possiveis para todos os espacos */
permutacoes_possiveis_espacos_aux(_, [], Perms_poss_esps, Perms_poss_esps) :-
    !.
permutacoes_possiveis_espacos_aux(Espacos, [PEspaco|REspacos], Ac, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    permutacoes_possiveis_espaco(Espacos, Perms_soma, PEspaco, Perms_poss),
    append([Ac, [Perms_poss]], NovoAc),
    permutacoes_possiveis_espacos_aux(Espacos, REspacos, NovoAc, Perms_poss_esps).


% ------------------------------------------------------------------------------
% 3.1.11   numeros_comuns(Lst_Perms, Numeros_comuns)
% ------------------------------------------------------------------------------

numeros_comuns([PPerm|RPerms], Numeros_comuns) :-
    numeros_comuns_aux([PPerm|RPerms], PPerm, 1, [], Numeros_comuns),
    !.

/* obtem, de forma iterativa, as listas de posicao-numero comuns aos espacos */
numeros_comuns_aux(_, [], _, Numeros_comuns, Numeros_comuns).
/* se o elemento no indice atual e' igual para todas as listas adicionar ao acumulador */
numeros_comuns_aux(Perms, [El|R], AcIndice, Ac, Numeros_comuns) :-
    elemento_do_indice_e_igual(El, AcIndice, Perms),
    append([Ac, [(AcIndice, El)]], NovoAc),
    NovoAcIndice is AcIndice + 1,
    numeros_comuns_aux(Perms, R, NovoAcIndice, NovoAc, Numeros_comuns).
/* se o elemento no indice atual nao e' igual para todas as listas passar 'a frente */
numeros_comuns_aux(Perms, [El|R], AcIndice, Ac, Numeros_comuns) :-
    \+ elemento_do_indice_e_igual(El, AcIndice, Perms),
    NovoAcIndice is AcIndice + 1,
    numeros_comuns_aux(Perms, R, NovoAcIndice, Ac, Numeros_comuns).

/* testar por recursao se para todas as listas os elementos na posicao indice sao iguais */
elemento_do_indice_e_igual(_, _, []) :-
    !.
elemento_do_indice_e_igual(N, Indice, [P|R]) :-
    nth1(Indice, P, El),
    El == N,
    elemento_do_indice_e_igual(N, Indice, R).


% ------------------------------------------------------------------------------
% 3.1.12   atribui_comuns(Perms_Possiveis)
% ------------------------------------------------------------------------------

atribui_comuns([]) :-
    !. 
/* se a lista de permutacoes possiveis para um espaco so tem um elemento entao unifica a permutacao com o espaco */
atribui_comuns([PPerm|RPerms]) :-
    PPerm = [Espaco, Perms],
    length(Perms, Len), Len =:= 1,
    nth1(1, Perms, Perm),
    Espaco = Perm,
    atribui_comuns(RPerms).
/* se a lista de permutacoes possiveis tiver mais de uma permutacao possivel entao 
obtem os elementos comuns a todas as permutacoes e insere-os no espaco Espaco. */
atribui_comuns([PPerm|RPerms]) :-
    PPerm = [Espaco, Perms],
    length(Perms, Len), Len > 1,
    numeros_comuns(Perms, Numeros_comuns),
    poe_comuns(Espaco, Numeros_comuns),  /* insere elementos comuns no espaco */
    atribui_comuns(RPerms).

/* dados os indices e os elementos comuns a todas as permutacoes insere-os no espaco atraves de unificacao */
poe_comuns(_, []) :-
    !.
poe_comuns(Espaco, [PNumero|RNumeros]) :-
    PNumero = (Indice, N),
    nth1(Indice, Espaco, El),
    El = N,  /* inserir o elemento no espaco atraves de unificacao */
    poe_comuns(Espaco, RNumeros).


% ------------------------------------------------------------------------------ 
% 3.1.13   retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% ------------------------------------------------------------------------------

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    retira_impossiveis_aux(Perms_Possiveis, [], Novas_Perms_Possiveis).

/* de forma iterativa retira permutacoes que deixaram de ser possiveis */
retira_impossiveis_aux([], Novas_Perms_Possiveis, Novas_Perms_Possiveis).
retira_impossiveis_aux([PPerm|RPerms], Ac, Novas_Perms_Possiveis) :-
    PPerm = [Espaco, Perms],
    include(unifica(Espaco), Perms, NovasPerms),  /* "filtra" as permutacoes deixando apenas aquelas */
    append([[Espaco], [NovasPerms]], NovaPerm),   /* que continuam a unificar com os espaco */
    append([Ac, [NovaPerm]], NovoAc),
    retira_impossiveis_aux(RPerms, NovoAc, Novas_Perms_Possiveis).

/* Verdadeiro se a unificacao e' possivel (mas nao faz unificacao). Falso c.c..  */
unifica(Espaco, Perm) :-
    \+ \+Espaco = Perm.


% ------------------------------------------------------------------------------
% 3.1.14   simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% ------------------------------------------------------------------------------

/* se apos simplificar nao ha alteracoes chega ao fim da recursao (caso de paragem) */
simplifica(Perms_Possiveis, Perms_Possiveis) :-
    simplifica_algoritmo(Perms_Possiveis, Novas_Perms_Possiveis),
    Novas_Perms_Possiveis == Perms_Possiveis,
    !.
/* else ainda nao terminou a recursao. Invoca de novo os passos que compoem o algoritmo
de simplificacao e volta a chamar simplifica. Se nao houver alteracoes a recursao para.  */
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    simplifica_algoritmo(Perms_Possiveis, Perms_Aux),
    simplifica(Perms_Aux, Novas_Perms_Possiveis).

/* o algoritmo de simplificacao e' composto por atribuir os numeros comuns e
retirar os espacos que passaram a ser impossiveis com essas atribuicoes. */
simplifica_algoritmo(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis), 
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis). 


% ------------------------------------------------------------------------------
% 3.1.15   inicializa(Puzzle, Perms_Possiveis)
% ------------------------------------------------------------------------------

/* a inicializacao e' composta pela obtencao dos espacos, 
pela obtencao das permutacoes para os espacos e pela simplificacao */
inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_aux),
    simplifica(Perms_Possiveis_aux, Perms_Possiveis).


% ------------------------------------------------------------------------------
% 3.2.1   escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% ------------------------------------------------------------------------------

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    include(mais_de_uma_permutacao, Perms_Possiveis, Perms_Aux),  /* obter uma lista com todos os espacos-permutacoes com mais de uma permutacao */
    maplist(nth1(2), Perms_Aux, Permutacoes),                     /* obter uma lista com apenas as listas de permutacoes de todos os espacos */
    findall(Len, (member(El, Permutacoes), length(El, Len)), Tamanhos),  /* obter uma lista com o numero de permutacoes para cada espaco */
    min_list(Tamanhos, Min),           /* obter o numero minimo de permutacoes de entre as listas de permutacoes para os espacos */
    nth1(Indice, Tamanhos, Min),       /* obter o indice do espaco-permutacoes com menor numero de permutacoes */
    nth1(Indice, Perms_Aux, Escolha),  /* escolha e' a permutacao cujo indice corresponde ao numero minimo de permutacoes */
    !.

/* testa se a lista de permutacoes para um espaco so tem uma permutacao */
mais_de_uma_permutacao(Perm) :-
    Perm = [_, Perms],
    length(Perms, Len), Len > 1.


% ------------------------------------------------------------------------------
% 3.2.2   experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
% ------------------------------------------------------------------------------

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    experimenta_perm_aux(Escolha, Perms_Possiveis, [], Novas_Perms_Possiveis).

experimenta_perm_aux(_, [], Novas_Perms_Possiveis, Novas_Perms_Possiveis).
/* se escolha corresponder ao elemento atual entao unificar Esp com Perm e subsituir 
na lista de permutacoes a lista antiga pela nova lista resultante da unificacao */
experimenta_perm_aux(Escolha, [PPerm|RPerms], Ac, Novas_Perms_Possiveis) :-
    PPerm == Escolha,
    Escolha = [Esp, Lst_Perms],
    member(Perm, Lst_Perms),
    Esp = Perm,
    NovaPerm = [Esp, [Perm]],
    append([Ac, [NovaPerm]], NovoAc),
    experimenta_perm_aux(Escolha, RPerms, NovoAc, Novas_Perms_Possiveis). /* continuar a recursao para preencher o acumulador ate ao fim */
/* se a escolha nao corresponder ao elemento atual passar 'a frente */
experimenta_perm_aux(Escolha, [PPerm|RPerms], Ac, Novas_Perms_Possiveis) :-
    PPerm \== Escolha,
    append([Ac, [PPerm]], NovoAc),
    experimenta_perm_aux(Escolha, RPerms, NovoAc, Novas_Perms_Possiveis).    


% ------------------------------------------------------------------------------
% 3.2.3   resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% ------------------------------------------------------------------------------

/* se o puzzle esta resolvido terminar a recursao */
resolve_aux(Perms_Possiveis, Perms_Possiveis) :-
    esta_resolvido(Perms_Possiveis),
    !.
/* se o puzzle nao esta resolvido repete os passos que o resolvem */
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    \+ esta_resolvido(Perms_Possiveis),
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis, Perms_Possiveis_Aux1),
    simplifica(Perms_Possiveis_Aux1, Perms_Possiveis_Aux2),
    resolve_aux(Perms_Possiveis_Aux2, Novas_Perms_Possiveis).

/* o puzzle esta resolvido quando as listas de permutacoes para todos os espacos medem um */
esta_resolvido(Perms_Possiveis) :-
    maplist(nth1(2), Perms_Possiveis, Perms),
    maplist(comprimento_um, Perms).

/* testa se o comprimento de uma lista e' um */
comprimento_um(Lista) :-
    length(Lista, 1).


% ------------------------------------------------------------------------------
% 3.3.1   resolve(Puz)
% ------------------------------------------------------------------------------

/* resolver o puzzle corresponde a inicializa-lo e a chamar o algoritmo de resolucao */
resolve(Puz) :-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).
