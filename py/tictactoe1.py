"""
Exercício 7.10 Escreva um jogo da velha para dois jogadores. O jogo deve
perguntar onde você quer jogar e alternar entre os jogadores. A cada jogada,
verifique se a posição está livre. Verifique também quando um jogador venceu
a partida. Um jogo da velha pode ser visto como uma lista de 3 elementos, onde
cada elemento é outra lista, também com três elementos.
Exemplo do jogo:
X | O |
---+---+---
| X | X
---+---+---
|
| O
Onde cada posição pode ser vista como um número. Confira abaixo um exemplo
das posições mapeadas para a mesma posição de seu teclado numérico.
7 | 8 | 9
---+---+---
4 | 5 | 6
---+---+---
1 | 2 | 3
"""

P = [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]]
TRAD = [(2, 0), (2, 1), (2, 2), (1, 0), (1, 1), (1, 2), (0, 0), (0, 1), (0, 2)]
VEZ = "X"
TEM_VENCEDOR = False
while True:
    posicao = int(input(f"Posição para colocar o {VEZ}: "))
    # checar entrada
    if posicao < 1 or posicao > 9:
        print("Posição entre 1 e 9.")
        continue
    # checar casa ocupada
    if P[TRAD[posicao - 1][0]][TRAD[posicao - 1][1]] != " ":
        print(f"A casa {posicao} já está ocupada.")
        continue
    # preencher casa
    P[TRAD[posicao - 1][0]][TRAD[posicao - 1][1]] = VEZ
    # exibir tabuleiro
    tabuleiro = f"""
+---+---+---+
| {P[0][0]} | {P[0][1]} | {P[0][2]} |
+---+---+---+
| {P[1][0]} | {P[1][1]} | {P[1][2]} |
+---+---+---+
| {P[2][0]} | {P[2][1]} | {P[2][2]} |
+---+---+---+
"""
    print(tabuleiro)
    # verificar se há vencedor
    res = [
        P[0][0] + P[0][1] + P[0][2],
        P[1][0] + P[1][1] + P[1][2],
        P[2][0] + P[2][1] + P[2][2],
        P[0][0] + P[1][0] + P[2][0],
        P[0][1] + P[1][1] + P[2][1],
        P[0][2] + P[1][2] + P[2][2],
        P[0][0] + P[1][1] + P[2][2],
        P[0][2] + P[1][1] + P[2][0],
    ]
    for e in res:
        if e in ("XXX", "OOO"):
            print(f"O vencedor é {VEZ}!")
            TEM_VENCEDOR = True
            break
    if TEM_VENCEDOR:
        break
    # verificar se dá para continuar jogando e troca jogador
    if " " in "".join(P[0] + P[1] + P[2]):
        if VEZ == "X":
            VEZ = "O"
        else:
            VEZ = "X"
        continue
    # se chegou não há mais casas para jogar
    print("Jogo empatado!")
    break

