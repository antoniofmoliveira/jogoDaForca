"""
Exercício 9.34 Altere o programa da listagem 7.45, o jogo da forca. Dessa vez, utilize
as funções de tempo para cronometrar a duração das partidas.

Exercício 9.15 Altere o programa da listagem 7.5, o jogo da forca. Utilize um arqui-
vo em que uma palavra seja gravada a cada linha. Use um editor de textos para
gerar o arquivo. Ao iniciar o programa, utilize esse arquivo para carregar a lista de
palavras. Experimente também perguntar o nome do jogador e gerar um arquivo
com o número de acertos dos cinco melhores.

Exercício 7.9 Modifique o programa para utilizar listas de strings para
desenhar o boneco da forca. Você pode utilizar uma lista para cada linha e
organizá-las em uma lista de listas. Em vez de controlar quando imprimir cada
parte, desenhe nessas listas, substituindo o elemento a desenhar.
"""
import random
import time

lista_palavras = []
with open("palavrasforca.txt", "r", encoding="utf-8") as arquivo:
    for linha in arquivo.readlines():
        lista_palavras.append(linha)
nome = input("Digite o nome do jogador: ")
patibulo = ["X==:==", "X  :  ", "X", "X", "X", "X", "==========="]
indice = random.randint(0, len(lista_palavras))
palavra = lista_palavras[indice].lower().strip()
for x in range(100):
    print()
digitadas = []
acertos = []
ERROS = 0
INICIO_JOGO = time.time()
while True:
    SENHA = ""
    for letra in palavra:
        SENHA += letra if letra in acertos else "."
    print(SENHA)
    if SENHA == palavra:
        print("Você acertou!")
        break
    tentativa = input("\nDigite uma letra:").lower().strip()
    if tentativa in digitadas:
        print("Você já tentou esta letra!")
        continue
    # else:
    digitadas += tentativa
    if tentativa in palavra:
        acertos += tentativa
    else:
        ERROS += 1
        print("Você errou!")
    if ERROS == 1:
        patibulo[2] = "X  O  "
    elif ERROS == 2:
        patibulo[3] = "X  |  "
    elif ERROS == 3:
        patibulo[3] = "X /|  "
    elif ERROS == 4:
        patibulo[3] = "X /|\\ "
    elif ERROS == 5:
        patibulo[4] = "X /   "
    elif ERROS == 6:
        patibulo[4] = "X / \\"
    for e in patibulo:
        print(e)
    if ERROS == 6:
        print(f"Enforcado! A palavra é '{palavra}'.")
        break

FINAL_JOGO = time.time()
DURACAO_JOGO = int(FINAL_JOGO - INICIO_JOGO)
minutos = DURACAO_JOGO // 60
segundos = DURACAO_JOGO % 60
print(f"O jogo durou {minutos:d} minutos e {segundos:d} segundos.")

if ERROS < 6:
    lista_recordes = {}
    with open("forcarecordes.txt", "r", encoding="utf-8") as recordes:
        for linha in recordes.readlines():
            dados = linha.split(sep=";")
            lista_recordes[dados[0]] = int(dados[1])
    lista_recordes[nome] = ERROS
    lista_recordes = dict(sorted(lista_recordes.items(), key=lambda item: item[1]))
    with open("forcarecordes.txt", "w", encoding="utf-8") as recordes:
        i = 0
        for k, v in lista_recordes.items():
            if i < 5:
                recordes.write(f"{k:s};{v:d}\n")
            i += 1

print("\nRecordistas")
print("============")
with open("forcarecordes.txt", "r", encoding="utf-8") as recordes:
    for linha in recordes.readlines():
        dados = linha.split(sep=";")
        print(f"{dados[0]:20s} {int(dados[1]):d}")
